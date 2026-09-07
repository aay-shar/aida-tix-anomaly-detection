# =====================================================================
#
# Run AFTER 01_preprocessing.R AND 02_aida in the SAME R session.
# Assumes `out` (from sheet 1) and `scores` (the AIDA score vector from
# sheet 2, section 10) are already in the workspace.
#
# IMPORTANT: `scores` must be the AIDA scores (primary method), not the
# isolation.forest baseline. If you ran the iforest block in 02_aida after the
# AIDA re-import, re-run the re-import so `scores` holds the AIDA values before
# running this sheet.
#
# Like AIDA, this runs in two phases around an external binary:
#   11a. EXPORT outlier indices -> aida_run/claims_input_outliers.dat
#        >> run the TIX binary now to produce aida_run/claims_tix.dat
#   11b. RE-IMPORT claims_tix.dat for the explanations (auto-runs once the
#        file exists; the inner file.exists() guard handles the ordering).
# =====================================================================

# --- FLAGS ---
RUN_TIX <- FALSE  # set TRUE to export outlier indices / read back TIX output
if (!exists("out"))    stop("`out` not found -- run 01_preprocessing.R first (same session).")
if (!exists("scores_aida")) stop("`scores` not found -- run 02_aida (section 10 re-import) first (same session).")

# 11. TIX EXPORT + EXPLAIN   [optional -- guarded by RUN_TIX]
# =====================================================================
# Two-step, like AIDA. First half writes the outlier-index file; then run the
# TIX binary to produce aida_run/claims_tix.dat; the second half re-imports it.
# Requires `scores` from section 10 (AIDA) to be in scope.

if (isTRUE(RUN_TIX)) {

  #exporting outliers file from R
  # How many top anomalies to explain
  k <- 100

  # Get the ROW POSITIONS (not sorted) of the top-k by AIDA score.
  # scores is your AIDA score vector, aligned to out$X row order.
  top_idx_1based <- order(scores_aida, decreasing = TRUE)[seq_len(k)]

  # Convert to 0-based for C++
  top_idx_0based <- top_idx_1based - 1

  # Write the outliers file: first line = count, then one index per line
  out_path_outliers <- file.path("aida_run", "claims_input_outliers.dat")
  con <- file(out_path_outliers, "w")
  writeLines(as.character(k), con)
  writeLines(as.character(top_idx_0based), con)
  close(con)

  # Keep the mapping so you can trace TIX rows back to real claims later
  top_keys <- out$keys[top_idx_1based, ]
  top_keys$aida_score <- scores_aida[top_idx_1based]
  top_keys$tix_index_0based <- top_idx_0based

  cat(sprintf("Wrote %d outlier indices\n", k))

  # ----------------------------------------------------------------
  # >> Run the TIX binary now to produce aida_run/claims_tix.dat <<
  # The block below re-imports that output. It is gated separately so the
  # outlier-export half can run on its own first.
  # ----------------------------------------------------------------

  source_col_of <- function(feat) {
    if (str_detect(feat, "^freq__"))                    return(str_remove(feat, "^freq__"))
    if (str_detect(feat, "^flag_missing__"))            return(str_remove(feat, "^flag_missing__"))
    if (str_detect(feat, "^flag_present__"))            return(str_remove(feat, "^flag_present__"))
    if (str_detect(feat, "^miss__"))                    return(str_remove(feat, "^miss__"))
    if (str_detect(feat, "__"))                         return(str_remove(feat, "__.*$"))  # one-hot: take text before "__"
    return(feat)                                        # numeric: name is the source
  }
  
  
  if (file.exists(file.path("aida_run", "claims_tix.dat"))) {
    # ---- Re-import TIX output ----
    tix_lines <- readLines(file.path("aida_run", "claims_tix.dat"))[-1]
    
    tix_mat <- do.call(rbind, lapply(tix_lines, function(line) {
      as.numeric(strsplit(trimws(line), "\\s+")[[1]])
    }))
    
    tix_index_0based <- tix_mat[, 1]
    tix_contrib      <- tix_mat[, -1, drop = FALSE]
    
    stopifnot(ncol(tix_contrib) == ncol(out$X))
    stopifnot(nrow(tix_contrib) == k)
    stopifnot(identical(as.integer(tix_index_0based),
                        as.integer(top_idx_0based)))
    
    colnames(tix_contrib) <- colnames(out$X)
    explain_tix_full <- function(row_in_tix, top_features = 10) {
      claim_row <- top_idx_1based[row_in_tix]        # real master_claim row for this claim
      contrib   <- tix_contrib[row_in_tix, ]
      ord       <- order(abs(contrib), decreasing = TRUE)[seq_len(top_features)]
      
      feats <- colnames(tix_contrib)[ord]
      
      # For each flagged feature, recover its source column and read the
      # actual value from master_claim for this specific claim.
      raw_vals <- vapply(feats, function(f) {
        src <- source_col_of(f)
        if (!src %in% names(out$master_claim)) return(NA_character_)
        val <- out$master_claim[[src]][claim_row]
        if (is.na(val)) "MISSING" else as.character(val)
      }, character(1))
      
      data.frame(
        feature      = feats,
        contribution = round(contrib[ord], 4),
        source       = out$feature_manifest$source[match(feats, out$feature_manifest$feature)],
        source_field = vapply(feats, source_col_of, character(1)),
        raw_value    = raw_vals,
        row.names    = NULL
      )
    }
    
    # Explain the single most anomalous claim, raw values correctly aligned.
    print(top_keys[1, ])              # which real claim this is (File, CaseID, score)
    print(explain_tix_full(1))       # features + their actual values, joined per-row


    #sense check the (1) line item, aare the contibutions genuine or artefact?
    i <- top_idx_1based[1]   # the real row index of this top claim
    out$master_claim[i, ] %>%
      dplyr::select(max_line_gap, n_price_mismatch, n_inflated_lines,
                    sum_abs_gap, sum_inflated_gap, n_parts,
                    parts_submit_total, parts_actual_total, parts_price_gap) %>%
      dplyr::glimpse()

   # end TIX re-import
  tix_worked_examples <- list(
    top_claim_1 = explain_tix_full(1),
    top_claim_2 = explain_tix_full(2),
    top_claim_3 = explain_tix_full(3)
  )
  writexl::write_xlsx(tix_worked_examples,
                      file.path("aida_run", "tix_worked_examples1.xlsx"))
  cat("Wrote worked TIX examples to aida_run/tix_worked_examples1.xlsx\n")
  } # end RUN_TIX
}


