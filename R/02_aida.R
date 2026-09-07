# =====================================================================
#
# Run AFTER 01_preprocessing.R in the SAME R session. This file
# assumes `out` (out$X, out$keys, out$master_claim, out$feature_manifest)
# is already in the workspace.
#
# Because the AIDA scorer is an external C++ binary, this sheet runs in phases:
#   8.  EXPORT  -- always runs; writes aida_run/claims_input.dat
#       >> run the AIDA binary now, e.g.:
#       >>   ./aida aida_run/claims_input.dat aida_run/claims_scores.dat
#   9.  IFOREST baseline (optional) -- self-contained, no binary
#   10. RE-IMPORT -- after the binary produces aida_run/claims_scores.dat,
#       set RUN_AIDA_REIMPORT <- TRUE and re-run. Produces `scores`, which
#       sheet 3 (03_tix.R) consumes.
# =====================================================================

# --- LIBRARIES (in addition to those loaded by the preprocessing sheet) ---
suppressPackageStartupMessages({
  library(writexl)   # master_snapshot / scored_claims xlsx exports
  library(isotree)   # isolation.forest baseline (optional, section 9)
})

# --- FLAGS ---
# Section 8 (export) always runs. The phases below default to FALSE so a fresh
# run stops cleanly after the export; flip them once the .dat files exist.
RUN_IFOREST       <- TRUE  # isolation.forest baseline scoring (no binary needed)
RUN_AIDA_REIMPORT <- FALSE  # set TRUE once aida_run/claims_scores.dat exists
RUN_FULL_MASTER_EXPORT = TRUE
# Guard: make sure the preprocessing sheet has been run in this session.
if (!exists("out")) stop("`out` not found -- run 01_preprocessing.R first (same session).")


# =====================================================================
# 8. EXPORT AIDA MATRIX
# =====================================================================
# Always runs. Writes the master snapshot + the headerless-ish .dat matrix
# the AIDA C++ binary consumes. After this, run the AIDA binary externally:
#     >> ./aida aida_run/claims_input.dat aida_run/claims_scores.dat
# then flip RUN_AIDA_REIMPORT <- TRUE and re-source.

write_xlsx(
  list(
    master_claim     = out$master_claim,
    flag_report      = out$flag_report,
    feature_manifest = out$feature_manifest
  ),
  "master_snapshot.xlsx"
)

purrr::map(out, ~ if (is.matrix(.x) || is.data.frame(.x)) dim(.x) else length(.x))

#CSV export - prerpocessed and AIDA ready matrix
aida_dir <- "aida_run"
if (!dir.exists(aida_dir)) dir.create(aida_dir)

# Output path
out_path <- file.path(aida_dir, "claims_input.dat")

# Build all rows in vectorised form, with full precision and no
# scientific notation.
data_lines <- apply(out$X, 1, function(row) {
  paste(sprintf("%.17g", row), collapse = ", ")
})

# Open connection, write the dimension header, then the data
con <- file(out_path, "w")
writeLines(sprintf("%d, %d", nrow(out$X), ncol(out$X)), con)
writeLines(data_lines, con)
close(con)

# Save keys and manifest 
write.csv(out$keys, file.path(aida_dir, "claims_keys.csv"), row.names = FALSE)
write.csv(out$feature_manifest, file.path(aida_dir, "feature_manifest.csv"),
          row.names = FALSE)

cat(sprintf("Exported %d rows x %d features to %s\n",
            nrow(out$X), ncol(out$X), out_path))

readLines(out_path, n = 3)

file.path(getwd(), "aida_run")


# =====================================================================
# 9. IFOREST BASELINE   [optional -- guarded by RUN_IFOREST]
# =====================================================================
# Isolation Forest is the BASELINE comparator (not the primary method).
# Self-contained: scores out$X directly, no external binary. NOTE this defines
# its own `scores` object; section 10 re-defines `scores` from the AIDA output.
# Keep them straight -- 03_tix (section 11) must run off the AIDA scores.

if (isTRUE(RUN_IFOREST)) {
  
  #iforest for rference 
  fit    <- isolation.forest(out$X, ntrees = 200, sample_size = 256, ndim = 1)
  scores_iforest <- predict(fit, out$X, type = "score")
  
  
  summary(scores_iforest)                                       # min/median/max
  quantile(scores_iforest, c(0.5, 0.9, 0.95, 0.99, 0.999))      # natural thresholds
  hist(scores_iforest, breaks = 50,
       main = "Anomaly score distribution",
       xlab = "Score", col = "lightblue")
  
  scored <- out$master_claim %>%
    dplyr::mutate(anomaly_score = scores_iforest) %>%
    dplyr::arrange(dplyr::desc(anomaly_score))
  
  write_xlsx(
    list(
      outliers_top_100 = scored %>% dplyr::slice_head(n = 100),
      all_scored       = scored,
      score_summary    = tibble::tibble(
        quantile = c("50%", "90%", "95%", "99%", "99.9%"),
        score    = quantile(scores_iforest, c(0.5, 0.9, 0.95, 0.99, 0.999))
      )
    ),
    "scored_claims.xlsx"
  )
  
} # end RUN_IFOREST


# =====================================================================
# 10. AIDA SCORE RE-IMPORT   [optional -- guarded by RUN_AIDA_REIMPORT]
# =====================================================================
# Run AFTER the AIDA binary has produced aida_run/claims_scores.dat.
# This `scores` vector (aligned to out$X row order) is the PRIMARY signal and
# is what section 11 (03_tix) consumes.

if (isTRUE(RUN_AIDA_REIMPORT)) {
  
  all_lines <- readLines(file.path("aida_run", "claims_scores.dat"))
  scores_aida    <- as.numeric(all_lines[-1])
  stopifnot(length(scores_aida) == nrow(out$X))
  
  scored <- out$master_claim %>%
    dplyr::mutate(anomaly_score = scores_aida) %>%
    dplyr::arrange(dplyr::desc(anomaly_score))
  
  summary(scores_aida)
  quantile(scores_aida, c(0.5, 0.9, 0.95, 0.99, 0.999))
  hist(scores_aida, breaks = 50)
  
  top20 <- scored %>% dplyr::slice_head(n = 20)
  if (interactive()) View(top20)
  
} # end RUN_AIDA_REIMPORT





# =====================================================================
# 15. FULL MASTER EXPORT (score-enriched)  [optional -- RUN_FULL_MASTER_EXPORT]
# =====================================================================
# Exports the ENTIRE master_claim dataset -- all claims, every
# engineered feature, human-readable (pre-encoding) -- with each claim's AIDA
# and Isolation Forest rank + score prepended. Lets you open it in Excel and
# filter to any CaseID(s) to inspect/discuss specific claims, while seeing
# where each ranks under both methods.
#
# Writes BOTH .xlsx (convenient) and .csv (robust for a large dataset).
# Requires scores_aida AND scores_iforest (run RUN_IFOREST + RUN_AIDA_REIMPORT).
if (isTRUE(RUN_FULL_MASTER_EXPORT)) {
  
  if (!exists("scores_aida"))    stop("`scores_aida` not found.")
  if (!exists("scores_iforest")) stop("`scores_iforest` not found.")
  stopifnot(length(scores_aida)    == nrow(out$master_claim),
            length(scores_iforest) == nrow(out$master_claim),
            nrow(out$keys)         == nrow(out$master_claim))
  
  # Start from master; if CaseID isn't already a column, pull it from keys.
  base_tbl <- out$master_claim
  if (!"Input.Accident.CaseID" %in% names(base_tbl)) {
    base_tbl <- dplyr::bind_cols(
      base_tbl,
      out$keys %>% dplyr::select(dplyr::any_of(
        c("File", "Input.Accident.CaseID", "Input.Stage"))) %>%
        # avoid duplicating columns that already exist in master
        dplyr::select(-dplyr::any_of(names(base_tbl)))
    )
  }
  
  full_export <- base_tbl %>%
    dplyr::mutate(
      aida_rank     = rank(-scores_aida,    ties.method = "min"),
      aida_score    = scores_aida,
      iforest_rank  = rank(-scores_iforest, ties.method = "min"),
      iforest_score = scores_iforest,
      rank_gap      = iforest_rank - aida_rank
    ) %>%
    dplyr::relocate(dplyr::any_of("Input.Accident.CaseID"),
                    dplyr::any_of("File"),
                    aida_rank, aida_score, iforest_rank, iforest_score, rank_gap) %>%
    dplyr::arrange(aida_rank)
  
  cat(sprintf("Full master export: %d claims x %d columns\n",
              nrow(full_export), ncol(full_export)))
  
  write.csv(full_export, "final_combined_preprocessed.csv", row.names = FALSE)
  cat("Wrote final_combined_preprocessed.csv\n")
  
  tryCatch({
    write_xlsx(full_export, "final_combined_preprocessed.xlsx")
    cat("Wrote final_combined_preprocessed.xlsx\n")
  }, error = function(e)
    message("xlsx export failed (likely size) -- use the CSV. Error: ",
            conditionMessage(e)))
  
  cc <- which(names(full_export) == "Input.Accident.CaseID")
  if (length(cc))
    cat(sprintf("Tip: CaseID is column %d -- filter there to pull specific claims.\n", cc))
  else
    cat("Note: CaseID still not found -- run the names() grep to locate it.\n")
}