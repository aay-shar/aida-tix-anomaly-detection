
# =====================================================================
# 12. EVALUATION WORKBOOK   [optional -- guarded by RUN_COMPARE]
# =====================================================================
# Exports one Excel workbook comparing AIDA vs Isolation Forest, plus the TIX
# explanations if they've been computed. Run this AFTER:
#   - RUN_IFOREST block (creates scores_iforest), AND
#   - RUN_AIDA_REIMPORT block (creates scores_aida),
#   - optionally TIX.R (creates tix_contrib / top_keys) for the TIX sheet.
#
# Both score vectors are aligned to out$X / out$master_claim row order, so we
# can bind them column-wise without a join.
RUN_COMPARE = TRUE
RUN_TOPBOTTOM_EXTRACT = TRUE
RUN_RULES_BENCHMARK <- TRUE # benchmark AIDA scores against the held-out rules table

if (isTRUE(RUN_COMPARE)) {
  
  # --- Preconditions: need both score vectors in scope ---
  if (!exists("scores_iforest"))
    stop("`scores_iforest` not found -- run the RUN_IFOREST block first.")
  if (!exists("scores_aida"))
    stop("`scores_aida` not found -- run the RUN_AIDA_REIMPORT block first.")
  stopifnot(length(scores_iforest) == nrow(out$master_claim),
            length(scores_aida)    == nrow(out$master_claim))
  
  # Higher score = more anomalous for both methods, so rank descending.
  # rank(-x) gives 1 = most anomalous. ties.method='min' so tied scores share
  # the better (lower) rank.
  rank_aida    <- rank(-scores_aida,    ties.method = "min")
  rank_iforest <- rank(-scores_iforest, ties.method = "min")
  
  # --- (a) Per-claim side-by-side comparison ---
  # Keys + both scores + both ranks + rank gap. Sorted by AIDA rank (primary).
  comparison <- out$keys %>%
    dplyr::mutate(
      aida_score    = scores_aida,
      iforest_score = scores_iforest,
      aida_rank     = rank_aida,
      iforest_rank  = rank_iforest,
      rank_gap      = iforest_rank - aida_rank   # +ve: AIDA ranks it more anomalous
    ) %>%
    dplyr::arrange(aida_rank)
  
  # --- (b) Each method's top-100, side by side as two ordered lists ---
  N <- 100
  top_aida <- comparison %>%
    dplyr::arrange(aida_rank) %>%
    dplyr::slice_head(n = N) %>%
    dplyr::transmute(aida_position = dplyr::row_number(),
                     CaseID = Input.Accident.CaseID,
                     aida_score, iforest_rank)
  top_iforest <- comparison %>%
    dplyr::arrange(iforest_rank) %>%
    dplyr::slice_head(n = N) %>%
    dplyr::transmute(iforest_position = dplyr::row_number(),
                     CaseID = Input.Accident.CaseID,
                     iforest_score, aida_rank)
  
  # --- (c) Agreement summary: overlap of the two top-N sets at a few cutoffs ---
  overlap_at <- function(k) {
    a <- comparison %>% dplyr::arrange(aida_rank)    %>%
      dplyr::slice_head(n = k) %>% dplyr::pull(Input.Accident.CaseID)
    f <- comparison %>% dplyr::arrange(iforest_rank) %>%
      dplyr::slice_head(n = k) %>% dplyr::pull(Input.Accident.CaseID)
    length(intersect(a, f))
  }
  cutoffs <- c(10, 25, 50, 100, 250)
  cutoffs <- cutoffs[cutoffs <= nrow(comparison)]
  agreement <- tibble::tibble(
    top_k          = cutoffs,
    shared_claims  = vapply(cutoffs, overlap_at, integer(1)),
    pct_overlap    = round(100 * vapply(cutoffs, overlap_at, integer(1)) / cutoffs, 1)
  )
  # Spearman rank correlation across ALL claims: how similarly do they order?
  spearman <- suppressWarnings(
    stats::cor(rank_aida, rank_iforest, method = "spearman")
  )
  agreement_meta <- tibble::tibble(
    metric = c("Spearman rank corr (all claims)",
               "n claims", "AIDA top-100 also in IF top-100"),
    value  = c(round(spearman, 4),
               nrow(comparison),
               overlap_at(min(100, nrow(comparison))))
  )
  
  # --- Assemble the sheet list ---
  sheets <- list(
    comparison        = comparison,
    top100_aida       = top_aida,
    top100_iforest    = top_iforest,
    agreement_overlap = agreement,
    agreement_summary = agreement_meta
  )
  
  # --- (d) TIX sheet, only if TIX has been run this session ---
  if (exists("tix_contrib") && exists("top_keys")) {
    # Long-format TIX: one row per (explained claim, feature) with its
    # contribution -- readable and filterable in Excel. Keep only non-trivial
    # contributions to avoid a giant sparse dump.
    tix_long <- as.data.frame(tix_contrib)
    tix_long$CaseID <- top_keys$Input.Accident.CaseID
    tix_long$aida_score <- top_keys$aida_score
    tix_long <- tix_long %>%
      tidyr::pivot_longer(cols = -c(CaseID, aida_score),
                          names_to = "feature", values_to = "contribution") %>%
      dplyr::filter(abs(contribution) > 1e-9) %>%
      dplyr::left_join(out$feature_manifest, by = "feature") %>%
      dplyr::group_by(CaseID) %>%
      dplyr::arrange(dplyr::desc(abs(contribution)), .by_group = TRUE) %>%
      dplyr::ungroup()
    
    # Compact "top-10 drivers per claim" view alongside the full long table.
    tix_top_drivers <- tix_long %>%
      dplyr::group_by(CaseID) %>%
      dplyr::slice_head(n = 10) %>%
      dplyr::ungroup()
    
    sheets$tix_top_drivers <- tix_top_drivers
    sheets$tix_full        <- tix_long
    message(sprintf("TIX sheets included (%d explained claims).",
                    nrow(top_keys)))
  } else {
    message("TIX objects not found -- exporting comparison without TIX sheets. ",
            "Run TIX.R first to include them.")
  }
  
  write_xlsx(sheets, "method_comparison.xlsx")
  cat(sprintf("Wrote method_comparison.xlsx with %d sheets.\n", length(sheets)))
  
} # end RUN_COMPARE

# =====================================================================
# 13. RULES-TABLE BENCHMARK   [optional -- guarded by RUN_RULES_BENCHMARK]
# =====================================================================
# Weak-label benchmark: how well do AIDA's anomaly scores align with the
# verdicts of existing rule-based system?
#
# The rules table is HELD OUT -- excluded from AIDA's features to avoid
# answer-key leakage -- so we read the RAW df_rules here, NOT the rule_*
# columns that agg_rules folded into the master matrix.
#
# A rule firing is a weak label, not ground-truth fraud: a fired rule doesn't
# prove fraud, and fraud can occur with no rule firing. So this measures
# AGREEMENT with the existing system, and -- more interesting for the thesis --
# DISAGREEMENT (claims AIDA flags that the rules miss, and vice versa).
#
# Requires: `out` and `scores_aida` (run the RUN_AIDA_REIMPORT block first).

if (isTRUE(RUN_RULES_BENCHMARK)) {
  
  if (!exists("scores_aida"))
    stop("`scores_aida` not found -- run the RUN_AIDA_REIMPORT block first.")
  if (!exists("df_rules"))
    stop("`df_rules` not found -- it should be in the session from preprocessing.")
  
  # --- (1) Reduce the raw rules table to one row per claim ---
  # Mirrors agg_rules' logic (lower-case Outcome, count rules fired) but keeps
  # ONLY the File key + the fired-rule count -- no per-rule columns leak in.
  rules_per_claim <- df_rules %>%
    dplyr::mutate(Outcome = stringr::str_to_lower(stringr::str_trim(Outcome))) %>%
    dplyr::group_by(File, ID) %>%
    dplyr::summarise(fired = as.integer(any(Outcome == "true")), .groups = "drop") %>%
    dplyr::group_by(File) %>%
    dplyr::summarise(n_rules_fired = sum(fired), .groups = "drop")
  
  # --- (2) Attach rule counts to the scored claims (aligned by row, then key) ---
  scored_keys <- out$keys %>%
    dplyr::mutate(aida_score = scores_aida,
                  aida_rank  = rank(-scores_aida, ties.method = "min"))
  
  bench <- scored_keys %>%
    dplyr::left_join(rules_per_claim, by = "File") %>%
    # claims with no rows in the rules table fired zero rules
    dplyr::mutate(n_rules_fired = tidyr::replace_na(n_rules_fired, 0L),
                  rules_flagged = n_rules_fired >= 1L)
  
  n_total   <- nrow(bench)
  n_flagged <- sum(bench$rules_flagged)
  cat(sprintf("\n--- Rules coverage ---\n%d of %d claims tripped >=1 rule (%.1f%%)\n",
              n_flagged, n_total, 100 * n_flagged / n_total))
  cat("Distribution of rules fired per claim:\n")
  print(table(bench$n_rules_fired))
  
  if (n_flagged == 0) {
    warning("No claims fired any rule after filtering -- benchmark cannot run. ",
            "Check that df_rules covers the car-claims File keys.")
  } else {
    
    # --- (3) Does AIDA rank rule-flagged claims as more anomalous? ---
    # If AIDA agrees with the rules, flagged claims should have BETTER (lower)
    # ranks. Compare rank distributions and run a one-sided Wilcoxon test.
    r_flagged <- bench$aida_rank[bench$rules_flagged]
    r_clean   <- bench$aida_rank[!bench$rules_flagged]
    cat(sprintf("\n--- AIDA rank by rules verdict (lower rank = more anomalous) ---\n"))
    cat(sprintf("  rule-flagged claims:  median rank %.0f  (n=%d)\n",
                median(r_flagged), length(r_flagged)))
    cat(sprintf("  non-flagged claims:   median rank %.0f  (n=%d)\n",
                median(r_clean), length(r_clean)))
    wt <- suppressWarnings(stats::wilcox.test(r_flagged, r_clean,
                                              alternative = "less"))
    cat(sprintf("  Wilcoxon (flagged ranked better): p = %.3g\n", wt$p.value))
    
    # --- (4) Precision/recall-style overlap at AIDA top-k ---
    # Treat rule-flagged as the (weak) positive class. At each cutoff: how many
    # of AIDA's top-k are rule-flagged (precision), and what share of all
    # flagged claims AIDA's top-k captures (recall).
    overlap_topk <- function(k) {
      topk_ids <- bench %>% dplyr::arrange(aida_rank) %>%
        dplyr::slice_head(n = k) %>% dplyr::pull(File)
      hits <- sum(bench$File %in% topk_ids & bench$rules_flagged)
      tibble::tibble(top_k = k,
                     precision = round(hits / k, 3),
                     recall    = round(hits / n_flagged, 3),
                     hits      = hits)
    }
    ks <- c(50, 100, 250, 500, 1000)
    ks <- ks[ks <= n_total]
    pr <- purrr::map_dfr(ks, overlap_topk)
    cat("\n--- AIDA top-k vs rules (rule-flagged = weak positive) ---\n")
    print(pr)
    
    # Baseline precision = prevalence (what random selection would get).
    cat(sprintf("Baseline (prevalence) precision = %.3f. AIDA top-k above this = lift.\n",
                n_flagged / n_total))
    
    # --- (5) AUC: do AIDA scores separate flagged from non-flagged at all? ---
    # Rank-based AUC = P(random flagged claim scores higher than random clean
    # one). 0.5 = no separation; computed from the Wilcoxon statistic.
    auc <- as.numeric(wt$statistic) / (length(r_flagged) * length(r_clean))
    # wt tested ranks (lower=better), so invert to express "higher score, more
    # likely flagged":
    auc <- 1 - auc
    cat(sprintf("\nAIDA-vs-rules AUC = %.3f  (0.5 = no separation)\n", auc))
    
    # --- (6) The interesting cases: disagreement ---
    # AIDA's top picks that fired NO rule -- candidate value-add (anomalies the
    # rule system misses). And rule-flagged claims AIDA ranks low -- gaps.
    aida_top_no_rule <- bench %>%
      dplyr::filter(aida_rank <= 100, !rules_flagged) %>%
      dplyr::arrange(aida_rank) %>%
      dplyr::select(Input.Accident.CaseID, aida_rank, aida_score, n_rules_fired)
    cat(sprintf("\n--- AIDA top-100 that fired NO rule: %d claims (potential value-add) ---\n",
                nrow(aida_top_no_rule)))
    print(utils::head(aida_top_no_rule, 15))
    
    rule_flagged_aida_low <- bench %>%
      dplyr::filter(rules_flagged, aida_rank > n_total/2) %>%
      dplyr::arrange(dplyr::desc(n_rules_fired))
    cat(sprintf("\n--- Rule-flagged claims AIDA ranks in the bottom half: %d (gaps) ---\n",
                nrow(rule_flagged_aida_low)))
    print(utils::head(rule_flagged_aida_low %>%
                        dplyr::select(Input.Accident.CaseID, aida_rank,
                                      n_rules_fired), 15))
    
    # --- (7) Export the full benchmark to Excel ---
    write_xlsx(
      list(
        benchmark_per_claim = bench,
        precision_recall    = pr,
        aida_top_no_rule    = aida_top_no_rule,
        rule_flagged_gaps   = rule_flagged_aida_low %>%
          dplyr::select(Input.Accident.CaseID, aida_rank, aida_score,
                        n_rules_fired),
        summary = tibble::tibble(
          metric = c("n claims", "n rule-flagged", "prevalence",
                     "median rank (flagged)", "median rank (clean)",
                     "Wilcoxon p", "AUC"),
          value  = c(n_total, n_flagged, round(n_flagged/n_total, 4),
                     round(median(r_flagged)), round(median(r_clean)),
                     signif(wt$p.value, 3), round(auc, 4))
        )
      ),
      "rules_benchmark.xlsx"
    )
    cat("\nWrote rules_benchmark.xlsx\n")
  }
  
} # end RUN_RULES_BENCHMARK

# =====================================================================
# 14. TOP / BOTTOM CLAIM-RECORD EXTRACTION  [optional -- RUN_TOPBOTTOM_EXTRACT]
# =====================================================================
# For EACH method, take its top-100 most anomalous claims, then pull the FULL
# claim record for the top 10 (ranks 1-10, clearly anomalous) and the bottom
# 10 (ranks 91-100, the borderline picks that barely made the top-100).
# 20 claims per method, 40 before de-duplication.
#
# Output, BOTH ways:
#   (a) one union table with method/bucket flag columns (best for comparison)
#   (b) four separate sheets: AIDA_top10, AIDA_bottom10, IF_top10, IF_bottom10
#
# Records come from out$master_claim (full, pre-encoding features), aligned to
# the score vectors by row order. Requires scores_aida AND scores_iforest.

if (isTRUE(RUN_TOPBOTTOM_EXTRACT)) {
  
  if (!exists("scores_aida"))
    stop("`scores_aida` not found -- run the RUN_AIDA_REIMPORT block first.")
  if (!exists("scores_iforest"))
    stop("`scores_iforest` not found -- run the RUN_IFOREST block first.")
  stopifnot(length(scores_aida)    == nrow(out$master_claim),
            length(scores_iforest) == nrow(out$master_claim))
  
  master <- out$master_claim
  
  # Row indices (into master) of a method's rank band, given its score vector.
  # rank 1 = highest score. order(desc) gives rows from most to least anomalous.
  band_rows <- function(scores, from_rank, to_rank) {
    ord <- order(scores, decreasing = TRUE)
    ord[from_rank:to_rank]
  }
  
  idx_aida_top    <- band_rows(scores_aida,     1,  10)
  idx_aida_bottom <- band_rows(scores_aida,    91, 100)
  idx_if_top      <- band_rows(scores_iforest,  1,  10)
  idx_if_bottom   <- band_rows(scores_iforest, 91, 100)
  
  # Build one labelled slice: full record + which method, which bucket, the
  # claim's rank and score under BOTH methods (so you can see cross-placement).
  rank_aida    <- rank(-scores_aida,    ties.method = "min")
  rank_iforest <- rank(-scores_iforest, ties.method = "min")
  
  make_slice <- function(rows, method, bucket) {
    if (length(rows) == 0) return(NULL)
    master[rows, , drop = FALSE] %>%
      dplyr::mutate(
        flagged_by   = method,
        bucket       = bucket,
        aida_rank    = rank_aida[rows],
        aida_score   = scores_aida[rows],
        iforest_rank = rank_iforest[rows],
        iforest_score= scores_iforest[rows]
      ) %>%
      # put the label/score columns first for readability (relocate is broadly
      # supported; avoids relying on mutate's .before argument)
      dplyr::relocate(flagged_by, bucket, aida_rank, aida_score,
                      iforest_rank, iforest_score)
  }
  
  s_aida_top    <- make_slice(idx_aida_top,    "AIDA",    "top10")
  s_aida_bottom <- make_slice(idx_aida_bottom, "AIDA",    "bottom10")
  s_if_top      <- make_slice(idx_if_top,      "IForest", "top10")
  s_if_bottom   <- make_slice(idx_if_bottom,   "IForest", "bottom10")
  
  # --- (a) UNION TABLE ---
  # Stack all four. A claim appearing under two method/bucket labels (e.g. AIDA
  # top10 AND IForest bottom10) yields two rows -- intended, so each labelled
  # appearance is visible. The leading flag columns make filtering trivial.
  union_tbl <- dplyr::bind_rows(s_aida_top, s_aida_bottom, s_if_top, s_if_bottom)
  
  # Quick console view of just the identity + cross-ranks, sorted for reading.
  cat("\n--- Top/bottom extraction: identity + cross-method placement ---\n")
  union_tbl %>%
    dplyr::select(flagged_by, bucket, Input.Accident.CaseID,
                  aida_rank, iforest_rank) %>%
    dplyr::arrange(flagged_by, bucket, aida_rank) %>%
    print(n = 40)
  
  # Overlap note: does any claim appear in more than one method/bucket?
  dup_ids <- union_tbl %>%
    dplyr::count(Input.Accident.CaseID) %>%
    dplyr::filter(n > 1) %>% dplyr::pull(Input.Accident.CaseID)
  if (length(dup_ids) > 0) {
    cat(sprintf("\n%d claim(s) appear in multiple buckets: %s\n",
                length(dup_ids), paste(dup_ids, collapse = ", ")))
  } else {
    cat("\nNo claim appears in more than one method/bucket (fully disjoint).\n")
  }
  
  # --- (b) SEPARATE SHEETS + the union, all in one workbook ---
  write_xlsx(
    list(
      union_all     = union_tbl,
      AIDA_top10    = s_aida_top,
      AIDA_bottom10 = s_aida_bottom,
      IF_top10      = s_if_top,
      IF_bottom10   = s_if_bottom
    ),
    "top_bottom_claims.xlsx"
  )
  cat("\nWrote top_bottom_claims.xlsx (union + 4 per-bucket sheets)\n")
  
} # end RUN_TOPBOTTOM_EXTRACT