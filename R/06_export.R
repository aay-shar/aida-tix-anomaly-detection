k <- 100

# ---- 1. Map each model feature back to its source field ----
origin_field <- function(feat) {
  f <- sub("^freq__", "", feat)
  f <- sub("^flag_missing__", "", f)
  f <- sub("^flag_present__", "", f)
  f <- sub("^miss__", "", f)
  f <- sub("__.*$", "", f)
  f
}

# ---- 2. Human-readable label for a model feature ----
# Strips the Input.X.Y. prefixes and phrases flags/one-hots in plain language.
readable_label <- function(feat) {
  base <- sub("^(flag_missing__|flag_present__|miss__|freq__)", "", feat)
  field <- sub("__.*$", "", base)
  field_clean <- sub("^Input\\.[^.]+\\.", "", field)   # drop "Input.Insured." etc.
  field_clean <- gsub("\\.", " ", field_clean)
  
  if (grepl("^flag_present__", feat))  return(paste0(field_clean, " is recorded (unusual)"))
  if (grepl("^flag_missing__", feat))  return(paste0(field_clean, " is missing (unusual)"))
  if (grepl("^miss__", feat))          return(paste0(field_clean, " missing"))
  if (grepl("__Missing$", feat))       return(paste0(field_clean, " missing"))
  if (grepl("__", base)) {                              # one-hot level
    level <- sub("^[^_]*__", "", feat)
    level <- sub("^.*__", "", feat)
    return(paste0(field_clean, " = ", level))
  }
  field_clean                                           # plain numeric
}

# ---- 3. Build the per-outlier TIX detail (field-level rollup) ----
feat_names   <- colnames(tix_contrib)
feat_field   <- origin_field(feat_names)
feat_label   <- vapply(feat_names, readable_label, character(1))

tix_detail <- lapply(seq_len(k), function(r) {
  contrib <- tix_contrib[r, ]
  # roll up to field level by summing absolute contributions
  agg <- tapply(abs(contrib), feat_field, sum)
  agg <- sort(agg, decreasing = TRUE)
  fields <- names(agg)
  
  tibble(
    rank       = r,
    CaseID     = top_keys$Input.Accident.CaseID[r],
    field      = fields,
    importance = round(as.numeric(agg), 1)
  ) %>%
    # attach the claim's actual value for each field, where it exists
    rowwise() %>%
    mutate(
      claim_value = {
        v <- out$master_claim[[field]][ top_idx_1based[r] ]
        if (is.null(v)) NA else as.character(v)
      }
    ) %>%
    ungroup() %>%
    slice_head(n = 8)          # top 8 fields per claim
}) %>% bind_rows()

# ---- 4. Build the Summary sheet (one row per outlier) ----
# Plain-language "top reasons" = top 4 field labels, comma-joined
top_reasons <- tix_detail %>%
  group_by(rank, CaseID) %>%
  summarise(top_reasons = paste(head(field, 4), collapse = "; "), .groups = "drop")

summary_sheet <- top_keys %>%
  mutate(rank = row_number()) %>%
  select(rank, CaseID = Input.Accident.CaseID, File, aida_score) %>%
  left_join(top_reasons %>% select(rank, top_reasons), by = "rank") %>%
  # bolt on a few key raw values for at-a-glance context
  mutate(
    parts_submit_total   = out$master_claim$parts_submit_total[top_idx_1based],
    parts_price_gap      = out$master_claim$parts_price_gap[top_idx_1based],
    labour_amount_total  = out$master_claim$labour_amount_total[top_idx_1based],
    n_parts              = out$master_claim$n_parts[top_idx_1based],
    reporting_delay_days = out$master_claim$reporting_delay_days[top_idx_1based],
  ) %>%
  mutate(aida_score = round(aida_score, 3))

# ---- 5. Full claim data for the 100 outliers ----
full_data <- out$master_claim[top_idx_1based, ] %>%
  mutate(rank = row_number(), aida_score = round(top_keys$aida_score[seq_len(k)], 3)) %>%
  relocate(rank, aida_score)

# ---- 6. Write the workbook ----
write_xlsx(
  list(
    Summary          = summary_sheet,
    TIX_detail       = tix_detail,
    Full_claim_data  = full_data
  ),
  "top100_anomalies.xlsx"
)