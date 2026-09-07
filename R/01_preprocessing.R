# =====================================================================
# EDA + preprocessing pipeline for claims data -> AIDA / TIX input.
#
# Structure:
#   0. LIBRARIES
#   1. CONFIG
#   2. LOAD DATA
#   3. SHARED HELPERS
#   4. EDA / REPORTING        [optional -- guarded by RUN_EDA]
#   5. PIPELINE FUNCTIONS
#   6. RUN
#   7. POST-RUN DIAGNOSTICS   [optional -- guarded by RUN_DIAGNOSTICS]
#
# EDA runs on the RAW loaded tables; the pipeline transforms them into the
# AIDA matrix. They share only the loaded data and the cleaning helper.
#
# This is the FIRST of three sheets, run in sequence in the same R session:
#   1. EDA&preprocessing.R  (this file)  -> leaves `out` in scope
#   2. AIDA.R                       -> uses `out`, produces `scores`
#   3. TIX.R                        -> uses `out` and `scores`
# Run this file first and keep the session alive; AIDA.R and TIX.R assume the
# objects it creates are still in the workspace.
# =====================================================================


# =====================================================================
# 0. LIBRARIES
# =====================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr)
  library(lubridate); library(purrr); library(tibble); library(forcats)
  library(naniar)    # missing-data utilities (gg_miss_var)
  library(skimr)
  library(ggplot2)
})
# END


# =====================================================================
# 1. CONFIG
# =====================================================================

# Toggle the optional reporting / diagnostics blocks.
RUN_EDA         <- TRUE   # data dictionary, missingness reports, plots, Sheets export
RUN_DIAGNOSTICS <- TRUE   # post-run absurd-value sanity probes
EXPORT_TO_SHEETS <- FALSE   # TRUE only on your machine, with a sheet ID in config.R
if (isTRUE(EXPORT_TO_SHEETS)) {
  library(googlesheets4)
  gs4_auth()
}
PII_DROP        <- c("Input.InjuredPersons.Person.Name") #dropping the protected attribute
STAGE_FILTER    <- "REPEST"
UNIT            <- "File"
DROP_LEAKAGE    <- TRUE
PRESENCE_HI     <- 0.80
PRESENCE_LO     <- 0.20
MIN_COMPLETE    <- 0.02
RARE_LEVEL_MIN  <- 0.01
STAGE_ORDER     <- c("INCOMING", "INSAUTH", "INSSEND", "REPEST")
VEHTYPE_KEEP <- c(1:13, 16:17, 20, 27:28, 37:52, 54:56, 58, 63:65, 67, 72)
#collapse multiple file snapshots per caseid at repest to one row (latest)
COLLAPSE_CASEID <- TRUE 
# NULL = no filter; integer vector = keep only these VehType codes (cars)
# --- Monetary plausibility ceilings ----------
# Values above these are treated as DATA ERRORS and set to NA (not clipped),
# so the missingness machinery handles them. Provisional.
PRICE_CEIL_PART    <- 2e8    #   200M (~$12k)  single part line
PRICE_CEIL_LABOUR  <- 1e8    #   100M (~$6k)   single labour line
PRICE_CEIL_TOTAL   <- 2e9    #     2B (~$120k)  claim-level monetary totals
PRICE_CEIL_INSURED <- 1e10   #    10B (~$600k)  sum insured (vehicle value)
APPLY_PRICE_CEILINGS <- TRUE  # master switch for the plausibility step


# =====================================================================
# 2. LOAD DATA
# =====================================================================

#loading dataframe from local storage
# Replace the path below with your .RData file (was load(file.choose())).
# Expected objects: df, df_history, df_rules, df_parts, df_labours
if (!file.exists("config.R"))
  stop("Copy config_example.R to config.R and fill in your paths.")
source("config.R")

load(file.path(data_dir, "df.Rdata"))            # restores df
load(file.path(data_dir, "df_claimants.Rdata"))  # restores df_claimants
load(file.path(data_dir, "df_history.Rdata"))    # restores df_history
load(file.path(data_dir, "df_rules.Rdata"))      # restores df_rules
load(file.path(data_dir, "df_parts.Rdata"))      # restores df_parts
load(file.path(data_dir, "df_labours.Rdata"))    # restores df_labours

#putting all dataframes in a list, so can traverse through and see all the columns
# (claimants removed from the schema -- five tables only)
df_list <- list(
  claims = df, history = df_history, rules = df_rules, parts = df_parts, labours = df_labours
)


# =====================================================================
# 3. SHARED HELPERS
# =====================================================================

#Putting in R identifiable NA so that any NA's are actually recognized. 
na_strings <- c("", " ", "NA", "N/A", "n/a", "na", "NULL", "null",
                "None", "none", "Unknown", "UNKNOWN", "unknown",
                "?", "-", "--", ".", "#N/A", "nan", "NaN")

# ---- Helper: convert sentinels to real NA across a dataframe ----
# Single shared cleaner (formerly clean_na + clean_blanks). Uses the wider
# na_strings sentinel list and also cleans factor levels, so it is a strict
# superset of both originals. Pipeline functions call this as clean_blanks().
clean_blanks <- function(df, extras = character(0)) {
  bad <- unique(c(na_strings, extras))
  df %>%
    mutate(across(where(is.character),
                  ~ {
                    x <- str_trim(.x)              # strip leading/trailing whitespace
                    x[x %in% bad] <- NA_character_
                    x
                  })) %>%
    # If you have factors, also clean their levels:
    mutate(across(where(is.factor),
                  ~ {
                    x <- as.character(.x)
                    x <- str_trim(x)
                    x[x %in% bad] <- NA_character_
                    factor(x)
                  }))
}

as_num <- function(x) suppressWarnings(as.numeric(str_remove_all(str_trim(x), "[ ,]")))

# Null values above a ceiling (data errors), leaving NA for the missingness
# machinery. Reports how many were nulled. `label` is just for the message.
null_above <- function(x, ceiling, label = "") {
  x <- as_num(x)
  bad <- !is.na(x) & x > ceiling
  if (any(bad)) {
    message(sprintf("  plausibility: nulled %d value(s) over %.0e in %s",
                    sum(bad), ceiling, label))
  }
  x[bad] <- NA_real_
  x
}

parse_dt <- function(x) {
  parse_date_time(x, orders = c("Ymd HMS", "Ymd HM", "Ymd"),
                  truncated = 3, quiet = TRUE)
}

presence_rate <- function(df) colMeans(!is.na(df))


# =====================================================================
# 4. EDA / REPORTING   [optional -- guarded by RUN_EDA]
# =====================================================================
# Runs on the RAW loaded tables (df_list), independent of the pipeline.

if (isTRUE(RUN_EDA)) {

  # ---- Apply sentinel cleaning to all dataframes (EDA view) ----
  df_list <- map(df_list, clean_blanks)

  lapply(df_list, dim)
  lapply(df_list, colnames)


  # =========================================================
  # 4a. Data dictionary
  # =========================================================

  # Function to classify Continuous vs. Discrete
  # Rule: If it's numeric and has more than 15 unique values, it's continuous. Otherwise, discrete.
  classify_variable <- function(column_data) {
    # If it's numeric AND has more than 5 unique values, it's continuous
    if (length(unique(column_data)) > 5) {
      return("Continuous")
    } else {
      return("Discrete")
    }
  }

  # Function to extract all metadata from a single dataframe
  extract_metadata <- function(df, table_name) {
    tibble(
      Table_Name = table_name,
      Column_Name = names(df),
      Data_Type = map_chr(df, ~class(.x)[1]), # Gets numeric, factor, character, etc.
      Variable_Type = map_chr(df, classify_variable)
    )
  }

  # Apply the extraction function to all dataframes and combine them into one table
  raw_dictionary <- imap_dfr(df_list, extract_metadata)

  unique_dictionary <- raw_dictionary %>%
    group_by(Column_Name) %>%
    summarise(
      Occurrence_Count = n(),                                 # Counts how many times the column appears
      Found_In_Tables = paste(Table_Name, collapse = ", "),   # Lists the specific tables it lives in!
      Data_Type = first(Data_Type),                           # Grabs the type from the first occurrence
      Variable_Type = first(Variable_Type)                    # Grabs Continuous/Discrete from the first occurrence
    ) %>%
    ungroup() %>%
    arrange(desc(Occurrence_Count)) # Sorts so the most common columns are at the top

  #Export to Google Sheets
  if (isTRUE(EXPORT_TO_SHEETS)) {
    gs4_create(
      name = "Thesis_Data_Dictionary_Unique",
      sheets = list("Unique_Attributes" = unique_dictionary)
    )
  }
  #END


  #EDA, missingness etc

  # =========================================================
  # 1. High-level shape + overall missingness per dataframe
  # =========================================================
  overview <- map_dfr(df_list, function(df) {
    tibble(
      n_rows         = nrow(df),
      n_cols         = ncol(df),
      total_cells    = nrow(df) * ncol(df),
      missing_cells  = sum(is.na(df)),
      pct_missing    = round(100 * sum(is.na(df)) / (nrow(df) * ncol(df)), 2),
      cols_any_na    = sum(map_lgl(df, ~ any(is.na(.)))),
      complete_rows  = sum(complete.cases(df)),
      pct_complete_rows = round(100 * sum(complete.cases(df)) / nrow(df), 2)
    )
  }, .id = "dataframe")

  print(overview)

  
  # =========================================================
  # 2. Column-level missingness for every dataframe
  # =========================================================
  col_missing <- map_dfr(df_list, function(df) {
    tibble(
      column      = names(df),
      type        = map_chr(df, ~ class(.)[1]),
      n_missing   = map_int(df, ~ sum(is.na(.))),
      pct_missing = map_dbl(df, ~ round(100 * mean(is.na(.)), 2)),
      n_unique    = map_int(df, ~ dplyr::n_distinct(., na.rm = TRUE))
    )
  }, .id = "dataframe") %>%
    arrange(dataframe, desc(pct_missing))

  print(col_missing, n = 50)

  # Columns that are basically empty (>80% missing) — usually candidates to drop
  col_missing %>% filter(pct_missing > 80)

  # =========================================================
  # 3. Quick skim per dataframe (distributions, n_missing, etc.)
  # =========================================================

  # ---- Helper: turn a skim_df into a flat, sheet-friendly tibble ----
  skim_to_flat <- function(df) {
    s <- skim(df)
    # skim() can have a histogram column with unicode spark bars; keep as text.
    # Drop any list columns just in case (rare, but safer for Sheets).
    s %>%
      as_tibble() %>%
      mutate(across(where(is.list), ~ map_chr(.x, ~ paste(unlist(.x), collapse = "; ")))) %>%
      mutate(across(everything(), as.character))   # Sheets is happiest with text/number atomics
  }

  # ---- Build one tibble per dataframe ----
  skim_tables <- map(df_list, skim_to_flat)
  # names(skim_tables) become the sheet/tab names
  if (isTRUE(EXPORT_TO_SHEETS)) {
    ss <- as_sheets_id(SHEETS_ID)
    iwalk(skim_tables, function(tbl, nm) {
      sheet_write(data = tbl, ss = ss, sheet = nm)
    })
  }

  # =========================================================
  # 4. Visualize missingness
  # =========================================================
  # Restrict the claims table to car claims only -- this matches the modelled
  # population (the pipeline's VEHTYPE_KEEP filter), so the missingness shown
  # here reflects the data AIDA actually sees. Child tables are left as-is;
  # they share the File key and get filtered downstream in build_master.
  claims_cars <- df_list$claims %>%
    filter(suppressWarnings(as.integer(Input.Vehicle.VehType)) %in% VEHTYPE_KEEP)
  message(sprintf("EDA: claims filtered to cars: %d -> %d rows",
                  nrow(df_list$claims), nrow(claims_cars)))


  # (a) Bar chart of % missing per column (car claims)
  print(gg_miss_var(claims_cars, show_pct = TRUE) +
          ggtitle("% missing per column — car claims"))

  # (b) Missingness-correlation heatmap (replaces the upset plot).
  # Shows which fields tend to be missing TOGETHER: build a 0/1 indicator
  # matrix from is.na(), keep only columns that are actually sometimes-missing
  # (constant columns have no correlation and would divide by zero), then
  # correlate and plot. This is the scalable, citable version of the
  # co-occurrence story the upset plot was gesturing at.
  miss_ind <- as.data.frame(lapply(claims_cars, function(x) as.integer(is.na(x))))
  varies   <- vapply(miss_ind, function(x) length(unique(x)) > 1, logical(1))
  miss_ind <- miss_ind[, varies, drop = FALSE]

  if (ncol(miss_ind) >= 2) {
    miss_cor <- cor(miss_ind, use = "pairwise.complete.obs")

    # Long format for ggplot; order columns by a simple clustering so blocks
    # of co-missing fields sit together.
    ord      <- hclust(as.dist(1 - abs(miss_cor)))$order
    lvls     <- colnames(miss_cor)[ord]
    cor_long <- as.data.frame(as.table(miss_cor))
    names(cor_long) <- c("field_x", "field_y", "corr")
    cor_long$field_x <- factor(cor_long$field_x, levels = lvls)
    cor_long$field_y <- factor(cor_long$field_y, levels = lvls)

    print(
      ggplot(cor_long, aes(field_x, field_y, fill = corr)) +
        geom_tile() +
        scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac",
                             midpoint = 0, limits = c(-1, 1), name = "corr") +
        labs(title = "Missingness co-occurrence — car claims",
             subtitle = "Correlation of is.na() indicators; blue = missing together",
             x = NULL, y = NULL) +
        theme_minimal(base_size = 8) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    )
  } else {
    message("Missingness heatmap skipped: <2 columns with variable missingness.")
  }
  #END

  # ---- Build a wide dictionary: 1 column per attribute, values down the rows ----
  if (isTRUE(EXPORT_TO_SHEETS)) {
  make_unique_wide <- function(df_list, max_values = 1000) {
    cols <- list()
    for (df_name in names(df_list)) {
      df <- df_list[[df_name]]
      for (col_name in names(df)) {
        vals <- df[[col_name]]
        vals <- sort(unique(vals[!is.na(vals)]))     # unique, sorted, NA dropped
        vals <- head(vals, max_values)
        # prefix with dataframe name so same-named columns don't clash
        key  <- paste(df_name, col_name, sep = " | ")
        cols[[key]] <- as.character(vals)
      }
    }
    # pad every column to equal length so it forms a rectangle
    n <- max(lengths(cols), 1)
    cols <- lapply(cols, function(x) c(x, rep(NA_character_, n - length(x))))
    as_tibble(cols)
  }

  unique_wide <- make_unique_wide(df_list, max_values = 1000)

  # ---- Write to the new tab ----
  sheet_write(unique_wide, ss = ss, sheet = "Thesis_Data_Dictionary_Unique")
}
} # end RUN_EDA


# =====================================================================
# 5. PIPELINE FUNCTIONS
# =====================================================================

# 2.COLUMN SELECTION. Categorizing columns for easier access

claims_ids_pii <- c(
  "Input.Policy.PolicyNo", "Input.Vehicle.VehRegNo", "Input.Vehicle.VehEngNo",
  "Input.Vehicle.VehChassis", "Input.Accident.ClaimNo", "Input.Driver.DrvProvLics",
  "Input.Insured.InsuredName", "Input.Insured.InsuredID", "Input.Insured.InsDOB",
  "Input.Insured.InsAdd1", "Input.Insured.InsAdd2", "Input.Insured.InsContact",
  "Input.Driver.DrvName", "Input.Driver.DrvID", "Input.Driver.DrvDOB",
  "Input.Driver.DrvAdd1", "Input.Driver.DrvAdd2", "Input.Driver.DrvContact",
  "Input.Workshop.WkrRegNo", "Input.Workshop.WrkContact"
)
claims_free_text <- c(
  "Input.Accident.DescAccLoss", "Input.Accident.AccPlace", "Input.Vehicle.VehVariant"
)
claims_leakage <- c(
  "Input.Accident.ClaimStatus",
  "Input.Accident.Financials.TotalPaidAmt",
  "Input.Accident.Financials.TotalRsvAmt"
)
claim_keys <- c("File", "Input.Accident.CaseID", "Input.Stage") #explicit list of columns that are identifiers not features

#This runs on post filtered data i.e. only REPEST
#no information by distribution
auto_drop_cols <- function(df, keep, min_complete = MIN_COMPLETE) {
  #presence rate() calls the helper colMeans(), true counts as 1, from this we get presnce rate which is the franction of trues
  pr  <- presence_rate(df) 
  #counts the distinct non-NA values in each column. is there's only 1 
  nun <- sapply(df, function(c) dplyr::n_distinct(c, na.rm = TRUE))
  #combines the two functions: the feature should b more than 20% complete and should have more than 1 unique value and the   
  drop <- names(df)[(pr < min_complete | nun <= 1) & !(names(df) %in% keep)]
  #This will drop the columns identifed by the drop variables, the dropped arg is used to be able to identify what wass 
  #actually dropped
  list(df = df %>% select(-all_of(drop)), dropped = drop) 
}

#specific columns that auto-drop can't catch
#no information by purpose 
select_claims_features <- function(claims) {
  #concatenating leakage, pii, and free_text columns, claim_leakage is optional so should only be dropped 
  #if drop_leakage is true. It just puts in an empty character vector.
  manual <- c(claims_ids_pii, claims_free_text,
              if (DROP_LEAKAGE) claims_leakage else character(0))
  #does the actual col drop , any_of allows to skip a col if it's not found (since it could already be dropped previously)
  claims2 <- claims %>% select(-any_of(manual))
  #runs the auto drop and the manual drop in order.
  ad <- auto_drop_cols(claims2, keep = claim_keys)
  #transparency hook for methodology chapter
  message(sprintf("claims: dropped %d manual + %d auto = %d of %d columns",
                  length(manual), length(ad$dropped),
                  length(manual) + length(ad$dropped), ncol(claims)))
  ad$df
}

# 3. CHILD-TABLE AGGREGATORS

agg_history <- function(history) {
h <- history %>% clean_blanks() %>%
  mutate(accidentdate = parse_dt(accidentdate))

# Step 1: one row per (File, prior caseID) -- avoids double-counting
# when the same prior case has multiple stage-snapshots in the lookback.
h_per_case <- h %>%
  group_by(File, caseID) %>%
  summarise(
    any_total_loss   = any(str_to_upper(TotalLossType) %in% c("2","Y"),
                           na.rm = TRUE),
    max_offer_paid   = suppressWarnings(max(as_num(OfferPaidAmt),
                                            na.rm = TRUE)),
    max_repairer_est = suppressWarnings(max(as_num(TotalRepairerEstimate),
                                            na.rm = TRUE)),
    latest_accident  = suppressWarnings(max(accidentdate, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    max_offer_paid   = if_else(is.infinite(max_offer_paid),   NA_real_, max_offer_paid),
    max_repairer_est = if_else(is.infinite(max_repairer_est), NA_real_, max_repairer_est),
    latest_accident  = if_else(is.infinite(latest_accident),  as.POSIXct(NA), latest_accident)
  )

# Step 2: per-File aggregate over the now-distinct prior cases.
per_file_cases <- h_per_case %>%
  group_by(File) %>%
  summarise(
    n_prior_cases           = n_distinct(caseID),
    n_total_loss_history    = sum(any_total_loss, na.rm = TRUE),
    hist_offerpaid_total    = sum(max_offer_paid,    na.rm = TRUE),
    hist_repairer_est_total = sum(max_repairer_est,  na.rm = TRUE),
    hist_most_recent_prior  = suppressWarnings(max(latest_accident, na.rm = TRUE)),
    hist_oldest_prior       = suppressWarnings(min(latest_accident, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    hist_most_recent_prior = if_else(is.infinite(hist_most_recent_prior),
                                     as.POSIXct(NA), hist_most_recent_prior),
    hist_oldest_prior      = if_else(is.infinite(hist_oldest_prior),
                                     as.POSIXct(NA), hist_oldest_prior),
    hist_priors_span_days  = as.numeric(difftime(hist_most_recent_prior,
                                                 hist_oldest_prior, units = "days"))
  )

# Step 3: row-level signals where duplicate snapshots are fine to keep.
per_file_rows <- h %>%
  group_by(File) %>%
  summarise(
    n_history_rows    = n(),
    n_distinct_status = n_distinct(ClaimStatus, na.rm = TRUE),
    .groups = "drop"
  )

per_file_cases %>% left_join(per_file_rows, by = "File")
}

#multi-hot encoding rules dataset, housekeeping and adding a separate column "fired" to signify the no. of rules attaced to a claim
agg_rules <- function(rules) {
  #removing blanks just in case
  rules %>% clean_blanks() %>%
    #the outcome column is true/false, but could be inconsistent with case
    mutate(Outcome = str_to_lower(Outcome),
           #using ID instead of name, prepenting rule_ in front so as to separate from other columns
           rule = paste0("rule_", str_trim(ID))) %>%
    group_by(File, rule) %>%
    summarise(fired = as.integer(any(Outcome == "true")), .groups = "drop") %>%
    pivot_wider(names_from = rule, values_from = fired, values_fill = 0) %>%
    mutate(n_rules_fired = rowSums(across(starts_with("rule_"))))
}

# Apply plausibility ceilings to the raw child tables before aggregation.
# Done here (pre-aggregation) so corrupt line-items don't poison the
# derived discrepancy/total features downstream.
clean_child_prices <- function(parts, labours) {
  if (!isTRUE(APPLY_PRICE_CEILINGS)) return(list(parts = parts, labours = labours))
  
  parts <- parts %>%
    mutate(
      SubmittedPartsPrice = null_above(SubmittedPartsPrice, PRICE_CEIL_PART,
                                       "parts$SubmittedPartsPrice"),
      ActualPartsPrice    = null_above(ActualPartsPrice,    PRICE_CEIL_PART,
                                       "parts$ActualPartsPrice")
    )
  
  labours <- labours %>%
    mutate(
      LabourAmount = null_above(LabourAmount, PRICE_CEIL_LABOUR,
                                "labours$LabourAmount")
    )
  
  list(parts = parts, labours = labours)
}

agg_parts <- function(parts) {
  parts %>% clean_blanks() %>%
    mutate(
      actual_price    = as_num(ActualPartsPrice),
      submit_price    = as_num(SubmittedPartsPrice),
      line_gap        = submit_price - actual_price,
      line_inflated   = as.integer(line_gap > 0),
      price_mismatch  = as.integer(!is.na(line_gap) & abs(line_gap) > 0.01),
      partno_mismatch = as.integer(!is.na(ActualPartsNo) & !is.na(SubmittedPartsNo) &
                                     ActualPartsNo != SubmittedPartsNo)
    ) %>%
    group_by(File) %>%
    summarise(
      n_parts              = n(),
      parts_actual_total   = sum(actual_price,             na.rm = TRUE),
      parts_submit_total   = sum(submit_price,             na.rm = TRUE),
      parts_mean_discount  = mean(as_num(SpecialDiscount), na.rm = TRUE),
      n_parts_categories   = n_distinct(PartsCategory,     na.rm = TRUE),
      
      # Line-level matching
      n_price_mismatch     = sum(price_mismatch,           na.rm = TRUE),
      share_price_mismatch = mean(price_mismatch,          na.rm = TRUE),
      n_partno_mismatch    = sum(partno_mismatch,          na.rm = TRUE),
      
      # Line-level inflation
      n_inflated_lines     = sum(line_inflated,            na.rm = TRUE),
      max_line_gap         = suppressWarnings(max(line_gap, na.rm = TRUE)),
      sum_abs_gap          = sum(abs(line_gap),            na.rm = TRUE),
      sum_inflated_gap     = sum(pmax(line_gap, 0),        na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    mutate(
      parts_price_gap = parts_submit_total - parts_actual_total,
      max_line_gap    = if_else(is.infinite(max_line_gap), NA_real_, max_line_gap)
    )
}


agg_labours <- function(labours) {
  labours %>% clean_blanks() %>%
    group_by(File) %>%
    summarise(
      n_labour_items      = n(),
      labour_amount_total = sum(as_num(LabourAmount), na.rm = TRUE),
      n_labour_replace    = sum(str_to_upper(RepairType) == "R", na.rm = TRUE),
      .groups = "drop"
    )
}

# 4. BUILD MASTER

build_master <- function(claims, history, rules, parts, labours) {
  #Removing personally identifiable info (Policy decision)
  claims <- claims %>% select(-any_of(PII_DROP))
  #If stage filter is set to repest apply filter otherwise keep as is
  if (!is.na(STAGE_FILTER)) {
    #capture the rowcount before filtering, so we can havea before/after comparison
    before <- nrow(claims)
    claims <- claims %>% filter(Input.Stage == STAGE_FILTER)
    #to check exactly how many claims fell in each step. 
    message(sprintf("Stage filter '%s': %d -> %d claim rows retained",
                    STAGE_FILTER, before, nrow(claims)))
  }
  #filter for vehicle type
  if (!is.null(VEHTYPE_KEEP)) {
    before <- nrow(claims)
    claims <- claims %>%
      filter(suppressWarnings(as.integer(Input.Vehicle.VehType)) %in% VEHTYPE_KEEP)
    message(sprintf("VehType filter (cars only): %d -> %d claim rows retained",
                    before, nrow(claims)))
  }
  #the final set of claims we are keeping
  keep_f    <- claims$File
  history   <- history   %>% filter(File %in% keep_f)
  rules     <- rules     %>% filter(File %in% keep_f)
  parts     <- parts     %>% filter(File %in% keep_f)
  labours   <- labours   %>% filter(File %in% keep_f)
  # Apply monetary plausibility ceilings to raw child tables
  cleaned  <- clean_child_prices(parts, labours)
  parts    <- cleaned$parts
  labours  <- cleaned$labours
  base <- claims %>% clean_blanks() %>% select_claims_features()
  
  #left joins to preserve information, NA where there's no info available
  master <- base %>%
    left_join(agg_history(history),     by = "File") %>%
    left_join(agg_parts(parts),         by = "File") %>%
    left_join(agg_labours(labours),     by = "File")
  
  count_cols <- c("n_claimants","n_main_claimants","n_secondary_claimants",
                  "n_history_events","n_distinct_status","n_total_loss_history",
                  "n_parts","n_parts_categories","n_labour_items",
                  "n_labour_replace")
  master <- master %>%
    mutate(across(any_of(count_cols), ~replace_na(., 0)))
  
  master
}

# 5b. INTRA-CASEID COLLAPSE 

collapse_intra_caseid <- function(df) {
  if (!isTRUE(COLLAPSE_CASEID)) return(df)
  before <- nrow(df)
  
  out <- df %>%
    group_by(Input.Accident.CaseID) %>%
    arrange(Input.Accident.NotificationDate, .by_group = TRUE) %>%
    mutate(
      n_repest_snapshots      = n(),
      repest_span_days        = as.numeric(difftime(
        last(Input.Accident.NotificationDate),
        first(Input.Accident.NotificationDate),
        units = "days")),
      estimate_revision_ratio = ifelse(
        !is.na(first(hist_repairer_est_total)) &
          first(hist_repairer_est_total) > 0,
        last(hist_repairer_est_total) /
          first(hist_repairer_est_total),
        NA_real_)
    ) %>%
    slice_max(Input.Accident.NotificationDate, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  message(sprintf("Intra-CaseID collapse: %d -> %d rows retained",
                  before, nrow(out)))
  out
}



# 5. TYPE CASTING

DATE_FIELDS <- c("Input.Policy.PolEffDate", "Input.Policy.PolEndDate",
                 "Input.Accident.LossDate", "Input.Accident.NotificationDate",
                 "Input.Driver.LicenseCoverStartDate",
                 "Input.Driver.LicenseCoverEndDate")

NUMERIC_FIELDS <- c("Input.Policy.SumInsured", "Input.Vehicle.OdometerReading",
                    "Input.Vehicle.VehManYear", "Input.Vehicle.EngCubicCap",
                    "Input.Accident.Financials.InitialEstimation",
                    "Input.Accident.Financials.ExcessAmt",
                    "Input.Accident.Financials.InsVatAmt",
                    "Input.Accident.TowingCharges",
                    "Input.Workshop.LabourParts.PartsDiscount")

cast_types <- function(df) {
  #the report list shows us how many values fails the coercion.
  report <- list()
  #Counts of how many values failed to be properly converted, and therefore are reflected as NA
  for (col in intersect(DATE_FIELDS, names(df))) {
    before <- sum(!is.na(df[[col]]))
    df[[col]] <- parse_dt(df[[col]])
    report[[col]] <- before - sum(!is.na(df[[col]]))
  }
  for (col in intersect(NUMERIC_FIELDS, names(df))) {
    before <- sum(!is.na(df[[col]]))
    df[[col]] <- as_num(df[[col]])
    report[[col]] <- before - sum(!is.na(df[[col]]))
  }
  #find every remaining character column after conversions and convert them to factors, Factors canbe encoded to aida
  cat_cols <- setdiff(names(df)[sapply(df, is.character)], claim_keys)
  df <- df %>% mutate(across(all_of(cat_cols), as.factor))
  #keeping only the entries where the count of failed  conversions is greater than 0
  failed <- report[unlist(report) > 0]
  if (length(failed))
    message("Coercion failures (now NA): ",
            paste(sprintf("%s=%d", names(failed), unlist(failed)), collapse = ", "))
  # --- Plausibility ceilings on claim-level monetary fields -----------
  if (isTRUE(APPLY_PRICE_CEILINGS)) {
    total_fields <- c("Input.Accident.Financials.InitialEstimation",
                      "Input.Accident.TowingCharges")
    for (col in intersect(total_fields, names(df))) {
      df[[col]] <- null_above(df[[col]], PRICE_CEIL_TOTAL, col)
    }
    if ("Input.Policy.SumInsured" %in% names(df)) {
      df[["Input.Policy.SumInsured"]] <-
        null_above(df[["Input.Policy.SumInsured"]], PRICE_CEIL_INSURED,
                   "Input.Policy.SumInsured")
    }
  }
  df
}

# 6. FEATURE ENGINEERING
#turning dates into meaningful periods, that algo can actually use
engineer_features <- function(df) {
  d <- function(name) if (name %in% names(df)) df[[name]] else as.POSIXct(NA)
  loss   <- d("Input.Accident.LossDate")
  notif  <- d("Input.Accident.NotificationDate")
  peff   <- d("Input.Policy.PolEffDate")
  pend   <- d("Input.Policy.PolEndDate")
  licS   <- d("Input.Driver.LicenseCoverStartDate")
  licE   <- d("Input.Driver.LicenseCoverEndDate")
  dd <- function(a, b) as.numeric(difftime(a, b, units = "days"))
  df %>% mutate(
    #Days between the loss event and the claim being reported
    reporting_delay_days  = dd(notif, loss),
    #Total policy length in days
    policy_duration_days  = dd(pend,  peff),
    #how many days into the policy the loss occurred
    days_into_policy      = dd(loss,  peff),
    #how many days into the policy the loss occurred
    days_to_policy_end    = dd(pend,  loss),
    #driver lisence cover duration
    license_duration_days = dd(licE,  licS),
    #if the loss occurred before the policy started, impossible
    loss_before_policy    = as.integer(days_into_policy   < 0),
    #loss after policy end
    loss_after_policy_end = as.integer(days_to_policy_end < 0),
    #if the loss occurred within the first 30 days of the policy
    claim_near_pol_start  = as.integer(days_into_policy   >= 0 & days_into_policy   < 30),
    #if the loss occurred in the last 30 days of the policy
    claim_near_pol_end    = as.integer(days_to_policy_end >= 0 & days_to_policy_end < 30),
    #days since prior claim
    hist_days_since_prior = as.numeric(difftime(loss, hist_most_recent_prior, units = "days")),
    #vehicle age at the time of loss
    vehicle_age_at_loss   = year(loss) - suppressWarnings(
      as.integer(as.character(Input.Vehicle.VehManYear)))
  )
}

# 7. MISSINGNESS AUDIT + 80/20 FLAGS

missingness_audit <- function(df, keys = claim_keys) {
  cols <- setdiff(names(df), keys)
  pr   <- presence_rate(df[cols])
  tibble(
    field     = cols,
    presence  = round(pr, 4),
    n_missing = sapply(df[cols], function(c) sum(is.na(c))),
    band = case_when(pr >= PRESENCE_HI ~ "mostly_present",
                     pr <= PRESENCE_LO ~ "mostly_absent",
                     TRUE              ~ "variable")
  ) %>% arrange(presence)
}

build_flag_matrix <- function(df, audit) {
  flags <- tibble(.rows = nrow(df))
  for (i in seq_len(nrow(audit))) {
    f <- audit$field[i]; col <- df[[f]]
    if (audit$band[i] == "mostly_present")
      flags[[paste0("flag_missing__", f)]] <- as.integer(is.na(col))
    else if (audit$band[i] == "mostly_absent")
      flags[[paste0("flag_present__", f)]] <- as.integer(!is.na(col))
    else
      flags[[paste0("miss__", f)]] <- as.integer(is.na(col))
  }
  flags$n_flags <- rowSums(flags)
  flags
}

# 8. AIDA / TIX PREP 
#only numericals, no NA, no degenerate feature, comparable scale among features
prepare_for_aida <- function(df, flags) {
  keys <- df %>% select(any_of(claim_keys))
  
  num <- df %>% select(where(is.numeric))
  cat <- df %>% select(where(is.factor))
  
  nzv <- sapply(num, function(c) {
    v <- c[!is.na(c)]; length(unique(v)) <= 1 })
  num <- num[, !nzv, drop = FALSE]
  
  money <- intersect(c("Input.Policy.SumInsured",
                       "Input.Accident.Financials.InitialEstimation",
                       "Input.Accident.Financials.ExcessAmt",
                       "Input.Accident.Financials.InsVatAmt",
                       "Input.Accident.TowingCharges",
                       "parts_actual_total","parts_submit_total",
                       "parts_price_gap","labour_amount_total",
                       "hist_offerpaid_total","hist_repairer_est_total"),
                     names(num))
  #Median impute all remaining NAs , the flag step already captured missingness pattern fpr we do not lose signal
  num <- num %>% mutate(across(all_of(money), ~log1p(pmax(., 0, na.rm = TRUE))))
  
  num <- num %>% mutate(across(everything(),
                               ~replace_na(., median(., na.rm = TRUE))))
  #robust scaling - subtract median and fivide by IQR - better than z-scoring since its affected by outliers
  rscale <- function(x) {
    iqr <- IQR(x, na.rm = TRUE)
    if (iqr == 0) iqr <- mad(x, na.rm = TRUE)        # robust fallback
    if (iqr == 0) iqr <- sd(x, na.rm = TRUE)          # last resort
    if (iqr == 0 || is.na(iqr)) iqr <- 1              # truly constant -> leave as-is
    (x - median(x, na.rm = TRUE)) / iqr
  }
  
  num_scaled <- as.data.frame(lapply(num, rscale))
  
  cat_enc <- names(cat) %>%
    purrr::map(function(nm) {
      x  <- cat[[nm]]
      
      # Add NA as an explicit "Missing" level so model.matrix doesn't
      # silently drop those rows (which breaks bind_cols downstream).
      x  <- addNA(x, ifany = TRUE)
      levels(x)[is.na(levels(x))] <- "Missing"
      
      lv <- table(x) / length(x)
      x  <- fct_other(x, keep = names(lv)[lv >= RARE_LEVEL_MIN],
                      other_level = "Other")
      x  <- droplevels(x)
      
      if (nlevels(x) < 2) return(NULL)
      
      if (nlevels(x) > 25) {
        freq <- as.numeric(lv[as.character(x)])
        freq[is.na(freq)] <- 0
        setNames(tibble(freq), paste0("freq__", nm))
      } else {
        mm <- model.matrix(~ x - 1)
        colnames(mm) <- paste0(nm, "__", levels(x)[seq_len(ncol(mm))])
        as_tibble(mm)
      }
    }) %>%
    purrr::compact() %>%
    bind_cols()
  
  X <- bind_cols(num_scaled, cat_enc, flags %>% select(-n_flags))
  X <- X[, colSums(is.na(X)) == 0, drop = FALSE]
  
  manifest <- tibble(
    feature = names(X),
    source  = case_when(
      str_starts(names(X), "flag_|miss__") ~ "missingness_flag",
      str_starts(names(X), "freq__")       ~ "freq_encoded_categorical",
      str_detect(names(X), "__")           ~ "onehot_categorical",
      TRUE                                 ~ "numeric"))
  
  list(X = as.matrix(X), keys = keys, manifest = manifest)
}


# =====================================================================
# 6. RUN
# =====================================================================

run_pipeline <- function(claims, history, rules, parts, labours) {
  master <- build_master(claims, history, rules, parts, labours)
  master <- cast_types(master)
  master <- collapse_intra_caseid(master)  
  master <- engineer_features(master)
  
  audit  <- missingness_audit(master)
  flags  <- build_flag_matrix(master, audit)
  aida   <- prepare_for_aida(master, flags)
  
  message(sprintf("master_claim: %d rows x %d cols | AIDA matrix: %d x %d",
                  nrow(master), ncol(master), nrow(aida$X), ncol(aida$X)))
  
  list(master_claim = master,
       flag_report  = audit,
       flag_matrix  = flags,
       X            = aida$X,
       keys         = aida$keys,
       feature_manifest = aida$manifest)
}

out <- run_pipeline(df, df_history, df_rules, df_parts, df_labours)
###END


# =====================================================================
# 7. POST-RUN DIAGNOSTICS   [optional -- guarded by RUN_DIAGNOSTICS]
# =====================================================================

if (isTRUE(RUN_DIAGNOSTICS)) {

  #check how many claims have absurd values 
  # How many claims have absurd parts values?
  out$master_claim %>%
    dplyr::filter(parts_submit_total > 1e8) %>%
    dplyr::select(Input.Accident.CaseID, n_parts,
                  parts_submit_total, parts_actual_total) %>%
    print(n = 50)

  # And in the raw parts table, before aggregation -- find the source rows
  df_parts %>%
    dplyr::mutate(sp = as_num(SubmittedPartsPrice)) %>%
    dplyr::filter(sp > 1e8) %>%
    dplyr::select(File, SubmittedPartsPrice, ActualPartsPrice, PartsCategory) %>%
    head(50)

  out$master_claim %>%
    dplyr::select(where(is.numeric)) %>%
    dplyr::summarise(dplyr::across(everything(), ~max(., na.rm = TRUE))) %>%
    tidyr::pivot_longer(everything(), names_to = "feature", values_to = "max_value") %>%
    dplyr::arrange(dplyr::desc(max_value)) %>%
    print(n = 25)

  out$master_claim %>%
    dplyr::filter(parts_submit_total > 7e8) %>%
    dplyr::select(Input.Accident.CaseID, n_parts,
                  parts_submit_total, parts_actual_total,
                  parts_price_gap, n_inflated_lines) %>%
    print()

} # end RUN_DIAGNOSTICS
