#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(survey)
  library(tibble)
  library(tidyr)
})

options(survey.lonely.psu = "adjust")

dataset_specs <- tribble(
  ~dataset, ~path,
  "hnir52", "data/derived/hnir52fl_derived.rds",
  "hnir62", "data/derived/hnir62fl_derived.rds",
  "hnir72", "data/derived/hnir72sd_derived.rds"
)

label_agegroup <- function(x) {
  case_when(
    x == 1L ~ "<19",
    x == 2L ~ "20-24",
    x == 3L ~ "25-34",
    x == 4L ~ "35+",
    TRUE ~ NA_character_
  )
}

prepare_dataset <- function(df) {
  df %>%
    mutate(
      agegroup_c4 = factor(
        label_agegroup(agegroup_c4),
        levels = c("25-34", "20-24", "<19", "35+")
      ),
      region = factor(region),
      psu_id = as.numeric(psu_id),
      strat = as.numeric(strat),
      weight = as.numeric(weight),
      unmet_need = as.numeric(unmet_need)
    ) %>%
    filter(
      !is.na(unmet_need),
      !is.na(agegroup_c4),
      !is.na(region),
      !is.na(psu_id),
      !is.na(strat),
      !is.na(weight)
    )
}

estimate_direct <- function(df) {
  design <- svydesign(
    ids = ~psu_id,
    strata = ~strat,
    weights = ~weight,
    data = df,
    nest = TRUE
  )

  direct_age_region <- svyby(
    ~unmet_need,
    ~agegroup_c4 + region,
    design,
    svymean,
    vartype = c("se", "var", "ci", "cv"),
    na.rm = TRUE,
    keep.names = FALSE,
    drop.empty.groups = TRUE
  ) %>%
    as_tibble() %>%
    rename(
      estimate = unmet_need,
      std_error = se,
      variance = var,
      ci_lower = ci_l,
      ci_upper = ci_u,
      cv = cv
    )

  # Add unweighted sample size per cell
  n_unweighted <- df %>%
    count(agegroup_c4, region, name = "n_unweighted")

  # Add weighted denominator per cell
  weighted_totals <- df %>%
    group_by(agegroup_c4, region) %>%
    summarise(weighted_total = sum(weight, na.rm = TRUE), .groups = "drop")

  direct_age_region %>%
    left_join(n_unweighted, by = c("agegroup_c4", "region")) %>%
    left_join(weighted_totals, by = c("agegroup_c4", "region")) %>%
    transmute(
      margin_type = "direct_age_by_region",
      agegroup = as.character(agegroup_c4),
      region = as.character(region),
      estimate,
      std_error,
      variance,
      ci_lower,
      ci_upper,
      cv,
      n_unweighted,
      weighted_total
    )
}

process_dataset <- function(dataset, path) {
  if (!file.exists(path)) {
    warning(sprintf("%s not found at %s; skipping", dataset, path))
    return(NULL)
  }

  df <- readRDS(path)
  df_ready <- prepare_dataset(df)

  if (!nrow(df_ready)) {
    warning(sprintf("%s has no complete cases after filtering; skipping", dataset))
    return(NULL)
  }

  estimate_direct(df_ready) %>%
    mutate(dataset = dataset, .before = 1)
}

results <- dataset_specs %>%
  mutate(result = map2(dataset, path, process_dataset)) %>%
  pull(result) %>%
  compact() %>%
  bind_rows()

if (!nrow(results)) {
  stop("No datasets were processed; check that derived RDS files exist.")
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1) {
  output_path <- args[[1]]
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(results, output_path)
  message("Saved direct age x region estimates to ", output_path)
} else {
  print(results, n = nrow(results))
}
