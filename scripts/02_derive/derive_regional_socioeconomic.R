#!/usr/bin/env Rscript
# Design-based regional proportions among ADOLESCENTS — six SES covariates (your list).
#
#   Rscript scripts/02_derive/derive_regional_socioeconomic.R
#
# See regional_socioeconomic_VARIABLES.md for column definitions.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(survey)
})

options(survey.lonely.psu = "adjust")

args_cmd <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_cmd[grep("^--file=", args_cmd)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  setwd(dirname(dirname(script_dir)))
}

# Six binary outcomes (0/1) → svyby mean = design-based proportion
VAR_DEFS <- tibble::tibble(
  internal = c("poor_q1", "rich_q5", "no_edu", "sec_plus", "married", "parity_ge2"),
  p_col = c(
    "p_poorest_wealth_quintile",
    "p_richest_wealth_quintile",
    "p_no_education",
    "p_secondary_or_higher",
    "p_married_union_adolescent",
    "p_parity_two_or_more"
  ),
  se_col = c(
    "se_poorest_wealth_quintile",
    "se_richest_wealth_quintile",
    "se_no_education",
    "se_secondary_or_higher",
    "se_married_union_adolescent",
    "se_parity_two_or_more"
  )
)

specs <- list(
  list(country = "Honduras", survey_year = "2011-12", rds = "data/derived/hnir62fl_derived.rds"),
  list(country = "Honduras", survey_year = "2019", rds = "data/derived/hnir72fl_derived.rds"),
  list(country = "Colombia", survey_year = "2010", rds = "data/derived/coir61fl_derived.rds"),
  list(country = "Colombia", survey_year = "2015", rds = "data/derived/coir72fl_derived.rds")
)

ensure_w_quintile <- function(df, rds_path) {
  if ("w_quintile" %in% names(df)) return(df)
  if ("V190" %in% names(df)) {
    message("w_quintile from V190: ", basename(rds_path))
    return(mutate(df, w_quintile = if_else(V190 %in% 1:5, as.integer(V190), NA_integer_)))
  }
  if ("windex5" %in% names(df)) {
    message("w_quintile from windex5: ", basename(rds_path))
    return(mutate(df, w_quintile = if_else(windex5 %in% 1:5, as.integer(windex5), NA_integer_)))
  }
  stop("No w_quintile/V190/windex5 in ", rds_path)
}

prep_adol <- function(df, survey_year) {
  df %>%
    mutate(
      survey_year = survey_year,
      psu_id = as.numeric(psu_id),
      strat = as.numeric(strat),
      weight = as.numeric(weight),
      region = as.character(region)
    ) %>%
    filter(
      Adolescent == 1L,
      !is.na(region), region != "",
      !is.na(psu_id), !is.na(strat), !is.na(weight), weight > 0
    ) %>%
    mutate(
      strat_combined = paste(survey_year, strat, sep = "_"),
      psu_combined = paste(survey_year, strat, psu_id, sep = "_"),
      poor_q1 = as.integer(w_quintile == 1L),
      rich_q5 = as.integer(w_quintile == 5L),
      no_edu = as.integer(education_c4 == 0L),
      sec_plus = as.integer(!is.na(education_c4) & education_c4 >= 2L),
      married = as.integer(married_union == 1L),
      parity_ge2 = as.integer(!is.na(parity) & parity >= 2L)
    )
}

estimate_region <- function(df, country, survey_year) {
  des <- svydesign(
    ids = ~psu_combined,
    strata = ~strat_combined,
    weights = ~weight,
    data = df,
    nest = TRUE
  )
  res <- NULL
  for (i in seq_len(nrow(VAR_DEFS))) {
    vn <- VAR_DEFS$internal[i]
    f <- as.formula(paste0("~", vn))
    x <- svyby(f, ~region, des, svymean, covmat = TRUE, na.rm = TRUE, keep.names = FALSE)
    reg <- as.character(x$region)
    mu <- as.numeric(x[[vn]])
    V <- vcov(x)
    se <- if (length(mu) == 1L) sqrt(pmax(as.numeric(V), 0)) else sqrt(pmax(diag(V), 0))
    ti <- tibble(region = reg)
    ti[[VAR_DEFS$p_col[i]]] <- mu
    ti[[VAR_DEFS$se_col[i]]] <- se
    res <- if (is.null(res)) ti else full_join(res, ti, by = "region")
  }

  miss_p <- setdiff(VAR_DEFS$p_col, names(res))
  miss_s <- setdiff(VAR_DEFS$se_col, names(res))
  if (length(miss_p) || length(miss_s)) {
    stop("Missing output columns: ", paste(c(miss_p, miss_s), collapse = ", "))
  }

  n_ct <- df %>% count(region, name = "n_adolescents")
  res %>%
    mutate(country = country, survey_year = survey_year, .before = 1) %>%
    left_join(n_ct, by = "region") %>%
    mutate(region = as.character(region))
}

out <- map_dfr(specs, function(s) {
  if (!file.exists(s$rds)) {
    warning("Skip (missing): ", s$rds)
    return(NULL)
  }
  df <- readRDS(s$rds) %>% ensure_w_quintile(s$rds)
  df <- prep_adol(df, s$survey_year)
  if (nrow(df) < 30L) return(NULL)
  estimate_region(df, s$country, s$survey_year)
})

if (!nrow(out)) stop("No regional rows; run derive_* first.")

out_path_rds <- "data/derived/regional_socioeconomic_adol.rds"
out_path_csv <- "data/derived/regional_socioeconomic_adol.csv"
saveRDS(out, out_path_rds)
write_csv(out, out_path_csv)
walk(split(out, out$country), function(x) {
  write_csv(x, file.path("data/derived", paste0("regional_socioeconomic_adol_", x$country[1], ".csv")))
})

message("Wrote ", nrow(out), " rows; six proportions + six SEs + n_adolescents.")
message("Columns: ", paste(setdiff(names(out), c("country", "survey_year", "region", "n_adolescents")), collapse = ", "))
print(out, n = min(25L, nrow(out)))
