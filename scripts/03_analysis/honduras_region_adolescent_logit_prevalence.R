#!/usr/bin/env Rscript
# Design-based weighted unmet_need prevalence by region × survey — **adolescents only**.
# Outputs **sampling SE only** per cell (probability scale from survey; logit SE via delta method).
#
# **One svydesign per survey** (not one pooled design across waves). A single combined
# svydesign over 2005-06 + 2011-12 + 2019 makes survey::svyby(..., ~region + survey, ...)
# return **SE = Inf** for every cell (invalid multi-survey variance). Per-wave designs match
# DHS/MICS practice and give finite SEs.
#
#   Rscript scripts/03_analysis/honduras_region_adolescent_logit_prevalence.R
#
# Writes:
#   output/tables/honduras_region_adolescent_logit_prevalence.csv
#     columns: survey, region, is_mainland (1 = mainland department, 0 = Islas de la Bahía only),
#              cell_id, has_obs (1 = design estimate, 0 = no sample that wave),
#              prevalence, se_prevalence, logit_prevalence, se_logit
#
# Every wave has **18 rows** (DHS departments 1–18). Regions not sampled in a wave (e.g. GRACIAS A DIOS,
# ISLAS DE LA BAHIA in 2005–06 HNIR52) appear with NA estimates.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(survey)
})

args_cmd <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_cmd[grep("^--file=", args_cmd)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  setwd(dirname(dirname(script_dir)))
}
options(survey.lonely.psu = "adjust")

# Fixed order: DHS V024 1 … 18 (same labels as derive_hnir52 / hnir62 / hnir72fl)
regions_all <- c(
  "ATLANTIDA", "COLON", "COMAYAGUA", "COPAN", "CORTES", "CHOLUTECA", "EL PARAISO",
  "FRANCISCO MORAZAN", "GRACIAS A DIOS", "INTIBUCA", "ISLAS DE LA BAHIA", "LA PAZ",
  "LEMPIRA", "OCOTEPEQUE", "OLANCHO", "SANTA BARBARA", "VALLE", "YORO"
)

paths <- c(
  "data/derived/hnir52fl_derived.rds",
  "data/derived/hnir62fl_derived.rds",
  "data/derived/hnir72fl_derived.rds"
)
surveys <- c("2005-06", "2011-12", "2019")
miss <- paths[!file.exists(paths)]
if (length(miss)) {
  stop("Missing derived RDS: ", paste(miss, collapse = ", "), "\nRun derive_hnir52.R, derive_hnir62.R, derive_hnir72fl.R")
}

eps <- 1e-8
out_list <- vector("list", length(paths))

for (i in seq_along(paths)) {
  df <- readRDS(paths[[i]]) %>%
    filter(Adolescent == 1L) %>%
    filter(
      !is.na(unmet_need), !is.na(region), nzchar(region),
      !is.na(psu_id), !is.na(strat), !is.na(weight), weight > 0
    ) %>%
    mutate(
      psu_id = as.numeric(psu_id),
      strat = as.numeric(strat),
      weight = as.numeric(weight),
      region = factor(as.character(region)),
      unmet_need = as.numeric(unmet_need)
    )

  des <- svydesign(
    ids = ~psu_id, strata = ~strat, weights = ~weight,
    data = df, nest = TRUE
  )

  by_res <- svyby(
    ~unmet_need, ~region, des, svymean,
    na.rm = TRUE, keep.names = FALSE, drop.empty.groups = TRUE
  )

  se_prob <- as.numeric(SE(by_res))
  p_hat <- as.numeric(by_res$unmet_need)
  p_clip <- pmin(pmax(p_hat, eps), 1 - eps)
  se_logit <- se_prob / (p_clip * (1 - p_clip))

  obs <- tibble(
    region = as.character(by_res$region),
    prevalence = p_hat,
    se_prevalence = se_prob,
    logit_prevalence = qlogis(p_clip),
    se_logit = se_logit
  )

  out_list[[i]] <- tibble(region = regions_all, survey = surveys[[i]]) %>%
    left_join(obs, by = "region") %>%
    mutate(
      is_mainland = as.integer(region != "ISLAS DE LA BAHIA"),
      cell_id = paste(region, " | ", survey, sep = ""),
      has_obs = as.integer(!is.na(prevalence))
    ) %>%
    select(survey, region, is_mainland, cell_id, has_obs, prevalence, se_prevalence, logit_prevalence, se_logit)
}

out_tbl <- bind_rows(out_list) %>%
  mutate(region = factor(region, levels = regions_all)) %>%
  arrange(survey, region) %>%
  mutate(region = as.character(region))

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1L && dir.exists(args[[1]])) args[[1]] else "output/tables"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(out_tbl, file.path(out_dir, "honduras_region_adolescent_logit_prevalence.csv"))
message("Wrote ", file.path(out_dir, "honduras_region_adolescent_logit_prevalence.csv"))

if (any(!is.finite(out_tbl$se_prevalence) & !is.na(out_tbl$prevalence))) {
  warning("Some se_prevalence not finite; check lonely PSUs / sparse cells.")
}
