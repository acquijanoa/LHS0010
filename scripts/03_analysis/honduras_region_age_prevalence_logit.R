#!/usr/bin/env Rscript
# Design-based weighted unmet_need prevalence by region × age cell × survey:
# **Adolescent** (<20) and **Young_Adult** (20–34) separately (derive_hnir* indicators).
# Outputs on logit scale only: estimate, SE, full covariance across all cells
# (delta method from Taylor-linearization VCOV on p).
#
# Usage (project root):
#   Rscript scripts/03_analysis/honduras_region_age_prevalence_logit.R
#   Rscript scripts/03_analysis/honduras_region_age_prevalence_logit.R output/tables
#
# Writes:
#   output/tables/honduras_region_age_logit_prevalence.csv
#   output/tables/honduras_region_age_vcov_logit.rds
#   output/tables/honduras_region_age_vcov_logit_matrix.csv
#   output/tables/honduras_region_age_logit_full.rds

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

paths <- c("data/derived/hnir62fl_derived.rds", "data/derived/hnir72fl_derived.rds")
surveys <- c("2011-12", "2019")

if (!all(file.exists(paths))) {
  stop("Missing derived RDS. Run scripts/02_derive/derive_hnir62.R and derive_hnir72fl.R")
}

parts <- mapply(
  function(path, survey_label) {
    readRDS(path) %>%
      mutate(
        survey = survey_label,
        psu_id = as.numeric(psu_id),
        strat = as.numeric(strat),
        weight = as.numeric(weight),
        region = as.character(region),
        unmet_need = as.numeric(unmet_need)
      )
  },
  paths,
  surveys,
  SIMPLIFY = FALSE
)

df <- bind_rows(parts) %>%
  filter(Adolescent == 1L | Young_Adult == 1L) %>%
  filter(
    !is.na(unmet_need),
    !is.na(region), nzchar(region),
    !is.na(psu_id), !is.na(strat), !is.na(weight), weight > 0
  ) %>%
  mutate(
    agegroup = if_else(Adolescent == 1L, "Adolescent", "Young_Adult"),
    strat_combined = paste(survey, strat, sep = "_"),
    psu_combined = paste(survey, strat, psu_id, sep = "_"),
    region = factor(region),
    agegroup = factor(agegroup, levels = c("Adolescent", "Young_Adult")),
    survey = factor(survey, levels = surveys)
  )

des <- svydesign(
  ids = ~psu_combined,
  strata = ~strat_combined,
  weights = ~weight,
  data = df,
  nest = TRUE
)

by_res <- svyby(
  ~unmet_need,
  ~region + agegroup + survey,
  des,
  svymean,
  covmat = TRUE,
  na.rm = TRUE,
  keep.names = FALSE,
  drop.empty.groups = TRUE
)

vcov_prob <- attr(by_res, "var")
if (is.null(vcov_prob)) stop("svyby did not return covmat")
# Covariance structure (expected): almost all off-diagonals are **zero**.
# - Different **regions** → disjoint PSUs → zero covariance between regional means.
# - Different **surveys** (2011-12 vs 2019) → disjoint samples → zero across waves.
# - **Non-zero off-diagonals** only within the same (region, survey) pair, between
#   **Adolescent** and **Young_Adult** (same clusters contribute to both age means).
# So V is block-diagonal with 2×2 blocks (18 regions × 2 surveys = 36 non-zero off-diags), not full dense.

est_prob <- by_res$unmet_need
nms <- apply(
  by_res[, c("region", "agegroup", "survey")],
  1,
  function(r) paste(r, collapse = " | ")
)
rownames(vcov_prob) <- colnames(vcov_prob) <- nms

eps <- 1e-8
p_clip <- pmin(pmax(est_prob, eps), 1 - eps)
J <- diag(1 / (p_clip * (1 - p_clip)), nrow = length(est_prob), ncol = length(est_prob))
vcov_logit <- J %*% vcov_prob %*% J
rownames(vcov_logit) <- colnames(vcov_logit) <- nms
est_logit <- qlogis(p_clip)
se_logit <- sqrt(diag(vcov_logit))

out_tbl <- tibble(
  survey = as.character(by_res$survey),
  region = as.character(by_res$region),
  agegroup = as.character(by_res$agegroup),
  cell_id = nms,
  logit_prevalence = est_logit,
  se_logit = se_logit
)

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1 && dir.exists(args[[1]])) args[[1]] else "output/tables"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(out_tbl, file.path(out_dir, "honduras_region_age_logit_prevalence.csv"))
saveRDS(vcov_logit, file.path(out_dir, "honduras_region_age_vcov_logit.rds"))
saveRDS(
  list(
    agegroups = c("Adolescent", "Young_Adult"),
    labels = nms,
    logit_prevalence = est_logit,
    se_logit = se_logit,
    vcov_logit = vcov_logit,
    design_note = "Separate domain means per region×agegroup×survey; Taylor VCOV on p then delta to logit"
  ),
  file.path(out_dir, "honduras_region_age_logit_full.rds")
)
utils::write.csv(
  as.data.frame(vcov_logit),
  file.path(out_dir, "honduras_region_age_vcov_logit_matrix.csv")
)

message("Wrote ", file.path(out_dir, "honduras_region_age_logit_prevalence.csv"))
message("Wrote honduras_region_age_vcov_logit.rds + matrix; honduras_region_age_logit_full.rds")
print(out_tbl, n = min(40L, nrow(out_tbl)))
