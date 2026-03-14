#!/usr/bin/env Rscript
# Colombia: design-based adolescent unmet_need by department × DHS wave (2005, 2010, 2015).
# San Andres y Providencia = island (is_mainland = 0); ICAR excludes it in spatial model.
#
#   Rscript scripts/03_analysis/colombia_region_adolescent_logit_prevalence.R
#
# Writes: output/tables/colombia_region_adolescent_logit_prevalence.csv

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

# Same department labels as derive_coir53/61/72 (region column)
regions_all <- c(
  "Antioquia", "Atlantico", "Bogota", "Bolivar", "Boyaca", "Caldas", "Caqueta", "Cauca",
  "Cesar", "Cordoba", "Cundinamarca", "Choco", "Huila", "La Guajira", "Magdalena", "Meta",
  "Narimo", "Norte de Santander", "Quindio", "Risaralda", "Santander", "Sucre", "Tolima",
  "Valle", "Arauca", "Casanare", "Putumayo", "San Andres y Providencia", "Amazonas",
  "Guainia", "Guaviare", "Vaupes", "Vichada"
)
paths <- c(
  "data/derived/coir53fl_derived.rds",
  "data/derived/coir61fl_derived.rds",
  "data/derived/coir72fl_derived.rds"
)
surveys <- c("2005", "2010", "2015")
miss <- paths[!file.exists(paths)]
if (length(miss)) {
  stop("Missing: ", paste(miss, collapse = ", "), "\nRun derive_coir53.R, derive_coir61.R, derive_coir72.R")
}

eps <- 1e-8
out_list <- vector("list", length(paths))
for (i in seq_along(paths)) {
  df <- readRDS(paths[[i]]) %>%
    filter(Adolescent == 1L) %>%
    filter(
      !is.na(unmet_need), !is.na(region), nzchar(as.character(region)),
      !is.na(psu_id), !is.na(strat), !is.na(weight), weight > 0
    ) %>%
    mutate(
      psu_id = as.numeric(psu_id),
      strat = as.numeric(strat),
      weight = as.numeric(weight),
      region = as.character(region),
      unmet_need = as.numeric(unmet_need)
    )
  des <- svydesign(ids = ~psu_id, strata = ~strat, weights = ~weight, data = df, nest = TRUE)
  by_res <- svyby(~unmet_need, ~region, des, svymean, na.rm = TRUE, keep.names = FALSE, drop.empty.groups = TRUE)
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
      is_mainland = as.integer(region != "San Andres y Providencia"),
      cell_id = paste(region, " | ", survey, sep = ""),
      has_obs = as.integer(!is.na(prevalence))
    ) %>%
    select(survey, region, is_mainland, cell_id, has_obs, prevalence, se_prevalence, logit_prevalence, se_logit)
}

out_tbl <- bind_rows(out_list) %>%
  mutate(region = factor(region, levels = regions_all)) %>%
  arrange(survey, region) %>%
  mutate(region = as.character(region), survey = as.character(survey))

out_dir <- "output/tables"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(out_tbl, file.path(out_dir, "colombia_region_adolescent_logit_prevalence.csv"))
message("Wrote ", file.path(out_dir, "colombia_region_adolescent_logit_prevalence.csv"))
