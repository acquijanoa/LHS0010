#!/usr/bin/env Rscript
# One RTF per country: regional posterior prevalence (mean, 95% CrI) by survey wave +
# national total inv_logit(alpha + beta[t]).
#
# Inputs (any one path per country is enough if fit RDS exists):
#   *_p_mean_by_cell.csv — optional p_q025/p_q975 (filled from fit RDS if missing)
#   *_p_total_by_wave.csv — optional (built from fit RDS or run diagnostics)
#   *_fit.rds — used when CSVs missing or incomplete
#
# Suggested order: diagnostics (writes p_total CSV) → this script; or full fit (writes both CSVs).
#
# Outputs:
#   output/reports/spatial_bayes_prevalence_Colombia.rtf
#   output/reports/spatial_bayes_prevalence_Honduras.rtf
#   output/tables/spatial_bayes_prevalence_*_long.csv

suppressPackageStartupMessages({
  library(cmdstanr)
  library(dplyr)
  library(readr)
  library(reporter)
  library(tidyr)
  library(tibble)
})

args_cmd <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_cmd[grep("^--file=", args_cmd)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  setwd(dirname(dirname(script_dir)))
}

fmt <- function(m, lo, hi) {
  sprintf("%.1f%% (%.1f–%.1f)", 100 * m, 100 * lo, 100 * hi)
}

p_total_table_from_draws <- function(p_total_draws, surveys) {
  tibble(
    survey = surveys,
    p_total_mean = as.numeric(colMeans(p_total_draws)),
    p_total_q025 = as.numeric(apply(p_total_draws, 2L, quantile, 0.025)),
    p_total_q975 = as.numeric(apply(p_total_draws, 2L, quantile, 0.975))
  )
}

p_total_draws_from_fit <- function(fit, surveys) {
  pt <- tryCatch(
    fit$draws(variables = "p_total", format = "draws_matrix"),
    error = function(e) NULL
  )
  if (!is.null(pt) && ncol(pt) == length(surveys)) {
    colnames(pt) <- surveys
    return(pt)
  }
  dm <- fit$draws(
    variables = c("alpha", paste0("beta[", seq_along(surveys), "]")),
    format = "draws_matrix"
  )
  a <- dm[, "alpha"]
  n <- length(a)
  mat <- matrix(NA_real_, n, length(surveys))
  colnames(mat) <- surveys
  mat[, 1] <- plogis(a)
  for (t in seq_len(length(surveys))[-1]) {
    mat[, t] <- plogis(a + dm[, paste0("beta[", t, "]")])
  }
  mat
}

render_country <- function(
  country_name,
  regions_all,
  surveys,
  p_cell_csv,
  p_total_csv,
  fit_rds,
  out_rtf,
  out_long_csv
) {
  if (!file.exists(fit_rds) && !file.exists(p_cell_csv)) {
    stop("Need ", fit_rds, " or ", p_cell_csv)
  }
  fit <- if (file.exists(fit_rds)) readRDS(fit_rds) else NULL

  if (file.exists(p_total_csv)) {
    tot <- read_csv(p_total_csv, show_col_types = FALSE, col_types = cols(survey = col_character()))
  } else if (!is.null(fit)) {
    tot <- p_total_table_from_draws(p_total_draws_from_fit(fit, surveys), surveys)
    message("Built p_total from fit RDS (", basename(fit_rds), ") — consider saving CSV via diagnostics.")
  } else {
    stop("Missing ", p_total_csv, " and ", fit_rds)
  }

  if (!file.exists(p_cell_csv)) {
    stop("Missing ", p_cell_csv, " — run BYM2 fit to write regional posterior CSV.")
  }
  cell <- read_csv(p_cell_csv, show_col_types = FALSE, col_types = cols(survey = col_character())) %>%
    mutate(region = as.character(region), survey = as.character(survey)) %>%
    filter(region %in% regions_all, survey %in% surveys)

  need_q <- !all(c("p_q025", "p_q975") %in% names(cell))
  if (need_q) {
    if (is.null(fit)) stop("Regional CSV lacks p_q025/p_q975; re-run fit or provide ", fit_rds)
    dm <- fit$draws(variables = "p", format = "draws_matrix")
    if (nrow(cell) != ncol(dm)) {
      stop("p_mean_by_cell rows (", nrow(cell), ") != posterior p columns (", ncol(dm), ") — rerun fit.")
    }
    cell$p_mean <- as.numeric(colMeans(dm))
    cell$p_q025 <- as.numeric(apply(dm, 2L, quantile, 0.025))
    cell$p_q975 <- as.numeric(apply(dm, 2L, quantile, 0.975))
  }

  long_out <- cell %>%
    transmute(
      country = country_name,
      level = "region",
      region,
      survey,
      p_mean,
      p_q025,
      p_q975,
      estimate_95cri = fmt(p_mean, p_q025, p_q975)
    ) %>%
    bind_rows(
      tot %>%
        transmute(
          country = country_name,
          level = "national_total",
          region = NA_character_,
          survey = as.character(survey),
          p_mean = p_total_mean,
          p_q025 = p_total_q025,
          p_q975 = p_total_q975,
          estimate_95cri = fmt(p_total_mean, p_total_q025, p_total_q975)
        )
    )
  write_csv(long_out, out_long_csv)

  nat <- tot %>%
    mutate(estimate_95cri = fmt(p_total_mean, p_total_q025, p_total_q975)) %>%
    select(survey, estimate_95cri) %>%
    pivot_wider(names_from = survey, values_from = estimate_95cri)
  nat_row <- tibble(
    section = "National total (Stan)",
    region = "inv_logit(alpha + beta[t]); wave 1 = baseline",
    wv1 = nat[[surveys[1]]],
    wv2 = nat[[surveys[2]]],
    wv3 = nat[[surveys[3]]]
  )

  reg_wide <- cell %>%
    mutate(estimate_95cri = fmt(p_mean, p_q025, p_q975)) %>%
    select(region, survey, estimate_95cri) %>%
    pivot_wider(names_from = survey, values_from = estimate_95cri)
  for (s in surveys) {
    if (!s %in% names(reg_wide)) reg_wide[[s]] <- NA_character_
  }
  reg_wide <- tibble(region = regions_all) %>%
    left_join(reg_wide, by = "region") %>%
    select(region, all_of(surveys)) %>%
    rename(wv1 = all_of(surveys[1]), wv2 = all_of(surveys[2]), wv3 = all_of(surveys[3])) %>%
    mutate(section = "Region", .before = 1) %>%
    rename(region_name = region)

  wide <- bind_rows(nat_row, reg_wide)
  dir.create(dirname(out_rtf), showWarnings = FALSE, recursive = TRUE)

  tbl <- create_table(wide, first_row_blank = TRUE) %>%
    titles(
      paste0("Table. Bayesian prevalence — ", country_name),
      "Regional: inv_logit(theta[r,t]). National: inv_logit(alpha + beta[t])."
    ) %>%
    stub(c(section, region_name), label = " ") %>%
    define(section, label_row = TRUE, blank_after = FALSE) %>%
    define(region_name, align = "left", indent = 0.12) %>%
    spanning_header(from = "wv1", to = "wv3", label = "Survey wave", level = 1) %>%
    define(wv1, label = surveys[1], align = "center", width = 1.15) %>%
    define(wv2, label = surveys[2], align = "center", width = 1.15) %>%
    define(wv3, label = surveys[3], align = "center", width = 1.15)

  rpt <- create_report(
    out_rtf,
    output_type = "RTF",
    font = "Times",
    font_size = 9,
    orientation = "portrait"
  ) %>%
    add_content(tbl)
  write_report(rpt)
  message("Wrote ", out_rtf)
}

co_regions <- c(
  "Antioquia", "Atlantico", "Bogota", "Bolivar", "Boyaca", "Caldas", "Caqueta", "Cauca",
  "Cesar", "Cordoba", "Cundinamarca", "Choco", "Huila", "La Guajira", "Magdalena", "Meta",
  "Narimo", "Norte de Santander", "Quindio", "Risaralda", "Santander", "Sucre", "Tolima",
  "Valle", "Arauca", "Casanare", "Putumayo", "San Andres y Providencia", "Amazonas",
  "Guainia", "Guaviare", "Vaupes", "Vichada"
)
co_surveys <- c("2005", "2010", "2015")
hn_regions <- c(
  "ATLANTIDA", "COLON", "COMAYAGUA", "COPAN", "CORTES", "CHOLUTECA", "EL PARAISO",
  "FRANCISCO MORAZAN", "GRACIAS A DIOS", "INTIBUCA", "ISLAS DE LA BAHIA", "LA PAZ",
  "LEMPIRA", "OCOTEPEQUE", "OLANCHO", "SANTA BARBARA", "VALLE", "YORO"
)
hn_surveys <- c("2005-06", "2011-12", "2019")

dir.create("output/reports", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

render_country(
  "Colombia", co_regions, co_surveys,
  "output/spatial_colombia_st/colombia_bym2_fh_st_p_mean_by_cell.csv",
  "output/spatial_colombia_st/colombia_bym2_fh_st_p_total_by_wave.csv",
  "output/spatial_colombia_st/colombia_bym2_fh_st_fit.rds",
  "output/reports/spatial_bayes_prevalence_Colombia.rtf",
  "output/tables/spatial_bayes_prevalence_Colombia_long.csv"
)
render_country(
  "Honduras", hn_regions, hn_surveys,
  "output/spatial_honduras_st/hnir_bym2_fh_st_p_mean_by_cell.csv",
  "output/spatial_honduras_st/hnir_bym2_fh_st_p_total_by_wave.csv",
  "output/spatial_honduras_st/hnir_bym2_fh_st_fit.rds",
  "output/reports/spatial_bayes_prevalence_Honduras.rtf",
  "output/tables/spatial_bayes_prevalence_Honduras_long.csv"
)
