#!/usr/bin/env Rscript
# Honduras: shared latent BYM2 Fay–Herriot model for BOTH survey waves.
# Outcome 1 = Adolescent logit prevalence; outcome 2 = Young Adult logit prevalence.
# Uses known 2x2 V_i per region from honduras_region_age_vcov_logit_matrix.csv.
#
# Inputs:
#   output/tables/honduras_region_age_logit_prevalence.csv
#   output/tables/honduras_region_age_vcov_logit_matrix.csv
#   data/shp/Shp_mgd.shp
# Requires: cmdstanr, sf, spdep, readr, dplyr
#
# Run from repo root:
#   Rscript scripts/03_analysis/honduras_shared_bym2_fay_herriot.R
#
# Env (optional):
#   LHS_BYM2_CHAINS=4 LHS_BYM2_WARMUP=1000 LHS_BYM2_SAMPLE=1000

args_cmd <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_cmd[grep("^--file=", args_cmd)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  setwd(dirname(dirname(script_dir)))
}

suppressPackageStartupMessages({
  library(sf)
  library(spdep)
  library(readr)
  library(dplyr)
})

source(file.path("scripts/03_analysis/shared_bym2_fay_herriot_fit.R"))

est_csv <- "output/tables/honduras_region_age_logit_prevalence.csv"
vcov_csv <- "output/tables/honduras_region_age_vcov_logit_matrix.csv"
shp_path <- "data/shp/Shp_mgd.shp"
stan_file <- "scripts/03_analysis/shared_bym2_fay_herriot.stan"
out_dir <- "output/spatial_honduras"

stopifnot(file.exists(est_csv), file.exists(vcov_csv), file.exists(shp_path), file.exists(stan_file))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

chains <- as.integer(Sys.getenv("LHS_BYM2_CHAINS", unset = "4"))
iter_warmup <- as.integer(Sys.getenv("LHS_BYM2_WARMUP", unset = "1000"))
iter_sampling <- as.integer(Sys.getenv("LHS_BYM2_SAMPLE", unset = "1000"))

# 18-department layer (same as honduras_spatial_smooth_stan.R)
honduras18 <- st_read(shp_path, quiet = TRUE) %>%
  filter(CNTRYNAMEE == "Honduras") %>%
  mutate(
    DHSREGEN = ifelse(DHSREGEN == "Resto Francisco Morazan", "FRANCISCO MORAZAN", DHSREGEN),
    DHSREGEN = ifelse(DHSREGEN == "Resto Cortes", "CORTES", DHSREGEN),
    REGION_NAME = toupper(DHSREGEN)
  ) %>%
  mutate(
    dept_fixed = case_when(
      REGION_NAME == "SAN PEDRO SULA" ~ "CORTES",
      REGION_NAME == "DISTRITO CENTRAL" ~ "FRANCISCO MORAZAN",
      TRUE ~ REGION_NAME
    )
  ) %>%
  group_by(dept_fixed) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  transmute(REGION_NAME = dept_fixed)

honduras18 <- st_make_valid(honduras18)
nb <- poly2nb(honduras18, queen = TRUE)
for (i in which(card(nb) <= 1L)) {
  container <- setdiff(which(st_contains(honduras18, honduras18[i, ], sparse = FALSE)[, 1]), i)
  if (length(container) >= 1L) {
    j <- container[1L]
    nb[[i]] <- as.integer(unique(c(nb[[i]], j)))
    nb[[j]] <- as.integer(unique(c(nb[[j]], i)))
  }
}

sc <- scale_icar_from_nb(nb)
ed <- nb_to_stan_edges(nb)
message("scale_icar = ", round(sc$scale_icar, 6), "  N_edges = ", ed$N_edges)

Vmat <- as.matrix(read.csv(vcov_csv, row.names = 1, check.names = FALSE))
prev <- read_csv(est_csv, show_col_types = FALSE)

surveys <- c("2011-12", "2019")
slug <- c("2011_12", "2019")

build_wave <- function(survey_label) {
  sub <- prev %>% filter(survey == survey_label)
  ad <- sub %>% filter(agegroup == "Adolescent") %>%
    select(region, y1 = logit_prevalence)
  ya <- sub %>% filter(agegroup == "Young_Adult") %>%
    select(region, y2 = logit_prevalence)
  wide <- full_join(ad, ya, by = "region")
  if (nrow(wide) != 18L || any(is.na(wide$y1)) || any(is.na(wide$y2))) {
    stop("Need 18 regions with both age groups for survey ", survey_label)
  }
  wide <- wide[match(honduras18$REGION_NAME, wide$region), , drop = FALSE]
  if (any(is.na(wide$region))) stop("Region names do not match shapefile for ", survey_label)
  N <- nrow(wide)
  y <- cbind(wide$y1, wide$y2)
  V_list <- vector("list", N)
  for (i in seq_len(N)) {
    r <- wide$region[i]
    ii <- c(
      match(paste0(r, " | Adolescent | ", survey_label), rownames(Vmat)),
      match(paste0(r, " | Young_Adult | ", survey_label), rownames(Vmat))
    )
    if (any(is.na(ii))) stop("VCov rows missing: ", r, " ", survey_label)
    V_list[[i]] <- Vmat[ii, ii, drop = FALSE]
  }
  list(
    y = y,
    V_list = V_list,
    region_names = wide$region,
    N = N
  )
}

all_tables <- list()

for (w in seq_along(surveys)) {
  survey_label <- surveys[w]
  tag <- slug[w]
  message("Fitting survey ", survey_label, " ...")
  wave <- build_wave(survey_label)
  N <- wave$N
  X1 <- matrix(1, N, 1, dimnames = list(NULL, "(Intercept)"))
  X2 <- matrix(1, N, 1, dimnames = list(NULL, "(Intercept)"))

  stan_data <- build_stan_data_shared_bym2(
    y = wave$y,
    V = wave$V_list,
    X1 = X1,
    X2 = X2,
    node1 = ed$node1,
    node2 = ed$node2,
    N_edges = ed$N_edges,
    scale_icar = sc$scale_icar
  )

  mod <- cmdstanr::cmdstan_model(stan_file, force_recompile = (w == 1L))
  fit <- mod$sample(
    data = stan_data,
    chains = chains,
    parallel_chains = chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    refresh = 100,
    seed = 1000L + w,
    max_treedepth = 12,
    adapt_delta = 0.95,
    output_dir = out_dir
  )

  fit$save_output_files(dir = out_dir, basename = paste0("shared_bym2_fh_", tag))
  tab <- region_posterior_table(fit, N = N, region_names = wave$region_names)
  tab$survey <- survey_label
  tab <- tab %>% select(survey, region, everything())
  out_csv <- file.path(out_dir, paste0("shared_bym2_fh_", tag, "_region_summary.csv"))
  write_csv(tab, out_csv)
  message("Wrote ", out_csv)
  all_tables[[tag]] <- tab
}

combined <- bind_rows(all_tables)
write_csv(combined, file.path(out_dir, "shared_bym2_fh_both_waves_region_summary.csv"))
message("Wrote ", file.path(out_dir, "shared_bym2_fh_both_waves_region_summary.csv"))
message("Done.")
