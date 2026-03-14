#!/usr/bin/env Rscript
# Honduras spatio-temporal shared BYM2 Fay–Herriot: T=2 waves jointly (AR(1) on b).
#
#   Rscript scripts/03_analysis/honduras_shared_bym2_fay_herriot_st.R
#
# Env: LHS_BYM2_CHAINS LHS_BYM2_WARMUP LHS_BYM2_SAMPLE

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
  library(tidyr)
})

source(file.path("scripts/03_analysis/shared_bym2_fay_herriot_fit.R"))
source(file.path("scripts/03_analysis/shared_bym2_fay_herriot_st_fit.R"))

est_csv <- "output/tables/honduras_region_age_logit_prevalence.csv"
vcov_csv <- "output/tables/honduras_region_age_vcov_logit_matrix.csv"
shp_path <- "data/shp/Shp_mgd.shp"
stan_file <- "scripts/03_analysis/shared_bym2_fay_herriot_st.stan"
out_dir <- "output/spatial_honduras"

stopifnot(file.exists(est_csv), file.exists(vcov_csv), file.exists(shp_path), file.exists(stan_file))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

chains <- as.integer(Sys.getenv("LHS_BYM2_CHAINS", unset = "4"))
iter_warmup <- as.integer(Sys.getenv("LHS_BYM2_WARMUP", unset = "1500"))
iter_sampling <- as.integer(Sys.getenv("LHS_BYM2_SAMPLE", unset = "1500"))

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
N <- nrow(honduras18)
region_names <- honduras18$REGION_NAME

time_labels <- c("2011-12", "2019")
T <- length(time_labels)
Vmat <- as.matrix(read.csv(vcov_csv, row.names = 1, check.names = FALSE))
prev <- read_csv(est_csv, show_col_types = FALSE)

# y[i,t,] and V[[i]][[t]] in polygon order; t=1 first wave
y_arr <- array(NA_real_, dim = c(N, T, 2))
V_list <- vector("list", N)
for (i in seq_len(N)) V_list[[i]] <- vector("list", T)

for (ti in seq_len(T)) {
  lab <- time_labels[ti]
  sub <- prev %>% filter(survey == lab)
  ad <- sub %>% filter(agegroup == "Adolescent") %>% select(region, y1 = logit_prevalence)
  ya <- sub %>% filter(agegroup == "Young_Adult") %>% select(region, y2 = logit_prevalence)
  wide <- full_join(ad, ya, by = "region")
  wide <- wide[match(region_names, wide$region), ]
  if (any(is.na(wide$y1))) stop("Missing data wave ", lab)
  for (i in seq_len(N)) {
    y_arr[i, ti, ] <- c(wide$y1[i], wide$y2[i])
    r <- region_names[i]
    ii <- c(
      match(paste0(r, " | Adolescent | ", lab), rownames(Vmat)),
      match(paste0(r, " | Young_Adult | ", lab), rownames(Vmat))
    )
    V_list[[i]][[ti]] <- Vmat[ii, ii, drop = FALSE]
  }
}

p1 <- 1L
p2 <- 1L
stan_data <- build_stan_data_st(
  y = y_arr,
  V = V_list,
  N = N,
  T = T,
  p1 = p1,
  p2 = p2,
  node1 = ed$node1,
  node2 = ed$node2,
  N_edges = ed$N_edges,
  scale_icar = sc$scale_icar
)

message("Fitting spatio-temporal BYM2 (N=", N, ", T=", T, ") ...")
fit <- fit_shared_bym2_st(
  stan_data,
  stan_file,
  chains = chains,
  parallel_chains = chains,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  output_dir = out_dir
)
fit$save_output_files(dir = out_dir, basename = "shared_bym2_fh_st")

sum_p <- summarize_st_prevalence(fit, region_names, time_labels)
sum_th <- summarize_st_theta(fit, region_names, time_labels)
readr::write_csv(sum_p, file.path(out_dir, "shared_bym2_fh_st_prevalence_by_region_time.csv"))
readr::write_csv(sum_th, file.path(out_dir, "shared_bym2_fh_st_theta_by_region_time.csv"))

# Wide table for mapping (like single-wave summaries)
wide_out <- sum_p %>%
  pivot_wider(
    id_cols = c(region, survey),
    names_from = outcome,
    values_from = c(p_median, p_lo, p_hi),
    names_sep = "_"
  )
readr::write_csv(wide_out, file.path(out_dir, "shared_bym2_fh_st_region_summary_wide.csv"))
message("Wrote shared_bym2_fh_st_*_by_region_time.csv and region_summary_wide.csv")
message("Done.")
