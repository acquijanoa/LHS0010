#!/usr/bin/env Rscript
# Honduras: BYM2 + regional SES covariates (design-based proportions) on logit scale.
# Falls back to no-cov model if SES file missing or LHS_NO_COV=1.
#
# Requires: shp, honduras_region_age_logit_prevalence.csv (Adolescent rows), regional_socioeconomic_adol.csv
#   Rscript scripts/03_analysis/honduras_spatial_smooth_stan.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
  library(spdep)
  library(cmdstanr)
})

args_cmd <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_cmd[grep("^--file=", args_cmd)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  setwd(dirname(dirname(script_dir)))
}

shp_path <- "data/shp/Shp_mgd.shp"
est_csv <- "output/tables/honduras_region_age_logit_prevalence.csv"
ses_csv <- "data/derived/regional_socioeconomic_adol.csv"
stan_cov <- "scripts/03_analysis/icar_logit_prevalence_cov.stan"
stan_base <- "scripts/03_analysis/icar_logit_prevalence.stan"
out_dir <- "output/spatial_honduras"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(shp_path), file.exists(est_csv))

use_cov <- !Sys.getenv("LHS_NO_COV", "") %in% c("1", "TRUE", "true") &&
  file.exists(ses_csv) && file.exists(stan_cov)
if (!use_cov) {
  message("Covariate model off (LHS_NO_COV=1 or missing ", ses_csv, ") — using base Stan model.")
  stopifnot(file.exists(stan_base))
} else {
  message("Using regional SES covariates + ", basename(stan_cov))
}

# Single 18-polygon layer: SPS ∪ CORTES → CORTES; DC ∪ FRANCISCO MORAZAN → FRANCISCO MORAZAN
# Used for BOTH 2011-12 and 2019 so Bayes adjacency and year-diff are comparable.
honduras_shp_raw <- st_read(shp_path, quiet = TRUE) %>%
  filter(CNTRYNAMEE == "Honduras") %>%
  mutate(
    DHSREGEN = ifelse(DHSREGEN == "Resto Francisco Morazan", "FRANCISCO MORAZAN", DHSREGEN),
    DHSREGEN = ifelse(DHSREGEN == "Resto Cortes", "CORTES", DHSREGEN),
    REGION_NAME = toupper(DHSREGEN)
  )

honduras18 <- honduras_shp_raw %>%
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

scaling_from_nb <- function(nb) {
  W <- nb2mat(nb, style = "B", zero.policy = TRUE)
  Q <- diag(rowSums(W)) - W
  n <- nrow(Q)
  tryCatch(exp(mean(log(diag(solve(Q + diag(1e-6, n)))))), error = function(e) 1.0)
}

nb_edges_scaling <- function(sf_obj) {
  sf_obj <- st_make_valid(sf_obj)
  nb <- poly2nb(sf_obj, queen = TRUE)
  for (i in which(card(nb) <= 1L)) {
    container <- setdiff(which(st_contains(sf_obj, sf_obj[i, ], sparse = FALSE)[, 1]), i)
    if (length(container) >= 1L) {
      j <- container[1L]
      nb[[i]] <- as.integer(unique(c(nb[[i]], j)))
      nb[[j]] <- as.integer(unique(c(nb[[j]], i)))
    }
  }
  R <- nrow(sf_obj)
  edges <- do.call(rbind, lapply(seq_len(R), function(i) {
    if (!length(nb[[i]])) return(NULL)
    cbind(node1 = i, node2 = nb[[i]])
  })) |> as.data.frame()
  edges <- edges[edges$node1 < edges$node2, , drop = FALSE]
  rownames(edges) <- NULL
  list(edges = edges, scaling = scaling_from_nb(nb))
}

#' Direct design estimates on logit scale (from honduras_region_age_prevalence_logit.R, Adolescent only)
prep_direct <- function(df_est) {
  y <- df_est$logit_prevalence
  se <- df_est$se_logit
  has_se <- as.integer(is.finite(se) & se > 0 & se < 8)
  v <- ifelse(has_se == 1L, se^2, 0)
  p <- plogis(y)
  tibble(
    REGION_NAME = toupper(df_est$region),
    y = y,
    has_se = has_se,
    v = pmax(v, 1e-10),
    p_direct = p
  )
}

# Same order as regional_socioeconomic_VARIABLES.md / derive_regional_socioeconomic.R
COV_COLS <- c(
  "p_poorest_wealth_quintile",
  "p_richest_wealth_quintile",
  "p_no_education",
  "p_secondary_or_higher",
  "p_married_union_adolescent",
  "p_parity_two_or_more"
)

build_X <- function(direct_tbl, survey_year, ses_long) {
  ses <- ses_long %>%
    filter(country == "Honduras", survey_year == !!survey_year) %>%
    mutate(REGION_NAME = toupper(region))
  m <- direct_tbl %>%
    left_join(ses %>% select(REGION_NAME, all_of(COV_COLS)), by = "REGION_NAME")
  for (cc in COV_COLS) {
    mu <- mean(m[[cc]], na.rm = TRUE)
    if (!is.finite(mu)) mu <- 0.5
    m[[cc]][is.na(m[[cc]])] <- mu
  }
  Xraw <- as.matrix(m[, COV_COLS])
  X <- scale(Xraw)
  attr(X, "cov_names") <- COV_COLS
  X
}

run_one_survey <- function(survey_year, sf_regions, direct_tbl, mod, trust_direct, icar_strength, use_cov) {
  common <- sort(intersect(unique(sf_regions$REGION_NAME), unique(direct_tbl$REGION_NAME)))
  if (length(common) < 3L) stop("Need ≥3 regions: ", survey_year)

  sf_regions <- sf_regions %>% filter(REGION_NAME %in% common)
  direct_tbl <- direct_tbl %>% filter(REGION_NAME %in% common)
  common <- common[order(common)]
  sf_regions <- sf_regions[match(common, sf_regions$REGION_NAME), , drop = FALSE]
  direct_tbl <- direct_tbl[match(common, direct_tbl$REGION_NAME), , drop = FALSE]

  geo <- nb_edges_scaling(sf_regions)
  if (nrow(geo$edges) < 1L) stop("No edges: ", survey_year)

  stan_data <- list(
    R = nrow(sf_regions),
    y = direct_tbl$y,
    has_se = direct_tbl$has_se,
    v = direct_tbl$v,
    E = nrow(geo$edges),
    node1 = as.integer(geo$edges$node1),
    node2 = as.integer(geo$edges$node2),
    scaling_factor = geo$scaling,
    trust_direct = trust_direct,
    icar_strength = icar_strength
  )
  X_save <- NULL
  if (use_cov) {
    X_save <- build_X(direct_tbl, survey_year, read_csv(ses_csv, show_col_types = FALSE))
    stan_data$K <- ncol(X_save)
    stan_data$X <- unname(as.matrix(X_save))
  }

  fit <- mod$sample(
    data = stan_data,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 2000,
    refresh = 400,
    max_treedepth = 12,
    show_messages = FALSE
  )

  if (use_cov) {
    print(fit$summary(c("alpha", "beta", "rho", "sigma", "sigma_miss")))
  } else {
    print(fit$summary(c("alpha", "rho", "sigma", "sigma_miss")))
  }

  draws <- fit$draws(variables = c("p_smoothed"), format = "draws_matrix")
  R <- stan_data$R
  p_mean <- numeric(R)
  p_lo <- numeric(R)
  p_hi <- numeric(R)
  for (j in seq_len(R)) {
    x <- draws[, paste0("p_smoothed[", j, "]")]
    p_mean[j] <- mean(x)
    p_lo[j] <- quantile(x, 0.025)
    p_hi[j] <- quantile(x, 0.975)
  }

  out_tbl <- tibble(
    survey_year = survey_year,
    REGION_NAME = common,
    p_direct = direct_tbl$p_direct,
    has_se = direct_tbl$has_se,
    p_smoothed_mean = p_mean,
    p_smoothed_q025 = p_lo,
    p_smoothed_q975 = p_hi
  )

  slug <- gsub("[^0-9a-zA-Z]+", "_", survey_year)
  write_csv(out_tbl, file.path(out_dir, paste0("smoothed_prevalence_", slug, ".csv")))
  saveRDS(fit, file.path(out_dir, paste0("fit_", slug, ".rds")))

  map_sf <- left_join(sf_regions, out_tbl, by = "REGION_NAME")
  meta <- list(
    survey_year = survey_year,
    slug = slug,
    use_covariates = use_cov,
    covariates = if (use_cov) COV_COLS else NULL,
    trust_direct = trust_direct,
    icar_strength = icar_strength,
    written = Sys.time()
  )
  saveRDS(meta, file.path(out_dir, paste0("meta_", slug, ".rds")))
  saveRDS(map_sf, file.path(out_dir, paste0("spatial_layer_", slug, ".rds")))
  if (use_cov && !is.null(X_save)) {
    saveRDS(
      cbind(tibble(REGION_NAME = common), as.data.frame(X_save)),
      file.path(out_dir, paste0("X_standardized_", slug, ".rds"))
    )
  }

  message("Saved spatial_layer_", slug, ".rds (cov=", use_cov, ")")
  invisible(fit)
}

est_hn <- read_csv(est_csv, show_col_types = FALSE) %>%
  filter(agegroup == "Adolescent") %>%
  rename(survey_year = survey)
trust_direct <- min(2.5, max(0.7, as.numeric(Sys.getenv("LHS_TRUST_DIRECT", "1.25"))))
icar_strength <- min(1, max(0.15, as.numeric(Sys.getenv("LHS_ICAR_STRENGTH", "0.4"))))
message("trust_direct=", trust_direct, " icar_strength=", icar_strength)
message("Compiling Stan …")
mod <- cmdstan_model(if (use_cov) stan_cov else stan_base, force_recompile = FALSE)

for (s in list(
  list(year = "2011-12", sf = honduras18, est = filter(est_hn, survey_year == "2011-12")),
  list(year = "2019", sf = honduras18, est = filter(est_hn, survey_year == "2019"))
)) {
  if (nrow(s$est) < 5L) next
  message("Fitting ", s$year, " …")
  run_one_survey(s$year, s$sf, prep_direct(s$est), mod, trust_direct, icar_strength, use_cov)
}

message("Plots: Rscript scripts/03_analysis/honduras_spatial_plot_maps.R")
