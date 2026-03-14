#!/usr/bin/env Rscript
# Prepare region × wave logit direct estimates and fit BYM2 Fay–Herriot + wave effects (no ST interaction).
#
#   Rscript scripts/03_analysis/hnir_bym2_fh_st_fit.R
#
# Inputs:
#   output/tables/honduras_region_adolescent_logit_prevalence.csv
#   data/shp/Shp_mgd.shp (18 DHS departments, same as honduras_spatial_smooth_stan.R)
# Outputs (default output/spatial_honduras_st/):
#   fit CSV summaries + RDS + plots/map_fitted_*.png (3 waves + faceted)

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

regions_all <- c(
  "ATLANTIDA", "COLON", "COMAYAGUA", "COPAN", "CORTES", "CHOLUTECA", "EL PARAISO",
  "FRANCISCO MORAZAN", "GRACIAS A DIOS", "INTIBUCA", "ISLAS DE LA BAHIA", "LA PAZ",
  "LEMPIRA", "OCOTEPEQUE", "OLANCHO", "SANTA BARBARA", "VALLE", "YORO"
)
surveys <- c("2005-06", "2011-12", "2019")
T_waves <- length(surveys)
R_reg <- length(regions_all)

est_csv <- "output/tables/honduras_region_adolescent_logit_prevalence.csv"
shp_path <- "data/shp/Shp_mgd.shp"
stan_file <- "scripts/03_analysis/hnir_bym2_fh_st.stan"
out_dir <- "output/spatial_honduras_st"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(est_csv), file.exists(shp_path), file.exists(stan_file))

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
  Rloc <- nrow(sf_obj)
  edges <- do.call(rbind, lapply(seq_len(Rloc), function(i) {
    if (!length(nb[[i]])) return(NULL)
    cbind(node1 = i, node2 = nb[[i]])
  })) |> as.data.frame()
  edges <- edges[edges$node1 < edges$node2, , drop = FALSE]
  rownames(edges) <- NULL
  list(edges = edges, scaling = scaling_from_nb(nb), nb = nb)
}

# 18-polygon layer aligned with DHS V024 names
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

if (!all(regions_all %in% honduras18$REGION_NAME)) {
  stop("Shapefile missing departments: ", paste(setdiff(regions_all, honduras18$REGION_NAME), collapse = ", "))
}
sf_regions <- honduras18[match(regions_all, honduras18$REGION_NAME), , drop = FALSE]
# Mainland-only graph: isolated island (Islas de la Bahía) excluded from ICAR / edges / scaling
regions_mainland <- regions_all[regions_all != "ISLAS DE LA BAHIA"]
R_main <- length(regions_mainland)
sf_mainland <- honduras18[match(regions_mainland, honduras18$REGION_NAME), , drop = FALSE]
geo <- nb_edges_scaling(sf_mainland)
if (nrow(geo$edges) < 1L) stop("No adjacency edges on mainland")

is_mainland <- as.integer(regions_all != "ISLAS DE LA BAHIA")
mainland_id <- integer(R_reg)
j <- 0L
for (r in seq_len(R_reg)) {
  if (is_mainland[r]) {
    j <- j + 1L
    mainland_id[r] <- j
  }
}

tbl <- read_csv(est_csv, show_col_types = FALSE) %>%
  mutate(region = toupper(as.character(region)), survey = as.character(survey))

grid <- expand.grid(region = regions_all, survey = surveys, stringsAsFactors = FALSE) %>%
  left_join(
    tbl %>% select(region, survey, has_obs, logit_prevalence, se_logit),
    by = c("region", "survey")
  ) %>%
  mutate(
    has_obs = as.integer(ifelse(is.na(has_obs), 0L, has_obs)),
    y = ifelse(has_obs == 1L & is.finite(logit_prevalence), logit_prevalence, 0),
    v = ifelse(has_obs == 1L & is.finite(se_logit) & se_logit > 0, se_logit^2, 1.0)
  )

N <- nrow(grid)
area_id <- match(grid$region, regions_all)
time_id <- match(grid$survey, surveys)

stan_data <- list(
  R = R_reg,
  R_main = R_main,
  T = T_waves,
  N = N,
  y = grid$y,
  v = pmax(grid$v, 1e-10),
  has_obs = grid$has_obs,
  area_id = as.integer(area_id),
  time_id = as.integer(time_id),
  is_mainland = as.integer(is_mainland),
  mainland_id = as.integer(mainland_id),
  E = nrow(geo$edges),
  node1 = as.integer(geo$edges$node1),
  node2 = as.integer(geo$edges$node2),
  scaling_factor = geo$scaling
)

mod <- cmdstan_model(stan_file, force_recompile = FALSE)
fit <- mod$sample(
  data = stan_data,
  seed = 12345,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  adapt_delta = 0.95,
  max_treedepth = 15
)

summ <- fit$summary(variables = c("alpha", "beta", "sigma_b", "phi", "p_total"))
readr::write_csv(summ, file.path(out_dir, "hnir_bym2_fh_st_summary_main.csv"))

draws_p <- fit$draws(variables = "p", format = "draws_matrix")
draws_pt <- fit$draws(variables = "p_total", format = "draws_matrix")
p_mean <- colMeans(draws_p)
p_lo <- apply(draws_p, 2L, quantile, 0.025)
p_hi <- apply(draws_p, 2L, quantile, 0.975)
out_pred <- grid %>%
  mutate(
    p_mean = as.numeric(p_mean),
    p_q025 = as.numeric(p_lo),
    p_q975 = as.numeric(p_hi),
    cell_id = paste(region, " | ", survey, sep = ""),
    survey = as.character(survey),
    region = as.character(region)
  ) %>%
  select(cell_id, region, survey, has_obs, p_mean, p_q025, p_q975)
write_csv(out_pred, file.path(out_dir, "hnir_bym2_fh_st_p_mean_by_cell.csv"))
pt_mean <- colMeans(draws_pt)
pt_lo <- apply(draws_pt, 2L, quantile, 0.025)
pt_hi <- apply(draws_pt, 2L, quantile, 0.975)
readr::write_csv(
  tibble::tibble(
    survey = surveys,
    p_total_mean = as.numeric(pt_mean),
    p_total_q025 = as.numeric(pt_lo),
    p_total_q975 = as.numeric(pt_hi)
  ),
  file.path(out_dir, "hnir_bym2_fh_st_p_total_by_wave.csv")
)

fit$save_object(file.path(out_dir, "hnir_bym2_fh_st_fit.rds"))

message("Wrote ", out_dir, " (maps: Rscript scripts/03_analysis/honduras_bym2_fh_st_plot_maps.R)")
