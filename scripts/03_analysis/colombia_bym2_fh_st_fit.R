#!/usr/bin/env Rscript
# Colombia BYM2 Fay–Herriot + 3 waves; mainland ICAR; San Andres y Providencia = island (no ICAR).
# Reuses scripts/03_analysis/hnir_bym2_fh_st.stan
#
#   Rscript scripts/03_analysis/colombia_region_adolescent_logit_prevalence.R  # first
#   Rscript scripts/03_analysis/colombia_bym2_fh_st_fit.R
#
# Inputs: output/tables/colombia_region_adolescent_logit_prevalence.csv
#         Polygons: **only** `data/col_shps/*.shp` (your updated boundaries; no other source).
# Output: output/spatial_colombia_st/

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
  "Antioquia", "Atlantico", "Bogota", "Bolivar", "Boyaca", "Caldas", "Caqueta", "Cauca",
  "Cesar", "Cordoba", "Cundinamarca", "Choco", "Huila", "La Guajira", "Magdalena", "Meta",
  "Narimo", "Norte de Santander", "Quindio", "Risaralda", "Santander", "Sucre", "Tolima",
  "Valle", "Arauca", "Casanare", "Putumayo", "San Andres y Providencia", "Amazonas",
  "Guainia", "Guaviare", "Vaupes", "Vichada"
)
surveys <- c("2005", "2010", "2015")
R_reg <- length(regions_all)
T_waves <- length(surveys)
ISLAND <- "San Andres y Providencia"

est_csv <- "output/tables/colombia_region_adolescent_logit_prevalence.csv"
col_shp_dir <- "data/col_shps"
stan_file <- "scripts/03_analysis/hnir_bym2_fh_st.stan"
out_dir <- "output/spatial_colombia_st"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(col_shp_dir, showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists(est_csv), file.exists(stan_file))
shp_files <- list.files(col_shp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
if (!length(shp_files)) stop("No .shp files in ", col_shp_dir, "/ — add Colombia boundaries there only.")

norm <- function(x) {
  x <- toupper(as.character(x))
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("\\s+", " ", trimws(x))
  chartr("ÁÉÍÓÚÑ", "AEIOUN", x)
}
# Label aliases -> DHS IR department names in regions_all
shp_to_region <- c(
  "BOGOTA DC" = "Bogota", "BOGOTA D C" = "Bogota", "DISTRITO CAPITAL DE BOGOTA" = "Bogota",
  "BOGOTA" = "Bogota", "NARINO" = "Narimo",
  "SAN ANDRES" = "San Andres y Providencia",
  "ARCHIPIELAGO DE SAN ANDRES" = "San Andres y Providencia",
  "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA" = "San Andres y Providencia",
  "QUINDIO" = "Quindio", "CHOCO" = "Choco", "VALLE DEL CAUCA" = "Valle"
)

pick_name_col <- function(raw) {
  for (c in c(
    "NAME_1", "ADM1_NAME", "REGNAME", "DHSREGEN", "OTHREGNA", "LEVELNA",
    "DEPARTMENT", "DPTO_CNMSR", "NAMELSAD", "REGNAME"
  )) {
    if (c %in% names(raw)) return(c)
  }
  for (nm in names(raw)) {
    if (nm == "geometry") next
    if (is.character(raw[[nm]]) || is.factor(raw[[nm]])) return(nm)
  }
  stop("No text label column in shapefile (need department names).")
}

map_to_regions <- function(raw, name_col) {
  raw %>%
    mutate(
      .nm = .data[[name_col]],
      REGION_NAME = vapply(.nm, function(z) {
        k <- norm(z)
        if (k %in% names(shp_to_region)) return(shp_to_region[[k]])
        for (r in regions_all) if (norm(r) == k) return(r)
        for (r in regions_all) if (grepl(norm(r), k, fixed = TRUE) || grepl(k, norm(r), fixed = TRUE)) return(r)
        as.character(z)
      }, character(1))
    ) %>%
    group_by(REGION_NAME) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")
}

load_department_sf <- function() {
  best <- NULL
  best_n <- -1L
  for (f in shp_files) {
    raw <- tryCatch(st_read(f, quiet = TRUE), error = function(e) NULL)
    if (is.null(raw)) next
    raw <- st_make_valid(raw)
    nc <- pick_name_col(raw)
    col_shp <- tryCatch(map_to_regions(raw, nc), error = function(e) NULL)
    if (is.null(col_shp)) next
    n_ok <- sum(regions_all %in% col_shp$REGION_NAME)
    if (n_ok > best_n) {
      best_n <- n_ok
      best <- col_shp
      message("Using ", f, " (name column ", nc, ") — matched ", n_ok, " / ", length(regions_all), " departments")
    }
  }
  if (is.null(best) || best_n < 20L) {
    stop(
      "No shapefile in ", col_shp_dir, " matched ≥20 departments. ",
      "Ensure one layer has department names (NAME_1, REGNAME, DHSREGEN, …) aligned with DHS."
    )
  }
  miss_shp <- setdiff(regions_all, best$REGION_NAME)
  if (length(miss_shp)) {
    stop("After name match, still missing: ", paste(miss_shp, collapse = ", "))
  }
  st_make_valid(best[match(regions_all, best$REGION_NAME), , drop = FALSE])
}

sf_regions <- load_department_sf()

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
  list(edges = edges, scaling = scaling_from_nb(nb))
}

regions_mainland <- regions_all[regions_all != ISLAND]
R_main <- length(regions_mainland)
sf_mainland <- sf_regions[match(regions_mainland, sf_regions$REGION_NAME), , drop = FALSE]
geo <- nb_edges_scaling(sf_mainland)
if (nrow(geo$edges) < 1L) stop("No mainland adjacency edges")

is_mainland <- as.integer(regions_all != ISLAND)
mainland_id <- integer(R_reg)
j <- 0L
for (r in seq_len(R_reg)) {
  if (is_mainland[r]) {
    j <- j + 1L
    mainland_id[r] <- j
  }
}

tbl <- read_csv(est_csv, show_col_types = FALSE, col_types = cols(survey = col_character())) %>%
  mutate(region = as.character(region), survey = as.character(survey))
grid <- expand.grid(region = regions_all, survey = as.character(surveys), stringsAsFactors = FALSE) %>%
  mutate(region = as.character(region), survey = as.character(survey)) %>%
  left_join(
    tbl %>% mutate(region = as.character(region), survey = as.character(survey)) %>%
      select(region, survey, has_obs, logit_prevalence, se_logit),
    by = c("region", "survey")
  ) %>%
  mutate(
    has_obs = as.integer(ifelse(is.na(has_obs), 0L, has_obs)),
    y = ifelse(has_obs == 1L & is.finite(logit_prevalence), logit_prevalence, 0),
    v = ifelse(has_obs == 1L & is.finite(se_logit) & se_logit > 0, se_logit^2, 1.0)
  )

stan_data <- list(
  R = R_reg,
  R_main = R_main,
  T = T_waves,
  N = nrow(grid),
  y = grid$y,
  v = pmax(grid$v, 1e-10),
  has_obs = grid$has_obs,
  area_id = as.integer(match(grid$region, regions_all)),
  time_id = as.integer(match(grid$survey, surveys)),
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
readr::write_csv(
  fit$summary(variables = c("alpha", "beta", "sigma_b", "phi", "p_total")),
  file.path(out_dir, "colombia_bym2_fh_st_summary_main.csv")
)
draws_p <- fit$draws(variables = "p", format = "draws_matrix")
draws_pt <- fit$draws(variables = "p_total", format = "draws_matrix")
p_lo <- apply(draws_p, 2L, quantile, 0.025)
p_hi <- apply(draws_p, 2L, quantile, 0.975)
out_pred <- grid %>%
  mutate(
    p_mean = as.numeric(colMeans(draws_p)),
    p_q025 = as.numeric(p_lo),
    p_q975 = as.numeric(p_hi),
    cell_id = paste(region, " | ", survey, sep = ""),
    survey = as.character(survey),
    region = as.character(region)
  ) %>%
  select(cell_id, region, survey, has_obs, p_mean, p_q025, p_q975)
write_csv(out_pred, file.path(out_dir, "colombia_bym2_fh_st_p_mean_by_cell.csv"))
pt_mean <- colMeans(draws_pt)
pt_lo <- apply(draws_pt, 2L, quantile, 0.025)
pt_hi <- apply(draws_pt, 2L, quantile, 0.975)
readr::write_csv(
  tibble(
    survey = surveys,
    p_total_mean = as.numeric(pt_mean),
    p_total_q025 = as.numeric(pt_lo),
    p_total_q975 = as.numeric(pt_hi)
  ),
  file.path(out_dir, "colombia_bym2_fh_st_p_total_by_wave.csv")
)
fit$save_object(file.path(out_dir, "colombia_bym2_fh_st_fit.rds"))
message("Wrote ", out_dir, " (run maps separately: Rscript scripts/03_analysis/colombia_bym2_fh_st_plot_maps.R)")
