#!/usr/bin/env Rscript
# Maps + direct vs fitted — Colombia BYM2 FH (3 waves). San Andrés shown; mainland context.
#
#   Rscript scripts/03_analysis/colombia_bym2_fh_st_plot_maps.R
#
# Reads: output/spatial_colombia_st/colombia_bym2_fh_st_p_mean_by_cell.csv
#        output/tables/colombia_region_adolescent_logit_prevalence.csv
#        data/col_shps/*.shp only (same logic as colombia_bym2_fh_st_fit.R)
#        data/shp/World_Countries_Generalized.shp

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
  library(ggplot2)
  library(scales)
  library(grid)
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
csv_path <- "output/spatial_colombia_st/colombia_bym2_fh_st_p_mean_by_cell.csv"
est_csv <- "output/tables/colombia_region_adolescent_logit_prevalence.csv"
world_shp <- "data/shp/World_Countries_Generalized.shp"
plot_dir <- "output/spatial_colombia_st/plots"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.exists(csv_path), file.exists(est_csv), file.exists(world_shp))

out_pred <- read_csv(csv_path, show_col_types = FALSE, col_types = readr::cols(survey = readr::col_character())) %>%
  mutate(region = as.character(region), survey = as.character(survey))

shp_to_region <- c(
  "BOGOTA DC" = "Bogota", "NARINO" = "Narimo",
  "SAN ANDRES" = "San Andres y Providencia",
  "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA" = "San Andres y Providencia",
  "VALLE DEL CAUCA" = "Valle", "QUINDIO" = "Quindio", "CHOCO" = "Choco"
)
norm <- function(x) {
  x <- toupper(gsub("[[:punct:]]", " ", as.character(x)))
  chartr("ÁÉÍÓÚÑ", "AEIOUN", gsub("\\s+", " ", trimws(x)))
}
pick_name_col <- function(raw) {
  for (c in c("NAME_1", "ADM1_NAME", "REGNAME", "DHSREGEN", "OTHREGNA", "LEVELNA", "DEPARTMENT")) {
    if (c %in% names(raw)) return(c)
  }
  for (nm in names(raw)) {
    if (nm != "geometry" && (is.character(raw[[nm]]) || is.factor(raw[[nm]]))) return(nm)
  }
  stop("No label column in col_shps shapefile")
}
shp_files <- list.files("data/col_shps", "\\.shp$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
if (!length(shp_files)) stop("No .shp in data/col_shps/")
best <- NULL
best_n <- -1L
for (f in shp_files) {
  raw <- tryCatch(st_read(f, quiet = TRUE), error = function(e) NULL)
  if (is.null(raw)) next
  raw <- st_make_valid(raw)
  nc <- pick_name_col(raw)
  col_shp <- raw %>%
    mutate(
      REGION_NAME = vapply(.data[[nc]], function(z) {
        k <- norm(z)
        if (k %in% names(shp_to_region)) return(shp_to_region[[k]])
        for (r in regions_all) if (norm(r) == k) return(r)
        for (r in regions_all) if (grepl(norm(r), k, fixed = TRUE)) return(r)
        as.character(z)
      }, character(1))
    ) %>%
    group_by(REGION_NAME) %>% summarise(geometry = st_union(geometry), .groups = "drop")
  n_ok <- sum(regions_all %in% col_shp$REGION_NAME)
  if (n_ok > best_n) {
    best_n <- n_ok
    best <- col_shp
  }
}
if (is.null(best)) stop("Maps: no usable .shp in data/col_shps/")
miss <- setdiff(regions_all, best$REGION_NAME)
if (length(miss)) {
  stop("Maps: missing departments after name match: ", paste(miss, collapse = ", "))
}
sf_regions <- best[match(regions_all, best$REGION_NAME), , drop = FALSE] %>% st_make_valid()

fitted_sf <- sf_regions %>%
  select(REGION_NAME, geometry) %>%
  inner_join(out_pred, by = c("REGION_NAME" = "region"))

lim <- range(fitted_sf$p_mean, na.rm = TRUE)
pad <- max(0.01, diff(lim) * 0.04)
lim_fill <- c(0, min(1, max(lim[2], 0.02) + pad))
fill_pal_orange <- grDevices::colorRampPalette(c(
  "#fff5eb", "#fee6ce", "#fdd0a2", "#fdae6b", "#fd8d3c", "#e6550d", "#a63603"
))(256L)

crs_wgs <- 4326
crs_lbl <- 32618L
rect_sfc <- function(xmin, ymin, xmax, ymax, crs = crs_wgs) {
  ring <- matrix(c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin), ncol = 2, byrow = TRUE)
  st_sfc(st_polygon(list(ring)), crs = crs)
}
mainland_only <- sf_regions %>% filter(REGION_NAME != "San Andres y Providencia")
bb <- st_bbox(st_union(st_geometry(mainland_only)))
dw <- as.numeric(diff(bb[c("xmin", "xmax")]))
dh <- as.numeric(diff(bb[c("ymin", "ymax")]))
xlim_map <- c(bb$xmin - 0.05 * dw, bb$xmax + 0.06 * dw)
ylim_map <- c(bb$ymin - 0.06 * dh, bb$ymax + 0.08 * dh)
sea_backdrop <- rect_sfc(xlim_map[1], ylim_map[1], xlim_map[2], ylim_map[2])
world_raw <- st_read(world_shp, quiet = TRUE) %>% st_transform(crs_wgs)
world_crop <- tryCatch(
  st_crop(st_make_valid(world_raw), rect_sfc(xlim_map[1] - 1, ylim_map[1] - 1, xlim_map[2] + 1, ylim_map[2] + 1)),
  error = function(e) world_raw
)
if ("COUNTRY" %in% names(world_crop)) world_crop <- world_crop %>% filter(COUNTRY != "Colombia")
land_fill_world <- rgb(0.94, 0.94, 0.92, alpha = 0.8)
sea_blue <- "#6ba3b8"
caption_map <- "Source: DHS Colombia (COIR53, COIR61, COIR72), adolescent women. Posterior mean unmet need by department."

plot_one <- function(map_sf, wave) {
  lbl <- map_sf %>%
    mutate(lab = ifelse(is.finite(p_mean), format(round(p_mean, 2), nsmall = 2), "")) %>%
    filter(lab != "") %>%
    st_transform(crs_lbl) %>%
    mutate(geometry = st_point_on_surface(geometry)) %>%
    st_transform(crs_wgs)
  ggplot() +
    geom_sf(data = sea_backdrop, fill = sea_blue, color = NA) +
    geom_sf(data = world_crop, fill = land_fill_world, color = NA, linewidth = 0) +
    geom_sf(data = map_sf, aes(fill = p_mean), color = "gray20", linewidth = 0.35) +
    geom_sf_label(data = lbl, aes(label = lab, geometry = geometry), size = 1.6, fill = alpha("white", 0.9),
                  linewidth = 0.1, show.legend = FALSE) +
    coord_sf(xlim = xlim_map, ylim = ylim_map, crs = crs_wgs, expand = FALSE, datum = crs_wgs) +
    scale_fill_gradientn(colours = fill_pal_orange, name = "Posterior mean\nprevalence",
                        limits = lim_fill, oob = scales::squish, labels = percent_format(accuracy = 1)) +
    theme_bw() +
    theme(legend.position = "right") +
    labs(title = "Unmet need (adolescents) — Colombia, by department",
         subtitle = paste("DHS", wave), caption = caption_map)
}
slug <- function(s) gsub("[^0-9a-zA-Z]+", "_", s)
for (sv in surveys) {
  ggsave(file.path(plot_dir, paste0("col_map_fitted_", slug(sv), ".png")),
         plot_one(filter(fitted_sf, survey == sv), sv), width = 9, height = 10, dpi = 150)
}
lbl_fac <- fitted_sf %>%
  mutate(lab = ifelse(is.finite(p_mean), format(round(p_mean, 2), nsmall = 2), "")) %>%
  filter(lab != "") %>% st_transform(crs_lbl) %>% mutate(geometry = st_point_on_surface(geometry)) %>% st_transform(crs_wgs)
g_fac <- ggplot() +
  geom_sf(data = sea_backdrop, fill = sea_blue, color = NA) +
  geom_sf(data = world_crop, fill = land_fill_world, color = NA, linewidth = 0) +
  geom_sf(data = fitted_sf, aes(fill = p_mean), color = "gray15", linewidth = 0.15) +
  geom_sf_label(data = lbl_fac, aes(label = lab, geometry = geometry), size = 1.35, fill = alpha("white", 0.88),
                linewidth = 0.08, show.legend = FALSE) +
  facet_wrap(~survey, nrow = 1) +
  coord_sf(xlim = xlim_map, ylim = ylim_map, crs = crs_wgs, expand = FALSE, datum = crs_wgs) +
  scale_fill_gradientn(colours = fill_pal_orange, name = "Posterior mean prevalence",
                        limits = lim_fill, oob = scales::squish, labels = percent_format(accuracy = 1)) +
  theme_bw() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold")) +
  labs(title = "Colombia — posterior mean prevalence (3 DHS rounds)", caption = caption_map)
ggsave(file.path(plot_dir, "col_map_fitted_faceted_3_waves.png"), g_fac, width = 16, height = 8, dpi = 150)

# Direct vs fitted
direct_tbl <- read_csv(est_csv, show_col_types = FALSE, col_types = readr::cols(survey = readr::col_character())) %>%
  mutate(
    region = as.character(region),
    survey = as.character(survey),
    has_direct = as.integer(ifelse(is.na(has_obs), 0L, has_obs)),
    p_direct = prevalence
  )
cmp <- out_pred %>%
  mutate(region = as.character(region), survey = as.character(survey)) %>%
  left_join(direct_tbl %>% select(region, survey, p_direct, has_direct), by = c("region", "survey"))
obs <- cmp %>% filter(has_direct == 1L, is.finite(p_direct), is.finite(p_mean))
xr <- range(c(obs$p_direct, obs$p_mean), na.rm = TRUE)
xr <- c(max(0, xr[1] - 0.05), min(1, xr[2] + 0.05))
g_sc <- ggplot(obs, aes(p_direct, p_mean)) + geom_abline(slope = 1, intercept = 0, linetype = 2, color = "gray50") +
  geom_point(aes(color = survey), size = 2.5) + facet_wrap(~survey, nrow = 1) +
  coord_fixed(xlim = xr, ylim = xr) + scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + theme_bw() +
  labs(title = "Direct vs posterior mean — Colombia", x = "Direct", y = "Posterior mean")
ggsave(file.path(plot_dir, "col_direct_vs_fitted_scatter.png"), g_sc, width = 12, height = 4.5, dpi = 150)
message("Plots in ", normalizePath(plot_dir))
