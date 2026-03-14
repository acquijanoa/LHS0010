#!/usr/bin/env Rscript
# Choropleths of fitted prevalence (posterior mean p) for each survey wave — BYM2 FH ST fit.
#
#   Rscript scripts/03_analysis/honduras_bym2_fh_st_plot_maps.R
#
# Reads: output/spatial_honduras_st/hnir_bym2_fh_st_p_mean_by_cell.csv
#        output/tables/honduras_region_adolescent_logit_prevalence.csv (direct estimates)
#        data/shp/Shp_mgd.shp (Honduras departments)
#        data/shp/World_Countries_Generalized.shp (world boundaries + sea backdrop)
# Writes: map_fitted_*.png + map_fitted_faceted_3_waves.png + direct vs fitted plots

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
  "ATLANTIDA", "COLON", "COMAYAGUA", "COPAN", "CORTES", "CHOLUTECA", "EL PARAISO",
  "FRANCISCO MORAZAN", "GRACIAS A DIOS", "INTIBUCA", "ISLAS DE LA BAHIA", "LA PAZ",
  "LEMPIRA", "OCOTEPEQUE", "OLANCHO", "SANTA BARBARA", "VALLE", "YORO"
)
surveys <- c("2005-06", "2011-12", "2019")

csv_path <- "output/spatial_honduras_st/hnir_bym2_fh_st_p_mean_by_cell.csv"
est_csv <- "output/tables/honduras_region_adolescent_logit_prevalence.csv"
shp_path <- "data/shp/Shp_mgd.shp"
world_shp <- "data/shp/World_Countries_Generalized.shp"
plot_dir <- "output/spatial_honduras_st/plots"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(csv_path), file.exists(est_csv), file.exists(shp_path))
if (!file.exists(world_shp)) {
  stop("Missing world shapefile: ", world_shp)
}

out_pred <- read_csv(csv_path, show_col_types = FALSE) %>%
  mutate(region = toupper(as.character(region)), survey = as.character(survey))

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

sf_regions <- honduras18[match(regions_all, honduras18$REGION_NAME), , drop = FALSE]
sf_regions <- st_make_valid(sf_regions)

fitted_sf <- sf_regions %>%
  select(REGION_NAME, geometry) %>%
  inner_join(out_pred, by = c("REGION_NAME" = "region"))

lim <- range(fitted_sf$p_mean, na.rm = TRUE)
pad <- max(0.01, diff(lim) * 0.04)
lim <- c(max(0, lim[1] - pad), min(1, lim[2] + pad))
# Legend / color scale: 0% to data max (capped at 1)
lim_fill <- c(0, max(lim[2], 0.02))
lim_fill[2] <- min(1, lim_fill[2] + pad)

# Light orange -> dark orange (posterior mean prevalence)
fill_pal_orange <- grDevices::colorRampPalette(c(
  "#fff5eb", "#fee6ce", "#fdd0a2", "#fdae6b", "#fd8d3c", "#e6550d", "#a63603"
))(256L)

# ---- World basemap + zoom on Honduras ----
# Avoid st_as_sfc(st_bbox(...)) — can trigger !anyNA(x) errors if bbox is NA or sf build quirks.
crs_wgs <- 4326
crs_lbl <- 32616L # WGS 84 / UTM 16N — metric CRS so st_point_on_surface is valid
rect_sfc <- function(xmin, ymin, xmax, ymax, crs = crs_wgs) {
  xmin <- as.numeric(xmin)[1]
  ymin <- as.numeric(ymin)[1]
  xmax <- as.numeric(xmax)[1]
  ymax <- as.numeric(ymax)[1]
  if (!all(is.finite(c(xmin, ymin, xmax, ymax))) || xmax <= xmin || ymax <= ymin) {
    stop("rect_sfc: non-finite or invalid extent")
  }
  ring <- matrix(
    c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
    ncol = 2L,
    byrow = TRUE
  )
  st_sfc(st_polygon(list(ring)), crs = crs)
}

world_raw <- st_read(world_shp, quiet = TRUE) %>% st_transform(crs_wgs)
hn_union <- st_union(st_geometry(sf_regions))
bb_hn <- st_bbox(hn_union)
# Fallback extent (Honduras + margin) if union bbox is unusable
if (!all(is.finite(bb_hn))) {
  bb_hn <- st_bbox(c(xmin = -89.4, ymin = 12.8, xmax = -83.0, ymax = 16.6))
}
# Tight zoom; less padding northeast (Caribbean) so top/right are not mostly water
dw <- as.numeric(diff(bb_hn[c("xmin", "xmax")]))
dh <- as.numeric(diff(bb_hn[c("ymin", "ymax")]))
xlim_map <- c(
  bb_hn[["xmin"]] - 0.07 * dw,
  bb_hn[["xmax"]] + 0.025 * dw
)
# Tight north (top): minimal pad — was 0.03*dh and still ~10% water; clip near bbox ymax
ylim_map <- c(
  bb_hn[["ymin"]] - 0.08 * dh,
  bb_hn[["ymax"]] + 0.004 * dh
)
sea_backdrop <- rect_sfc(xlim_map[1], ylim_map[1], xlim_map[2], ylim_map[2])
crop_rect <- rect_sfc(
  xlim_map[1] - 0.2, ylim_map[1] - 0.2, xlim_map[2] + 0.2, ylim_map[2] + 0.2
)
world_crop <- tryCatch(
  st_crop(st_make_valid(world_raw), crop_rect),
  error = function(e) {
    message("st_crop world failed, using full world layer: ", conditionMessage(e))
    world_raw
  }
)
# Generalized world layer misaligns with Honduras; omit its Honduras polygon so only
# the DHS department map shows the country (neighbors still give context).
if ("COUNTRY" %in% names(world_crop)) {
  world_crop <- world_crop %>% filter(COUNTRY != "Honduras")
} else if ("NAME" %in% names(world_crop)) {
  world_crop <- world_crop %>% filter(!grepl("^Honduras$", NAME, ignore.case = TRUE))
}
sea_blue <- "#6ba3b8"
# World land: soft fill + opacity only (no country outline lines)
land_fill_world <- rgb(0.94, 0.94, 0.92, alpha = 0.8)

caption_map <- paste0(
  "Source: Encuesta Demográfica y de Salud (DHS), Honduras — adolescent women. ",
  "Department estimates; survey rounds 2005–06, 2011–12, and 2019. ",
  "Map: estimated share with unmet need for contraception by department."
)

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
    geom_sf(data = map_sf, aes(fill = p_mean), color = "gray15", linewidth = 0.5) +
    geom_sf_label(
      data = lbl,
      aes(label = lab, geometry = geometry),
      size = 2.4,
      fill = alpha("white", 0.92),
      linewidth = 0.15,
      color = "gray10",
      label.padding = unit(0.1, "lines"),
      show.legend = FALSE
    ) +
    coord_sf(
      xlim = xlim_map, ylim = ylim_map,
      crs = crs_wgs, expand = FALSE,
      datum = crs_wgs
    ) +
    scale_fill_gradientn(
      colours = fill_pal_orange,
      name = "Posterior mean\nprevalence",
      limits = lim_fill,
      oob = scales::squish,
      labels = percent_format(accuracy = 1),
      na.value = "grey85"
    ) +
    theme_bw() +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 13, hjust = 0, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.5, hjust = 0, color = "gray25", margin = margin(b = 6)),
      plot.caption = element_text(size = 7, hjust = 0, color = "gray35", lineheight = 1.15)
    ) +
    labs(
      title = "Unmet need for contraception among adolescents — by department",
      subtitle = sprintf("Honduras | Survey %s", wave),
      caption = caption_map
    )
}

slug <- function(s) gsub("[^0-9a-zA-Z]+", "_", s)

for (sv in surveys) {
  m <- fitted_sf %>% filter(survey == sv)
  ggsave(
    file.path(plot_dir, paste0("map_fitted_", slug(sv), ".png")),
    plot_one(m, sv),
    width = 10,
    height = 8.2,
    dpi = 160
  )
  message("Wrote ", file.path(plot_dir, paste0("map_fitted_", slug(sv), ".png")))
}

# Labels for faceted map (one point per department × wave; metric CRS for placement)
lbl_fac <- fitted_sf %>%
  mutate(lab = ifelse(is.finite(p_mean), format(round(p_mean, 2), nsmall = 2), "")) %>%
  filter(lab != "") %>%
  st_transform(crs_lbl) %>%
  mutate(geometry = st_point_on_surface(geometry)) %>%
  st_transform(crs_wgs)

g_fac <- ggplot() +
  geom_sf(data = sea_backdrop, fill = sea_blue, color = NA) +
  geom_sf(data = world_crop, fill = land_fill_world, color = NA, linewidth = 0) +
  geom_sf(data = fitted_sf, aes(fill = p_mean), color = "gray15", linewidth = 0.22) +
  geom_sf_label(
    data = lbl_fac,
    aes(label = lab, geometry = geometry),
    size = 1.85,
    fill = alpha("white", 0.9),
    linewidth = 0.1,
    color = "gray12",
    label.padding = unit(0.06, "lines"),
    show.legend = FALSE
  ) +
  facet_wrap(~survey, nrow = 1) +
  coord_sf(
    xlim = xlim_map, ylim = ylim_map,
    crs = crs_wgs, expand = FALSE, datum = crs_wgs
  ) +
  scale_fill_gradientn(
    colours = fill_pal_orange,
    name = "Posterior mean prevalence",
    limits = lim_fill,
    oob = scales::squish,
    labels = percent_format(accuracy = 1),
    na.value = "grey85"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 8.5, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 6.5, hjust = 0.5, color = "gray40", lineheight = 1.12)
  ) +
  labs(
    title = "Unmet need for contraception among adolescents — by department and survey round",
    subtitle = "Honduras | Rounds 2005–06, 2011–12, 2019",
    caption = caption_map
  )

ggsave(
  file.path(plot_dir, "map_fitted_faceted_3_waves.png"),
  g_fac,
  width = 15,
  height = 6.4,
  dpi = 160
)
message("Wrote ", file.path(plot_dir, "map_fitted_faceted_3_waves.png"))

# ---- Direct (design-based) vs Bayesian fitted (posterior mean) ----
direct_tbl <- read_csv(est_csv, show_col_types = FALSE) %>%
  mutate(
    region = toupper(as.character(region)),
    survey = as.character(survey),
    has_direct = as.integer(ifelse(is.na(has_obs), 0L, has_obs)),
    p_direct = prevalence
  )

cmp <- out_pred %>%
  left_join(
    direct_tbl %>% select(region, survey, p_direct, has_direct),
    by = c("region", "survey")
  )

obs <- cmp %>%
  filter(has_direct == 1L, is.finite(p_direct), is.finite(p_mean))

xr <- range(c(obs$p_direct, obs$p_mean), na.rm = TRUE)
pad <- max(0.02, diff(xr) * 0.06)
xr <- c(max(0, xr[1] - pad), min(1, xr[2] + pad))

g_scatter <- ggplot(obs, aes(x = p_direct, y = p_mean)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "gray45", linewidth = 0.4) +
  geom_point(aes(color = survey), size = 2.8, alpha = 0.9) +
  facet_wrap(~survey, nrow = 1) +
  coord_fixed(xlim = xr, ylim = xr, expand = FALSE) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_brewer(palette = "Dark2", guide = "none") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold")) +
  labs(
    title = "Direct survey estimate vs Bayesian smoothed (posterior mean)",
    subtitle = "Adolescent unmet need — points on y = x would mean no pooling; FH model shrinks toward shared structure.",
    x = "Direct estimate (design-based)",
    y = "Fitted prevalence (BYM2 Fay–Herriot + waves)"
  )

ggsave(
  file.path(plot_dir, "direct_vs_fitted_scatter.png"),
  g_scatter,
  width = 12,
  height = 4.8,
  dpi = 160
)
message("Wrote ", file.path(plot_dir, "direct_vs_fitted_scatter.png"))

obs_d <- obs %>%
  mutate(region_f = reorder(region, p_direct))

g_dumb <- ggplot(obs_d, aes(y = region_f)) +
  geom_segment(
    aes(x = p_direct, xend = p_mean, yend = region_f),
    linewidth = 0.55,
    color = "gray50"
  ) +
  geom_point(aes(x = p_direct), size = 2.4, color = "#2166ac", shape = 16) +
  geom_point(aes(x = p_mean), size = 2.4, color = "#b2182b", shape = 18) +
  facet_wrap(~survey, scales = "free_y", ncol = 1) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = function(l) {
    r <- range(l, na.rm = TRUE)
    pad <- diff(r) * 0.05
    c(max(0, r[1] - pad), min(1, r[2] + pad))
  }) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 6.5)
  ) +
  labs(
    title = "Direct (blue) → Bayesian fitted (red) by department",
    subtitle = "Segments connect design-based prevalence to posterior mean; only cells with a direct estimate.",
    x = "Probability (unmet need)",
    y = NULL,
    caption = "Blue = direct; red diamond = smoothed."
  )

n_obs <- obs %>% count(survey)
h_dumb <- max(6, 1.2 + 0.28 * max(n_obs$n))
ggsave(
  file.path(plot_dir, "direct_vs_fitted_dumbbell_faceted.png"),
  g_dumb,
  width = 8,
  height = h_dumb,
  dpi = 160,
  limitsize = FALSE
)
message("Wrote ", file.path(plot_dir, "direct_vs_fitted_dumbbell_faceted.png"))

message("Plots in ", normalizePath(plot_dir))
