#!/usr/bin/env Rscript
# Choropleth maps for shared BYM2 Fay–Herriot posterior prevalence (Honduras).
#
# Inputs:
#   output/spatial_honduras/shared_bym2_fh_both_waves_region_summary.csv
#   (or per-wave CSVs if combined missing)
#   data/shp/Shp_mgd.shp
#
# Outputs (PNG):
#   output/spatial_honduras/plots/shared_bym2_fh_map_adolescent_<slug>.png
#   output/spatial_honduras/plots/shared_bym2_fh_map_young_adult_<slug>.png
#   output/spatial_honduras/plots/shared_bym2_fh_map_faceted_<slug>.png
#   output/spatial_honduras/plots/shared_bym2_fh_map_adolescent_both_waves.png  (shared scale)
#
#   Rscript scripts/03_analysis/honduras_shared_bym2_plot_maps.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(sf)
  library(ggplot2)
  library(scales)
  library(rlang)
  library(grid)
})

args_cmd <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args_cmd[grep("^--file=", args_cmd)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  setwd(dirname(dirname(script_dir)))
}

shp_path <- "data/shp/Shp_mgd.shp"
summary_both <- "output/spatial_honduras/shared_bym2_fh_both_waves_region_summary.csv"
summary_2011 <- "output/spatial_honduras/shared_bym2_fh_2011_12_region_summary.csv"
summary_2019 <- "output/spatial_honduras/shared_bym2_fh_2019_region_summary.csv"
plot_dir <- "output/spatial_honduras/plots"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(shp_path))

if (file.exists(summary_both)) {
  tab <- read_csv(summary_both, show_col_types = FALSE)
} else if (file.exists(summary_2011) && file.exists(summary_2019)) {
  tab <- bind_rows(
    read_csv(summary_2011, show_col_types = FALSE) %>% mutate(survey = "2011-12"),
    read_csv(summary_2019, show_col_types = FALSE) %>% mutate(survey = "2019")
  )
} else {
  stop(
    "Need shared_bym2_fh_both_waves_region_summary.csv or both per-wave CSVs.\n",
    "Run: Rscript scripts/03_analysis/honduras_shared_bym2_fay_herriot.R"
  )
}

# 18 departments — same as BYM2 fit script
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

slugify <- function(x) gsub("[^0-9a-zA-Z]+", "_", x)

fill_limits <- function(x, pad_ratio = 0.03) {
  x <- x[is.finite(x)]
  if (!length(x)) return(c(0, 1))
  lo <- min(x)
  hi <- max(x)
  if (hi <= lo) return(c(max(0, lo - 0.02), min(1, hi + 0.02)))
  pad <- (hi - lo) * pad_ratio
  c(max(0, lo - pad), min(1, hi + pad))
}

# Label point inside each polygon; prevalence to 2 decimals (proportion scale)
sf_with_prev_labels <- function(map_sf, p_var) {
  v <- map_sf[[p_var]]
  map_sf %>%
    mutate(
      lab = ifelse(is.finite(v), format(round(v, 2), nsmall = 2), ""),
      geometry = sf::st_point_on_surface(geometry)
    ) %>%
    filter(lab != "")
}

#' Single choropleth on prevalence scale [0,1]
plot_prev_map <- function(map_sf, fill_var, title, subtitle, lim, fill_name = "P(unmet)") {
  lbl <- sf_with_prev_labels(map_sf, fill_var)
  ggplot(map_sf) +
    geom_sf(aes(fill = !!sym(fill_var)), color = "gray25", linewidth = 0.2) +
    geom_sf_label(
      data = lbl,
      aes(label = lab),
      size = 2.4,
      fill = alpha("white", 0.9),
      label.size = 0.12,
      color = "gray15",
      label.padding = unit(0.12, "lines"),
      show.legend = FALSE
    ) +
    scale_fill_viridis_c(
      option = "D", # classic viridis (perceptually uniform)
      name = fill_name,
      limits = lim,
      labels = scales::percent_format(accuracy = 1),
      na.value = "grey85"
    ) +
    theme_void() +
    theme(legend.position = "right") +
    labs(title = title, subtitle = subtitle)
}

survey_slugs <- tab %>%
  distinct(survey) %>%
  mutate(slug = slugify(survey))

for (k in seq_len(nrow(survey_slugs))) {
  surv <- survey_slugs$survey[k]
  slug <- survey_slugs$slug[k]
  d <- tab %>% filter(survey == surv)
  map_k <- honduras18 %>%
    left_join(d, by = c("REGION_NAME" = "region"))

  if (any(is.na(map_k$p1_median))) {
    warning("Unmatched regions for survey ", surv)
  }

  lim1 <- fill_limits(map_k$p1_median)
  lim2 <- fill_limits(map_k$p2_median)

  g1 <- plot_prev_map(
    map_k,
    "p1_median",
    paste0("Honduras — shared BYM2 (posterior median)"),
    paste0(surv, " | Adolescent | fill [", round(lim1[1], 3), ", ", round(lim1[2], 3), "]"),
    lim1,
    "Adolescent\nP(unmet)"
  )
  ggsave(file.path(plot_dir, paste0("shared_bym2_fh_map_adolescent_", slug, ".png")),
    g1, width = 9, height = 7.5, dpi = 160)

  g2 <- plot_prev_map(
    map_k,
    "p2_median",
    paste0("Honduras — shared BYM2 (posterior median)"),
    paste0(surv, " | Young adult | fill [", round(lim2[1], 3), ", ", round(lim2[2], 3), "]"),
    lim2,
    "Young adult\nP(unmet)"
  )
  ggsave(file.path(plot_dir, paste0("shared_bym2_fh_map_young_adult_", slug, ".png")),
    g2, width = 9, height = 7.5, dpi = 160)

  # Faceted: same wave, two outcomes (common scale per wave for comparability)
  lim_both <- fill_limits(c(map_k$p1_median, map_k$p2_median))
  long <- map_k %>%
    select(REGION_NAME, geometry, p1_median, p2_median) %>%
    pivot_longer(c(p1_median, p2_median), names_to = "outcome", values_to = "p") %>%
    mutate(
      outcome = recode(outcome, p1_median = "Adolescent", p2_median = "Young adult"),
      lab = ifelse(is.finite(p), format(round(p, 2), nsmall = 2), "")
    )
  lbl_facet <- long %>%
    mutate(geometry = sf::st_point_on_surface(geometry)) %>%
    filter(lab != "")
  gf <- ggplot(long) +
    geom_sf(aes(fill = p), color = "gray25", linewidth = 0.2) +
    geom_sf_label(
      data = lbl_facet,
      aes(label = lab),
      size = 2.2,
      fill = alpha("white", 0.9),
      label.size = 0.1,
      color = "gray15",
      label.padding = unit(0.1, "lines"),
      show.legend = FALSE
    ) +
    facet_wrap(~outcome, nrow = 1) +
    scale_fill_viridis_c(
      option = "D", # classic viridis (perceptually uniform)
      name = "P(unmet)",
      limits = lim_both,
      labels = scales::percent_format(accuracy = 1),
      na.value = "grey85"
    ) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(
      title = paste0("Honduras — shared BYM2 | ", surv),
      subtitle = "Posterior median prevalence (shared spatial factor)"
    )
  ggsave(file.path(plot_dir, paste0("shared_bym2_fh_map_faceted_", slug, ".png")),
    gf, width = 12, height = 6, dpi = 160)

  message("Wrote maps for survey ", surv)
}

# Both waves, adolescent only — shared color scale (facet, no patchwork)
if (nrow(survey_slugs) >= 2L) {
  lim_ado <- fill_limits(tab$p1_median)
  mb <- lapply(seq_len(nrow(survey_slugs)), function(k) {
    surv <- survey_slugs$survey[k]
    d <- tab %>% filter(survey == surv) %>% select(region, p1_median)
    honduras18 %>%
      left_join(d, by = c("REGION_NAME" = "region")) %>%
      mutate(survey = surv)
  })
  mb <- bind_rows(mb)
  mb_lbl <- mb %>%
    mutate(
      lab = ifelse(is.finite(p1_median), format(round(p1_median, 2), nsmall = 2), ""),
      geometry = sf::st_point_on_surface(geometry)
    ) %>%
    filter(lab != "")
  g_bw <- ggplot(mb) +
    geom_sf(aes(fill = p1_median), color = "gray25", linewidth = 0.2) +
    geom_sf_label(
      data = mb_lbl,
      aes(label = lab),
      size = 2.2,
      fill = alpha("white", 0.9),
      label.size = 0.1,
      color = "gray15",
      show.legend = FALSE
    ) +
    facet_wrap(~survey, nrow = 1) +
    scale_fill_viridis_c(
      option = "D", # classic viridis (perceptually uniform)
      name = "Adolescent\nP(unmet)",
      limits = lim_ado,
      labels = scales::percent_format(accuracy = 1),
      na.value = "grey85"
    ) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(
      title = "Honduras — shared BYM2 | Adolescent prevalence",
      subtitle = paste0("Common fill scale [", round(lim_ado[1], 3), ", ", round(lim_ado[2], 3), "]")
    )
  ggsave(
    file.path(plot_dir, "shared_bym2_fh_map_adolescent_both_waves.png"),
    g_bw, width = 14, height = 6.5, dpi = 160
  )
  message("Wrote shared_bym2_fh_map_adolescent_both_waves.png")
}

message("Plots in ", normalizePath(plot_dir))
