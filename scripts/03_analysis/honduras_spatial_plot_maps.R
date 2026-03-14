#!/usr/bin/env Rscript
# Maps from saved Bayesian spatial layers (no Stan). Edit this file freely for visuals.
#
# Inputs (from honduras_spatial_smooth_stan.R):
#   output/spatial_honduras/spatial_layer_<slug>.rds   — sf + p_smoothed_mean, CIs, direct
#   output/spatial_honduras/smoothed_prevalence_<slug>.csv — same table without geometry
# If no .rds files exist, the script exits 0 and prints hints (no error).
#
# Fill scale uses min/max of smoothed prevalence on that map so contrast is visible.
#
#   Rscript scripts/03_analysis/honduras_spatial_plot_maps.R
#   Rscript scripts/03_analysis/honduras_spatial_plot_maps.R 2011-12   # one survey only

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

out_dir <- "output/spatial_honduras"
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

trail <- commandArgs(trailingOnly = TRUE)
slug_filter <- if (length(trail) >= 1) gsub("[^0-9a-zA-Z]+", "_", trail[[1]]) else NULL

layer_files <- list.files(out_dir, pattern = "^spatial_layer_.*\\.rds$", full.names = TRUE)
if (!length(layer_files)) {
  message(
    "No spatial_layer_*.rds in ", normalizePath(out_dir, mustWork = FALSE), ".\n",
    "  → Adolescent-only BYM2 maps:  Rscript scripts/03_analysis/honduras_spatial_smooth_stan.R\n",
    "     then re-run this script.\n",
    "  → Shared-factor (Adolescent + Young adult) maps:  Rscript scripts/03_analysis/honduras_shared_bym2_plot_maps.R\n",
    "     (uses shared_bym2_fh_*_region_summary.csv; no spatial_layer rds needed)."
  )
  quit(save = "no", status = 0)
}

fill_limits <- function(x, pad_ratio = 0.03) {
  x <- x[is.finite(x)]
  if (!length(x)) return(c(0, 1))
  lo <- min(x)
  hi <- max(x)
  if (hi <= lo) {
    eps <- 0.02
    return(c(max(0, lo - eps), min(1, hi + eps)))
  }
  pad <- (hi - lo) * pad_ratio
  c(max(0, lo - pad), min(1, hi + pad))
}

plot_choropleth <- function(map_sf, survey_year, slug) {
  lim <- fill_limits(map_sf$p_smoothed_mean)
  lbl <- map_sf %>%
    mutate(
      lab = ifelse(is.finite(p_smoothed_mean), format(round(p_smoothed_mean, 2), nsmall = 2), ""),
      geometry = st_point_on_surface(geometry)
    ) %>%
    filter(lab != "")
  ggplot(map_sf) +
    geom_sf(aes(fill = p_smoothed_mean), color = "gray20", linewidth = 0.15) +
    geom_sf_label(
      data = lbl,
      aes(label = lab),
      size = 2.4,
      fill = ggplot2::alpha("white", 0.9),
      label.size = 0.12,
      color = "gray15",
      label.padding = grid::unit(0.12, "lines"),
      show.legend = FALSE
    ) +
    scale_fill_viridis_c(
      option = "D",
      name = "Smoothed\nP(unmet)",
      limits = lim,
      na.value = "grey85"
    ) +
    theme_void() +
    theme(legend.position = "right") +
    labs(
      title = "Honduras — smoothed unmet need (adolescents)",
      subtitle = sprintf("%s | fill range [%.3f, %.3f] | labels = prevalence (2 dp)", survey_year, lim[1], lim[2]),
      caption = "Edit scripts/03_analysis/honduras_spatial_plot_maps.R to change style."
    )
}

#' Scatter y≈constant when model pools strongly → looks like a flat line. Use dumbbell instead.
plot_direct_vs_smooth_dumbbell <- function(map_sf, survey_year) {
  df <- st_drop_geometry(map_sf) %>%
    mutate(
      REGION_NAME = reorder(REGION_NAME, p_direct),
      se_ok = ifelse(has_se == 1L, "Direct SE known", "Direct SE missing")
    )
  xr <- range(c(df$p_direct, df$p_smoothed_mean), na.rm = TRUE)
  pad <- max(0.02, diff(xr) * 0.08)
  xr <- c(max(0, xr[1] - pad), min(1, xr[2] + pad))

  ggplot(df, aes(y = REGION_NAME)) +
    geom_segment(
      aes(x = p_direct, xend = p_smoothed_mean, yend = REGION_NAME),
      linewidth = 0.6,
      color = "gray55"
    ) +
    geom_point(aes(x = p_direct, shape = se_ok), size = 2.8, color = "#2166ac") +
    geom_point(aes(x = p_smoothed_mean), size = 2.8, color = "#b2182b", shape = 18) +
    scale_x_continuous(limits = xr, expand = c(0, 0), labels = scales::percent_format(accuracy = 0.1)) +
    scale_shape_manual(values = c("Direct SE known" = 16, "Direct SE missing" = 1), name = NULL) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 7)
    ) +
    labs(
      x = "Probability (unmet need)",
      y = NULL,
      title = paste("Direct (blue) → smoothed (red) —", survey_year),
      subtitle = "Spatial model shrinks noisy direct estimates toward a shared surface; scatter y~x often looks flat.",
      caption = "Blue = direct survey estimate; red diamond = posterior mean (BYM2)."
    )
}

#' Optional: shrinkage size per region (horizontal bars)
plot_shrinkage <- function(map_sf, survey_year) {
  df <- st_drop_geometry(map_sf) %>%
    mutate(
      delta = p_smoothed_mean - p_direct,
      REGION_NAME = reorder(REGION_NAME, delta)
    )
  ggplot(df, aes(x = delta, y = REGION_NAME, fill = delta > 0)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, linetype = 2) +
    scale_fill_manual(values = c(`TRUE` = "#d6604d", `FALSE` = "#4393c3"), guide = "none") +
    theme_bw() +
    labs(
      x = "Smoothed − direct",
      y = NULL,
      title = paste("Pooling shift —", survey_year),
      subtitle = "Negative = smoothed pulled below direct (usually high-noise highs)"
    )
}

for (f in layer_files) {
  slug <- sub("^spatial_layer_|\\.rds$", "", basename(f))
  if (length(slug_filter) && slug != slug_filter) next

  map_sf <- readRDS(f)
  survey_year <- unique(map_sf$survey_year)
  if (length(survey_year) != 1L) survey_year <- slug

  ggsave(
    file.path(plot_dir, paste0("map_smoothed_", slug, ".png")),
    plot_choropleth(map_sf, survey_year, slug),
    width = 9,
    height = 7.5,
    dpi = 160
  )

  ggsave(
    file.path(plot_dir, paste0("direct_vs_smooth_", slug, ".png")),
    plot_direct_vs_smooth_dumbbell(map_sf, survey_year),
    width = 8,
    height = max(5, 0.22 * nrow(map_sf) + 2),
    dpi = 160,
    limitsize = FALSE
  )
  ggsave(
    file.path(plot_dir, paste0("shrinkage_", slug, ".png")),
    plot_shrinkage(map_sf, survey_year),
    width = 7,
    height = max(4, 0.2 * nrow(map_sf) + 1.5),
    dpi = 160,
    limitsize = FALSE
  )
  message("Wrote plots for ", slug)
}

message("Plots in ", normalizePath(plot_dir))
