# Outputs

Generated tables and reports. Re-run the pipeline (see main README) to regenerate.

## Layout

- **`tables/`** – CSV and other tabular outputs.
  - `honduras_region_adolescent_logit_prevalence.csv` – **18 rows per wave**; **`is_mainland`** = 1 for mainland departments, **0** for Islas de la Bahía; **`has_obs`** = 1 if there is a design estimate, 0 if NA (not sampled). Uses **one `svydesign` per wave**. Spatial Stan uses only rows with finite direct logit.
  - `colombia_region_adolescent_logit_prevalence.csv` – **32 departments × 3 waves** (2005, 2010, 2015); **`is_mainland`** = 0 for **San Andres y Providencia** only. From `colombia_region_adolescent_logit_prevalence.R`.
- **`spatial_honduras/`** – BYM2 spatial smooth outputs (`honduras_spatial_smooth_stan.R`) and plots (`honduras_spatial_plot_maps.R`).
- **`spatial_honduras_st/`** – BYM2 Fay–Herriott fit (`hnir_bym2_fh_st_fit.R`): `plots/map_fitted_*.png` (2005-06, 2011-12, 2019) + `map_fitted_faceted_3_waves.png`. Replot only: `Rscript scripts/03_analysis/honduras_bym2_fh_st_plot_maps.R`.
- **`spatial_colombia_st/`** – Colombia BYM2 FH (`colombia_bym2_fh_st_fit.R`): mainland ICAR; **San Andrés** island = iid only. **Plots:** `colombia_bym2_fh_st_plot_maps.R`. **Diagnostics / PPC:** `colombia_bym2_fh_st_diagnostics.R` → `spatial_colombia_st/diagnostics/`.
- **`reports/`** – RTF/PDF reports.
  - `*_characteristics.rtf` – Baseline characteristics (**adolescents**) by survey (`scripts/04_reporting/create_covariate_report_table.R`).
