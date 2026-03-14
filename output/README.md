# Outputs

Generated tables and reports. Re-run the pipeline (see main README) to regenerate.

## Layout

- **`tables/`** – CSV and other tabular outputs.
  - `honduras_region_age_logit_prevalence.csv` – **Adolescent** vs **Young Adult** cells: logit prevalence + SE by region × age × wave; `honduras_region_age_vcov_logit.rds` / `*_matrix.csv` = full logit covariance (`scripts/03_analysis/honduras_region_age_prevalence_logit.R`). Used by spatial Stan (Adolescent rows).
- **`spatial_honduras/`** – BYM2 / shared-factor model outputs and plots.
  - `shared_bym2_fh_*_region_summary.csv` – posterior medians & 95% CI per region × wave, **separate fit per wave** (`honduras_shared_bym2_fay_herriot.R`).
  - `shared_bym2_fh_st_*` – **joint** spatio-temporal fit (`honduras_shared_bym2_fay_herriot_st.R`): `*_prevalence_by_region_time.csv`, `*_theta_by_region_time.csv`, `*_region_summary_wide.csv`.
  - `plots/shared_bym2_fh_map_*.png` – choropleths from `honduras_shared_bym2_plot_maps.R`.
- **`reports/`** – RTF/PDF reports.
  - `*_characteristics.rtf` – Baseline characteristics by age group (from `scripts/04_reporting/create_covariate_report_table.R`).
