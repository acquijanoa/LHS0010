# Outputs

Generated tables and reports. Re-run the pipeline (see main README) to regenerate.

## Layout

- **`tables/`** – CSV and other tabular outputs.
  - `age_region_estimates.csv` – Design-based age-by-region unmet-need proportions (from `scripts/03_analysis/estimate_age_region.R`, run with output path `output/tables/age_region_estimates.csv`).
- **`reports/`** – RTF/PDF reports.
  - `*_characteristics.rtf` – Baseline characteristics by age group (from `scripts/04_reporting/create_covariate_report_table.R`).
