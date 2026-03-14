# Scripts

Run all steps from **project root** (see main README).

**Paths:** All scripts use project-relative paths (`data/`, `output/`, `docs/`). When you run a script with `Rscript scripts/.../file.R` from the repo root, the script sets the working directory to the project root automatically.

- **01_metadata** – Metadata JSON (`generate_variable_metadata.R` for HNIR* including **`hnir72fl.sav`**; **`generate_bh_metadata.R`** → `docs/bh_metadata.json`).
- **02_derive** – **`derive_hnir52.R`**, **`derive_hnir62.R`**, **`derive_hnir72fl.R`**, **`derive_coir53.R`**, **`derive_coir61.R`**, **`derive_coir72.R`**.
- **01_metadata** – **`generate_coir53fl_metadata.R`** (and COIR61/72 generators) → `docs/*_metadata.json`.
- **03_analysis** – **`colombia_region_adolescent_logit_prevalence.R`**, **`colombia_bym2_fh_st_fit.R`**, **`colombia_bym2_fh_st_plot_maps.R`**, **`colombia_bym2_fh_st_diagnostics.R`** (Colombia 3-wave spatial Bayes, maps, PPC; `data/col_shps/`).
- **03_analysis** – **`honduras_region_adolescent_logit_prevalence.R`** – design-based adolescent prevalence + **SE** (and logit + SE) by region × wave. **`honduras_spatial_smooth_stan.R`** – BYM2 smooth (shapefile + CmdStan).
- **04_reporting** – Covariate tables (adolescents only), derived-variable dictionary. **`spatial_bayes_prevalence_reporter_table.R`** — one RTF per country; needs `*_fit.rds` + `*_p_mean_by_cell.csv` (national `p_total` CSV optional — filled from RDS; regional CrI from RDS if missing columns). **Diagnostics** (`colombia_bym2_fh_st_diagnostics.R`, `hnir_bym2_fh_st_diagnostics.R`) write `p_total_national_*.png` and `*_p_total_by_wave.csv`.
- **scratch** – One-off or test scripts.
- **run_all.R** – Full pipeline: metadata → derive → estimates → reports.
