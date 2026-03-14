# Scripts

Run all steps from **project root** (see main README).

**Longleaf (UNC):** SLURM batch scripts use `module add r/4.4.0` and `Rscript` per [Longleaf SLURM examples](https://help.rc.unc.edu/longleaf-slurm-examples/): `scripts/03_analysis/run_honduras_bym2_st_longleaf.sbatch` (spatio-temporal Stan), `run_honduras_bym2_longleaf.sbatch` (per-wave Stan). Submit with `mkdir -p logs && sbatch scripts/03_analysis/run_honduras_bym2_st_longleaf.sbatch`.

**Paths:** All scripts use project-relative paths (`data/`, `output/`, `docs/`). When you run a script with `Rscript scripts/.../file.R` from the repo root, the script sets the working directory to the project root automatically, so there are no path conflicts. If you run a script interactively (e.g. Source in RStudio), set the working directory to the project root first (e.g. **Session → Set Working Directory → To Project Directory**), or paths like `data/derived` will be resolved relative to the current directory and may fail.

- **01_metadata** – Metadata JSON (`generate_variable_metadata.R` for HNIR* including **`hnir72fl.sav`**; **`generate_bh_metadata.R`** → `docs/bh_metadata.json`).
- **02_derive** – Build derived analysis datasets from raw DHS recode files; **`derive_regional_socioeconomic.R`** → adolescent regional SES covariates (`data/derived/regional_socioeconomic_adol.*`) for spatial/Bayes models.
- **03_analysis** – Direct estimates and spatial modeling. **`shared_bym2_fay_herriot_st.stan`** + **`honduras_shared_bym2_fay_herriot_st.R`** — spatio-temporal shared BYM2 (AR(1) on `b`, joint T=2 waves). **`honduras_shared_bym2_plot_maps.R`** — maps from `shared_bym2_fh_*_region_summary.csv` (after BYM2 fit). **`honduras_region_age_prevalence_logit.R`** – unmet-need **logit** prevalence + SE by **region × (Adolescent | Young Adult) × wave**, full **logit** covariance → `output/tables/honduras_region_age_*logit*`. **`shared_bym2_fay_herriot.stan`** + **`shared_bym2_fay_herriot_fit.R`** – helpers for the shared **BYM2** Fay–Herriot model. **`honduras_shared_bym2_fay_herriot.R`** – fits that model for **both** Honduras waves (2011-12 and 2019); outputs under `output/spatial_honduras/shared_bym2_fh_*` (needs CmdStan).
- **04_reporting** – Covariate tables, derived-variable dictionary, and Honduras unmet-need plots (`unmet_need_covariate_plots_honduras.R` → `output/reports/unmet_need_honduras/unmet_need_covariates_report.md`).
- **scratch** – One-off or test scripts.
- **run_all.R** – Run full pipeline: metadata → derive → estimates → reports.
