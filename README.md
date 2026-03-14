# LHS0010

This repository contains scripts to derive and document variables from DHS Honduras (HNIR) and Colombia (COIR) datasets. Data files are not included in version control. Place the DHS data files in the expected folders and run the pipeline to reproduce derived datasets and outputs.

**Reproducing for publication:** After obtaining IR files from the [DHS Data Portal](https://dhsprogram.com/Data/), follow **`docs/REPRODUCIBILITY.md`** (portal layout, R packages, CmdStan, optional spatial shapefile).

**Project (Latin America Health Studies, LHS0010):** Geographic inequities in adolescent unmet need for modern contraception in Honduras.

**Objective:** Adolescent unmet need for modern contraception and subnational geographic inequities (DHS/MICS + spatial smoothing).

---

## Project structure

```
LHS0010/
  data/                    # Not in git. Raw + derived data (see Data layout below).
  docs/                    # Prose and variable metadata JSON (see docs/README.md).
  output/
    tables/                # CSV outputs (e.g. age-by-region estimates).
    reports/               # RTF/PDF reports (e.g. baseline characteristics).
  scripts/
    01_metadata/           # Generate variable metadata JSON from SAS.
    02_derive/             # Derive analysis datasets from raw recode files.
    03_analysis/           # Modeling and direct estimates.
    04_reporting/          # Tables and report generation.
    scratch/               # One-off or test scripts.
    run_all.R              # Single entry point: metadata → derive → SES → estimates → reports (+ optional spatial).
```

---

## Data layout

Create and populate `data/` as follows (all paths relative to project root). See **`data/README.md`** for the same layout.

| Path | Description |
|------|-------------|
| `data/raw/HNIR52SD/hnir52fl.sas7bdat` | Honduras DHS IR 52 (2005–06) |
| `data/raw/HNIR62SD/hnir62fl.sas7bdat` | Honduras DHS IR 62 recode |
| `data/raw/HNIR72SD/hnir72fl.sav` (or `hnir72fl.sas7bdat`) | Honduras **MICS 2019** women (`wm.sav` → `hnir72fl.sav`; not DHS) |
| `data/raw/COIR61SD/COIR61FL.SAS7BDAT` | Colombia DHS IR 61 recode |
| `data/raw/COIR72SD/COIR72FL.SAS7BDAT` | Colombia DHS IR 72 recode |
| `data/derived/*.rds` | Derived datasets (produced by `02_derive` scripts) |

Optional: SAS setup files (e.g. `HNIR62FL.SAS`) in the same folder as the `.sas7bdat` for value labels in metadata. The `data/raw/` and `data/derived/` directories are kept in the tree via `.gitkeep`; contents are not versioned.

---

## Scripts

| Folder | Script | Purpose |
|--------|--------|---------|
| **01_metadata** | `generate_variable_metadata.R` | Build JSON variable metadata for HNIR* SAS files in `data/raw/`. |
| **01_metadata** | `generate_coir61fl_metadata.R` | Build JSON variable metadata for COIR61FL. |
| **01_metadata** | `generate_coir72fl_metadata.R` | Build JSON variable metadata for COIR72FL. |
| **02_derive** | `derive_hnir52.R` | HNIR52 2005–06 → `data/derived/hnir52fl_derived.rds`. |
| **02_derive** | `derive_hnir62.R` | Derive analysis variables for HNIR62 → `data/derived/hnir62fl_derived.rds`. |
| **02_derive** | `derive_hnir72fl.R` | MICS 2019 women → `data/derived/hnir72fl_derived.rds`. |
| **02_derive** | `derive_coir61.R` | Derive analysis variables for COIR61 → `data/derived/coir61fl_derived.rds`. |
| **02_derive** | `derive_coir72.R` | Derive analysis variables for COIR72 → `data/derived/coir72fl_derived.rds`. |
| **03_analysis** | `honduras_region_adolescent_logit_prevalence.R` | Design-based **adolescent** unmet need by region × wave; prevalence + SE, logit + SE → spatial inputs. |
| **03_analysis** | `smooth_OR.R` | Spatial smoothing (ICAR, etc.). |
| **03_analysis** | `honduras_spatial_smooth_stan.R` | Honduras BYM2 spatial smooth (`icar_logit_prevalence.stan`; optional cov model via `LHS_USE_COV=1`). |
| **03_analysis** | `honduras_spatial_plot_maps.R` | Maps from saved layers only (no Stan); fill scale = min–max smoothed *p*. |
| **04_reporting** | `create_covariate_report_table.R` | Baseline characteristics tables (RTF) → `output/reports/`. |
| **04_reporting** | `print_derived_dictionary.R` | Print derived-variable dictionary to console. |

All scripts assume they are run from the **project root** (paths like `data/derived` are relative to root). When invoked via `Rscript scripts/.../file.R`, each script sets the working directory to the project root automatically.

---

## Reproduce results

**Prerequisites:** R + packages (`Rscript scripts/bootstrap_packages.R`), and for spatial models **CmdStan** (`cmdstanr::install_cmdstan()`). See **`docs/REPRODUCIBILITY.md`**.

**Option 1 – Full pipeline (from project root)** — requires all four IR `.sas7bdat` files in `data/raw/` as in the table above:

```bash
Rscript scripts/run_all.R
```

Order: **metadata** (JSON) → **derive** → **HNIR adolescent logit** → **covariate RTF reports** → dictionary. Optional spatial: place `data/shp/Shp_mgd.shp` and run:

```bash
LHS_RUN_SPATIAL=1 Rscript scripts/run_all.R
```

**Option 2 – Step by step (from project root):**

1. Generate variable metadata:
   - `Rscript scripts/01_metadata/generate_variable_metadata.R`
   - `Rscript scripts/01_metadata/generate_coir61fl_metadata.R`
   - `Rscript scripts/01_metadata/generate_coir72fl_metadata.R`
2. Generate derived datasets:
   - `Rscript scripts/02_derive/derive_hnir52.R`
   - `Rscript scripts/02_derive/derive_hnir62.R`
   - `Rscript scripts/02_derive/derive_hnir72fl.R`
   - `Rscript scripts/02_derive/derive_coir61.R`
   - `Rscript scripts/02_derive/derive_coir72.R`
3. HNIR region × wave (adolescent SEs): `Rscript scripts/03_analysis/honduras_region_adolescent_logit_prevalence.R` → `output/tables/honduras_region_adolescent_logit_prevalence.csv`.
4. Covariate report tables:
   - `Rscript scripts/04_reporting/create_covariate_report_table.R`
5. Print derived dictionary:
   - `Rscript scripts/04_reporting/print_derived_dictionary.R`

---

## Outputs

| Output | Produced by |
|--------|-------------|
| `docs/*_metadata.json` | `01_metadata` scripts |
| `data/derived/*.rds` | `02_derive` scripts |
| `output/tables/honduras_region_adolescent_logit_prevalence.csv` | `honduras_region_adolescent_logit_prevalence.R` |
| `output/reports/*_characteristics.rtf` | `create_covariate_report_table.R` |
| `output/spatial_honduras/*` | `honduras_spatial_smooth_stan.R` (+ plot script) |

See `output/README.md`, `docs/README.md`, and **`docs/REPRODUCIBILITY.md`**.
