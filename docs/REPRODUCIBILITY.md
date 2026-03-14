# Reproducibility (publication)

This document is for readers who want to **obtain DHS data from the DHS program**, place files as below, install software, and **re-run the full analysis**.

---

## 1. What you need from the DHS Data Portal

Register at **[DHS](https://dhsprogram.com/Data/)** for **HNIR62** (2011–12 IR). For **Honduras 2019** this project uses **UNICEF MICS** (women’s file), not DHS—see table below. Exact survey names can differ slightly on the portal; you need the **IR** file for each survey/wave.

| Country   | Survey (source)        | Place file here      | Expected filename (case may vary) |
|-----------|------------------------|----------------------|-----------------------------------|
| Honduras  | DHS 2011–12 (IR)       | `data/raw/HNIR62SD/` | `hnir62fl.sas7bdat`               |
| Honduras  | **UNICEF MICS 2019** (Women) | `data/raw/HNIR72SD/` | **`hnir72fl.sav`** (rename **`wm.sav`**) or **`hnir72fl.sas7bdat`** |
| Colombia  | DHS 2010                      | `data/raw/COIR61SD/`      | `COIR61FL.SAS7BDAT`               |
| Colombia  | DHS 2015                      | `data/raw/COIR72SD/`      | `COIR72FL.SAS7BDAT`               |

**Important**

- DHS requires agreeing to **terms of use**; do not redistribute raw `.sas7bdat` files—only describe how others can request the same files.
- If the portal gives a **different base name** (e.g. `HNIR62FL` vs `hnir62fl`), rename or adjust paths in `scripts/02_derive/*.R` to match your download, **or** rename your files to match the table above (scripts expect these paths; see main `README.md`).

Optional: download the **SAS setup** (`.sas` + labels) into the same folder as each `.sas7bdat` if you want value labels in metadata JSON.

---

## 2. Spatial layer (Honduras maps & BYM2 models only)

Scripts `honduras_spatial_smooth_stan.R` and `honduras_spatial_plot_maps.R` expect:

```text
data/shp/Shp_mgd.shp   (+ .dbf, .shx, .prj, …)
```

That layer is **not** redistributed with this repo (licensing / size). To reproduce spatial results you must either:

1. Obtain a **Honduras DHS-administrative or GIS product** that includes the same region naming as in the analysis (see script comments in `honduras_spatial_smooth_stan.R` for polygon merging), **or**
2. Contact the authors for the exact shapefile used in the publication (if their data agreement allows sharing).

If the shapefile is missing, everything **except** the spatial Stan pipeline and maps still runs via `Rscript scripts/run_all.R` (core tables and reports).

**Optional:** run without SES covariates in Stan (base ICAR model only):

```bash
export LHS_NO_COV=1
Rscript scripts/03_analysis/honduras_spatial_smooth_stan.R
```

---

## 3. Software

| Component | Role |
|-----------|------|
| **R** (≥ 4.0 recommended) | All scripts |
| **CmdStan** | Bayesian spatial models (`cmdstanr`) |
| **C++ toolchain** | For CmdStan (see [CmdStan install](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)) |

Install R packages once:

```bash
Rscript scripts/bootstrap_packages.R
```

Then install CmdStan from R:

```r
cmdstanr::install_cmdstan()
```

---

## 4. One-command pipeline (after data are in place)

From the **repository root**:

```bash
Rscript scripts/run_all.R
```

This runs, in order:

1. Metadata JSON (optional for analysis; useful for documentation)
2. All four IR derivations → `data/derived/*.rds`
3. Regional SES table → `data/derived/regional_socioeconomic_adol.csv` (needed for covariate spatial model)
4. HNIR design-based region×age×wave (logit) → `output/tables/honduras_region_age_logit_prevalence.csv` (+ vcov)
5. Covariate RTF reports → `output/reports/`
6. Derived-variable dictionary (console)

**Optional – full spatial pipeline** (needs shapefile + CmdStan; can be slow):

```bash
export LHS_RUN_SPATIAL=1
Rscript scripts/run_all.R
```

That additionally runs Honduras Stan fits, maps, and posterior year-difference outputs under `output/spatial_honduras/`.

---

## 5. Reproducibility checklist for the paper

- [ ] DHS IR files placed as in §1 (and paths match scripts)
- [ ] `Rscript scripts/bootstrap_packages.R` then `cmdstanr::install_cmdstan()`
- [ ] `Rscript scripts/run_all.R`
- [ ] (If claiming spatial results) shapefile in `data/shp/` + `LHS_RUN_SPATIAL=1`
- [ ] Record **R `sessionInfo()`** and **CmdStan version** in supplementary material

---

## 6. Random seeds

Stan runs use MCMC; results are **stochastic** unless you fix seeds inside the Stan call scripts and document them. For publication, report **package versions**, **CmdStan version**, **number of chains/draws**, and optionally save **posterior draws** (RDS) as part of outputs—already done for spatial fits under `output/spatial_honduras/` when that pipeline is run.

---

## 7. What is not fully deterministic

- **MCMC** (spatial models): small differences across machines; conclusions should be stable if chains converge.
- **Floating point**: negligible for tables at reported precision.

For strict bit-identical replication, archive **derived `.rds`** and **Stan RDS outputs** as supplementary files (subject to DHS rules for derived aggregates—not microdata).
