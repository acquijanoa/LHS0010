# Data

**Raw** (`raw/`) and **shapefiles** (`shp/`) are listed in `.gitignore` and are not pushed to the repo—add them locally after cloning. This README, `raw/.gitkeep`, and `derived/` (if you track it) stay in git. Placeholders describe expected layout below.

**How to obtain raw files:** Register at the [DHS Data Portal](https://dhsprogram.com/Data/), request the **Individual Recode (IR)** for each survey, download **SAS** format, and place `.sas7bdat` files as in the table. Step-by-step for replication: **`docs/REPRODUCIBILITY.md`**.

## Layout

Raw DHS recode files go under **`raw/`**; derived outputs go in **`derived/`**.

| Path | Description |
|------|-------------|
| `raw/HNIR62SD/hnir62fl.sas7bdat` | Honduras DHS IR 62 recode (raw) |
| `raw/HNIR72SD/hnir72fl.sav` (or `.sas7bdat`) | Honduras **MICS 2019** women; see `raw/HNIR72SD/README.md` |
| `raw/COIR61SD/COIR61FL.SAS7BDAT` | Colombia DHS IR 61 recode (raw) |
| `raw/COIR72SD/COIR72FL.SAS7BDAT` | Colombia DHS IR 72 recode (raw) |
| `derived/*.rds` | Derived datasets from `scripts/02_derive/` |

Optional: SAS setup files (e.g. `HNIR62FL.SAS`) next to each `.sas7bdat` for value labels in metadata.
