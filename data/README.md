# Data

Not in version control except this README and placeholder files. Add raw and derived files as below.

## Layout

Raw DHS recode files go under **`raw/`**; derived outputs go in **`derived/`**.

| Path | Description |
|------|-------------|
| `raw/HNIR62SD/hnir62fl.sas7bdat` | Honduras DHS IR 62 recode (raw) |
| `raw/HNIR72SD/hnir72sd.sas7bdat` | Honduras DHS IR 72 recode (raw) |
| `raw/COIR61SD/COIR61FL.SAS7BDAT` | Colombia DHS IR 61 recode (raw) |
| `raw/COIR72SD/COIR72FL.SAS7BDAT` | Colombia DHS IR 72 recode (raw) |
| `derived/*.rds` | Derived datasets from `scripts/02_derive/` |

Optional: SAS setup files (e.g. `HNIR62FL.SAS`) next to each `.sas7bdat` for value labels in metadata.
