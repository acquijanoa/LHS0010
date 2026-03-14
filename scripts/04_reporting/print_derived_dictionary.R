library(tibble)

# Run from project root when invoked via Rscript
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  project_root <- dirname(dirname(script_dir))
  setwd(project_root)
}

dictionary <- tibble(
  variable = c(
    "strat",
    "psu_id",
    "weight",
    "pregnant",
    "sexually_active",
    "married_union",
    "urban",
    "region",
    "windex_c3",
    "education_c4",
    "agegroup_c4",
    "Adolescent",
    "employed",
    "parity",
    "using_modern_contraceptive",
    "unfecund",
    "unmet_need_c3"
  ),
  description = c(
    "Sample stratum (V022)",
    "Primary sampling unit (V021)",
    "Sampling weight (V005/1e6)",
    "Currently pregnant (V213)",
    "Sexually active in last 30 days (excluding 1 month; never had sex=0)",
    "Currently married or living together (V501)",
    "Urban residence (V025)",
    "Department/region (V024, ASCII uppercase)",
    "Wealth index (collapsed to 3 categories) (V190)",
    "Education level (4 categories) (V106)",
    "Age group (4 categories) (V012)",
    "Adolescent indicator (1 if agegroup_c4=1, else 0)",
    "Currently working (V714)",
    "Children ever born (V201)",
    "Currently using modern contraception (V313)",
    "Unfecund: menopause/never menstruated/hysterectomy/never had sex (V215/V376/V3A08D/V602)",
    "Unmet need categories: 0 none, 1 spacing, 2 limiting"
  ),
  values = c(
    "Numeric",
    "Numeric",
    "Numeric",
    "1=Yes, 0=No, NA=Missing",
    "1=Yes (<=30 days), 0=No (>30 days), NA=Missing/NI",
    "1=Currently in union, 0=Not currently in union, NA=Missing",
    "1=Urban, 0=Rural, NA=Missing",
    "Uppercase string",
    "1=Poorest/Poorer, 2=Middle, 3=Richer/Richest, NA=Missing",
    "0=None, 1=Primary, 2=Secondary, 3=Higher, NA=Missing",
    "1=<20, 2=20-24, 3=25-34, 4=35+, NA=Missing",
    "1=Adolescent (agegroup_c4=1), 0=No, NA=Missing",
    "1=Yes, 0=No, NA=Missing",
    "Numeric count",
    "1=Modern method, 0=No/folkloric/traditional, NA=Missing",
    "1=Unfecund, 0=Fecund, NA=Missing",
    "0=None, 1=Spacing, 2=Limiting, NA=Excluded/unknown"
  )
)

print(dictionary, n = nrow(dictionary))
