library(haven)
library(dplyr)
library(tidyr)

# Run from project root when invoked via Rscript
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  project_root <- dirname(dirname(script_dir))
  setwd(project_root)
}

input_path <- "data/raw/HNIR72SD/hnir72sd.sas7bdat"
output_path <- "data/derived/hnir72sd_derived.rds"

data <- read_sas(input_path)

required_vars <- c(
  "wmweight", "stratum", "PSU", "WM17", "HH6", "HH7", "WB4", "WB5", "WB6A", "WB6B",
  "MA1", "MA2", "CP1", "CP2", "CP4A", "CP4B", "CP4C", "CP4D", "CP4E", "CP4F", "CP4G",
  "CP4H", "CP4I", "CP4J", "CP4L", "CP4M", "CP4X", "SB2U", "SB2N", "UN14U", "UN14N",
  "UN12B", "UN12C", "UN12D", "UN12E", "UN12H", "UN12X", "UN7", "UN8U", "UN8N", "UN2",
  "UN4", "windex5", "CEB"
)

missing_vars <- setdiff(required_vars, names(data))
if (length(missing_vars) > 0) {
  stop(paste("Missing required variables:", paste(missing_vars, collapse = ", ")))
}

region_map <- c(
  "1" = "ATLANTIDA",
  "2" = "COLON",
  "3" = "COMAYAGUA",
  "4" = "COPAN",
  "5" = "CORTES",
  "6" = "CHOLUTECA",
  "7" = "EL PARAISO",
  "8" = "FRANCISCO MORAZAN",
  "9" = "GRACIAS A DIOS",
  "10" = "INTIBUCA",
  "11" = "ISLAS DE LA BAHIA",
  "12" = "LA PAZ",
  "13" = "LEMPIRA",
  "14" = "OCOTEPEQUE",
  "15" = "OLANCHO",
  "16" = "SANTA BARBARA",
  "17" = "VALLE",
  "18" = "YORO",
  "19" = "SAN PEDRO SULA",
  "20" = "DISTRITO CENTRAL"
)

hnir72_derived <- data %>%
  mutate(
    strat = stratum,
    psu_id = PSU,
    weight = wmweight / 1e6,
    interviewed = if_else(replace_na(WM17 == 1, FALSE), 1L, 0L),
    urban = if_else(HH6 == 1, 1L, if_else(HH6 == 2, 0L, NA_integer_)),
    region = unname(region_map[as.character(HH7)]),
    region_code = HH7,
    windex_c3 = if_else(
      windex5 %in% c(1, 2),
      1L,
      if_else(windex5 == 3, 2L, if_else(windex5 %in% c(4, 5), 3L, NA_integer_))
    ),
    agegroup_c4 = case_when(
      WB4 < 20 ~ 1L,
      WB4 >= 20 & WB4 <= 24 ~ 2L,
      WB4 >= 25 & WB4 <= 34 ~ 3L,
      WB4 >= 35 ~ 4L,
      TRUE ~ NA_integer_
    ),
    Adolescent = as.integer(agegroup_c4 == 1L),
    Young_Adult = as.integer(agegroup_c4 %in% c(2L, 3L)),
    education_c4 = case_when(
      WB5 %in% c(2, 9) | WB6A %in% c(0, 1) ~ 0L,
      WB6A %in% c(3, 4) | (WB6A == 2 & WB6B > 6) ~ 2L,
      WB6A %in% c(5, 6, 7, 8) ~ 3L,
      WB6A == 2 & WB6B <= 6 ~ 1L,
      TRUE ~ NA_integer_
    ),
    parity = if_else(!is.na(CEB), as.integer(CEB), NA_integer_),
    employed = NA_integer_,  # not collected in HNIR72 (MICS); kept for schema alignment
    married_union = if_else(replace_na(MA1 %in% c(1, 2), FALSE), 1L, 0L),
    ma2 = MA2,
    pregnant = if_else(replace_na(CP1 == 1, FALSE), 1L, 0L),
    sexually_active = if_else(
      replace_na(
        married_union == 1L |
          (married_union == 0L & (
          (SB2U == 1 & SB2N <= 30) |
            (SB2U == 2 & SB2N <= 4)
          )),
        FALSE
      ),
      1L,
      0L
    ),
    period_last_6mo = if_else(
      replace_na(
        pregnant == 0L & (
          (UN14U == 1 & UN14N <= 30) |
            (UN14U == 2 & UN14N <= 4) |
            (UN14U == 3 & UN14N <= 1)
        ),
        FALSE
      ),
      1L,
      0L
    ),
    unfecund = if_else(
      UN12B == "B" | UN12C == "C" | UN12D == "D" | UN12E == "E" | UN12H == "H" | UN12X == "X",
      1L,
      0L
    ),
    contraceptive_use = if_else(replace_na(CP2 == 1, FALSE), 1L, 0L),
    modern_method = case_when(
      CP1 != 1 & (CP4A == "A" | CP4B == "B" | CP4C == "C" | CP4D == "D" | CP4E == "E" |
                    CP4F == "F" | CP4G == "G" | CP4H == "H" | CP4I == "I" | CP4J == "J") ~ 1L,
      CP4L == "L" | CP4M == "M" | CP4X == "X" ~ 0L,
      TRUE ~ NA_integer_
    ),
    using_modern_contraceptive = case_when(
      contraceptive_use == 1L & modern_method == 1L ~ 1L,
      contraceptive_use == 1L & modern_method == 0L ~ 0L,
      contraceptive_use == 0L ~ 0L,
      TRUE ~ NA_integer_
    ),
    kids_soon = if_else(
      replace_na(
        sexually_active == 1L & pregnant == 0L & unfecund == 0L & UN7 == 1 & (
          (UN8U == 1 & UN8N <= 24) |
            (UN8U == 2 & UN8N <= 2) |
            UN8N == 93
        ),
        FALSE
      ),
      1L,
      0L
    ),
    unmet_need_c3 = case_when(
      pregnant == 1L & UN2 == "1" ~ 0L,
      pregnant == 1L & UN2 == "2" & UN4 == "2" ~ 2L,
      pregnant == 1L & UN2 == "2" ~ 1L,
      pregnant == 1L ~ NA_integer_,
      sexually_active != 1L ~ NA_integer_,
      unfecund == 1L ~ NA_integer_,
      contraceptive_use == 1L & modern_method == 1L ~ 0L,
      contraceptive_use == 1L & !coalesce(modern_method == 1L, FALSE) & UN7 == 2 ~ 2L,
      contraceptive_use == 1L & !coalesce(modern_method == 1L, FALSE) & kids_soon == 1L ~ 0L,
      contraceptive_use == 1L & !coalesce(modern_method == 1L, FALSE) & UN7 == 3 ~ NA_integer_,
      contraceptive_use == 1L & !coalesce(modern_method == 1L, FALSE) & UN7 %in% c(1, 8) ~ 1L,
      contraceptive_use == 1L & !coalesce(modern_method == 1L, FALSE) ~ NA_integer_,
      contraceptive_use == 0L & kids_soon == 1L ~ 0L,
      contraceptive_use == 0L & kids_soon != 1L & UN7 == 3 ~ NA_integer_,
      contraceptive_use == 0L & kids_soon != 1L & (UN7 == 2 | (UN7 == 1 & UN8N == 94)) ~ 2L,
      contraceptive_use == 0L & kids_soon != 1L ~ 1L,
      TRUE ~ NA_integer_
    ),
    unmet_need = if_else(
      is.na(unmet_need_c3),
      NA_integer_,
      if_else(unmet_need_c3 %in% c(1, 2), 1L, 0L)
    )
  ) %>%
  filter(interviewed == 1L)

dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(hnir72_derived, output_path)
cat("Saved derived dataset to:", output_path, "\n")
