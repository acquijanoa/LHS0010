# Derive COIR61 (Colombia DHS 2010) to same schema as other DHS/MICS derived datasets.
# Variable mapping aligned with derive_coir72.R (Colombia department = SDEPTO).

library(haven)
library(dplyr)

# Run from project root when invoked via Rscript
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(script_path) == 1) {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
  project_root <- dirname(dirname(script_dir))
  setwd(project_root)
}

input_path <- "data/raw/COIR61SD/COIR61FL.SAS7BDAT"
output_path <- "data/derived/coir61fl_derived.rds"

data <- read_sas(input_path)

# Same required vars as COIR72 (DHS standard recode); COIR61 may use V024 if SDEPTO missing
required_vars <- c(
  "V005", "V015", "V021", "V022", "V025", "V012", "V106",
  "V190", "V201", "V213", "V215", "V225", "V313", "V376",
  "V501", "V525", "V527", "V528", "V602", "V603", "V714"
)
# SDEPTO (department) or V024 (region) for Colombia
region_var <- if ("SDEPTO" %in% names(data)) "SDEPTO" else "V024"
required_vars <- c(required_vars, region_var)

# Optional: ensure V3A08D exists for unfecund logic (older surveys may not have it)
if (!"V3A08D" %in% names(data)) data$V3A08D <- NA_real_

missing_vars <- setdiff(required_vars, names(data))
if (length(missing_vars) > 0) {
  stop(paste("Missing required variables:", paste(missing_vars, collapse = ", ")))
}

# Colombia departments (same as COIR72)
region_map <- c(
  "5" = "Antioquia",
  "8" = "Atlantico",
  "11" = "Bogota",
  "13" = "Bolivar",
  "15" = "Boyaca",
  "17" = "Caldas",
  "18" = "Caqueta",
  "19" = "Cauca",
  "20" = "Cesar",
  "23" = "Cordoba",
  "25" = "Cundinamarca",
  "27" = "Choco",
  "41" = "Huila",
  "44" = "La Guajira",
  "47" = "Magdalena",
  "50" = "Meta",
  "52" = "Narimo",
  "54" = "Norte de Santander",
  "63" = "Quindio",
  "66" = "Risaralda",
  "68" = "Santander",
  "70" = "Sucre",
  "73" = "Tolima",
  "76" = "Valle",
  "81" = "Arauca",
  "85" = "Casanare",
  "86" = "Putumayo",
  "88" = "San Andres y Providencia",
  "91" = "Amazonas",
  "94" = "Guainia",
  "95" = "Guaviare",
  "97" = "Vaupes",
  "99" = "Vichada"
)

region_code <- data[[region_var]]

coir61_derived <- data %>%
  mutate(
    strat = V022,
    psu_id = V021,
    weight = V005 / 1e6,
    interviewed = if_else(V015 == 1, 1L, if_else(V015 %in% 2:7, 0L, NA_integer_)),
    pregnant = if_else(
      V213 == 1,
      1L,
      if_else(V213 == 0, 0L, NA_integer_)
    ),
    # V528 has high missing in Colombia (COIR61 ~17%); use only definitive 95/31, else fall back to V527
    sexually_active = if_else(
      V525 == 0,
      0L,
      if_else(
        V528 == 95,
        1L,
        if_else(
          V528 == 31,
          0L,
          if_else(
            !is.na(V527),
            if_else(
              V527 >= 100 & V527 <= 199,
              1L,
              if_else(
                V527 >= 200 & V527 <= 299,
                1L,
                if_else(
                  V527 == 995,
                  1L,
                  if_else(
                    V527 >= 300 & V527 <= 399,
                    0L,
                    if_else(V527 >= 400 & V527 <= 499, 0L, NA_integer_)
                  )
                )
              )
            ),
            NA_integer_
          )
        )
      )
    ),
    married_union = if_else(
      V501 %in% c(1, 2),
      1L,
      if_else(V501 %in% c(0, 3, 4, 5), 0L, NA_integer_)
    ),
    urban = if_else(
      V025 == 1,
      1L,
      if_else(V025 == 2, 0L, NA_integer_)
    ),
    region = unname(region_map[as.character(region_code)]),
    region_code = as.integer(region_code),
    w_quintile = if_else(V190 %in% 1:5, as.integer(V190), NA_integer_),
    windex_c3 = if_else(
      V190 %in% c(1, 2),
      1L,
      if_else(V190 == 3, 2L, if_else(V190 %in% c(4, 5), 3L, NA_integer_))
    ),
    education_c4 = if_else(
      V106 %in% c(0, 1, 2, 3),
      as.integer(V106),
      NA_integer_
    ),
    agegroup_c4 = if_else(
      V012 < 20,
      1L,
      if_else(
        V012 >= 20 & V012 <= 24,
        2L,
        if_else(V012 >= 25 & V012 <= 34, 3L, if_else(V012 >= 35, 4L, NA_integer_))
      )
    ),
    Adolescent = as.integer(agegroup_c4 == 1L),
    Young_Adult = as.integer(agegroup_c4 %in% c(2L, 3L)),
    employed = if_else(
      V714 == 1,
      1L,
      if_else(V714 == 0, 0L, NA_integer_)
    ),
    parity = if_else(
      !is.na(V201),
      as.integer(V201),
      NA_integer_
    ),
    using_modern_contraceptive = if_else(
      V313 == 3,
      1L,
      if_else(V313 %in% c(0, 1, 2), 0L, NA_integer_)
    ),
    unfecund = if_else(
      V215 %in% c(993, 994, 996) | V376 == 23 | coalesce(V3A08D == 1, FALSE) | V602 == 6,
      1L,
      if_else(V215 %in% c(100:499, 995), 0L, NA_integer_)
    ),
    unmet_need_c3 = if_else(
      coalesce(unfecund, 0L) == 1L,
      NA_integer_,
      if_else(
        pregnant == 1L,
        if_else(V225 == 1, 0L, if_else(V225 == 3, 2L, 1L)),
        if_else(
          pregnant == 0L & (sexually_active == 1L | married_union == 1L),
          if_else(
            using_modern_contraceptive == 1L,
            0L,
            if_else(
              V602 == 3,
              2L,
              if_else(
                V602 == 2,
                1L,
                if_else(
                  V602 == 1,
                  if_else(
                    V603 == 994 |
                      (V603 >= 100 & V603 <= 199 & (V603 - 100) < 24) |
                      (V603 >= 200 & V603 <= 299 & (V603 - 200) < 2),
                    0L,
                    if_else(
                      (V603 >= 100 & V603 <= 199 & (V603 - 100) >= 24) |
                        (V603 >= 200 & V603 <= 299 & (V603 - 200) >= 2),
                      1L,
                      if_else(V603 %in% c(199, 299, 993, 996, 997, 998), 1L, NA_integer_)
                    )
                  ),
                  NA_integer_
                )
              )
            )
          ),
          NA_integer_
        )
      )
    ),
    unmet_need = if_else(
      is.na(unmet_need_c3),
      NA_integer_,
      if_else(unmet_need_c3 %in% c(1, 2), 1L, 0L)
    )
  ) %>%
  filter(interviewed == 1L)

dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(coir61_derived, output_path)
cat("Saved derived dataset to:", output_path, "\n")
