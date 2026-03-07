library(haven)
library(dplyr)

input_path <- "data/HNIR62SD/hnir62fl.sas7bdat"
output_path <- "data/derived/hnir62fl_derived.rds"

data <- read_sas(input_path)

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
  "18" = "YORO"
)

if (!"V213" %in% names(data)) {
  stop("V213 not found in dataset.")
}

hnir62_derived <- data %>%
  mutate(
    strat = V022,
    psu_id = V021,
    weight = V005 / 1e6,
    pregnant = if_else(
      V213 == 1,
      1L,
      if_else(V213 == 0, 0L, NA_integer_)
    ),
    sexually_active = if_else(
      V525 == 0,
      0L,
      if_else(
        !is.na(V528),
        if_else(V528 == 95, 1L, if_else(V528 == 31, 0L, NA_integer_)),
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
    region = unname(region_map[as.character(V024)]),
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
      V012 < 19,
      1L,
      if_else(
        V012 >= 19 & V012 <= 24,
        2L,
        if_else(V012 >= 25 & V012 <= 34, 3L, if_else(V012 >= 35, 4L, NA_integer_))
      )
    ),
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
      V215 %in% c(994, 996) | V376 == 23 | V3A08D == 1 | V602 == 6,
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
                    V603 == 994 | (V603 >= 100 & V603 <= 199 & (V603 - 100) < 24) | (V603 >= 200 & V603 <= 299 & (V603 - 200) < 2),
                    0L,
                  if_else(
                    (V603 >= 100 & V603 <= 199 & (V603 - 100) >= 24) | (V603 >= 200 & V603 <= 299 & (V603 - 200) >= 2),
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
    )
  )

dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(hnir62_derived, output_path)
cat("Saved derived dataset to:", output_path, "\n")
