# Honduras shapefiles

Place **`Shp_mgd.shp`** (+ sidecar `.dbf`, `.shx`, `.prj`, …) here. The spatial smoothing script (`scripts/03_analysis/honduras_spatial_smooth_stan.R`) expects:

- Attribute **`CNTRYNAMEE`** including `"Honduras"`
- Attribute **`DHSREGEN`** region names (aligned with DHS/MICS departments; see `smooth_OR.R` for name fixes)

**2011–12 (18 polygons):** San Pedro Sula merged into CORTES, Distrito Central into FRANCISCO MORAZAN (done in R).

**MICS 2019 microdata:** HH7 codes 19/20 (SAN PEDRO SULA, DISTRITO CENTRAL) are **recoded to CORTES / FRANCISCO MORAZAN** in `derive_hnir72.R` so Honduras matches **18 DHS departments** for maps and year-on-year comparison. Shapefile is still dissolved the same way in spatial scripts.
