# Colombia boundaries (project-only)

Scripts use **only** shapefiles under this folder — **no GADM, geodata, or downloads.**

- Put one or more **`.shp`** layers here (with `.dbf`, `.shx`, …).  
- **Department-level** polygons (≈32 + San Andrés) are required for BYM2 adjacency.  
- A **name** field is needed: e.g. `NAME_1`, `ADM1_NAME`, `REGNAME`, `DHSREGEN`, or any text column that matches DHS department labels (see `derive_coir*` `region` names).  
- **San Andres y Providencia** is treated as non-mainland in the Stan model.

Fit script scans **every** `*.shp` here and picks the file that matches the most departments to `regions_all`.
