# Initial code audit — Paper 1

## Confirmed from uploaded source files

- `global moran.R` calls `moran.test()` and builds k-nearest-neighbor weights with **k = 20**, `style = "minmax"`. It reads `Groundwater(17-22).csv`.
- `local moran.R` calls `localmoran()` with **k = 5**, `style = "minmax"`, and reads `for_local_moran.csv`.
- `local moran summary.R` also calls `localmoran()` using k = 5 and reads `Groundwater(17-22).csv`.
- `contribution (1).R` includes code calculating `cor(x, spatial_lag)` and assigning `1 - correlation` as a 'local contribution'. This is a custom heuristic **not automatically a standard variance or causal decomposition**; its meaning must be checked against the paper's methods.
- `(Spatial) Figure for paper 1.R` is a long multi-section figure workflow reading `Groundwater(17-22).csv` and GIS boundary `LSMD_ADM_SECT_UMD_44.shp`. Several source lines show character-encoding corruption and likely session-dependent state.
- `chungcheongnam-do water contaminants ratio.R` uses additional Excel workbooks, with some code referring to a different geographic area. Treat as a review candidate only.
- `LOCAL BBCOUNT.R`, despite its name, contains geological type categorization and lithology area percentage preparation. Its final-paper use is uncertain.

## Before marking figure reproduction verified

1. Supply the final Paper 1 manuscript/captions or figure methods to establish exact parameter choices.
2. Supply original analysis data or a permitted sanitized equivalent, and GIS shapefile companions.
3. Determine which scripts and sections ran for each figure; mark original vs revised/reconstructed.
4. Validate CRS, distance metric and neighborhood choice. Longitude/latitude passed directly to neighbor algorithms warrants explicit geographic-distance verification.
5. Review zero-to-1e-6 replacements, missing-value filtering, Korean text encoding, and LISA category assignment.
6. Check p-values and multiple-comparison handling, where relevant.
7. Capture R and package versions; only then create installable reproducible scripts.

**Execution:** Rscript is not installed in the present packaging environment; no R analyses were run.
