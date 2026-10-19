# Manuscript-to-code gap audit — Paper 1

**Source:** Jahan et al., *Water Research X* 29 (2025) 100409, DOI 10.1016/j.wroa.2025.100409.  
**Status:** preliminary static code comparison only. Source data and R execution environment were not supplied; no numerical reproduction has been demonstrated.

| Component | Paper reports | Uploaded R script evidence | Status / action |
|---|---|---|---|
| Global Moran's I | k=20, row-standardized weights, `moran.test`, 999 Monte Carlo replicates | `global moran.R`: k=20, `nb2listw(... style="minmax")`, `moran.test` | **Mismatch:** weighting; confirm source of reported Monte Carlo test |
| Local Moran/LISA | k=5, row-standardized weights, 999 permutations, p<0.05 and quadrant assignment | `local moran.R`, `local moran summary.R`: k=5, `style="minmax"`, `localmoran` | **Mismatch:** weights and permutation method; verify classification logic and comparison to published hotspots |
| Nitrate maps | Ordinary kriging, empirical variogram; spherical or exponential model selected by kriging CV RMSE, 500-m grid | `(Spatial) Figure for paper 1.R` includes `gstat::idw` for NO3-N and other parameters | **Mismatch:** IDW is not ordinary kriging; identify final map-generation code or reconstruct and validate |
| Correlation length | `variogram`, `fit.variogram`, 95% of fitted sill, kilometers | No matching dedicated spatial correlation-length script among the archived Paper 1 selections yet; `correlation length (1).R` was about resistance patterns | **Missing/uncertain:** locate source or reconstruct, check published Fig. 4a |
| Spatial contribution | Z-scores, lag with row-standardized weights, neighbor = `cor(x, Wx)`, local = 1 - neighbor (as described) | `contribution (1).R` uses `cor(df[[param]], lag.listw(...))`, but `style="minmax"` | **Partial match:** verify settings, full parameter coverage and numerical outputs. Note: a correlation is not mathematically identical to explained variance without additional assumptions |
| Land-use | Seven 1-km categories, Spearman correlations | The combined paper-figure script contains numerous exploratory analyses; final figure-generating sections not yet conclusively identified | **Not verified:** trace Figure 2 and Figure 5 to final sections and QGIS exports |
| Figure and Table 2 | Moran summary for eight contaminants; paper's Fe row lists I=0.004, z=2, p<0.05 | Reproduction not run | **Requires numerical check:** assess all output cells with raw data, including significance conventions |
| Data and GIS | 2349 wells and GIS data; original data available on request | Original R code refers to local CSVs and shapefiles not included | **Unavailable:** cannot claim full reproducibility without authorized files |

## Critical precautions

- **Keep `scripts/original/` unchanged.** Never retroactively overwrite archival source code to imply an unverified script generated a published figure.
- The difference **k=20 for Global Moran and k=5 for Local Moran is consistent with the paper**. The discrepancy is the **weight normalization and related inference settings**, not the neighbor counts.
- The manuscript states a row-standardized spatial lag and treats `cor(x, Wx)` as a variance contribution. This is a definition used by the publication, not automatically a general causal/source-apportionment result. Document it accurately, and avoid extending it beyond the paper's scope.
- Test figure-level agreement before labeling code `reproducible`.

## Needed next

1. Source dataset for 2017–2022 wells (with permission to use locally), GIS shapefiles and 1-km land-use buffer exports.
2. Any additional scripts for kriging, semivariograms, correlation length, land-use heatmaps, and Figure 5.
3. Final supplementary methods/figures if relevant.
4. R version and package snapshot, if available.
