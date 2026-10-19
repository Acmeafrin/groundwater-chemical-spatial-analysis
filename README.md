# Groundwater Chemical Contamination: Spatial Analysis

**Status: archival source-code audit in progress — not yet a verified reproduction package.**

This repository is being assembled for **Paper 1**, focused on chemical groundwater quality, spatial distributions, Global and Local Moran's I, and associations between chemical contamination and land use. Paper 2, focused on total coliforms, fecal coliforms and *E. coli*, is maintained separately.

## What is here

- `scripts/original/`: unchanged copies of the supplied R scripts that likely relate to Paper 1. These scripts have **not** been verified to run end-to-end.
- `scripts/review_candidates/`: related scripts whose role in the final publication is not yet established.
- `figures/reference/`: supplied final-figure candidates used for matching code to outputs; final paper mapping still needs verification.
- `docs/source_manifest.csv`: source filename, copied filename, line count, and SHA-256 checksum for traceability.
- `docs/FIGURE_CODE_MATRIX.md`: tentative matches and evidence gaps.
- `docs/AUDIT_NOTES.md`: methodological and reproducibility issues to resolve.

## How to reproduce the results

**Not yet reproducible from this repository alone.** The supplied scripts require CSV/Excel data, GIS shapefile companions, R packages, and in some cases Windows-specific paths. The exact script version and parameters used for each final figure still need to be confirmed. No results have been regenerated or validated.

## Scientific scope

Paper 1 uses chemical-indicator mapping, Global Moran's I, Local Moran's I (LISA), spatial association, and land-use analysis. Scripts with microbial-indicator local join counts belong to **Paper 2** and are intentionally excluded.

## Before publication

Validate figure-by-figure reproduction, verify neighborhood definition and coordinate reference system, add the paper citation/DOI and license with author approval, and confirm rights to publish data, figures, and collaborators' code.
