# Figure → code traceability (provisional)

**Legend:** Candidate = analytical overlap only; Unverified = no demonstrated exact reproduction. No script-to-figure assignment is confirmed yet.

| Supplied figure | Likely original code to inspect | Status | Required evidence |
|---|---|---|---|
| Figure 1 | `paper1_figure_workflow_original.R` (well-distribution mapping section) | Candidate | Compare shape, geographic bounds, sample size, colors and legend |
| Figure 2 | `paper1_figure_workflow_original.R` (correlation-related sections, if present) | Unverified | Locate exact variable selection, correlation method, and plotted matrix |
| Figure 3a–c | `local_moran_original.R`, `local_moran_summary_original.R`, plus mapping sections of the figure script | Candidate | Confirm LISA grouping, p-value, neighbors, and geospatial layer |
| Figure 4a–b | `global_moran_original.R`, `spatial_contribution_original.R` | Candidate | Confirm which figure panels show Moran results, correlation length and contribution definitions |
| Figure 5 | `paper1_figure_workflow_original.R` plus land-use analysis sections | Unverified | Find exact plotting/statistical sections and source data |
| Supplementary Fig. S1 | `paper1_figure_workflow_original.R` | Unverified | Confirm map layers and plotting method |
| Supplementary Fig. S2 | `local_moran_original.R` and main figure script | Candidate | Confirm pH/F panels, spatial method and classification |

The original full-resolution figure files are in `figures/reference/`.
