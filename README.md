# MoBa-diagnostics
Code for re-formatting and cleaning up diagnostic data received from different sources for MoBa data

## Structure
- `NPR/`, `KPR/`, `KUHR/` – parsing scripts per register; the run order is in `runOrder.txt`.
- `00_config.R`, `common.R`, `run_all.R`, `test_common.R` – shared settings and functions used by
  the NPR and KPR scripts (from the 2026-10 delivery). Run from the repository root, e.g.
  `Rscript run_all.R NPR`.
- `control/` – control and QC scripts for the NPR/KPR delivery of 2026-10-19.

See `README_2026-10_delivery.md` for the changes made for the 2026-10 delivery and the results
of the quality control.
