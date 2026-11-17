# NPR/KPR delivery 2026-10 – changes and control scripts

Scripts used on TSD (p697) by Christine to control and parse the NPR/KPR delivery
(H-1037-E-E1). FHI first delivered the data on 2026-10-07; the 'x' codes were missing,
so FHI redelivered on 2026-10-19 (NPR now with data until 31 Aug 2026; the 2026-10-07
files were removed). The final run (21.10.2026) used the 2026-10-19 delivery. The parsing scripts are
Pravesh Parekh's scripts from github.com/precimed/MoBa-diagnostics, adapted to
this delivery. The parsing logic itself is unchanged.

On TSD the same files are in `/ess/p697/cluster/users/p697-chrd/parsing_2026-10-07/`
(logs in `logs/`) and the results in `/ess/p697/cluster/users/p697-chrd/2026-10_NPR_KPR_parsing/`.

## control/ (new folder)
- `control_NPR_KPR.R` – checks the raw files against the cover letters
  (lines, persons, years, 'nokkel' links) and the requested ICD-10/ICPC-2 lists
  (requested vs received, compared with the 2025-05-15 delivery, codes received
  that were not requested). Writes aggregated TSV tables only.
- `check_x_codes.R` – line counts in each NPR code file for codes written with an
  'x' in the application (F89x, R56x …) next to codes written in full (F84, F90).
  Result for the 2026-10-07 delivery: 0 lines for all 'x' codes in all files (fixed in 2026-10-19).
- `check_linkage.R` – checks that the linkage files link serial numbers to the right MoBa
  individuals (sex-specific KPR codes vs. MFR sex; lasting NPR diagnoses found again from the
  previous release). **Run it before parsing a new delivery or with new linkage files.**
- `qc_parsed_2026-11-04.R` – full QC of the parsed files (linkage, sex-specific codes, roles,
  persistence from the 2025 release, consent, parsed vs. raw, internal consistency, plausibility).
  Result 04.11.2026: 48 checks OK, 0 deviations.
- `qc2_parsed_2026-11-04.R` – independent rebuild of all long files from raw data + linkage; every
  1-cell of the wide lifetime/2023 files; every cell of all counts files; count table; ID, MFR and
  plausibility checks.
- `qc3_contact_files_2026-11-04.R` – contact-level files (ParsedNPR.csv, ParsedKPR-All.csv):
  key uniqueness, row counts per source/year, every variable row by row as text vs raw data,
  all yearly wide files cell by cell, SmallestDiffDate, all counts files, participant lists.
- `compare_with_2025_person_level.R` – person-level comparison with the parsed 2025 release
  (persons in both, years up to 2024): diagnoses per year/source, contacts, dates and every
  contact variable. Writes `sammenligning_2025_personniva.csv` (aggregated only).
- `qc4_final_checks_2026-11-06.R` – (A) every raw file read completely (lines vs rows read, no
  warnings); (B) contacts in 2026 as a share of 2025 (data until 31 Aug gives ~0.67);
  (C) birth test: children born 2008–09 and their mothers should have a somatic contact in the
  birth year far more often than four years later (equal shares if the linkage were random).
- `compare_with_2025_counts.R` – compares the new delivery with the counts from the previous
  release (NPR majorCodes, KPR ICPC/ICD codes) for every year and role, cumulative (`<year>`) and
  cross-sectional (`x<year>`), plus yearly totals 2020–2026. Writes `QC_*.csv` (aggregated only).

## Parsing scripts (repository root, `NPR/`, `KPR/`)
- `00_config.R` – all paths and settings; every script sources it first.
- `common.R` – shared functions for the 0/1 coded (wide) files and the counts.
- `run_all.R` – runs the steps in order, one R process and one log per step. Run it from the
  repository root: `Rscript run_all.R KPR`, `Rscript run_all.R NPR 4` (= NPR from step 4).
  The order is also listed in `NPR/runOrder.txt` and `KPR/runOrder.txt`.
- `test_common.R` – quick test of `common.R` on made-up data; run it before the real run.
- `NPR/`, `KPR/` – the adapted scripts. `KUHR/` and `NPR/getDiagInfo.R` are unchanged.
- `changes_vs_original.diff` (repository root) – unified diff of every changed or new script
  against the repository before these changes.

Requires the R packages `data.table`, `haven` and either `arrow` or `nanoparquet`.

### Changes compared with the original scripts
1. Paths and settings moved to `00_config.R`.
2. File names: `<date the first step was run>_SV-<SV_INFO version>`, e.g.
   `2026-10-18_SV-V12_20260618-MoBa-ParsedNPR.csv`. `run_all.R` reads `Sys.Date()` when step 1
   runs and passes it on to the other steps (also when a run is resumed from a later step).
3. SV_INFO: several `PDB2445_SV_INFO*.sav` now sit in `sv_infofiles/`. `svInfoChoice` in the config
   pins the current one (`V12_20260618`, per Ida); set it to `NA` to pick the newest. The others are
   treated as older versions together with those in `Old/` (read with full paths).
4. Linkage files `Barn_/Mor_/Far_ID_2445_<linkDate>.sav` (now `20260605`): the serial-number
   column is now `PID_2445` (was `PID_1037_<date>`); detected automatically, compared as character.
   Older SV_INFO files are read from `sv_infofiles/old/` (lower case; was `Old/`).
5. `mergeKPR.R`: bills without a diagnosis only have `lopenr` in the billing file. The original
   kept `lopenr.x` and dropped `lopenr.y`, so 16.4 million rows had no serial number and the
   row-count check in `linkKPR.R` failed. `lopenr` is now filled from the billing file.
6. `parseKPR.R`: rows without a diagnosis code are kept in `ParsedKPR-All` but excluded from the
   diagnosis tables.
7. `mergeNPR.R`: column names lower-cased; AVT now contains both somatic and mental-health
   specialists, so `sektor` is kept as `AVT_Sector`; missing columns give a warning.
8. 0/1 coded (wide) files, following Pravesh's suggestion:
   - saved as **parquet** instead of csv, with the same column names as before
     (`<code>_lifetime`, `<code>_<year>` cumulative, `<code>_x<year>` cross-sectional);
   - built in blocks of persons (`maxCells` in the config, default 2e9 cells ≈ 8 GB), so they fit
     on a 62 GB TSD VM. A table that needs more than one block is written as a folder of
     `part-NNN.parquet` files; read it with `arrow::open_dataset(<folder>)`;
   - per code set: a `-lifetime` file, one file per year (cumulative + cross-sectional for that
     year, like the old `splitInfo` files) and the full table. For NPR, the full table for all
     ~2,800 codes is skipped by default (`makeFullMinorNPR = FALSE`); the major-code table (first
     three characters) is made in full;
   - a long diagnosis file (`-ParsedNPR_diagnoses.csv`, `-ParsedKPR_ICD10_diagnoses.csv`,
     `-ParsedKPR_ICPC_diagnoses.csv`): one row per person, code and year.
9. `doCounts*.R`: rewritten to count from the long diagnosis file instead of the wide files (which
   are now too large to read in whole). Same output file names and columns as before
   (code, `lifetime`, cumulative years, `x<year>`), for all/children/mothers/fathers and the
   genotyped subset, with the same check that all = children + mothers + fathers.

Reading example (only the columns you need):
```r
library(arrow); library(dplyr)
d <- open_dataset("<runTag>-MoBa-ParsedNPR_majorCodes") |>
  select(ID_2445, Role, PREG_ID_2445, ChildNumber, WithdrawnConsent18years,
         starts_with("F") & ends_with("_lifetime")) |>
  collect()
```

### Not changed
- `compileDiagInfo.R` (first contact date per code) still uses the original loop, which may be slow
  with the many new codes.

### Status (04.11.2026)
- **21.10.2026 (withdrawn):** first run used the linkage files from March 2026 (`20260305`).
  The registry serial numbers belong to the corrected sample sent to the registries in June 2026,
  so diagnoses were assigned to the wrong individuals (found by Piotr; `check_linkage.R`:
  female-specific KPR codes 50–54 % boys, 4.5 % of F84 children found again). Counts per code and
  role were roughly right; anything person-level or genotype-based was wrong.
- **04.11.2026 (current):** re-run with the June linkage (`20260605`), SV_INFO `V12_20260618`,
  older SV_INFO files from `sv_infofiles/old/` and `MoBaPsychGen_v1`. All 12 steps ok.
  `check_linkage.R`: X09/X89/W15 0–2 % boys; 99.2–100 % of E10/G40/F84 cases in the 2025 release
  found again (children, mothers, fathers). Results in
  `/tsd/p697/data/durable/phenotypes/{npr,kpr}/2026-10-19/parsed/`.
- **05.11.2026:** KPR re-run from step 2. Municipality/county numbers (kommunenr, praksiskommune,
  fylkenr, praksisfylkenr) were read as numbers, so leading zeros were lost (0301 -> 301; same in
  the original scripts). They are now read as text in mergeKPR.R, linkKPR.R and parseKPR.R. Only
  the KPR contact files changed; diagnosis, wide and count files are byte-identical.
- `options(scipen = 999)` in 00_config.R: PREG_ID_2445 = 100000 became "1e+05" in IDs (also in the
  original scripts; the child was missing from the 2025 release).
- Final QC 05.11.2026: QC1 52/52, QC2 69 checks (3 explained: couple-registered infertility codes,
  8 single implausible codes in the registry, one QC-script bug), QC3 85/85 OK.
- Person-level comparison with 2025 (06.11.2026): NPR diagnoses 99.0–100 % found again per year
  (the gap 2014–2022 is rehabilitation moved from REH to SOM); every 2025 KPR contact found with the
  same day number; contact variables 100 % equal except the pseudonymised institution ID (re-made
  each delivery) and municipality numbers, which differ only by the restored leading zeros.
  KPR has ~3.5x more contacts (bills without a diagnosis are now included).
- QC4 (06.11.2026): A 15/15 raw files read completely; B 2026/2025 = 0.63–0.66 for KPR, SOM, AVT
  (PHV 0.72, TSB 0.77, PHBU 0.42 – the cohort is moving into adult services); C children 46 % vs
  23 %, mothers 99.9 % vs 38 % – the mother and child linkage is correct.
- QC (`compare_with_2025_counts.R`): all requested codes delivered; 0.2–0.5 % lower than the 2025
  release in all years and roles; only deviation children 2023, F chapter only (+1.3 %, late
  registrations in mental health care, mainly PHBU; NPR advises caution for PHBU in Central Norway
  Nov 2022–2023).
