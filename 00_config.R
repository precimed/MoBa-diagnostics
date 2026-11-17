# =============================================================================
# Shared settings for parsing the NPR/KPR delivery of 2026-10-19 (first delivered 2026-10-07)
# Based on Pravesh Parekh's scripts (github.com/precimed/MoBa-diagnostics).
# Every script in NPR/ and KPR/ sources this file first.
# =============================================================================

# Folder with the scripts (scripts are started from here; some call setwd() later)
options(scipen = 999)   # no scientific notation: PREG_ID_2445 = 100000 became '1e+05' in IDs and csv files
dirScripts <- normalizePath(getwd())
if (!file.exists(file.path(dirScripts, "common.R"))) stop("Start the scripts from the folder that holds 00_config.R and common.R.")

# Raw data (the delivery)
dirRawNPR  <- "/tsd/p697/data/durable/phenotypes/npr/2026-10-19/rawdata"
dirRawKPR  <- "/tsd/p697/data/durable/phenotypes/kpr/2026-10-19/rawdata"

# Where the results are written (change if the group has a fixed location)
dirBase    <- file.path("/ess/p697/cluster/users", Sys.getenv("USER"), "2026-10_NPR_KPR_parsing")
dirOutNPR  <- file.path(dirBase, "NPR")
dirOutKPR  <- file.path(dirBase, "KPR")
for (d in c(dirOutNPR, dirOutKPR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# SV_INFO and linkage files
dirSVINFO  <- "/tsd/p697/data/durable/phenotypes/sv_infofiles"
linkDate   <- "20260605"   # Barn_/Far_/Mor_ID_2445_<linkDate>.sav (from the README of the delivery)

# Which SV_INFO file is current. Set to NA to pick the newest one
# (highest date in the file name) when there are several in the folder.
svInfoChoice <- "V12_20260618"   # as per Ida's e-mail

# Find the folder holding the linkage files with the right date (the README says
# sv_infofiles/, the previous delivery used sv_infofiles/koblingsbro/)
dirLink <- local({
  candidates <- c(dirSVINFO, file.path(dirSVINFO, "koblingsbro"))
  hits <- candidates[vapply(candidates, function(d)
    length(list.files(d, pattern = paste0("^Barn_ID_2445_", linkDate, "\\.sav$"))) > 0, logical(1))]
  if (!length(hits)) stop("Could not find Barn_ID_2445_", linkDate, ".sav in ", paste(candidates, collapse = " or "))
  hits[1]
})

# Genetics (only used in doCounts for counts in the genotyped sample)
dirGenetics <- "/tsd/p697/data/durable/genotype/MoBaPsychGen_v1"

# Wide 0/1 files (parquet): maximum number of cells built in memory at a time.
# 2e9 cells = about 8 GB per block. Lower it if R runs out of memory.
maxCells         <- 2e9
# The full NPR table for all codes (~2,800 codes x 19 years x 2 + lifetime) is very
# wide. The lifetime and per-year files are always made; the full table only if TRUE.
makeFullMinorNPR <- FALSE

# ---- SV_INFO file -------------------------------------------------------------
svInfoFile <- local({
  all <- list.files(dirSVINFO, pattern = "^PDB2445_SV_INFO.*\\.sav$")
  if (is.na(svInfoChoice)) {
    date   <- suppressWarnings(as.numeric(sub(".*_([0-9]{8}).*", "\\1", all)))
    chosen <- all[which.max(date)]
  } else {
    chosen <- all[grepl(paste0("^PDB2445_SV_INFO_", svInfoChoice, ".*\\.sav$"), all)]
  }
  if (length(chosen) != 1) stop("Could not find exactly one current SV_INFO file. SV_INFO files in the folder: ",
                                paste(all, collapse = ", "))
  chosen
})
svTag <- sub("^PDB2445_SV_INFO_(.*)\\.sav$", "\\1", svInfoFile)

# ---- Tag in the file names: <date the first step was run>_SV-<SV_INFO version> ----
# run_all.R sets RUN_DATE when step 1 runs and passes it on to the other steps,
# so all files from the same run get the same date.
runDate <- Sys.getenv("RUN_DATE")
if (runDate == "") {
  runDate <- as.character(Sys.Date())
  message("RUN_DATE is not set (the script was not started via run_all.R) - using today's date ", runDate)
}
runTag <- paste0(runDate, "_SV-", svTag)

cat("Config loaded: runTag =", runTag, "| SV_INFO =", svInfoFile, "| linkage files in", dirLink, "\n")
