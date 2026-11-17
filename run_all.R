# =============================================================================
# Runs the full parsing of the NPR/KPR delivery of 2026-10-19 (first delivered 2026-10-07), step by step.
# Each step runs in its own R process (frees memory between steps) and gets its
# own log in the folder logs/. Stops at the first error.
#
# Usage (from the folder this file is in):
#   Rscript run_all.R            # NPR and KPR
#   Rscript run_all.R NPR        # NPR only
#   Rscript run_all.R KPR 3      # KPR only, starting from step 3 (linkKPR.R)
# From RStudio: setwd() to this folder, then system("Rscript run_all.R KPR").
# =============================================================================

args      <- commandArgs(trailingOnly = TRUE)
register  <- if (length(args) >= 1) args[1] else "both"   # "NPR", "KPR" or "both"
startStep <- if (length(args) >= 2) as.integer(args[2]) else 1L

steps <- list(
  NPR = c("makeParticipantList.R", "mergeNPR.R", "linkNPR.R", "parseNPR.R",
          "doCounts.R", "compileDiagInfo.R"),
  KPR = c("makeParticipantList.R", "mergeKPR.R", "linkKPR.R", "parseKPR.R",
          "doCounts_ICD10.R", "doCounts_ICPC.R"))

if (!file.exists("00_config.R")) stop("Run from the folder that holds 00_config.R (use setwd()).")
regs <- if (register == "both") c("NPR", "KPR") else register
dir.create("logs", showWarnings = FALSE)
rscript <- file.path(R.home("bin"), "Rscript")

for (reg in regs) {
  # Date in the file names: set when step 1 runs and reused when a run is resumed
  # from a later step, so that all files from one run get the same date.
  dateFile <- file.path("logs", paste0("runDate_", reg, ".txt"))
  if (startStep == 1L) {
    runDate <- as.character(Sys.Date())
    writeLines(runDate, dateFile)
  } else {
    if (!file.exists(dateFile)) stop("Could not find ", dateFile, ". Start ", reg, " from step 1.")
    runDate <- readLines(dateFile)[1]
  }
  Sys.setenv(RUN_DATE = runDate)
  cat(reg, ": date in file names =", runDate, "\n")

  for (i in seq_along(steps[[reg]])) {
    if (i < startStep) next
    s       <- steps[[reg]][i]
    logFile <- file.path("logs", paste0(reg, "_", i, "_", sub("\\.R$", "", s), ".log"))
    cat(format(Sys.time(), "%H:%M:%S"), " ", reg, " step ", i, ": ", s, " ... ", sep = "")
    t0     <- Sys.time()
    status <- system2(rscript, file.path(reg, s), stdout = logFile, stderr = logFile)
    if (status != 0) {
      cat("ERROR\n")
      cat(tail(readLines(logFile), 25), sep = "\n")
      stop("Step ", i, " (", reg, "/", s, ") failed - see ", logFile,
           "\nOnce the error is fixed: Rscript run_all.R ", reg, " ", i)
    }
    cat("ok (", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min)\n", sep = "")
  }
}
cat("\nDone. Results in:\n")
source("00_config.R")
cat(" ", dirOutNPR, "\n ", dirOutKPR, "\n")
