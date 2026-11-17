# =============================================================================
# Check that the linkage files (Barn_/Mor_/Far_ID_2445_<date>.sav) link the
# registry serial numbers (lopenr) to the right MoBa individuals.
# Run BEFORE parsing a new delivery or with new linkage files.
#
# Test A (children, KPR): sex-specific ICPC codes vs. child sex from MFR.
#   Correct linkage: X09/X89/W15 (female) ~0 % boys; Y10 (male) clearly > 50 %.
#   Wrong linkage:   all ~50 % (= share of boys among all children).
# Test B (children, mothers, fathers, NPR): persons with lasting diagnoses
#   (E10, G40, F84) in the previous release should also have them now.
#   Correct linkage: > 0.95; wrong linkage: close to the prevalence (~0.01-0.05).
#
# 2026-11: the March linkage (20260305) failed both tests (50-56 % boys;
# 4.5 % of F84 children found again); the June linkage (20260605) passed
# (0-2 % boys for female codes; 99.2-100 % found again).
# Writes nothing; prints aggregated results only.
# =============================================================================
library(data.table); library(haven)

linkDate <- "20260605"
dirSV    <- "/tsd/p697/data/durable/phenotypes/sv_infofiles/"
fileMFR  <- "/tsd/p697/data/durable/phenotypes/mbrn/2025-12-16/P252634_PDB2445_uttrekk_MFR.sav"
rawNPR   <- "/tsd/p697/data/durable/phenotypes/npr/2026-10-19/rawdata/"
fileKPR  <- "/tsd/p697/data/durable/phenotypes/kpr/2026-10-19/rawdata/diagnose.csv"
oldNPR   <- "/tsd/p697/data/durable/phenotypes/npr/2025-05-15/npr_parsed-20250522_SVINFO-20250425/2025-05-22-MoBa-ParsedNPR_majorCodes.csv"

readLink <- function(role) {
  f <- list.files(dirSV, pattern = paste0("^", role, "_ID_.*", linkDate, "\\.sav$"), full.names = TRUE)
  if (length(f) != 1) stop("Expected one ", role, " linkage file for ", linkDate, ", found: ", length(f))
  d   <- as.data.table(read_sav(f))
  pid <- grep("^PID", names(d), value = TRUE)[1]
  d[, lopenr := as.character(get(pid))][]
}
barn <- readLink("Barn"); mor <- readLink("Mor"); far <- readLink("Far")
barn[, ID := paste0(PREG_ID_2445, "_", BARN_NR)]

# ---- Test A ------------------------------------------------------------------
mfr <- as.data.table(read_sav(fileMFR, col_select = c("PREG_ID_2445", "BARN_NR", "KJONN")))
mfr[, `:=`(ID = paste0(PREG_ID_2445, "_", BARN_NR), boy = KJONN == 1)]
kd  <- fread(fileKPR, select = c("lopenr", "diagnosekode"))[startsWith(lopenr, "Bm")]
cat("Coverage of child serial numbers in linkage file:",
    round(mean(unique(kd$lopenr) %in% barn$lopenr), 3), "\n")
cat("All children, share boys:", round(mfr[, mean(boy, na.rm = TRUE)], 2), "\n")
print(rbindlist(lapply(c("X09", "X89", "W15", "Y10"), function(code) {
  ids <- barn[lopenr %in% kd[startsWith(diagnosekode, code), lopenr], ID]
  data.table(code, n = length(ids), share_boys = mfr[ID %in% ids, round(mean(boy, na.rm = TRUE), 2)])
})))

# ---- Test B ------------------------------------------------------------------
k <- rbindlist(lapply(c("AVT", "PHBU", "PHV", "SOM", "TSB", "REH"), function(r)
  fread(list.files(rawNPR, paste0("^", r, "koder"), full.names = TRUE), select = c("lopenr", "kodeverdi"))))
link <- rbind(barn[, .(lopenr, ID)], mor[, .(lopenr, ID = M_ID_2445)], far[, .(lopenr, ID = F_ID_2445)])
old  <- fread(oldNPR, select = c("ID_2445", "Role", "E10_lifetime", "G40_lifetime", "F84_lifetime"))
print(rbindlist(lapply(c("E10", "G40", "F84"), function(code) {
  new <- link[lopenr %in% k[grepl(paste0("^", code), kodeverdi, useBytes = TRUE), lopenr], ID]
  old[get(paste0(code, "_lifetime")) == 1,
      .(code, n_previous = .N, share_found_again = round(mean(ID_2445 %in% new), 3)), by = Role]
})))
