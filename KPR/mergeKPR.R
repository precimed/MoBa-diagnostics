# --- Adapted to the 2026-10-07 delivery: paths and settings come from 00_config.R ---
source("00_config.R")

# Merge KPR data
library(data.table)

# File locations ----------------------------------------------------------
# Where output files should be written
dirWorking <- dirOutKPR
setwd(dirWorking)

# Current location of KPR files
dirKPR <- dirRawKPR

# The takst file contains billing information - do we care? Likely not, ignoring
data_diagnoses <- fread(file.path(dirKPR, "diagnose.csv"), sep = ";")
# 2026-11: read municipality/county numbers as text so leading zeros are kept (0301 not 301)
data_billing   <- fread(file.path(dirKPR, "regning.csv"),  sep = ";",
                        colClasses = list(character = grep("^(kommunenr|praksiskommune|fylkenr|praksisfylkenr)$",
                          names(fread(file.path(dirKPR, "regning.csv"), sep = ";", nrows = 0)), ignore.case = TRUE, value = TRUE)))
# 2026-10: make column names lower case
setnames(data_diagnoses, tolower(names(data_diagnoses)))
setnames(data_billing,   tolower(names(data_billing)))

# Merge
data_KPR <- merge(data_diagnoses, data_billing, by = "nokkel", all = TRUE)

# Remove duplicate lopenr column and rename
# 2026-10: bills without a diagnosis (e.g. dentist, physiotherapy) only have lopenr in
# the billing file (lopenr.y). Previously lopenr.y was dropped, so these rows had no
# serial number and fell out at linkage. lopenr is now filled from the billing file.
if (all(c("lopenr.x", "lopenr.y") %in% colnames(data_KPR))) {
  nMissing <- sum(is.na(data_KPR$lopenr.x))
  data_KPR[, lopenr := fcoalesce(as.character(lopenr.x), as.character(lopenr.y))]
  data_KPR[, c("lopenr.x", "lopenr.y") := NULL]
  cat("lopenr filled in from the billing file for", nMissing, "rows (bills without a diagnosis)\n")
}
stopifnot(!anyNA(data_KPR$lopenr))


# Renaming column names ----------------------------------------------------
# This is based mostly on the folgebrev
colnames(data_KPR)[colnames(data_KPR) == "kommunenr"]         <- "MunicipalityNumber"
colnames(data_KPR)[colnames(data_KPR) == "praksiskommune"]    <- "TreatmentMunicipality"
colnames(data_KPR)[colnames(data_KPR) == "fylkenr"]           <- "CountyNumber"
colnames(data_KPR)[colnames(data_KPR) == "praksisfylkenr"]    <- "TreatmentCounty"
colnames(data_KPR)[colnames(data_KPR) == "diagnosetabell"]    <- "DiagnosisType"
colnames(data_KPR)[colnames(data_KPR) == "diagnosekode"]      <- "DiagnosisCode"
colnames(data_KPR)[colnames(data_KPR) == "erhoveddiagnose"]   <- "isMainDiagnosis"
colnames(data_KPR)[colnames(data_KPR) == "aar"]               <- "Year"
colnames(data_KPR)[colnames(data_KPR) == "diffdager_inndato"] <- "DiffDays_Admission"
colnames(data_KPR)[colnames(data_KPR) == "tjenestetype"]      <- "ServiceType"
colnames(data_KPR)[colnames(data_KPR) == "lopenr"]            <- "SerialNumber"
colnames(data_KPR)[colnames(data_KPR) == "nokkel"]            <- "Key"

# Save merged KPR --------------------------------------------------------
outputName <- paste(runTag, "-MoBa-MergedKPR.csv", sep = "")
fwrite(data_KPR, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)
