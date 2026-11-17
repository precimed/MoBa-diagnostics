# --- Adapted to the 2026-10-07 delivery: paths and settings come from 00_config.R ---
source("00_config.R")

# Parse KPR
library(haven)
library(data.table)

# File locations ----------------------------------------------------------
# Where output files should be written
dirWorking <- dirOutKPR
setwd(dirWorking)

# Latest SV_INFO file
dirSVINFO   <- "/tsd/p697/data/durable/phenotypes/sv_infofiles"
fileCurrent <- svInfoFile
if (length(fileCurrent) > 1)
{
  stop('More than one current version of INFO file found; script is not designed for this')
}
SVINFOFile  <- fileCurrent[1]

# Merged and linked KPR
dirKPR      <- dirOutKPR
KPRFile     <- paste(runTag, "-MoBa-LinkedKPR-AllSubjects.csv", sep = "")

# Withdrawn consent
dirConsent  <- dirOutKPR
consentFile <- paste(runTag, "-MoBa-WithdrawnList.csv", sep = "")

# Invalid NFR
fileInvalid <- list.files(path = dirSVINFO, pattern = glob2rx("*invalid*.csv"))

# Read files --------------------------------------------------------------
# dataKPR     <- read.csv(file.path(dirKPR,      KPRFile),     sep = "\t")
dataKPR     <- fread(file = file.path(dirKPR,   KPRFile),   sep = "\t", colClasses = list(character = c("MunicipalityNumber", "TreatmentMunicipality", "CountyNumber", "TreatmentCounty")))   # 2026-11: keep leading zeros in municipality/county numbers
withdrawn   <- read.csv(file.path(dirConsent,  consentFile), sep = "\t")
dataInfo    <- read_sav(file.path(dirSVINFO,   SVINFOFile))
dataInvalid <- read.csv(file.path(dirSVINFO,  fileInvalid), sep = "\t") 

# ID2445 for children -----------------------------------------------------
dataKPR$ID_2445   <- paste(dataKPR$PREG_ID_2445,  dataKPR$ChildNumber, sep = "_")

# Replace all NA_NA with blanks
dataKPR$ID_2445[dataKPR$ID_2445 == "NA_NA"] <- ""


# Eliminate withdrawn consent ---------------------------------------------
# Make a list of parents which are present in INFO file and withdrawn list:
# these are borderline cases and for the moment we are keeping them
borderline_M_ID <- withdrawn$M_ID_2445[withdrawn$M_ID_2445 %in% dataInfo$M_ID_2445 & withdrawn$M_ID_2445 != ""]
borderline_F_ID <- withdrawn$F_ID_2445[withdrawn$F_ID_2445 %in% dataInfo$F_ID_2445 & withdrawn$F_ID_2445 != ""]

# Any pregnancy ID which is present in the withdrawn list should be removed from KPR
locEliminate <- (dataKPR$PREG_ID_2445 %in% withdrawn$PREG_ID_2445) & 
                 dataKPR$PREG_ID_2445 != "" & !is.na(dataKPR$PREG_ID_2445)
dataKPR      <- dataKPR[!locEliminate, ]

# Any F_ID or M_ID which is present in the withdrawn list should be removed from
# KPR, except for the boderline cases
locEliminate <- dataKPR$F_ID_2445 %in% withdrawn$F_ID_2445 & dataKPR$F_ID_2445 != "" & !(dataKPR$F_ID_2445 %in% borderline_F_ID)
dataKPR      <- dataKPR[!locEliminate, ]

locEliminate <- dataKPR$M_ID_2445 %in% withdrawn$M_ID_2445 & dataKPR$M_ID_2445 != "" & !(dataKPR$M_ID_2445 %in% borderline_M_ID)
dataKPR      <- dataKPR[!locEliminate, ]


# Invalid FNR should be removed
# Only parents because we know that invalid FNR only contains fathers at the moment
# Might need updating in the future? Hopefully not
locEliminate <- dataKPR$F_ID_2445 %in% dataInvalid$ID_2445 |
                dataKPR$M_ID_2445 %in% dataInvalid$ID_2445
dataKPR      <- dataKPR[!locEliminate, ]


# All codes seem to be of 3 character length
# Remove any periods (.)
dataKPR$DiagnosisCode  <- sub("\\.", "", dataKPR$DiagnosisCode)

# Convert all codes to upper case
dataKPR$DiagnosisCode <- toupper(dataKPR$DiagnosisCode)


# Split by diagnostic manual ----------------------------------------------
# Separate out ICPC codes
loc_ICPC     <- dataKPR$DiagnosisType == "ICPC-2" | dataKPR$DiagnosisType == "ICPC-2B"
dataKPR_ICPC <- dataKPR[loc_ICPC,]

# Leftover is ICD-10
dataKPR_ICD <- dataKPR[!loc_ICPC,]

stopifnot(nrow(dataKPR_ICPC) + nrow(dataKPR_ICD) - nrow(dataKPR) == 0)

# 2026-10: bills without a diagnosis (now kept after the fix in mergeKPR.R) have an
# empty DiagnosisCode. They stay in "ParsedKPR-All" but not in the diagnosis tables.
emptyCode      <- function(x) is.na(x) | x == ""
cat("Rows without a diagnosis code kept out of the diagnosis tables:",
    sum(emptyCode(dataKPR_ICD$DiagnosisCode)) + sum(emptyCode(dataKPR_ICPC$DiagnosisCode)), "\n")
dataKPR_ICD  <- dataKPR_ICD[!emptyCode(dataKPR_ICD$DiagnosisCode), ]
dataKPR_ICPC <- dataKPR_ICPC[!emptyCode(dataKPR_ICPC$DiagnosisCode), ]


## Save versions of data at this point - before counting
outputName <- paste(runTag, "-MoBa-ParsedKPR-All.csv", sep = "")
fwrite(dataKPR, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)

outputName <- paste(runTag, "-MoBa-ParsedKPR-Manual-ICPC.csv", sep = "")
fwrite(dataKPR_ICPC, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)

outputName <- paste(runTag, "-MoBa-ParsedKPR-Manual-ICD10.csv", sep = "")
fwrite(dataKPR_ICD, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)


# Variables to ignore -----------------------------------------------------
varsToIgnore <- c("SerialNumber",              "Key", 
                  "DiagnosisType",             "isMainDiagnosis",
                  "DiffDays_Admission",        "MunicipalityNumber",
                  "TreatmentMunicipality",     "ServiceType",
                  "CountyNumber",              "TreatmentCounty")

locsIgnore <- colnames(dataKPR) %in% varsToIgnore
locsIgnore <- !locsIgnore
dataKPR    <- dataKPR[, ..locsIgnore] # data.table nonsense

locsIgnore   <- colnames(dataKPR_ICPC) %in% varsToIgnore
locsIgnore   <- !locsIgnore
dataKPR_ICPC <- dataKPR_ICPC[, ..locsIgnore] # data.table nonsense

locsIgnore   <- colnames(dataKPR_ICD) %in% varsToIgnore
locsIgnore   <- !locsIgnore
dataKPR_ICD  <- dataKPR_ICD[, ..locsIgnore] # data.table nonsense


# Get rid of KPR full table
rm(dataKPR)

# Flatten table -----------------------------------------------------------
dupRows      <- duplicated(dataKPR_ICPC)
dataKPR_ICPC <- dataKPR_ICPC[!dupRows,]

dupRows      <- duplicated(dataKPR_ICD)
dataKPR_ICD  <- dataKPR_ICD[!dupRows,]


# Make one overall ID column ----------------------------------------------
dataKPR_ICPC$ID  <- paste(dataKPR_ICPC$ID_2445,  dataKPR_ICPC$M_ID_2445,  dataKPR_ICPC$F_ID_2445,  sep = "")
dataKPR_ICD$ID   <- paste(dataKPR_ICD$ID_2445,   dataKPR_ICD$M_ID_2445,   dataKPR_ICD$F_ID_2445,   sep = "")


# Add Role column ---------------------------------------------------------
locChildren               <- dataKPR_ICPC$ID_2445    != ""
locMothers                <- dataKPR_ICPC$M_ID_2445  != ""
locFathers                <- dataKPR_ICPC$F_ID_2445  != ""
dataKPR_ICPC$Role[locChildren] <- "Child"
dataKPR_ICPC$Role[locMothers]  <- "Mother"
dataKPR_ICPC$Role[locFathers]  <- "Father"

# Add Role column ---------------------------------------------------------
locChildren               <- dataKPR_ICD$ID_2445    != ""
locMothers                <- dataKPR_ICD$M_ID_2445  != ""
locFathers                <- dataKPR_ICD$F_ID_2445  != ""
dataKPR_ICD$Role[locChildren] <- "Child"
dataKPR_ICD$Role[locMothers]  <- "Mother"
dataKPR_ICD$Role[locFathers]  <- "Father"


# Eliminate other ID columns ----------------------------------------------
toEliminate  <- !colnames(dataKPR_ICPC) %in% c("ID_2445", "M_ID_2445", "F_ID_2445")
dataKPR_ICPC <- dataKPR_ICPC[, ..toEliminate]

toEliminate <- !colnames(dataKPR_ICD) %in% c("ID_2445", "M_ID_2445", "F_ID_2445")
dataKPR_ICD <- dataKPR_ICD[, ..toEliminate]


# Rename ID to ID_2445 ----------------------------------------------------
colnames(dataKPR_ICPC)[colnames(dataKPR_ICPC) == "ID"] <- "ID_2445"
colnames(dataKPR_ICD)[colnames(dataKPR_ICD) == "ID"]   <- "ID_2445"


# Long diagnosis files + wide 0/1 files (2026-10) --------------------------
# As for NPR: the 0/1 files are saved as parquet (common.R) with the same column
# names as before. KPR codes are already three characters, so no major-code files.
source(file.path(dirScripts, "common.R"))
toDx <- function(d) d[, .(ID_2445 = as.character(ID_2445), Role, PREG_ID_2445, ChildNumber,
                           WithdrawnConsent18years, Year = as.integer(Year), Code = DiagnosisCode)]
dxICD  <- unique(toDx(dataKPR_ICD))
dxICPC <- unique(toDx(dataKPR_ICPC))
rm(dataKPR_ICD, dataKPR_ICPC); invisible(gc())

fwrite(dxICD,  file.path(dirWorking, paste0(runTag, "-MoBa-ParsedKPR_ICD10_diagnoses.csv")), sep = "\t")
fwrite(dxICPC, file.path(dirWorking, paste0(runTag, "-MoBa-ParsedKPR_ICPC_diagnoses.csv")),  sep = "\t")
cat("ICD-10:", uniqueN(dxICD$ID_2445),  "persons,", uniqueN(dxICD$Code),  "codes\n")
cat("ICPC-2:", uniqueN(dxICPC$ID_2445), "persons,", uniqueN(dxICPC$Code), "codes\n")

makeWideFiles(dxICD,  file.path(dirWorking, paste0(runTag, "-MoBa-ParsedKPR")),
              nameMinor = "ICD10Codes", maxCells = maxCells)
makeWideFiles(dxICPC, file.path(dirWorking, paste0(runTag, "-MoBa-ParsedKPR")),
              nameMinor = "ICPCCodes",  maxCells = maxCells)
