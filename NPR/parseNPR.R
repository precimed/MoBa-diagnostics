# --- Adapted to the 2026-10-07 delivery: paths and settings come from 00_config.R ---
source("00_config.R")

# Parse NPR data to generate some usable file format
# Start timer
library(haven)
library(data.table)
startTime <- proc.time()

# File locations ----------------------------------------------------------
# Where output files should be written
dirOutput   <- dirOutNPR
setwd(dirOutput)

# Latest SV_INFO file
dirSVINFO   <- "/tsd/p697/data/durable/phenotypes/sv_infofiles"
fileCurrent <- svInfoFile
if (length(fileCurrent) > 1)
{
  stop('More than one current version of INFO file found; script is not designed for this')
}
SVINFOFile  <- fileCurrent[1]

# Merged and linked NPR
dirNPR      <- dirOutNPR
NPRFile     <- paste(runTag, "-MoBa-LinkedNPR-AllSubjects.csv", sep = "")

# Withdrawn consent
dirConsent  <- dirOutNPR
consentFile <- paste(runTag, "-MoBa-WithdrawnList.csv", sep = "")

# Invalid NFR
fileInvalid <- list.files(path = dirSVINFO, pattern = glob2rx("*invalid*.csv"))

# Read files --------------------------------------------------------------
# dataNPR     <- read.csv(file.path(dirNPR,      NPRFile),     sep = "\t")
dataNPR     <- fread(file = file.path(dirNPR,   NPRFile),   sep = "\t")
withdrawn   <- read.csv(file.path(dirConsent,  consentFile), sep = "\t")
dataInfo    <- read_sav(file.path(dirSVINFO,   SVINFOFile))
dataInvalid <- read.csv(file.path(dirSVINFO,  fileInvalid), sep = "\t") 

# ID2445 for children -----------------------------------------------------
dataNPR$ID_2445   <- paste(dataNPR$PREG_ID_2445,  dataNPR$ChildNumber, sep = "_")

# Replace all NA_NA with blanks
dataNPR$ID_2445[dataNPR$ID_2445 == "NA_NA"] <- ""


# Eliminate withdrawn consent ---------------------------------------------
# Make a list of parents which are present in INFO file and withdrawn list:
# these are borderline cases and for the moment we are keeping them
borderline_M_ID <- withdrawn$M_ID_2445[withdrawn$M_ID_2445 %in% dataInfo$M_ID_2445 & withdrawn$M_ID_2445 != ""]
borderline_F_ID <- withdrawn$F_ID_2445[withdrawn$F_ID_2445 %in% dataInfo$F_ID_2445 & withdrawn$F_ID_2445 != ""]

# Any pregnancy ID which is present in the withdrawn list should be removed from NPR
locEliminate <- (dataNPR$PREG_ID_2445 %in% withdrawn$PREG_ID_2445) & 
                 dataNPR$PREG_ID_2445 != "" & !is.na(dataNPR$PREG_ID_2445)
dataNPR      <- dataNPR[!locEliminate, ]

# Any F_ID or M_ID which is present in the withdrawn list should be removed from
# NPR, except for the boderline cases
locEliminate <- dataNPR$F_ID_2445 %in% withdrawn$F_ID_2445 & dataNPR$F_ID_2445 != "" & !(dataNPR$F_ID_2445 %in% borderline_F_ID)
dataNPR      <- dataNPR[!locEliminate, ]

locEliminate <- dataNPR$M_ID_2445 %in% withdrawn$M_ID_2445 & dataNPR$M_ID_2445 != "" & !(dataNPR$M_ID_2445 %in% borderline_M_ID)
dataNPR      <- dataNPR[!locEliminate, ]


# Invalid FNR should be removed - they don't seem to exist in latest NPR (2025-05)?
# Only parents because we know that invalid FNR only contains fathers at the moment
# Might need updating in the future? Hopefully not
locEliminate <- dataNPR$F_ID_2445 %in% dataInvalid$ID_2445 |
                dataNPR$M_ID_2445 %in% dataInvalid$ID_2445
dataNPR      <- dataNPR[!locEliminate, ]


# Clean up ICD10 codes ----------------------------------------------------
# Remove any periods (.) in ICD10 codes
dataNPR$Diagnosis_ICDCode  <- sub("\\.", "", dataNPR$Diagnosis_ICDCode)

# Remove any question marks (?) in ICD10 codes
dataNPR$Diagnosis_ICDCode  <- sub("\\?", "", dataNPR$Diagnosis_ICDCode)

# Convert all codes to upper case
dataNPR$Diagnosis_ICDCode <- toupper(dataNPR$Diagnosis_ICDCode)

# Convert all NCMP NCSP codes to upper case - not used
# Currently, generates an error because the entry at 8793620 is "S\x8fR" 
# (actually S<8f>R); manually correcting it
dataNPR$Procedures_NCMPNCSPCode[dataNPR$Procedures_NCMPNCSPCode == "S\x8fR"] <- "Unknown"
dataNPR$Procedures_NCMPNCSPCode <- toupper(dataNPR$Procedures_NCMPNCSPCode)

# Save a version of data at this point - before counting
outputName <- paste(runTag, "-MoBa-ParsedNPR.csv", sep = "")
fwrite(dataNPR, file = file.path(dirOutput, outputName), sep = "\t", row.names = F)

# write.table(dataNPR, file = file.path(dirOutput, outputName), sep = "\t", row.names = F)


# Variables to ignore -----------------------------------------------------
varsToIgnore <- c("SerialNumber",                    "Key",                 "AVT_Sector",
                  "SourceTableName",                 "DiffDays_Admission",
                  "DiffDays_Discharge",              "Duration",
                  "Somatic_LevelofCare",             "Somatic_LevelofCare_2008_2009",
                  "Somatic_LevelofCare_from2010",    "Somatic_TreatmentClinic_Derived",
                  "Somatic_TreatmentClinic_Reported","ReferralType",                    
                  "ReferralDecision",                "InstitutionID",
                  "InstitutionID_Psuedoanonymized",  "InstitutionID_MentalHealth",
                  "ManualName",                      "ManualType",
                  "Diagnosis_MainSecondary",         "Diagnosis_Axis", 
                  "ChildCodeSequence",               "Procedures_CodeNumber",           
                  "Procedures_NCMPNCSPCode",         "REH_partOf_SOM", 
                  "Unfinished_WardStay",             "Original_kodenr", 
                  "Original_kodeverdi")

locsIgnore <- colnames(dataNPR) %in% varsToIgnore
locsIgnore <- !locsIgnore
dataNPR    <- dataNPR[, ..locsIgnore] # data.table nonsense
# dataNPR     <- dataNPR[, !locsIgnore]


# Remove any row without ICD10 code ---------------------------------------
locEmpty    <- is.na(dataNPR$Diagnosis_ICDCode) | dataNPR$Diagnosis_ICDCode == ""
dataNPR     <- dataNPR[!locEmpty,]


# Flatten table -----------------------------------------------------------
dupRows     <- duplicated(dataNPR)
dataNPR     <- dataNPR[!dupRows,]
# dataNPR   <- unique(dataNPR) # alternate solution


# Make one overall ID column ----------------------------------------------
dataNPR$ID  <- paste(dataNPR$ID_2445,  dataNPR$M_ID_2445,  dataNPR$F_ID_2445,  sep = "")


# Add Role column ---------------------------------------------------------
locChildren               <- dataNPR$ID_2445    != ""
locMothers                <- dataNPR$M_ID_2445  != ""
locFathers                <- dataNPR$F_ID_2445  != ""
dataNPR$Role[locChildren] <- "Child"
dataNPR$Role[locMothers]  <- "Mother"
dataNPR$Role[locFathers]  <- "Father"


# Eliminate other ID columns ----------------------------------------------
toEliminate <- !colnames(dataNPR) %in% c("ID_2445", "M_ID_2445", "F_ID_2445")
dataNPR     <- dataNPR[, ..toEliminate]
# dataNPR <- dataNPR[, !(colnames(dataNPR) %in% c("ID_2445", "M_ID_2445", "F_ID_2445"))]


# Rename ID to ID_2445 ----------------------------------------------------
colnames(dataNPR)[colnames(dataNPR) == "ID"] <- "ID_2445"


# Long diagnosis file + wide 0/1 files (2026-10) ---------------------------
# Following Pravesh's suggestion, the 0/1 files are saved as parquet instead of csv,
# built in blocks of persons so they fit in memory (common.R). Column names are
# the same as before (<code>_lifetime, <code>_<year>, <code>_x<year>).
source(file.path(dirScripts, "common.R"))
dx <- dataNPR[, .(ID_2445 = as.character(ID_2445), Role, PREG_ID_2445, ChildNumber,
                  WithdrawnConsent18years, Year = as.integer(Year), Code = Diagnosis_ICDCode)]
rm(dataNPR); invisible(gc())

# Long diagnosis file: one row per person, code and year (used by doCounts.R)
outputName <- paste(runTag, "-MoBa-ParsedNPR_diagnoses.csv", sep = "")
fwrite(unique(dx), file = file.path(dirOutput, outputName), sep = "\t")
cat("Diagnoses:", nrow(unique(dx)), "rows,", uniqueN(dx$ID_2445), "persons,",
    uniqueN(dx$Code), "codes, years", paste(range(dx$Year), collapse = "-"), "\n")

# Wide files: all codes (lifetime + one file per year; the full table only if
# makeFullMinorNPR = TRUE) and major codes (first three characters; full table too)
makeWideFiles(dx, file.path(dirOutput, paste0(runTag, "-MoBa-ParsedNPR")),
              major = TRUE, fullMinor = makeFullMinorNPR, maxCells = maxCells)
