# --- Adapted to the 2026-10-07 delivery: paths and settings come from 00_config.R ---
source("00_config.R")

# Read the new NPR files (sector-wise distributed csv files) and create a
# composite file with mapping etc.

# Assumptions:  there are six sectors - AVT, PHBU, PHV, REH, SOM, TSB
#               Each sector has three files - Aktivitet, Prosedyrer, Tilstander
#               PHBU does not have an associated Prosedyrer file
#               The variables in these files are separated by semicolon (;)

# File locations ----------------------------------------------------------
# Where output files should be written
dirWorking <- dirOutNPR
setwd(dirWorking)

# Current location of updated NPR files
dirNPR <- dirRawNPR

# Find and read files for each sector -------------------------------------
# Using glob2rx("AVT*Aktivitet*.csv") for creating regular expression: this
# would result in the pattern "^AVT.*Aktivitet.*\\.csv$" which implies that the
# file name starts with AVT and after that has any number of character;
# somewhere in the middle there is a block of text "Aktivitet", and the ending
# is a csv extension
# AVT files
AVT_Activity    <- read.csv(file.path(dirNPR, list.files(path = dirNPR, pattern = glob2rx("AVT*aktivitet*.csv"))),  sep = ";")
AVT_Conditions  <- read.csv(file.path(dirNPR, list.files(path = dirNPR, pattern = glob2rx("AVT*koder*.csv"))),  sep = ";")

# PHBU files
PHBU_Activity   <- read.csv(file.path(dirNPR, list.files(path = dirNPR, pattern = glob2rx("PHBU*aktivitet*.csv"))),  sep = ";")
PHBU_Conditions <- read.csv(file.path(dirNPR, list.files(path = dirNPR, pattern = glob2rx("PHBU*koder*.csv"))), sep = ";") 

# PHV files
PHV_Activity    <- read.csv(file.path(dirNPR, list.files(path = dirNPR, pattern = glob2rx("PHV*aktivitet*.csv"))),  sep = ";")
PHV_Conditions  <- read.csv(file.path(dirNPR, list.files(path = dirNPR, pattern = glob2rx("PHV*koder*.csv"))), sep = ";") 

# REH files
REH_Activity    <- read.csv(file.path(dirNPR, list.files(path = dirNPR, pattern = glob2rx("REH*aktivitet*.csv"))),  sep = ";")
REH_Conditions  <- read.csv(file.path(dirNPR, list.files(path = dirNPR, pattern = glob2rx("REH*koder*.csv"))), sep = ";") 

# SOM files
SOM_Activity    <- read.csv(file.path(dirNPR, list.files(path = dirNPR, pattern = glob2rx("SOM*aktivitet*.csv"))),  sep = ";")
SOM_Conditions  <- read.csv(file.path(dirNPR, list.files(path = dirNPR, pattern = glob2rx("SOM*koder*.csv"))), sep = ";") 

# TSB files
TSB_Activity    <- read.csv(file.path(dirNPR, list.files(path = dirNPR, pattern = glob2rx("TSB*aktivitet*.csv"))),  sep = ";")
TSB_Conditions  <- read.csv(file.path(dirNPR, list.files(path = dirNPR, pattern = glob2rx("TSB*koder*.csv"))), sep = ";")


# 2026-10: make column names lower case, as the rest of the script expects
for (obj in c("AVT_Activity", "AVT_Conditions", "PHBU_Activity", "PHBU_Conditions",
              "PHV_Activity", "PHV_Conditions", "REH_Activity", "REH_Conditions",
              "SOM_Activity", "SOM_Conditions", "TSB_Activity", "TSB_Conditions"))
{
  tmp <- get(obj); colnames(tmp) <- tolower(colnames(tmp)); assign(obj, tmp)
}
rm(tmp)

# Merge tables ------------------------------------------------------------ 
# In this section, we perform outerjoin operations to merge Conditions tables 
# to the Activity table - the Activity table is the main table
AVT_full  <- merge(x = AVT_Activity, y = AVT_Conditions, by = "nokkel", all = TRUE)

PHBU_full <- merge(x = PHBU_Activity, y = PHBU_Conditions, by = "nokkel", all = TRUE)

PHV_full  <- merge(x = PHV_Activity, y = PHV_Conditions, by = "nokkel", all = TRUE)

REH_full  <- merge(x = REH_Activity, y = REH_Conditions, by = "nokkel", all = TRUE)

SOM_full  <- merge(x = SOM_Activity, y = SOM_Conditions, by = "nokkel", all = TRUE)

TSB_full  <- merge(x = TSB_Activity, y = TSB_Conditions, by = "nokkel", all = TRUE)


# In this release, lopenr is included in both tables; get rid of the one in
# Conditions as it may have missing information ---------------------------
AVT_full  <- AVT_full[,  !(colnames(AVT_full)  %in% "lopenr.y")]
PHBU_full <- PHBU_full[, !(colnames(PHBU_full) %in% "lopenr.y")]
PHV_full  <- PHV_full[,  !(colnames(PHV_full)  %in% "lopenr.y")]
REH_full  <- REH_full[,  !(colnames(REH_full)  %in% "lopenr.y")]
SOM_full  <- SOM_full[,  !(colnames(SOM_full)  %in% "lopenr.y")]
TSB_full  <- TSB_full[,  !(colnames(TSB_full)  %in% "lopenr.y")]

# Now, rename lopenr.x to lopenr
colnames(AVT_full)[colnames(AVT_full)   == "lopenr.x"] <- "lopenr"
colnames(PHBU_full)[colnames(PHBU_full) == "lopenr.x"] <- "lopenr"
colnames(PHV_full)[colnames(PHV_full)   == "lopenr.x"] <- "lopenr"
colnames(REH_full)[colnames(REH_full)   == "lopenr.x"] <- "lopenr"
colnames(SOM_full)[colnames(SOM_full)   == "lopenr.x"] <- "lopenr"
colnames(TSB_full)[colnames(TSB_full)   == "lopenr.x"] <- "lopenr"


# Clear up workspace ------------------------------------------------------
rm(AVT_Activity,  AVT_Conditions,  
   PHBU_Activity, PHBU_Conditions, 
   PHV_Activity,  PHV_Conditions,  
   REH_Activity,  REH_Conditions,  
   SOM_Activity,  SOM_Conditions,  
   TSB_Activity,  TSB_Conditions)

gc()

# Add a column for where the observation comes from -----------------------
# This is just a reference column to keep track of where a row comes from 
# (once all tables are merged)
AVT_full$Source   <- "AVT"
PHBU_full$Source  <- "PHBU"
PHV_full$Source   <- "PHV"
REH_full$Source   <- "REH"
SOM_full$Source   <- "SOM"
TSB_full$Source   <- "TSB"

# Merge all tables --------------------------------------------------------
# This is a series of outerjoin operations written in a nested manner to prevent
# creating intermediate tables
# In the present release (2025-05), nokkel can be the same across tables 
# (not sure if that was the case in the previous releases); getting rid of nokkel
AVT_full  <- AVT_full[,  !(colnames(AVT_full)  %in% "nokkel")]
PHBU_full <- PHBU_full[, !(colnames(PHBU_full) %in% "nokkel")]
PHV_full  <- PHV_full[,  !(colnames(PHV_full)  %in% "nokkel")]
REH_full  <- REH_full[,  !(colnames(REH_full)  %in% "nokkel")]
SOM_full  <- SOM_full[,  !(colnames(SOM_full)  %in% "nokkel")]
TSB_full  <- TSB_full[,  !(colnames(TSB_full)  %in% "nokkel")]

NPR_full  <- merge(x =
                     merge(
                       x = merge(
                         x = merge(
                           x = merge(
                             x = AVT_full, y = PHBU_full, all = TRUE),
                           y = PHV_full, all = TRUE),
                         y = REH_full, all = TRUE),
                       y = SOM_full, all = TRUE),
                   y = TSB_full, all = TRUE)

# In the new delivery, procedures and diagnostics are pooled together; separate
# out kodenr to Diagnosis_MainSecondary and Procedures_CodeNumber --------
# Initialize
NPR_full$Diagnosis_MainSecondary <- NA
NPR_full$Procedures_CodeNumber   <- NA

# Find locations where diagnosis is assigned
loc_diagnosis  <- which(NPR_full$kodenavn == "ICD-10" | NPR_full$Source == "PHBU")
loc_procedures <- which(NPR_full$kodenavn == "NCMP" | NPR_full$kodenavn == "NCSP")
loc_leftover   <- setdiff(1:nrow(NPR_full), c(loc_diagnosis, loc_procedures))

# Assign values
NPR_full$Diagnosis_MainSecondary[loc_diagnosis] <- NPR_full$kodenr[loc_diagnosis]
NPR_full$Procedures_CodeNumber[loc_procedures]  <- NPR_full$kodenr[loc_procedures]

# Similarly, split kodeverdi into Diagnosis_ICDCode and Procedures_NCMPNCSPCode
NPR_full$Diagnosis_ICDCode          <- NA
NPR_full$Procedures_NCMPNCSPCode    <- NA

# Can re-use same locations
NPR_full$Diagnosis_ICDCode[loc_diagnosis]        <- NPR_full$kodeverdi[loc_diagnosis]
NPR_full$Procedures_NCMPNCSPCode[loc_procedures] <- NPR_full$kodeverdi[loc_procedures]


# Rename variables --------------------------------------------------------
# Thanks to Viktoria Birkenæs for translating and creating new variable names
# colnames(NPR_full)[colnames(NPR_full) == "nokkel"]               <- "Key"
colnames(NPR_full)[colnames(NPR_full) == "lopenr"]               <- "SerialNumber"
colnames(NPR_full)[colnames(NPR_full) == "aar"]                  <- "Year"
colnames(NPR_full)[colnames(NPR_full) == "diffdager_inndato"]    <- "DiffDays_Admission"
colnames(NPR_full)[colnames(NPR_full) == "diffdager_utdato"]     <- "DiffDays_Discharge"

colnames(NPR_full)[colnames(NPR_full) == "omsorgsniva"]          <- "Somatic_LevelofCare"
colnames(NPR_full)[colnames(NPR_full) == "behandlingsniva3"]     <- "Somatic_LevelofCare_2008_2009"
# colnames(NPR_full)[colnames(NPR_full) == "aktivitetskategori3"]  <- "Somatic_LevelofCare_from2010"
colnames(NPR_full)[colnames(NPR_full) == "behandlingssted2"]     <- "Somatic_TreatmentClinic_Derived"
colnames(NPR_full)[colnames(NPR_full) == "behandlingsstedkode"]  <- "Somatic_TreatmentClinic_Reported"

colnames(NPR_full)[colnames(NPR_full) == "henvtype"]             <- "ReferralType"
colnames(NPR_full)[colnames(NPR_full) == "henvvurd"]             <- "ReferralDecision"

colnames(NPR_full)[colnames(NPR_full) == "institusjonid"]        <- "InstitutionID"
colnames(NPR_full)[colnames(NPR_full) == "institusjonid_pseud"]  <- "InstitutionID_Psuedoanonymized"
colnames(NPR_full)[colnames(NPR_full) == "psykinstid"]           <- "InstitutionID_MentalHealth"

colnames(NPR_full)[colnames(NPR_full) == "kodenavn"]             <- "ManualName"
colnames(NPR_full)[colnames(NPR_full) == "kodetype"]             <- "ManualType"
colnames(NPR_full)[colnames(NPR_full) == "kodenr"]               <- "Original_kodenr"
colnames(NPR_full)[colnames(NPR_full) == "kodeverdi"]            <- "Original_kodeverdi"
colnames(NPR_full)[colnames(NPR_full) == "tilstand"]             <- "ChildCodeSequence"
colnames(NPR_full)[colnames(NPR_full) == "akse"]                 <- "Diagnosis_Axis"

colnames(NPR_full)[colnames(NPR_full) == "Source"]               <- "SourceTableName"
colnames(NPR_full)[colnames(NPR_full) == "errehabilitering"]     <- "REH_partOf_SOM"
colnames(NPR_full)[colnames(NPR_full) == "overligger"]           <- "Unfinished_WardStay"
# 2026-10: AVT now holds both somatic and mental-health specialists in one file (sektor = SOM/PHV)
colnames(NPR_full)[colnames(NPR_full) == "sektor"]               <- "AVT_Sector"

# colnames(NPR_full)[colnames(NPR_full) == "rusinstID"]            <- "InstitutionID_Addiction"
# colnames(NPR_full)[colnames(NPR_full) == "Procedures_prosnr"]    <- "Procedures_OrderNumber"

# For AVT, if out date is missing, replace with in date -------------------
loc_missing <- is.na(NPR_full$DiffDays_Discharge) & NPR_full$SourceTableName == "AVT"
NPR_full$DiffDays_Discharge[loc_missing] <- NPR_full$DiffDays_Admission[loc_missing]


# Calculate duration of inpatient stay ------------------------------------
NPR_full$Duration <- NPR_full$DiffDays_Discharge - NPR_full$DiffDays_Admission


# Reorder columns ---------------------------------------------------------
wantedCols <- c("SerialNumber", "SourceTableName", "AVT_Sector", 
                          "Year", "DiffDays_Admission", "DiffDays_Discharge", "Duration", 
                          "Somatic_LevelofCare", "Somatic_LevelofCare_2008_2009", 
                          "Somatic_TreatmentClinic_Derived", 
                          "Somatic_TreatmentClinic_Reported", "ReferralType", "ReferralDecision", 
                          "InstitutionID", "InstitutionID_Psuedoanonymized", 
                          "InstitutionID_MentalHealth", 
                          "ManualName", "ManualType", 
                          "Diagnosis_MainSecondary", "Diagnosis_ICDCode", 
                          "Diagnosis_Axis", "ChildCodeSequence", 
                          "Procedures_CodeNumber", "Procedures_NCMPNCSPCode", 
                          "REH_partOf_SOM", "Unfinished_WardStay", 
                          "Original_kodenr", "Original_kodeverdi")
# 2026-10: keep only columns that exist, and report any that are missing
missingCols <- setdiff(wantedCols, colnames(NPR_full))
if (length(missingCols)) warning("Columns not found in this delivery: ", paste(missingCols, collapse = ", "))
extraCols   <- setdiff(colnames(NPR_full), c(wantedCols, "kodenavn", "kodetype", "kodenr", "kodeverdi"))
if (length(extraCols)) message("Columns in the delivery that are not carried forward: ", paste(extraCols, collapse = ", "))
NPR_full2   <- NPR_full[, intersect(wantedCols, colnames(NPR_full))]

# Save merged NPR ---------------------------------------------------------
outputName <- paste(runTag, "-MoBa-MergedNPR.csv", sep = "")
write.table(NPR_full2, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)
