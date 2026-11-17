# --- Adapted to the 2026-10-07 delivery: paths and settings come from 00_config.R ---
source("00_config.R")

# Compile info on first contact for each diagnoses
library(data.table)

# Read parsed NPR
dirNPR  <- dirOutNPR
dataNPR <- fread(file.path(dirNPR, paste0(runTag, "-MoBa-ParsedNPR.csv")), sep = "\t")

# Variables to ignore -----------------------------------------------------
varsToIgnore <- c("SerialNumber",                    "Key",                 "AVT_Sector",
                  "SourceTableName",                 
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


# Remove any row without ICD10 code ---------------------------------------
locEmpty    <- is.na(dataNPR$Diagnosis_ICDCode) | dataNPR$Diagnosis_ICDCode == ""
dataNPR     <- dataNPR[!locEmpty,]

# Flatten table -----------------------------------------------------------
dupRows     <- duplicated(dataNPR)
dataNPR     <- dataNPR[!dupRows,]

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

# Rename ID to ID_2445 ----------------------------------------------------
colnames(dataNPR)[colnames(dataNPR) == "ID"] <- "ID_2445"

# Reorganize NPR ----------------------------------------------------------
dataNPR     <- dataNPR[, c("ID_2445", "Role","PREG_ID_2445", "ChildNumber", 
                           "WithdrawnConsent18years", "Year", "Diagnosis_ICDCode",
                           "DiffDays_Admission", "DiffDays_Discharge", "Duration")]


# Unique ICD10 codes and time points -------------------------------------
uqICDCodes    <- sort(unique(dataNPR$Diagnosis_ICDCode))
uqTimePoints  <- sort(unique(dataNPR$Year))

# List of unique subjects
uqSubjs <- unique(dataNPR$ID_2445)

# Initialize results table
diagInformation  <- as.data.frame(matrix(data = NA, nrow = length(uqSubjs), ncol = length(uqICDCodes)+5))
tempSanityCounts <- as.data.frame(matrix(data = 0,  nrow = length(uqSubjs), ncol = length(uqICDCodes)+5))
colnames(diagInformation) <- c("ID_2445", "Role","PREG_ID_2445", "ChildNumber", 
                               "WithdrawnConsent18years", uqICDCodes)

colnames(tempSanityCounts) <- c("ID_2445", "Role","PREG_ID_2445", "ChildNumber", 
                               "WithdrawnConsent18years", uqICDCodes)

# Minimal dataNPR table
tmp_minData <- dataNPR[, 1:5]
dupRows     <- duplicated(tmp_minData)
tmp_minData <- tmp_minData[!dupRows,]

# Assign to diagInformation
diagInformation[, 1:5]  <- tmp_minData
tempSanityCounts[, 1:5] <- tmp_minData

# Compile
for (codes in uqICDCodes)
{
  # All rows in dataNPR having this diagnosis
  tmpRows <- dataNPR$Diagnosis_ICDCode == codes
  
  # For every found subject, work
  tmpData <- dataNPR[tmpRows,]
  tmpUq   <- unique(tmpData$ID_2445)
  
  for (ids in tmpUq)
  {
    toMark <- diagInformation$ID_2445 == ids
    loc    <- tmpData$ID_2445 == ids
    
    # Save results
    diagInformation[toMark,  eval(codes)] <- min(c(tmpData$DiffDays_Admission[loc], tmpData$DiffDays_Discharge[loc]), na.rm = T)
    tempSanityCounts[toMark, eval(codes)] <- 1
  }
  print(codes)
}

# Save
outputName  <- paste(runTag, "-MoBa-SmallestDiffDate-allCodes.csv", sep = "")
fwrite(diagInformation, file = file.path(dirNPR, outputName), sep = "\t", row.names = F)