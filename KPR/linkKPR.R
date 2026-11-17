# --- Adapted to the 2026-10-07 delivery: paths and settings come from 00_config.R ---
source("00_config.R")

# Link KPR
# Library for reading SPSS file (link files are SPSS files)
library(haven)
library(data.table)

# File locations ----------------------------------------------------------
# Where output files should be written
dirWorking <- dirOutKPR
setwd(dirWorking)

# Current location of link files
dirLink <- dirLink

# File name for the merged KPR
mergedFile <- paste(runTag, "-MoBa-MergedKPR.csv", sep = "")

# Read files --------------------------------------------------------------
dataKPR     <- fread(file.path(dirWorking, mergedFile), sep = "\t", colClasses = list(character = c("MunicipalityNumber", "TreatmentMunicipality", "CountyNumber", "TreatmentCounty")))   # 2026-11: keep leading zeros in municipality/county numbers
link_mother <- read_sav(file.path(dirLink, list.files(path = dirLink, pattern = paste0("^Mor_ID_2445_", linkDate, "\\.sav$"))))
link_father <- read_sav(file.path(dirLink, list.files(path = dirLink, pattern = paste0("^Far_ID_2445_", linkDate, "\\.sav$"))))
link_child  <- read_sav(file.path(dirLink, list.files(path = dirLink, pattern = paste0("^Barn_ID_2445_", linkDate, "\\.sav$"))))

# The serial-number column was PID_1037_<date> in 2025 and is now PID_2445; detect it
pidCol <- grep("^PID_", colnames(link_child), value = TRUE)
stopifnot(length(pidCol) == 1,
          pidCol %in% colnames(link_mother), pidCol %in% colnames(link_father))
cat("Linking on", pidCol, "\n")
# Compare as character (PID is character in the linkage files)
dataKPR$SerialNumber <- as.character(dataKPR$SerialNumber)
link_mother[[pidCol]] <- as.character(link_mother[[pidCol]])
link_father[[pidCol]] <- as.character(link_father[[pidCol]])
link_child[[pidCol]]  <- as.character(link_child[[pidCol]])
cat("Share of unique serial numbers found in the linkage files:",
    round(mean(unique(dataKPR$SerialNumber) %in% c(link_mother[[pidCol]], link_father[[pidCol]], link_child[[pidCol]])), 4), "\n")

# Join files --------------------------------------------------------------
# Here, we perform an inner join on the tables - entries which are not found are
# dropped; this naturally translates into three separate tables - one for
# mothers, one for fathers, and one for children
KPR_mothers  <- merge(x = dataKPR, y = link_mother, by.x = "SerialNumber", by.y = pidCol)
KPR_fathers  <- merge(x = dataKPR, y = link_father, by.x = "SerialNumber", by.y = pidCol)
KPR_children <- merge(x = dataKPR, y = link_child,  by.x = "SerialNumber", by.y = pidCol)

stopifnot(nrow(KPR_children) + nrow(KPR_mothers) + nrow(KPR_fathers) - nrow(dataKPR) == 0)


# Housekeeping prior to merging -------------------------------------------
# Since the children KPR have two more columns than parents (BARN_NR and
# OppfStopp18aar), we rename these columns
colnames(KPR_children)[colnames(KPR_children) == "OppfStopp18aar"] <- "WithdrawnConsent18years"
colnames(KPR_children)[colnames(KPR_children) == "BARN_NR"]        <- "ChildNumber"

# Save files - independent dataset ----------------------------------------
outputName <- paste(runTag, "-MoBa-LinkedKPR-Mothers.csv", sep = "")
fwrite(KPR_mothers, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)

outputName <- paste(runTag, "-MoBa-LinkedKPR-Fathers.csv", sep = "")
fwrite(KPR_fathers, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)

outputName <- paste(runTag, "-MoBa-LinkedKPR-Children.csv", sep = "")
fwrite(KPR_children, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)


# Merge tables ------------------------------------------------------------
# We add WithdrawnConsent18years and ChildNumber to the parents' KPR as
# prior to merging; additionally, prior to merging, additional columns like 
# F_ID_2445 and M_ID_2445 need to be added to the children's KPR (and partners' 
# ID to other partners' table)

KPR_mothers$WithdrawnConsent18years <- ""
KPR_mothers$ChildNumber             <- ""
KPR_mothers$F_ID_2445               <- ""
KPR_mothers$PREG_ID_2445            <- ""

KPR_fathers$WithdrawnConsent18years <- ""
KPR_fathers$ChildNumber             <- ""
KPR_fathers$M_ID_2445               <- ""
KPR_fathers$PREG_ID_2445            <- ""

KPR_children$M_ID_2445              <- ""
KPR_children$F_ID_2445              <- ""

# Merge everything --------------------------------------------------------
# Side effect: file size is huge and therefore takes a while to read...
KPR_All <- rbind(rbind(KPR_children, KPR_mothers), KPR_fathers)

# Save files --------------------------------------------------------------
outputName <- paste(runTag, "-MoBa-LinkedKPR-AllSubjects.csv", sep = "")
fwrite(KPR_All, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)