# --- Adapted to the 2026-10-07 delivery: paths and settings come from 00_config.R ---
source("00_config.R")

# Script to link merged NPR file with ID2445

# Library for reading SPSS file (link files are SPSS files)
library(haven)
library(data.table)

# File locations ----------------------------------------------------------
# Where output files should be written
dirWorking <- dirOutNPR
setwd(dirWorking)

# Current location of link files
dirNPR <- dirLink

# File name for the merged NPR
mergedFile <- paste(runTag, "-MoBa-MergedNPR.csv", sep = "")

# Read files --------------------------------------------------------------
dataNPR     <- fread(file.path(dirWorking, mergedFile), sep = "\t")
link_mother <- read_sav(file.path(dirNPR, list.files(path = dirNPR, pattern = paste0("^Mor_ID_2445_", linkDate, "\\.sav$"))))
link_father <- read_sav(file.path(dirNPR, list.files(path = dirNPR, pattern = paste0("^Far_ID_2445_", linkDate, "\\.sav$"))))
link_child  <- read_sav(file.path(dirNPR, list.files(path = dirNPR, pattern = paste0("^Barn_ID_2445_", linkDate, "\\.sav$"))))

# The serial-number column was PID_1037_<date> in 2025 and is now PID_2445; detect it
pidCol <- grep("^PID_", colnames(link_child), value = TRUE)
stopifnot(length(pidCol) == 1,
          pidCol %in% colnames(link_mother), pidCol %in% colnames(link_father))
cat("Linking on", pidCol, "\n")
# Compare as character (PID is character in the linkage files)
dataNPR$SerialNumber <- as.character(dataNPR$SerialNumber)
link_mother[[pidCol]] <- as.character(link_mother[[pidCol]])
link_father[[pidCol]] <- as.character(link_father[[pidCol]])
link_child[[pidCol]]  <- as.character(link_child[[pidCol]])
cat("Share of unique serial numbers found in the linkage files:",
    round(mean(unique(dataNPR$SerialNumber) %in% c(link_mother[[pidCol]], link_father[[pidCol]], link_child[[pidCol]])), 4), "\n")

# Join files --------------------------------------------------------------
# Here, we perform an inner join on the tables - entries which are not found are
# dropped; this naturally translates into three separate tables - one for
# mothers, one for fathers, and one for children
NPR_mothers  <- merge(x = dataNPR, y = link_mother, by.x = "SerialNumber", by.y = pidCol)
NPR_fathers  <- merge(x = dataNPR, y = link_father, by.x = "SerialNumber", by.y = pidCol)
NPR_children <- merge(x = dataNPR, y = link_child,  by.x = "SerialNumber", by.y = pidCol)

stopifnot(nrow(NPR_children) + nrow(NPR_mothers) + nrow(NPR_fathers) - nrow(dataNPR) == 0)

# Housekeeping prior to merging -------------------------------------------
# Since the children NPR have two more columns than parents (BARN_NR and
# OppfStopp18aar), we rename these columns
colnames(NPR_children)[colnames(NPR_children) == "OppfStopp18aar"] <- "WithdrawnConsent18years"
colnames(NPR_children)[colnames(NPR_children) == "BARN_NR"]        <- "ChildNumber"

# Save files - independent dataset ----------------------------------------
outputName <- paste(runTag, "-MoBa-LinkedNPR-Mothers.csv", sep = "")
fwrite(NPR_mothers, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)
# write.table(NPR_mothers, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)

outputName <- paste(runTag, "-MoBa-LinkedNPR-Fathers.csv", sep = "")
fwrite(NPR_fathers, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)
# write.table(NPR_fathers, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)

outputName <- paste(runTag, "-MoBa-LinkedNPR-Children.csv", sep = "")
fwrite(NPR_children, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)
# write.table(NPR_children, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)

# Merge tables ------------------------------------------------------------
# We add WithdrawnConsent18years and ChildNumber to the parents' NPR as
# prior to merging; additionally, prior to merging, additional columns like 
# F_ID_2445 and M_ID_2445 need to be added to the children's NPR (and partners' 
# ID to other partners' table)

NPR_mothers$WithdrawnConsent18years <- ""
NPR_mothers$ChildNumber             <- ""
NPR_mothers$F_ID_2445               <- ""
NPR_mothers$PREG_ID_2445            <- ""

NPR_fathers$WithdrawnConsent18years <- ""
NPR_fathers$ChildNumber             <- ""
NPR_fathers$M_ID_2445               <- ""
NPR_fathers$PREG_ID_2445            <- ""

NPR_children$M_ID_2445              <- ""
NPR_children$F_ID_2445              <- ""

# Merge everything --------------------------------------------------------
# Side effect: file size is huge and therefore takes a while to read...
NPR_All <- rbind(rbind(NPR_children, NPR_mothers), NPR_fathers)

# Save files --------------------------------------------------------------
outputName <- paste(runTag, "-MoBa-LinkedNPR-AllSubjects.csv", sep = "")
fwrite(NPR_All, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)
# write.table(NPR_All, file = file.path(dirWorking, outputName), sep = "\t", row.names = F)