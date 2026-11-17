# --- Adapted to the 2026-10-07 delivery: paths and settings come from 00_config.R ---
source("00_config.R")

# Generate a list of all participants that were ever in PDB2445 (MoBa-wide participant list)
# Preliminaries -----------------------------------------------------------
library(haven)


# Define paths etc --------------------------------------------------------
dirOutput       <- dirOutNPR
dirOldRecords   <- "/tsd/p697/data/durable/phenotypes/sv_infofiles/old"
dirCurrRecords  <- "/tsd/p697/data/durable/phenotypes/sv_infofiles/"
fileCurrent     <- svInfoFile
# 2026-10: full paths; older SV_INFO versions may sit both in Old/ and in the main folder
filesOld        <- c(list.files(path = dirOldRecords,  pattern = glob2rx("PDB2445_SV_INFO*.sav"), full.names = TRUE),
                     file.path(dirCurrRecords, setdiff(list.files(path = dirCurrRecords, pattern = glob2rx("PDB2445_SV_INFO*.sav")), svInfoFile)))
cat("Older SV_INFO files used:", basename(filesOld), sep = "\n  ")
setwd(dirOutput)

if (length(fileCurrent) > 1)
{
  stop('More than one current version of INFO file found; script is not designed for this')
}


# Read all files ----------------------------------------------------------
currentInfo     <- read_sav(file = file.path(dirCurrRecords, fileCurrent[1]))
oldInfo         <- read_sav(file = filesOld[1])

# Drop any other column except PREG_ID_2445, M_ID_2445, and F_ID_2445
colKeep         <- c("PREG_ID_2445", "M_ID_2445", "F_ID_2445")
oldInfo         <- oldInfo[, colKeep]

# Read additional old records
for (files in filesOld[-1])
{
  temp          <- read_sav(file = files)
  temp          <- temp[, colKeep]
  oldInfo       <- rbind(oldInfo, temp)
}

# Flatten old information - unique rows
oldInfo         <- unique(oldInfo)


# Merge everything --------------------------------------------------------
allSubjects     <- rbind(currentInfo, oldInfo)

# Flatten again: possible to do in one step rather than separately flattening oldInfo
allSubjects     <- unique(allSubjects)


# Handle duplicates -------------------------------------------------------
# There will be rows where the pregnancy ID is the same (and hopefully the
# mother ID); however, the father may have withdrawn consent down the line,
# making these rows different from their previous entries. Handle these
listDuplicates  <- allSubjects$PREG_ID_2445[duplicated(allSubjects$PREG_ID_2445)]
duplicatedData  <- allSubjects[allSubjects$PREG_ID_2445 %in% listDuplicates,]
count           <- 0

for (dupID in listDuplicates)
{
  # Find the locations where these duplicates are
  locDuplicates <- which(allSubjects$PREG_ID_2445 == dupID)
  
  # As a sanity check, ensure that the mother ID is the same!
  if (length(unique(allSubjects$M_ID_2445[locDuplicates])) != 1)
  {
    stop(paste("ID ", dupID, " seems to have different mother ID across files"))
  }
  
  # Now find all father IDs corresponding to this entry
  tmp_F_ID_2445 <- allSubjects$F_ID_2445[locDuplicates]
  
  # Remove empty
  tmp_F_ID_2445 <- tmp_F_ID_2445[tmp_F_ID_2445 != ""]
  
  # If more than one father ID left, there is a problem!
  if (length(tmp_F_ID_2445) > 1)
  {
    stop(paste("ID ", dupID, " seems to have different father ID across files"))
  }
  else
  {
    # If only one father ID left, spread it across rows
    if (length(tmp_F_ID_2445) == 1)
    {
      allSubjects$F_ID_2445[locDuplicates]  <- tmp_F_ID_2445
      count                                 <- count + 1
    }
  }
  
  # If no father ID left, father was never reported; move on
  # This shouldn't happen
}

print(paste("Replaced ", count, " father IDs out of ", length(listDuplicates), sep = ""))

# Final flattening --------------------------------------------------------
allSubjects     <- unique(allSubjects)


# Generate keep list of subjects ------------------------------------------
# These are subjects that are present in the latest INFO file
# This is just a sanity check type of thing - the generated data frame should be
# EXACTLY the same as the current INFO file

# Step 1) Make a list of pregnancy IDs which are present in latest INFO file
keep_pregID   <- allSubjects$PREG_ID_2445 %in% currentInfo$PREG_ID_2445
keep_data     <- allSubjects[keep_pregID, ]  

# Step 2) Any F_ID_2445 that is empty in current INFO file should be eliminated from keep_data
pregID_empty_F_ID <- currentInfo$PREG_ID_2445[currentInfo$F_ID_2445 == ""]
toEliminate_F_ID  <- allSubjects$F_ID_2445[allSubjects$PREG_ID_2445 %in% pregID_empty_F_ID]
toEliminate_F_ID  <- toEliminate_F_ID[toEliminate_F_ID != ""]
toEliminate_F_ID  <- unique(toEliminate_F_ID)
keep_data$F_ID_2445[keep_data$F_ID_2445 %in% toEliminate_F_ID] <- ""

# Demonstrate that there is no difference between current INFO and this file
if (sum(colSums(keep_data == currentInfo) == nrow(currentInfo)) != 3)
{
  stop("Keep file is not the same as INFO file; something went wrong")
}


# Generate a consent withdrawn list ---------------------------------------
# These are subjects that are present in allSubjects but NOT in the latest INFO file
# Step 1) Make a list of pregnancy IDs which are NOT present in the latest INFO file
withdrawn_pregID <- allSubjects[!(allSubjects$PREG_ID_2445 %in% currentInfo$PREG_ID_2445),]

# Step 2) Any F_ID_2445 that is empty in current INFO file should be eliminated
toEliminate_F_ID            <- as.data.frame(cbind(rep("", length(toEliminate_F_ID)), 
                                                   rep("", length(toEliminate_F_ID)), 
                                                   toEliminate_F_ID))
colnames(toEliminate_F_ID)  <- c("PREG_ID_2445", "M_ID_2445", "F_ID_2445")

withdrawn_data              <- rbind(withdrawn_pregID, toEliminate_F_ID)


# Ensure that all fathers we earlier looked up are eliminated -------------
tmp_duplicated_F_ID         <- keep_data$F_ID_2445[keep_data$PREG_ID_2445 %in% listDuplicates]
tmp_duplicated_F_ID         <- tmp_duplicated_F_ID[tmp_duplicated_F_ID != ""]
if (length(tmp_duplicated_F_ID) != 0)
{
  warning(paste(length(tmp_duplicated_F_ID),
      " father IDs which were previously looked up still exist in keep / current INFO file", sep = ""))
}


# Find parents who are withdrawn and to be kept ---------------------------
# This is a unique situation - certain pregnancy IDs could be withdrawn yet a sibling is retained!
borderline_M_ID <- withdrawn_data$M_ID_2445[withdrawn_data$M_ID_2445 %in% currentInfo$M_ID_2445]
borderline_M_ID <- borderline_M_ID[borderline_M_ID != ""]
if (length(borderline_M_ID) != 0)
{
  warning(paste(length(borderline_M_ID), " mother IDs are both in keep and withdrawn list", sep = ""))
}

borderline_F_ID <- withdrawn_data$F_ID_2445[withdrawn_data$F_ID_2445 %in% currentInfo$F_ID_2445]
borderline_F_ID <- borderline_F_ID[borderline_F_ID != ""]
if (length(borderline_F_ID) != 0)
{
  warning(paste(length(borderline_F_ID), " father IDs are both in keep and withdrawn list", sep = ""))
}


# Save tables -------------------------------------------------------------
outputName <- paste(runTag, "-MoBa-CompleteParticipantList.csv", sep = "")
write.table(allSubjects, file = file.path(dirOutput, outputName), sep = "\t", row.names = F)

outputName <- paste(runTag, "-MoBa-KeepList.csv", sep = "")
write.table(keep_data, file = file.path(dirOutput, outputName), sep = "\t", row.names = F)

outputName <- paste(runTag, "-MoBa-WithdrawnList.csv", sep = "")
write.table(withdrawn_data, file = file.path(dirOutput, outputName), sep = "\t", row.names = F)
