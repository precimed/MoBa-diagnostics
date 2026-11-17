# --- Adapted to the 2026-10-07 delivery: paths and settings come from 00_config.R ---
source("00_config.R")
source(file.path(dirScripts, "common.R"))

# Do counts (2026-10) ------------------------------------------------------
# Number of persons per ICD-10 code: lifetime, cumulative per year and cross-sectional
# per year, for all / children / mothers / fathers, and for the genotyped (MoBaPsychGen_v1).
# Same file names and columns as before, but counted from the long diagnosis file
# instead of the wide files (which are now parquet and too large to read in whole).
dx <- fread(file.path(dirOutNPR, paste0(runTag, "-MoBa-ParsedNPR_diagnoses.csv")),
            sep = "\t", colClasses = list(character = c("ID_2445", "Code")))
genIDs <- readGenIDs()
cat("Genotyped persons with an NPR diagnosis:", uniqueN(dx[ID_2445 %in% genIDs]$ID_2445), "\n")

dirCounts <- file.path(dirOutNPR, "counts")
# All codes
writeCounts(dx, "ICD10Code", dirCounts,
               "-MoBa-NPRCounts_allCodes-%s.csv", "-MoBa-NPRGeneticsCounts_allCodes-%s.csv", genIDs)
# Major codes (first three characters)
dxMajor <- unique(dx[, Code := substr(Code, 1, 3)])
writeCounts(dxMajor, "ICD10Code", dirCounts,
               "-MoBa-NPRCounts_majorCodes-%s.csv", "-MoBa-NPRGeneticsCounts_majorCodes-%s.csv", genIDs)
