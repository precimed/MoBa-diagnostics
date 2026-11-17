# --- Adapted to the 2026-10-07 delivery: paths and settings come from 00_config.R ---
source("00_config.R")
source(file.path(dirScripts, "common.R"))

# Do counts (2026-10) ------------------------------------------------------
# Number of persons per ICD-10 code: lifetime, cumulative per year and cross-sectional
# per year, for all / children / mothers / fathers, and for the genotyped (MoBaPsychGen_v1).
# Same file names and columns as before, but counted from the long diagnosis file.
dx <- fread(file.path(dirOutKPR, paste0(runTag, "-MoBa-ParsedKPR_ICD10_diagnoses.csv")),
            sep = "\t", colClasses = list(character = c("ID_2445", "Code")))
genIDs <- readGenIDs()
writeCounts(dx, "ICD10Code", file.path(dirOutKPR, "counts"),
               "-MoBa-KPRCounts_allICDCodes-%s.csv", "-MoBa-KPRGeneticsCounts_allICDCodes-%s.csv", genIDs)
