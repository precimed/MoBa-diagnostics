# =============================================================================
# Quick test of common.R on made-up data (no real data is used).
# Run from the script folder:  source("test_common.R")   - takes a few seconds.
# Checks that the wide parquet files and the counts are correct, also when the
# table is split into several parts.
# =============================================================================
suppressPackageStartupMessages(library(data.table))
source("common.R")
runTag <- "TEST"

# 4 persons: two children, one mother, one father. Codes and years chosen so the answer is known.
dx <- data.table(
  ID_2445 = c("B1", "B1", "B1", "B2", "M1", "M1", "F1"),
  Role    = c("Child", "Child", "Child", "Child", "Mother", "Mother", "Father"),
  PREG_ID_2445 = c(1, 1, 1, 2, NA, NA, NA),
  ChildNumber  = c(1, 1, 1, 1, NA, NA, NA),
  WithdrawnConsent18years = c(0, 0, 0, 0, NA, NA, NA),
  Year = c(2010L, 2012L, 2012L, 2011L, 2010L, 2012L, 2011L),
  Code = c("F900", "F900", "F840", "F840", "F320", "F321", "F900"))

tmp <- tempfile("common_test_"); dir.create(tmp)
readPq <- function(f) as.data.table(
  if (parquetPackage() == "arrow") arrow::read_parquet(f) else nanoparquet::read_parquet(f))
readWide <- function(path) {
  if (dir.exists(path)) rbindlist(lapply(sort(list.files(path, full.names = TRUE)), readPq))
  else readPq(paste0(path, ".parquet"))
}
ok <- function(condition, text) cat(if (isTRUE(condition)) "OK    " else "ERROR ", text, "\n")

# maxCells = 20 forces the full table to be split into several parts
makeWideFiles(dx, file.path(tmp, "T"), major = TRUE, fullMinor = TRUE, maxCells = 20)

full <- readWide(file.path(tmp, "T_allCodes"))
setkey(full, ID_2445)
ok(nrow(full) == 4, "full table has 4 persons")
ok(full["B1", F900_lifetime] == 1 && full["B2", F900_lifetime] == 0, "lifetime F900")
ok(full["B1", F900_2010] == 1 && full["B1", F900_2011] == 1 && full["B1", F900_x2011] == 0,
   "cumulative and cross-sectional F900 for B1 (diagnosed 2010 and 2012)")
ok(full["B1", F840_2011] == 0 && full["B1", F840_2012] == 1 && full["B1", F840_x2012] == 1, "F840 for B1 (2012)")
ok(full["F1", F900_2010] == 0 && full["F1", F900_2011] == 1, "F900 for father (2011)")
ok(identical(names(full)[1:5], idColumns), "the first five columns are the ID columns")

maj <- readWide(file.path(tmp, "T_majorCodes")); setkey(maj, ID_2445)
ok(maj["M1", F32_lifetime] == 1 && maj["M1", F32_x2010] == 1 && maj["M1", F32_x2011] == 0 && maj["M1", F32_2011] == 1,
   "major code F32 for mother (F320 in 2010, F321 in 2012)")

yr <- readWide(file.path(tmp, "T_allCodes-2011"))
ok(setequal(names(yr)[-(1:5)], c("F320_2011", "F321_2011", "F840_2011", "F900_2011",
                                  "F320_x2011", "F321_x2011", "F840_x2011", "F900_x2011")), "2011 file has the right columns")
lt <- readWide(file.path(tmp, "T_allCodes-lifetime"))
ok(sum(lt$F900_lifetime) == 2, "lifetime file: 2 persons with F900")

# Counts
writeCounts(dx, "ICD10Code", tmp, "-C-%s.csv", "-G-%s.csv", genIDs = c("B1", "M1"))
all_ <- fread(file.path(tmp, "TEST-C-allSubjects.csv"))
chld <- fread(file.path(tmp, "TEST-C-Children.csv"))
gen  <- fread(file.path(tmp, "TEST-G-allSubjects.csv"))
ok(all_[ICD10Code == "F900", lifetime] == 2 && all_[ICD10Code == "F900", `2010`] == 1 &&
   all_[ICD10Code == "F900", `2011`] == 2 && all_[ICD10Code == "F900", x2012] == 1, "counts F900, all")
ok(chld[ICD10Code == "F840", lifetime] == 2 && chld[ICD10Code == "F320", lifetime] == 0, "counts, children")
ok(gen[ICD10Code == "F900", lifetime] == 1, "counts, genotyped")

unlink(tmp, recursive = TRUE)
cat("\nDone. If any line says ERROR, check before the real run.\n")
