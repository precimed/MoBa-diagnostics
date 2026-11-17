# Shows that the ICD-10 codes written with an "x" suffix in the application
# (e.g. F89x, R56x) are absent from the NPR raw code files, compared with
# neighbouring codes that were written out in full (F84, F90).
# Output: number of code lines per code and file (aggregated, no person data).
library(data.table)
rawDir <- "/tsd/p697/data/durable/phenotypes/npr/2026-10-19/rawdata"
files <- list.files(rawDir, pattern = "koder", full.names = TRUE)
codes <- c("F84", "F90", "F89", "F88", "R56", "R63", "F89X")
res <- sapply(files, function(f) {
  # latin1 -> UTF-8: one NCMP/NCSP code contains an invalid byte
  k <- toupper(iconv(fread(f, select = "kodeverdi")$kodeverdi, "latin1", "UTF-8"))
  sapply(codes, function(x) sum(startsWith(k, x), na.rm = TRUE))
})
colnames(res) <- sub("\\.csv$", "", basename(files))
res
