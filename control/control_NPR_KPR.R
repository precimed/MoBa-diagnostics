# =============================================================================
# Control of the NPR/KPR delivery of 2026-10-19 (first delivered 2026-10-07) (H-1037-E-E1, MoBa p697)
#
# Runs directly on the raw data files on TSD - no linkage/parsing needed first.
# Answers:
#   1. Did we get all files in the cover letters, with the right number of lines/persons?
#   2. Do the activity/billing files and the code files link up (via 'nokkel')?
#   3. Do the periods ('aar') match the cover letters?
#   4. Did we get the codes we applied for (ICD-10 and ICPC-2)?  <- the main question
#   5. Did we get codes we did NOT apply for?
#   6. (Optional) Have any codes disappeared or dropped sharply compared with the
#      previous delivery (2025-05-15)?
#
# Results are written as TSV files in 'dirOut' + a log (control_log.txt).
# No individual-level data is written - only aggregated numbers per code/file.
# NB: before sharing the tables outside TSD, check the small-number rules in the approval.
#
# Requires: data.table
# =============================================================================

suppressPackageStartupMessages(library(data.table))

# ---- 1. Settings (change if needed) ------------------------------------------
dirNPR      <- "/tsd/p697/data/durable/phenotypes/npr/2026-10-19/rawdata"
dirKPR      <- "/tsd/p697/data/durable/phenotypes/kpr/2026-10-19/rawdata"

# The code lists we applied for (in coverletter_info; the file names contain spaces and commas)
fileICD10   <- "/tsd/p697/data/durable/phenotypes/npr/2026-10-19/coverletter_info/2026-02-13 ICD10 list, MoBaPsychGen_clean.csv"
fileICPC2   <- "/tsd/p697/data/durable/phenotypes/kpr/2026-10-19/coverletter_info/2026-02-13 ICPC-2 list, MoBaPsychGen_clean.csv"

# Previous delivery for comparison; set to NA to skip
dirPrevNPR  <- "/tsd/p697/data/durable/phenotypes/npr/2025-05-15/rawdata"
dirPrevKPR  <- "/tsd/p697/data/durable/phenotypes/kpr/2025-05-15/rawdata"

dirOut      <- file.path(getwd(), paste0("control_NPR_KPR_", Sys.Date()))

# Expected numbers from the cover letters (07.10.2026)
expNPR <- data.table(
  source = c("AVT", "AVT", "PHBU", "PHBU", "PHV", "PHV",
             "REH", "REH", "SOM", "SOM", "TSB", "TSB"),
  type   = rep(c("activity", "codes"), 6),
  exp_persons = c(194035, 161991, 28785, 22081, 40826, 37776,
                  1701, 748, 261733, 243970, 4616, 4307),
  exp_lines   = c(2226425, 2811748, 1522796, 1486008, 1238799, 2392243,
                  4555, 2102, 5084021, 6479336, 195121, 438894))
expNPR_totalPersons <- 271295
expNPR_years        <- c(2008, 2026)   # 01.01.2008 - 30.04.2026

expKPR <- data.table(
  source = "KPR",
  type   = c("billing", "diagnosis", "fee"),
  exp_persons = c(277541, 233879, 233879),
  exp_lines   = c(21984475, 6289578, 13273915))
expKPR_years <- c(2016, 2026)           # 01.07.2016 - 31.08.2026

# Flag if the number of persons with a code drops by more than this vs. the previous delivery
dropThreshold <- 0.90

# ---- 2. Helper functions -----------------------------------------------------
dir.create(dirOut, showWarnings = FALSE, recursive = TRUE)
logFile <- file.path(dirOut, "control_log.txt")
cat("", file = logFile)
logMsg <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  cat(msg, "\n", file = logFile, append = TRUE)
}
section <- function(title) logMsg("\n==== ", title, " ", strrep("=", max(0, 70 - nchar(title))))
writeOut <- function(dt, name) {
  fwrite(dt, file.path(dirOut, name), sep = "\t")
  logMsg("  -> written: ", name, " (", nrow(dt), " rows)")
}

# Normalise codes: upper case, remove dots/spaces/hyphens
normCode <- function(x) gsub("[^A-Z0-9]", "", toupper(trimws(x)))

# Find one file in a folder (pattern, case-insensitive)
findFile <- function(dir, pattern, required = TRUE) {
  f <- list.files(dir, pattern = pattern, ignore.case = TRUE, full.names = TRUE)
  f <- f[!grepl("\\.(zip|7z|gz|txt|pdf|xlsx?)$", f, ignore.case = TRUE)]
  if (length(f) == 0) {
    if (required) logMsg("  !! NO FILE FOUND for pattern '", pattern, "' in ", dir)
    return(NA_character_)
  }
  if (length(f) > 1) logMsg("  !! SEVERAL FILES for pattern '", pattern, "': ",
                            paste(basename(f), collapse = ", "), " - using the first")
  f[1]
}

# Read only the columns we need (names matched case-insensitively).
# Everything is read as character. Returns a data.table with lower-case column names.
readCols <- function(path, wanted) {
  hdr     <- names(fread(path, nrows = 0, sep = "auto"))
  have    <- hdr[tolower(hdr) %in% tolower(wanted)]
  missing <- setdiff(tolower(wanted), tolower(hdr))
  dt <- fread(path, select = have, colClasses = "character", sep = "auto",
              showProgress = FALSE, na.strings = c("", "NA", "NULL"))
  setnames(dt, tolower(names(dt)))
  attr(dt, "all_columns") <- hdr
  attr(dt, "missing")     <- missing
  dt
}

# ---- 3. Read the code lists we applied for ----------------------------------
# The files have a BOM and mixed line endings (CR/CRLF); readLines handles both.
readList <- function(path) {
  con <- file(path, encoding = "UTF-8-BOM"); on.exit(close(con))
  l <- readLines(con, warn = FALSE)
  l[nchar(gsub("[;[:space:]]", "", l)) > 0]
}

section("Requested code lists")

icdRaw <- readList(fileICD10)
icdRaw <- icdRaw[-1]                                    # header
icdReq <- data.table(original = sapply(strsplit(icdRaw, ";"), `[`, 1))
icdReq[, code := normCode(original)]
# A trailing 'x' (F03x, R56x ...) is read as a wildcard = all subcodes
icdReq[, wildcard := grepl("X$", code) & nchar(code) == 4]
icdReq[wildcard == TRUE, code := sub("X$", "", code)]
icdReq <- unique(icdReq, by = "code")
logMsg("ICD-10: ", nrow(icdReq), " requested codes/prefixes (",
       sum(icdReq$wildcard), " with an 'x' wildcard)")
odd <- icdReq[original != trimws(original) | nchar(code) < 3]
if (nrow(odd)) logMsg("  Note: unusual entries in the list (spaces/short prefixes): ",
                      paste0("'", odd$original, "'", collapse = ", "))

icpcRaw   <- readList(fileICPC2)
icpcParts <- strsplit(icpcRaw, ";")
icpcReq <- rbindlist(lapply(icpcParts, function(p) {
  p <- trimws(p)
  if (length(p) >= 3 && grepl("^[A-Z][0-9]{2}-[A-Z][0-9]{2}$", p[3])) {
    # Whole chapter/range, e.g. K01-K99, Z01-Z69
    from <- as.integer(substr(p[3], 2, 3)); to <- as.integer(substr(p[3], 6, 7))
    data.table(code = sprintf("%s%02d", substr(p[3], 1, 1), from:to),
               group = paste0(p[1], " (", p[3], ")"))
  } else if (length(p) >= 3 && grepl("^Enkeltkode", p[3])) {
    data.table(code = normCode(p[1]), group = "Single codes")
  } else NULL
}))
icpcReq <- unique(icpcReq, by = "code")
logMsg("ICPC-2: ", nrow(icpcReq), " requested codes (",
       paste(unique(icpcReq$group), collapse = "; "), ")")

# Map each received code to every requested code it falls under (prefix)
makeMap <- function(received, reqCodes) {
  received <- unique(received[!is.na(received) & received != ""])
  map <- rbindlist(lapply(reqCodes, function(r) {
    m <- received[startsWith(received, r)]
    if (length(m)) data.table(requested = r, code = m) else NULL
  }))
  if (!nrow(map)) map <- data.table(requested = character(), code = character())
  map
}

# ---- 4. File overview --------------------------------------------------------
section("Files in the rawdata folders")
fo <- rbindlist(lapply(c(NPR = dirNPR, KPR = dirKPR), function(d) {
  f <- list.files(d, full.names = TRUE, recursive = TRUE)
  data.table(folder = d, file = basename(f), MB = round(file.size(f) / 1e6, 1))
}))
print(fo); writeOut(fo, "00_file_overview.tsv")
if (any(grepl("\\.(zip|7z|gz)$", fo$file, ignore.case = TRUE)))
  logMsg("  !! There are compressed/encrypted archives in the folder - unpack them before the control.")

# ---- 5. NPR ------------------------------------------------------------------
section("NPR")
sources <- c("AVT", "PHBU", "PHV", "REH", "SOM", "TSB")
nprControl <- list(); nprCodes <- list()

for (k in sources) {
  fA <- findFile(dirNPR, paste0("^", k, ".*aktivitet.*\\.csv$"))
  fK <- findFile(dirNPR, paste0("^", k, ".*koder.*\\.csv$"))
  if (is.na(fA) || is.na(fK)) next
  logMsg("\n", k, ": ", basename(fA), " + ", basename(fK))

  A <- readCols(fA, c("nokkel", "lopenr", "aar", "sektor", "overligger", "errehabilitering"))
  K <- readCols(fK, c("nokkel", "lopenr", "kodenavn", "kodenr", "kodeverdi",
                      "sektor", "akse", "tilstand"))
  if (length(attr(K, "missing")))
    logMsg("  columns not found in the code file: ", paste(attr(K, "missing"), collapse = ", "))

  # Linkage key: 'nokkel' is unique per service area. AVT holds both AVTSOM and
  # AVTPHV in one file -> link on nokkel + sektor if both files have 'sektor'.
  keysA <- intersect(c("nokkel", "sektor"), names(A))
  keysK <- intersect(c("nokkel", "sektor"), names(K))
  keys  <- intersect(keysA, keysK)
  if (k == "AVT") {
    if ("sektor" %in% names(A)) {
      logMsg("  AVT 'sektor' values: ",
             paste(capture.output(print(A[, .N, by = sektor])), collapse = "\n    "))
      dup <- A[, uniqueN(sektor), by = nokkel][V1 > 1, .N]
      logMsg("  AVT: ", dup, " nokkel values occur in more than one sector",
             if (dup > 0 && !("sektor" %in% names(K)))
               " -- !! the code file lacks 'sektor', linking on nokkel alone will be wrong" else "")
    } else logMsg("  !! AVT activity file lacks 'sektor' (expected according to the cover letter)")
  }

  # Persons in the code file: use lopenr where it exists, otherwise via the activity file
  if (!"lopenr" %in% names(K)) {
    K <- merge(K, unique(A[, c(keys, "lopenr"), with = FALSE]), by = keys, all.x = TRUE)
  }

  orphans   <- K[!A, on = keys, .N]           # code lines without an episode
  yearRange <- suppressWarnings(range(as.integer(A$aar), na.rm = TRUE))

  nprControl[[k]] <- rbind(
    data.table(source = k, type = "activity", file = basename(fA),
               lines = nrow(A),
               persons = uniqueN(A$lopenr, na.rm = TRUE),
               year_min = yearRange[1], year_max = yearRange[2],
               code_lines_without_episode = NA_integer_),
    data.table(source = k, type = "codes", file = basename(fK),
               lines = nrow(K),
               persons = uniqueN(K$lopenr, na.rm = TRUE),
               year_min = NA, year_max = NA,
               code_lines_without_episode = orphans))

  if ("kodenavn" %in% names(K))
    logMsg("  code systems (kodenavn): ",
           paste(K[, .N, by = kodenavn][, paste0(kodenavn, "=", N)], collapse = ", "))
  if ("akse" %in% names(K))
    logMsg("  axes (PHBU): ", paste(K[, .N, by = akse][order(akse), paste0(akse, "=", N)],
                                    collapse = ", "))

  # Diagnosis codes: ICD-10 lines (PHBU: all axis codes, axes I-IV)
  isDiag <- if ("kodenavn" %in% names(K)) {
    grepl("ICD", K$kodenavn, ignore.case = TRUE) | k == "PHBU"
  } else rep(TRUE, nrow(K))
  nprCodes[[k]] <- unique(K[isDiag, .(source = k, code = normCode(kodeverdi), lopenr)])
  rm(A, K); gc(verbose = FALSE)
}

nprControl <- merge(rbindlist(nprControl), expNPR, by = c("source", "type"), all = TRUE)
nprControl[, diff_lines   := lines - exp_lines]
nprControl[, diff_persons := persons - exp_persons]
print(nprControl[, .(source, type, lines, exp_lines, diff_lines,
                     persons, exp_persons, diff_persons, year_min, year_max,
                     code_lines_without_episode)])
writeOut(nprControl, "01a_NPR_file_control.tsv")

nprCodes <- rbindlist(nprCodes)
logMsg("NPR: ", uniqueN(nprCodes$lopenr), " unique persons with at least one diagnosis code")

if (nrow(nprControl[!is.na(year_min) & (year_min < expNPR_years[1] | year_max > expNPR_years[2])]))
  logMsg("  !! 'aar' outside the expected period ", paste(expNPR_years, collapse = "-"))

# ---- 6. KPR ------------------------------------------------------------------
section("KPR")
fR <- findFile(dirKPR, "regning.*\\.csv$")
fD <- findFile(dirKPR, "diagnose.*\\.csv$")
fT <- findFile(dirKPR, "takst.*\\.csv$")

kprCodes <- data.table(codeSystem = character(), code = character(), lopenr = character())
if (!is.na(fR) && !is.na(fD)) {
  R <- readCols(fR, c("nokkel", "lopenr", "aar"))
  D <- readCols(fD, c("nokkel", "lopenr", "diagnosetabell", "diagnosekode", "erhoveddiagnose"))
  if (!"lopenr" %in% names(D)) D <- merge(D, unique(R[, .(nokkel, lopenr)]), by = "nokkel", all.x = TRUE)
  yearRange <- suppressWarnings(range(as.integer(R$aar), na.rm = TRUE))

  kprControl <- list(
    data.table(type = "billing",   file = basename(fR), lines = nrow(R),
               persons = uniqueN(R$lopenr, na.rm = TRUE),
               year_min = yearRange[1], year_max = yearRange[2], lines_without_bill = NA_integer_),
    data.table(type = "diagnosis", file = basename(fD), lines = nrow(D),
               persons = uniqueN(D$lopenr, na.rm = TRUE), year_min = NA, year_max = NA,
               lines_without_bill = D[!R, on = "nokkel", .N]))
  if (!is.na(fT)) {
    Tk <- readCols(fT, c("nokkel", "lopenr"))
    if (!"lopenr" %in% names(Tk)) Tk <- merge(Tk, unique(R[, .(nokkel, lopenr)]), by = "nokkel", all.x = TRUE)
    kprControl[[3]] <- data.table(type = "fee", file = basename(fT), lines = nrow(Tk),
                                  persons = uniqueN(Tk$lopenr, na.rm = TRUE),
                                  year_min = NA, year_max = NA,
                                  lines_without_bill = Tk[!R, on = "nokkel", .N])
    rm(Tk)
  }
  kprControl <- merge(rbindlist(kprControl), expKPR, by = "type", all = TRUE)
  kprControl[, diff_lines   := lines - exp_lines]
  kprControl[, diff_persons := persons - exp_persons]
  print(kprControl)
  writeOut(kprControl, "01b_KPR_file_control.tsv")

  logMsg("  diagnosetabell: ", paste(D[, .N, by = diagnosetabell][, paste0(diagnosetabell, "=", N)],
                                     collapse = ", "))
  if ("erhoveddiagnose" %in% names(D))
    logMsg("  erHoveddiagnose: ", paste(D[, .N, by = erhoveddiagnose][, paste0(erhoveddiagnose, "=", N)],
                                        collapse = ", "))
  logMsg("  bills per year:\n    ",
         paste(capture.output(print(R[, .(bills = .N, persons = uniqueN(lopenr)), by = aar][order(aar)])),
               collapse = "\n    "))
  if (yearRange[1] < expKPR_years[1] || yearRange[2] > expKPR_years[2])
    logMsg("  !! 'aar' outside the expected period ", paste(expKPR_years, collapse = "-"))

  D[, codeSystem := fifelse(grepl("ICPC", diagnosetabell, ignore.case = TRUE), "ICPC-2",
                    fifelse(grepl("ICD",  diagnosetabell, ignore.case = TRUE), "ICD-10",
                            as.character(diagnosetabell)))]
  kprCodes <- unique(D[, .(codeSystem, code = normCode(diagnosekode), lopenr)])
  rm(R, D); gc(verbose = FALSE)
}

# ---- 7. Previous delivery (optional) -----------------------------------------
readPrevNPR <- function(dir) {
  if (is.na(dir) || !dir.exists(dir)) return(NULL)
  rbindlist(lapply(sources, function(k) {
    fK <- findFile(dir, paste0("^", k, ".*koder.*\\.csv$"), required = FALSE)
    fA <- findFile(dir, paste0("^", k, ".*aktivitet.*\\.csv$"), required = FALSE)
    if (is.na(fK)) return(NULL)
    K <- readCols(fK, c("nokkel", "lopenr", "kodenavn", "kodeverdi"))
    if (!"lopenr" %in% names(K) && !is.na(fA)) {
      A <- readCols(fA, c("nokkel", "lopenr"))
      K <- merge(K, unique(A), by = "nokkel", all.x = TRUE)
    }
    isDiag <- if ("kodenavn" %in% names(K)) grepl("ICD", K$kodenavn, ignore.case = TRUE) | k == "PHBU" else TRUE
    unique(K[isDiag, .(code = normCode(kodeverdi), lopenr)])
  }))
}
readPrevKPR <- function(dir) {
  if (is.na(dir) || !dir.exists(dir)) return(NULL)
  fD <- findFile(dir, "diagnose.*\\.csv$", required = FALSE); fR <- findFile(dir, "regning.*\\.csv$", required = FALSE)
  if (is.na(fD)) return(NULL)
  D <- readCols(fD, c("nokkel", "lopenr", "diagnosetabell", "diagnosekode"))
  if (!"lopenr" %in% names(D) && !is.na(fR)) D <- merge(D, unique(readCols(fR, c("nokkel", "lopenr"))), by = "nokkel", all.x = TRUE)
  D[, codeSystem := fifelse(grepl("ICPC", diagnosetabell, ignore.case = TRUE), "ICPC-2",
                    fifelse(grepl("ICD", diagnosetabell, ignore.case = TRUE), "ICD-10", as.character(diagnosetabell)))]
  unique(D[, .(codeSystem, code = normCode(diagnosekode), lopenr)])
}
section("Previous delivery")
prevNPR <- readPrevNPR(dirPrevNPR)
prevKPR <- readPrevKPR(dirPrevKPR)
logMsg("Previous NPR read: ", !is.null(prevNPR), " | previous KPR read: ", !is.null(prevKPR))

# ---- 8. Requested vs. received -----------------------------------------------
# Number of persons per requested code/prefix (a person is counted once per prefix)
personsPerRequested <- function(dat, reqCodes, name) {
  res <- data.table(requested = reqCodes, V = 0L)
  if (!is.null(dat) && nrow(dat)) {
    map <- makeMap(dat$code, reqCodes)
    if (nrow(map)) {
      n <- merge(dat[, .(code, lopenr)], map, by = "code", allow.cartesian = TRUE)[
        , .(N = uniqueN(lopenr, na.rm = TRUE)), by = requested]
      res[n, V := i.N, on = "requested"]
    }
  }
  setnames(res, "V", name)
  res
}

section("ICD-10: requested vs. received")
icd <- copy(icdReq[, .(requested = code, original, wildcard)])
for (k in sources)
  icd <- merge(icd, personsPerRequested(nprCodes[source == k], icd$requested, paste0("NPR_", k)), by = "requested")
icd <- merge(icd, personsPerRequested(nprCodes, icd$requested, "NPR_total"), by = "requested")
icd <- merge(icd, personsPerRequested(kprCodes[codeSystem == "ICD-10"], icd$requested, "KPR"), by = "requested")
if (!is.null(prevNPR)) {
  icd <- merge(icd, personsPerRequested(prevNPR, icd$requested, "NPR_previous"), by = "requested")
  icd[, NPR_ratio := round(NPR_total / pmax(NPR_previous, 1), 2)]
}
# Exact code received? (for prefixes such as F10, the subcodes are counted above)
receivedNPR <- unique(nprCodes$code)
icd[, exact_code_in_NPR := requested %in% receivedNPR]
icd[, status := fifelse(NPR_total == 0 & KPR == 0, "NO PERSONS", "ok")]
if (!is.null(prevNPR)) {
  icd[NPR_previous > 0 & NPR_total == 0, status := "GONE (present in previous delivery)"]
  icd[NPR_previous >= 20 & NPR_ratio < dropThreshold & NPR_total > 0,
      status := paste0("DROP vs previous (<", dropThreshold, ")")]
  icd[NPR_previous == 0 & NPR_total > 0, status := "NEW (not in previous delivery)"]
}
setorder(icd, requested)
writeOut(icd, "02_ICD10_requested_vs_received.tsv")
logMsg("  status: ", paste(icd[, .N, by = status][, paste0(status, "=", N)], collapse = ", "))
logMsg("  Requested ICD-10 codes without a single person (NPR+KPR):")
logMsg("    ", paste(icd[status == "NO PERSONS", requested], collapse = " "))

section("ICPC-2: requested vs. received (KPR)")
icpc <- merge(icpcReq[, .(requested = code, group)],
              personsPerRequested(kprCodes[codeSystem == "ICPC-2"], icpcReq$code, "KPR"), by = "requested")
if (!is.null(prevKPR)) {
  icpc <- merge(icpc, personsPerRequested(prevKPR[codeSystem == "ICPC-2"], icpc$requested, "KPR_previous"), by = "requested")
  icpc[, KPR_ratio := round(KPR / pmax(KPR_previous, 1), 2)]
}
setorder(icpc, requested)
writeOut(icpc, "03_ICPC2_requested_vs_received.tsv")
summ <- icpc[, .(codes = .N, codes_with_persons = sum(KPR > 0), persons_sum = sum(KPR)), by = group]
logMsg(paste(capture.output(print(summ)), collapse = "\n"))
logMsg("  NB: ICPC-2 process codes (-30 to -69) are rarely used as diagnoses; zeros there are expected.")
logMsg("  Requested ICPC-2 diagnosis/symptom codes (01-29, 70-99) without persons: ",
       paste(icpc[KPR == 0 & !(as.integer(substr(requested, 2, 3)) %between% c(30, 69)), requested], collapse = " "))

# ---- 9. Received codes that were NOT requested -------------------------------
section("Received codes not covered by the application")
isCovered <- function(codes, req) {
  vapply(codes, function(x) any(startsWith(x, req)), logical(1))
}
notRequested <- rbind(
  nprCodes[, .(persons = uniqueN(lopenr)), by = .(source, code)][
    , .(register = paste0("NPR_", source), codeSystem = "ICD-10", code, persons)],
  kprCodes[, .(persons = uniqueN(lopenr)), by = .(codeSystem, code)][
    , .(register = "KPR", codeSystem, code, persons)])
notRequested <- notRequested[!is.na(code) & code != ""]
notRequested[codeSystem == "ICD-10", covered := isCovered(code, icdReq$code)]
notRequested[codeSystem == "ICPC-2", covered := isCovered(substr(code, 1, 3), icpcReq$code)]
notRequested <- notRequested[is.na(covered) | covered == FALSE][order(register, -persons)]
writeOut(notRequested, "04_codes_not_requested.tsv")
if (nrow(notRequested))
  logMsg("  !! ", nrow(notRequested), " received codes are not covered by the code lists - see file 04 ",
         "(may be legitimate variants, e.g. Norwegian national codes, but should be reviewed)")

section("Done")
logMsg("Results in: ", dirOut)
