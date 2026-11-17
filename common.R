# =============================================================================
# Shared functions for NPR/KPR parsing (added 2026-10, following Pravesh's suggestions)
#
#  * makeWideFiles(): writes the 0/1 coded tables (one row per person) as parquet.
#    Column names are the same as before:
#      <code>_lifetime  = the person has the diagnosis at least once
#      <code>_<year>    = cumulative: the diagnosis was given in or before this year
#      <code>_x<year>   = cross-sectional: the diagnosis was given in this year
#    The table is built in blocks of persons, so memory use stays below 'maxCells'
#    cells at a time (see 00_config.R). A table that needs more than one block is
#    written as a folder with part-001.parquet, part-002.parquet, ...
#    Read it with arrow::open_dataset(<folder>).
#
#  * writeCounts(): number of persons per code (lifetime, cumulative per year,
#    cross-sectional per year) for all, children, mothers and fathers, and for the
#    genotyped. Same file names and columns as the old doCounts scripts, but counted
#    directly from the long diagnosis file instead of the wide files.
#
# Expected input 'dx': data.table with the columns
#   ID_2445, Role, PREG_ID_2445, ChildNumber, WithdrawnConsent18years, Year, Code
# =============================================================================

suppressPackageStartupMessages(library(data.table))

idColumns <- c("ID_2445", "Role", "PREG_ID_2445", "ChildNumber", "WithdrawnConsent18years")

# ---- Parquet: use arrow if installed, otherwise nanoparquet ------------------
parquetPackage <- function() {
  if (requireNamespace("arrow", quietly = TRUE))       return("arrow")
  if (requireNamespace("nanoparquet", quietly = TRUE)) return("nanoparquet")
  stop("Neither the R package 'arrow' nor 'nanoparquet' was found.\n",
       "Install one of them, e.g. install.packages(\"nanoparquet\") (quick to install), ",
       "and run the step again.")
}
writeParquet <- function(x, file) {
  if (parquetPackage() == "arrow") arrow::write_parquet(x, file) else nanoparquet::write_parquet(x, file)
}

# ---- Wide 0/1 tables ---------------------------------------------------------
# persons: unique persons (idColumns) with the column 'pid' = row number
# fy:      one row per (pid, ci) with the first year 'first'   (ci = code index)
# cy:      one row per (pid, ci, Year)
buildWide <- function(persons, fy, cy, codes, yrs, lifetime, cum, cross, path, maxCells) {
  nC <- length(codes); nY <- length(yrs); nP <- nrow(persons)
  colNames <- c(if (lifetime) paste0(codes, "_lifetime"),
                if (cum)      paste0(rep(codes, each = nY), "_",  yrs),
                if (cross)    paste0(rep(codes, each = nY), "_x", yrs))
  offCum   <- if (lifetime) nC else 0L
  offCross <- offCum + if (cum) nC * nY else 0L
  nCol     <- length(colNames)
  chunk    <- max(1L, as.integer(floor(maxCells / nCol)))
  nChunk   <- ceiling(nP / chunk)

  if (nChunk > 1) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    unlink(list.files(path, pattern = "^part-.*\\.parquet$", full.names = TRUE))
  }
  for (k in seq_len(nChunk)) {
    r1 <- (k - 1L) * chunk + 1L; r2 <- min(k * chunk, nP); r0 <- r1 - 1L
    m  <- matrix(0L, nrow = r2 - r1 + 1L, ncol = nCol)
    f  <- fy[pid >= r1 & pid <= r2]
    if (lifetime && nrow(f)) m[cbind(f$pid - r0, f$ci)] <- 1L
    if (cum) for (j in seq_len(nY)) {
      s <- f[first <= yrs[j]]
      if (nrow(s)) m[cbind(s$pid - r0, offCum + (s$ci - 1L) * nY + j)] <- 1L
    }
    if (cross) {
      s <- cy[pid >= r1 & pid <= r2 & Year %in% yrs]
      if (nrow(s)) m[cbind(s$pid - r0, offCross + (s$ci - 1L) * nY + match(s$Year, yrs))] <- 1L
    }
    df <- as.data.frame(m); rm(m)
    names(df) <- colNames
    out <- cbind(persons[r1:r2, ..idColumns], df); rm(df)
    file <- if (nChunk > 1) file.path(path, sprintf("part-%03d.parquet", k)) else paste0(path, ".parquet")
    writeParquet(out, file); rm(out); invisible(gc())
  }
  cat(sprintf("  written: %s%s  (%d persons x %d code columns, %d part(s))\n",
              basename(path), if (nChunk > 1) "/" else ".parquet", nP, nCol, nChunk))
}

# dx: see top. prefix: e.g. file.path(dirOut, paste0(runTag, "-MoBa-ParsedNPR"))
# names: "allCodes" (all codes as given) and optionally "majorCodes" (first three characters)
makeWideFiles <- function(dx, prefix, major = FALSE, fullMinor = TRUE, maxCells = 1.5e9,
                          nameMinor = "allCodes", nameMajor = "majorCodes") {
  invisible(parquetPackage())
  dx <- copy(dx)
  dx[, Year := as.integer(Year)]
  # One row per unique combination of the ID columns (as in the original script), sorted by ID
  persons <- unique(dx[, ..idColumns])
  setorderv(persons, "ID_2445")
  persons[, pid := .I]
  key <- function(d) do.call(paste, c(as.list(d[, ..idColumns]), sep = "\r"))
  dx[, pid := match(key(dx), key(persons))]
  stopifnot(!anyNA(dx$pid))
  yrs <- sort(unique(dx$Year))

  make <- function(code, name, full) {
    codes <- sort(unique(code))
    d  <- data.table(pid = dx$pid, ci = match(code, codes), Year = dx$Year)
    fy <- d[, .(first = min(Year)), by = .(pid, ci)]
    cy <- unique(d)
    base <- paste0(prefix, "_", name)
    # lifetime: one file
    buildWide(persons, fy, cy, codes, yrs, TRUE, FALSE, FALSE, paste0(base, "-lifetime"), maxCells)
    # one year at a time (cumulative + cross-sectional for that year), like the old splitInfo files
    for (y in yrs)
      buildWide(persons, fy, cy, codes, y, FALSE, TRUE, TRUE, paste0(base, "-", y), maxCells)
    # the full table (lifetime + all years)
    if (full) {
      buildWide(persons, fy, cy, codes, yrs, TRUE, TRUE, TRUE, base, maxCells)
    } else {
      cat("  ", basename(base), ": full table not made (fullMinor = FALSE); use the -lifetime and per-year files\n", sep = "")
    }
  }
  make(dx$Code, nameMinor, fullMinor)
  if (major) make(substr(dx$Code, 1, 3), nameMajor, TRUE)
}

# ---- Counts ------------------------------------------------------------------
countTable <- function(dx, codeName, codes, yrs) {
  if (!nrow(dx)) dx <- data.table(ID_2445 = character(), Code = character(), Year = integer())
  fy  <- if (nrow(dx)) dx[, .(first = min(Year)), by = .(ID_2445, Code)] else
           data.table(ID_2445 = character(), Code = character(), first = integer())
  cy  <- unique(dx[, .(ID_2445, Code, Year)])
  out <- data.table(Code = codes)
  cols <- c("lifetime", as.character(yrs), paste0("x", yrs))
  for (k in cols) set(out, j = k, value = 0L)
  update <- function(n, k) if (nrow(n)) out[n, (k) := i.N, on = "Code"]
  update(fy[, .N, by = Code], "lifetime")
  for (y in yrs) update(fy[first <= y, .N, by = Code], as.character(y))
  for (y in yrs) update(cy[Year == y,  .N, by = Code], paste0("x", y))
  setnames(out, "Code", codeName)
  out
}

# filePattern: e.g. "-MoBa-NPRCounts_allCodes-%s.csv" (%s = allSubjects/Children/Mothers/Fathers)
writeCounts <- function(dx, codeName, dirOut, filePattern, filePatternGen, genIDs) {
  dir.create(dirOut, showWarnings = FALSE, recursive = TRUE)
  dx <- copy(dx); dx[, Year := as.integer(Year)]
  if (any(dx[, uniqueN(Role), by = ID_2445]$V1 > 1)) warning("Some IDs have more than one role")
  codes <- sort(unique(dx$Code)); yrs <- sort(unique(dx$Year))   # same rows/columns in all tables
  groups <- list(allSubjects = NULL, Children = "Child", Mothers = "Mother", Fathers = "Father")
  for (set_ in list(list(d = dx, pattern = filePattern),
                    list(d = dx[ID_2445 %in% genIDs], pattern = filePatternGen))) {
    tabs <- lapply(groups, function(r)
      countTable(if (is.null(r)) set_$d else set_$d[Role == r], codeName, codes, yrs))
    # check: all = children + mothers + fathers
    roleRows <- rbindlist(tabs[-1])
    sum3     <- roleRows[, lapply(.SD, sum, na.rm = TRUE), by = c(codeName)]
    check    <- merge(tabs$allSubjects, sum3, by = codeName, suffixes = c("", ".sum"))
    cols     <- setdiff(names(tabs$allSubjects), codeName)
    nDiff    <- sum(sapply(cols, function(k) sum(check[[k]] != check[[paste0(k, ".sum")]])))
    if (nDiff > 0) warning("All != children + mothers + fathers in ", nDiff, " cells (", set_$pattern, ")")
    for (g in names(tabs)) {
      file <- file.path(dirOut, paste0(runTag, sprintf(set_$pattern, g)))
      fwrite(tabs[[g]], file, sep = "\t")
    }
  }
  cat("  counts written to", dirOut, "\n")
}

# Genotyped persons (as in the old doCounts scripts)
readGenIDs <- function() {
  f <- file.path(dirGenetics, "MoBaPsychGen_v1-ec-eur-batch-basic-qc-cov.txt")
  if (!file.exists(f)) { warning("Could not find ", f, " - the genetics counts will be empty"); return(character()) }
  as.character(fread(f, sep = "\t", select = "ID_2445", colClasses = "character")$ID_2445)
}
