# =============================================================================
# Compare the NPR/KPR delivery of 2026-10-19 with the counts from the previous
# release (Pravesh Parekh, 2025-05: NPR majorCodes, KPR ICPC and ICD codes).
# For every year and role: cumulative (<year>) and cross-sectional (x<year>)
# number of persons per code, restricted to codes present in the old release.
# Also: yearly totals 2020-2026 in the new release (2026 = Jan-Aug).
# Writes aggregated tables only (QC_*.csv), no person-level data.
# =============================================================================
library(data.table); library(haven)

dirNPR  <- "/tsd/p697/data/durable/phenotypes/npr/2026-10-19/"
dirKPR  <- "/tsd/p697/data/durable/phenotypes/kpr/2026-10-19/"
dirSV   <- "/tsd/p697/data/durable/phenotypes/sv_infofiles/"
oldNPR  <- "/ess/p697/cluster/users/parekh/2025-05-21_NPR/counts"
oldKPR  <- "/ess/p697/cluster/users/parekh/2025-05-22_KPR/counts"
lastOldYear <- 2024   # last complete year in the old release

# Same code clean-up as parseNPR.R / parseKPR.R
clean <- function(x) toupper(sub("\\?", "", sub("\\.", "", x)))
roles <- c(Bm = "Children", Mm = "Mothers", Fm = "Fathers")

# ---- Consented persons (current SV_INFO + linkage files) ----------------------
sv   <- as.data.table(read_sav(paste0(dirSV, "PDB2445_SV_INFO_V12_20260618.sav")))
barn <- as.data.table(read_sav(paste0(dirSV, "Barn_ID_2445_20260305.sav")))
mor  <- as.data.table(read_sav(paste0(dirSV, "Mor_ID_2445_20260305.sav")))
far  <- as.data.table(read_sav(paste0(dirSV, "Far_ID_2445_20260305.sav")))
ok <- c(barn[PREG_ID_2445 %in% sv$PREG_ID_2445, PID_2445],
        mor[M_ID_2445 %in% sv$M_ID_2445, PID_2445],
        far[F_ID_2445 %in% sv$F_ID_2445, PID_2445])

# ---- New NPR: person x 3-character code x year ---------------------------------
rawNPR <- paste0(dirNPR, "rawdata/")
npr <- rbindlist(lapply(c("AVT", "PHBU", "PHV", "REH", "SOM", "TSB"), function(r) {
  k <- fread(list.files(rawNPR, paste0("^", r, "koder"),     full.names = TRUE))
  a <- fread(list.files(rawNPR, paste0("^", r, "aktivitet"), full.names = TRUE),
             select = c("lopenr", "nokkel", "aar"))
  if (r != "PHBU") k <- k[kodenavn == "ICD-10"]          # PHBU: all axes
  merge(k[, .(lopenr, nokkel, kode = clean(kodeverdi))], unique(a), by = c("lopenr", "nokkel"))
}))
npr <- unique(npr[kode != "" & lopenr %in% ok, .(lopenr, kode = substr(kode, 1, 3), aar)])
npr[, rolle := roles[substr(lopenr, 1, 2)]]

# ---- New KPR: person x full code x year (year from regning) --------------------
rawKPR <- paste0(dirKPR, "rawdata/")
kd <- fread(paste0(rawKPR, "diagnose.csv"))
kr <- unique(fread(paste0(rawKPR, "regning.csv"), select = c("lopenr", "nokkel", "aar")))
stopifnot(all(paste(kd$lopenr, kd$nokkel) %in% paste(kr$lopenr, kr$nokkel)))
kpr <- merge(kd[, .(lopenr, nokkel, tabell = diagnosetabell,
                    kode = toupper(sub("\\.", "", diagnosekode)))], kr, by = c("lopenr", "nokkel"))
kpr[, type := fifelse(tabell %in% c("ICPC-2", "ICPC-2B"), "ICPC", "ICD")]
kpr <- unique(kpr[lopenr %in% ok & kode != "", .(lopenr, type, kode, aar)])
kpr[, rolle := roles[substr(lopenr, 1, 2)]]

# ---- Old counts ------------------------------------------------------------------
readOld <- function(dir, pattern) {
  f <- tail(sort(list.files(dir, pattern, full.names = TRUE)), 1)
  g <- fread(f, sep = "\t"); setnames(g, 1, "kode"); g[, kode := as.character(kode)]; g
}
oldFile <- function(reg, type, rolle)
  if (reg == "NPR") readOld(oldNPR, paste0("-MoBa-NPRCounts_majorCodes-", rolle, ".csv$"))
  else              readOld(oldKPR, paste0("-MoBa-KPRCounts_all", type, "Codes-", rolle, ".csv$"))

# ---- Comparison per register x code set x role x year ---------------------------
compare <- function(reg, type, role, y, new) {
  g <- oldFile(reg, type, role)[, .(kode, gml = get(as.character(y)), gml_x = get(paste0("x", y)))]
  d <- new[rolle == role]
  res <- merge(g, merge(d[aar <= y, .(ny = uniqueN(lopenr)), by = kode],
                        d[aar == y, .(ny_x = uniqueN(lopenr)), by = kode], by = "kode", all = TRUE),
               by = "kode", all = TRUE)
  for (j in c("gml", "gml_x", "ny", "ny_x")) set(res, which(is.na(res[[j]])), j, 0L)
  res[, `:=`(register = reg, type = type, rolle = role, aar = y,
             pct   = round(100 * (ny - gml) / pmax(gml, 1), 1),
             pct_x = round(100 * (ny_x - gml_x) / pmax(gml_x, 1), 1))]
}

runCompare <- function(reg, type, new, years) {
  rbindlist(lapply(c("Children", "Mothers", "Fathers"), function(r)
    rbindlist(lapply(years, function(y) compare(reg, type, r, y, new)))))
}

yearsNPR <- 2008:lastOldYear
yearsKPR <- 2016:lastOldYear
all <- rbind(runCompare("NPR", "ICD",  npr,                 yearsNPR),
             runCompare("KPR", "ICPC", kpr[type == "ICPC"], yearsKPR),
             runCompare("KPR", "ICD",  kpr[type == "ICD"],  yearsKPR))

newCodes <- all[aar == lastOldYear & gml == 0 & ny > 0]   # codes not in the old release
all      <- all[gml > 0 | gml_x > 0]                       # codes in both releases

overview <- all[, .(gml_kum = sum(gml), ny_kum = sum(ny),
                    pct_kum = round(100 * (sum(ny) - sum(gml)) / sum(gml), 2),
                    gml_x = sum(gml_x), ny_x = sum(ny_x),
                    pct_x = round(100 * (sum(ny_x) - sum(gml_x)) / pmax(sum(gml_x), 1), 2)),
                by = .(register, type, rolle, aar)][order(register, type, rolle, aar)]
deviations  <- all[(gml >= 20 & abs(pct) > 2) | (gml_x >= 20 & abs(pct_x) > 2)]
disappeared <- all[(gml > 0 & ny == 0) | (gml_x > 0 & ny_x == 0)]

print(overview, nrows = Inf)
print(deviations[order(-abs(pct))][1:30])
print(disappeared)
print(newCodes[, .N, by = .(register, type, rolle)])

# ---- Yearly totals in the new release (persons x codes), 2020-2026 --------------
yearly <- function(d, by) rbindlist(lapply(2020:2026, function(y)
  d[, .(aar = y, kum = uniqueN(paste(lopenr, kode)[aar <= y]),
        x = uniqueN(paste(lopenr, kode)[aar == y])), by = by]))
yNPR <- yearly(npr, "rolle")
yKPR <- yearly(kpr, c("type", "rolle"))
# Ratio 2026/2025 (cross-sectional); 2026 covers Jan-Aug
print(rbind(yNPR[, .(kilde = "NPR", ratio_26_25 = round(x[aar == 2026] / x[aar == 2025], 2)), by = rolle],
            yKPR[type == "ICPC", .(kilde = "KPR-ICPC", ratio_26_25 = round(x[aar == 2026] / x[aar == 2025], 2)), by = rolle]))

# ---- Save (aggregated tables only) ------------------------------------------------
fwrite(overview[register == "NPR"],    paste0(dirNPR, "QC_NPR_2025_vs_2026_per_aar.csv"))
fwrite(deviations[register == "NPR"],  paste0(dirNPR, "QC_NPR_2025_vs_2026_avvik.csv"))
fwrite(yNPR,                           paste0(dirNPR, "QC_NPR_2026_aarlige_tall_2020-2026.csv"))
fwrite(overview[register == "KPR"],    paste0(dirKPR, "QC_KPR_2025_vs_2026_per_aar.csv"))
fwrite(deviations[register == "KPR"],  paste0(dirKPR, "QC_KPR_2025_vs_2026_avvik.csv"))
fwrite(yKPR,                           paste0(dirKPR, "QC_KPR_2026_aarlige_tall_2020-2026.csv"))
