# =============================================================================
# QC runde 3: kontaktfilene ParsedNPR.csv og ParsedKPR-All.csv (alle variabler).
# Kjør i R-konsollen:  source(".../qc3_kontaktfiler_2026-11-04.R")  (ca. 15-30 min, mye minne)
#
# N  Nøkler: er 'nokkel' unik i hver aktivitets-/regningsfil? Har kode-/diagnoserader
#    samme lopenr som aktiviteten/regningen de kobles til? (Skriptene kobler kun på nokkel.)
# O  Antall rader per kilde og år: parset mot rådata (koblet riktig på lopenr+nokkel,
#    kun personer med samtykke).
# P  Hver variabel per kilde: antall ikke-tomme, antall unike verdier og sum (tall)
#    i parset mot rådata.
# Q  Plausibilitet: varighet >= 0, utdato >= inndato, kodeformat for prosedyrer.
# R  ALLE brede årsfiler (NPR major/minor 2008-2026, KPR ICPC/ICD10 2016-2026) celle for celle
# S  SmallestDiffDate-allCodes.csv mot ParsedNPR.csv (første inndato per person og kode)
# V  Rad-for-rad: alle variabler i hver rad (lest som tekst) i parset mot rådata
# W  ALLE filer i counts/ (NPR og KPR, alle roller, med/uten genetikk) celle for celle
# T  Deltakerlister (KeepList/WithdrawnList) mot SV_INFO
# Skriver kun samletall - ingen ID-er.
# =============================================================================
library(data.table); library(haven); library(nanoparquet)
options(scipen = 999)

P     <- "/tsd/p697/data/durable/phenotypes/"
SV    <- paste0(P, "sv_infofiles/")
NPRP  <- paste0(P, "npr/2026-10-19/parsed/")
KPRP  <- paste0(P, "kpr/2026-10-19/parsed/")
NPRR  <- paste0(P, "npr/2026-10-19/rawdata/")
KPRR  <- paste0(P, "kpr/2026-10-19/rawdata/")
TAG   <- "2026-11-04_SV-V12_20260618-MoBa-"
LINK  <- "20260605"

RES <- list()
add <- function(gruppe, sjekk, verdi, krav, status)
  RES[[length(RES) + 1]] <<- data.table(gruppe, sjekk, verdi = as.character(verdi), krav, status)
run <- function(navn, expr) { cat(format(Sys.time(), "%H:%M:%S"), "sjekk", navn, "\n"); flush.console()
  tryCatch(expr, error = function(e) {
    add("FEIL", navn, conditionMessage(e), "-", "AVVIK"); message("FEIL i ", navn, ": ", conditionMessage(e)) }) }
okif <- function(x) ifelse(isTRUE(x), "OK", "AVVIK")
vis  <- function(tittel, d) { if (nrow(d)) { cat("\n--", tittel, "--\n"); print(d, nrows = 40) } }

# ---- Kobling og samtykke (som i parsingen) -----------------------------------
readLink <- function(role) {
  f <- list.files(SV, paste0("^", role, "_ID_.*", LINK, "\\.sav$"), full.names = TRUE)
  d <- as.data.table(read_sav(f)); d[, lopenr := as.character(PID_2445)][]
}
barn <- readLink("Barn"); mor <- readLink("Mor"); far <- readLink("Far")
svi  <- as.data.table(read_sav(paste0(SV, "PDB2445_SV_INFO_V12_20260618.sav")))
okLop <- c(barn[as.character(PREG_ID_2445) %in% as.character(svi$PREG_ID_2445), lopenr],
           mor[as.character(M_ID_2445) %in% as.character(svi$M_ID_2445), lopenr],
           far[as.character(F_ID_2445) %in% as.character(svi$F_ID_2445), lopenr])
# Personer i parsede filer (for å bruke samme utvalg som parsingen, inkl. WithdrawnList)
idmap <- rbind(barn[, .(lopenr, ID = paste0(PREG_ID_2445, "_", BARN_NR))],
               mor[, .(lopenr, ID = as.character(M_ID_2445))], far[, .(lopenr, ID = as.character(F_ID_2445))])

# Sammenlign variabler (antall ikke-tomme, antall unike, sum) per gruppe
profil <- function(d, vars, by) {
  rbindlist(lapply(intersect(vars, names(d)), function(v) {
    x <- d[, .(var = v,
               n_ikke_tom = sum(!is.na(get(v)) & as.character(get(v)) != ""),
               n_unike    = uniqueN(get(v)[!is.na(get(v)) & as.character(get(v)) != ""]),
               sum        = if (is.numeric(get(v))) sum(as.numeric(get(v)), na.rm = TRUE) else NA_real_),
           by = by]
  }))
}
cmp_profil <- function(navn, raw, par, map, by) {
  pr <- profil(raw, names(map), by); pr[, var := map[var]]
  pp <- profil(par, unname(map), by)
  m  <- merge(pr, pp, by = c(by, "var"), all = TRUE, suffixes = c("_raa", "_par"))
  m[, avvik := !(n_ikke_tom_raa %in% n_ikke_tom_par) | !(n_unike_raa %in% n_unike_par) |
               !(is.na(sum_raa) & is.na(sum_par) | abs(sum_raa - sum_par) <= 1e-6 * pmax(1, abs(sum_raa)))]
  for (v in unique(m$var)) {
    mm <- m[var == v]
    add("P Variabler", sprintf("%s: %s (antall %s med avvik)", navn, v, by), mm[avvik == TRUE, .N], "0", okif(mm[avvik == TRUE, .N] == 0))
  }
  vis(paste(navn, "- variabler med avvik"), m[avvik == TRUE])
}

# =============================================================================
# NPR
# =============================================================================
npr_map <- c(aar = "Year", diffdager_inndato = "DiffDays_Admission", diffdager_utdato = "DiffDays_Discharge",
             omsorgsniva = "Somatic_LevelofCare", behandlingsniva3 = "Somatic_LevelofCare_2008_2009",
             behandlingssted2 = "Somatic_TreatmentClinic_Derived", behandlingsstedkode = "Somatic_TreatmentClinic_Reported",
             henvtype = "ReferralType", henvvurd = "ReferralDecision", institusjonid = "InstitutionID",
             institusjonid_pseud = "InstitutionID_Psuedoanonymized", psykinstid = "InstitutionID_MentalHealth",
             kodenavn = "ManualName", kodetype = "ManualType", akse = "Diagnosis_Axis", tilstand = "ChildCodeSequence",
             errehabilitering = "REH_partOf_SOM", overligger = "Unfinished_WardStay", sektor = "AVT_Sector",
             kodenr = "Original_kodenr", kodeverdi = "Original_kodeverdi")

cat("Leser NPR rådata ...\n")
raw_npr <- rbindlist(lapply(c("AVT", "PHBU", "PHV", "REH", "SOM", "TSB"), function(r) {
  a <- fread(list.files(NPRR, paste0("^", r, "aktivitet"), full.names = TRUE), colClasses = list(character = c("lopenr", "nokkel")))
  k <- fread(list.files(NPRR, paste0("^", r, "koder"),     full.names = TRUE), colClasses = list(character = c("lopenr", "nokkel")))
  setnames(a, tolower(names(a))); setnames(k, tolower(names(k)))
  # N: nøkler
  run(paste0("N-", r), {
    nd <- a[, .N, by = nokkel][N > 1, .N]
    add("N Nøkler", sprintf("NPR %s: nokkel som forekommer mer enn én gang i aktivitetsfila", r), nd, "0", okif(nd == 0))
    if ("sektor" %in% names(a)) {
      ns <- a[, uniqueN(sektor), by = nokkel][V1 > 1, .N]
      add("N Nøkler", sprintf("NPR %s: nokkel som finnes i flere sektorer (AVTSOM/AVTPHV)", r), ns, "0", okif(ns == 0))
    }
    j  <- merge(k[, .(nokkel, lop_k = lopenr)], unique(a[, .(nokkel, lop_a = lopenr)]), by = "nokkel")
    nm <- j[lop_k != lop_a, .N]
    add("N Nøkler", sprintf("NPR %s: koderader der lopenr i kodefil != lopenr i aktivitet (kobling på nokkel)", r), nm, "0", okif(nm == 0))
    nu <- k[!nokkel %in% a$nokkel, .N]
    add("N Nøkler", sprintf("NPR %s: koderader uten aktivitet", r), nu, "0", okif(nu == 0))
  })
  k[, lopenr := NULL]
  d <- merge(a, k, by = "nokkel", all = TRUE)        # samme kobling som skriptet, men lopenr fra aktivitet
  d[, SourceTableName := r][]
}), fill = TRUE)
raw_npr <- raw_npr[lopenr %in% okLop]

cat("Leser ParsedNPR.csv ...\n")
par_npr <- fread(paste0(NPRP, TAG, "ParsedNPR.csv"))
idsPar  <- unique(par_npr[, { v <- function(x) { x <- as.character(x); x[!is.na(x) & x == ""] <- NA; x }; fcoalesce(v(ID_2445), v(M_ID_2445), v(F_ID_2445)) }])
# Bruk samme personutvalg som parset (WithdrawnList/ugyldige FNR fjernes i parseNPR)
raw_npr <- merge(raw_npr, idmap, by = "lopenr")[ID %in% idsPar]
# AVT: utdato fylles med inndato i skriptet
raw_npr[SourceTableName == "AVT" & is.na(diffdager_utdato), diffdager_utdato := diffdager_inndato]

run("O-NPR", {
  rr <- raw_npr[, .(raa = .N), by = .(SourceTableName, aar)]
  pp <- par_npr[, .(par = .N), by = .(SourceTableName, aar = Year)]
  m  <- merge(rr, pp, by = c("SourceTableName", "aar"), all = TRUE)
  m[is.na(raa), raa := 0L][is.na(par), par := 0L]
  add("O Rader", sprintf("NPR: rader parset / rådata (totalt %d / %d), antall kilde x år med avvik", sum(m$par), sum(m$raa)),
      m[raa != par, .N], "0", okif(m[raa != par, .N] == 0))
  vis("NPR rader per kilde og år med avvik", m[raa != par])
})
run("P-NPR", cmp_profil("NPR", raw_npr, par_npr, npr_map[names(npr_map) %in% names(raw_npr)], "SourceTableName"))
run("Q-NPR", {
  n1 <- par_npr[!is.na(Duration) & Duration < 0, .N]
  add("Q Plausibel", "NPR: episoder med negativ varighet (utdato < inndato)", n1, "~0", okif(n1 <= 0.0001 * nrow(par_npr)))
  n2 <- par_npr[!is.na(Duration) & Duration != DiffDays_Discharge - DiffDays_Admission, .N]
  add("Q Plausibel", "NPR: Duration = utdato - inndato", n2, "0", okif(n2 == 0))
  pc <- par_npr[!is.na(Procedures_NCMPNCSPCode) & Procedures_NCMPNCSPCode != "", Procedures_NCMPNCSPCode]
  fx <- function(v) { v <- as.character(v); b <- !is.na(v) & !validUTF8(v); v[b] <- iconv(v[b], "latin1", "UTF-8"); toupper(v) }
  pp <- par_npr[!is.na(Procedures_NCMPNCSPCode) & Procedures_NCMPNCSPCode != ""]
  n3 <- pp[fx(Procedures_NCMPNCSPCode) != fx(Original_kodeverdi) & Procedures_NCMPNCSPCode != "UNKNOWN", .N]
  add("Q Plausibel", sprintf("NPR: prosedyrekode = originalverdi (avvik utenom bevisst 'UNKNOWN', av %d)", nrow(pp)), n3, "0", okif(n3 == 0))
  n4 <- par_npr[!is.na(Diagnosis_ICDCode) & Diagnosis_ICDCode != "" & !is.na(Procedures_NCMPNCSPCode) & Procedures_NCMPNCSPCode != "", .N]
  add("Q Plausibel", "NPR: rader med både diagnose- og prosedyrekode", n4, "0", okif(n4 == 0))
  add("INFO", "NPR: varighet i dager per kilde (median / 99-persentil / maks)",
      par_npr[, .(v = paste(median(Duration, na.rm = TRUE), quantile(Duration, .99, na.rm = TRUE), max(Duration, na.rm = TRUE), sep = "/")), by = SourceTableName][, paste(SourceTableName, v, collapse = "  ")], "-", "INFO")
})
rm(raw_npr, par_npr); gc()

# =============================================================================
# KPR
# =============================================================================
kpr_map <- c(aar = "Year", diffdager_inndato = "DiffDays_Admission", kommunenr = "MunicipalityNumber",
             praksiskommune = "TreatmentMunicipality", fylkenr = "CountyNumber", praksisfylkenr = "TreatmentCounty",
             diagnosetabell = "DiagnosisType", diagnosekode = "DiagnosisCode", erhoveddiagnose = "isMainDiagnosis",
             tjenestetype = "ServiceType")
cat("Leser KPR rådata ...\n")
kd <- fread(paste0(KPRR, "diagnose.csv"), colClasses = list(character = c("lopenr", "nokkel")))
kr <- fread(paste0(KPRR, "regning.csv"),  colClasses = list(character = c("lopenr", "nokkel")))
setnames(kd, tolower(names(kd))); setnames(kr, tolower(names(kr)))
run("N-KPR", {
  n1 <- kr[, .N, by = nokkel][N > 1, .N]
  add("N Nøkler", "KPR: nokkel som forekommer mer enn én gang i regningsfila", n1, "0", okif(n1 == 0))
  j  <- merge(kd[, .(nokkel, lop_d = lopenr)], unique(kr[, .(nokkel, lop_r = lopenr)]), by = "nokkel")
  n2 <- j[lop_d != lop_r, .N]
  add("N Nøkler", "KPR: diagnoserader der lopenr != lopenr i regning (kobling på nokkel)", n2, "0", okif(n2 == 0))
  n3 <- kd[!nokkel %in% kr$nokkel, .N]
  add("N Nøkler", "KPR: diagnoserader uten regning", n3, "0", okif(n3 == 0))
})
kd[, lopenr := NULL]
raw_kpr <- merge(kd, kr, by = "nokkel", all = TRUE)[lopenr %in% okLop]
rm(kd, kr); gc()
cat("Leser ParsedKPR-All.csv ...\n")
par_kpr <- fread(paste0(KPRP, TAG, "ParsedKPR-All.csv"))
idsK    <- unique(par_kpr[, { v <- function(x) { x <- as.character(x); x[!is.na(x) & x == ""] <- NA; x }; fcoalesce(v(ID_2445), v(M_ID_2445), v(F_ID_2445)) }])
raw_kpr <- merge(raw_kpr, idmap, by = "lopenr")[ID %in% idsK]
raw_kpr[, grp := "KPR"]; par_kpr[, grp := "KPR"]
run("O-KPR", {
  rr <- raw_kpr[, .(raa = .N), by = .(aar)]
  pp <- par_kpr[, .(par = .N), by = .(aar = Year)]
  m  <- merge(rr, pp, by = "aar", all = TRUE); m[is.na(raa), raa := 0L][is.na(par), par := 0L]
  add("O Rader", sprintf("KPR: rader parset / rådata (totalt %d / %d), antall år med avvik", sum(m$par), sum(m$raa)),
      m[raa != par, .N], "0", okif(m[raa != par, .N] == 0))
  vis("KPR rader per år med avvik", m[raa != par])
})
run("P-KPR", {
  raw_kpr[, diagnosekode := toupper(sub("\\.", "", diagnosekode))]
  cmp_profil("KPR", raw_kpr, par_kpr, kpr_map[names(kpr_map) %in% names(raw_kpr)], "grp")
})


# =============================================================================
# R. Alle brede årsfiler, celle for celle, mot lang fil
# =============================================================================
rdlong <- function(f) { d <- fread(f, select = c("ID_2445", "Year", "Code"), colClasses = list(character = c("ID_2445", "Code")))
  d[, Year := as.integer(Year)][] }
nd <- rdlong(paste0(NPRP, TAG, "ParsedNPR_diagnoses.csv"))
kc <- rdlong(paste0(KPRP, TAG, "ParsedKPR_ICPC_diagnoses.csv"))
ki <- rdlong(paste0(KPRP, TAG, "ParsedKPR_ICD10_diagnoses.csv"))
ones_in <- function(f, suffix) {
  sch  <- read_parquet_schema(f)$name
  cols <- grep(paste0(suffix, "$"), sch, value = TRUE)
  ids  <- as.character(read_parquet(f, col_select = "ID_2445")$ID_2445)
  rbindlist(lapply(split(cols, ceiling(seq_along(cols) / 300)), function(cc) {
    d <- read_parquet(f, col_select = cc)
    rbindlist(lapply(cc, function(c) { v <- d[[c]]; if (any(v == 1, na.rm = TRUE)) data.table(ID = ids[which(v == 1)], kode = sub(paste0(suffix, "$"), "", c)) }))
  }))
}
sjekk_aar <- function(navn, mønster, long, kfun, aar) {
  fy  <- long[, .(ID = ID_2445, kode = kfun(Code), Year)][, .(first = min(Year)), by = .(ID, kode)]
  cy  <- unique(long[, .(ID = ID_2445, kode = kfun(Code), Year)])
  res <- rbindlist(lapply(aar, function(y) {
    f <- list.files(c(NPRP, KPRP), paste0(mønster, "-", y, "\\.parquet$"), full.names = TRUE)
    if (length(f) != 1) return(data.table(aar = y, kum = NA_integer_, tvs = NA_integer_, fil = length(f)))
    k <- ones_in(f, paste0("_", y)); x <- ones_in(f, paste0("_x", y))
    rk <- unique(fy[first <= y, .(ID, kode)]); rx <- unique(cy[Year == y, .(ID, kode)])
    data.table(aar = y, kum = nrow(fsetdiff(k, rk)) + nrow(fsetdiff(rk, k)),
               tvs = nrow(fsetdiff(x, rx)) + nrow(fsetdiff(rx, x)), fil = 1L)
  }))
  n <- res[is.na(kum) | kum > 0 | tvs > 0, .N]
  add("R Årsfiler", sprintf("%s: år med avvik (kumulativt eller tverrsnitt), av %d år", navn, length(aar)), n, "0", okif(n == 0))
  vis(paste(navn, "- år med avvik"), res[is.na(kum) | kum > 0 | tvs > 0])
}
run("R-NPR-major", sjekk_aar("NPR majorCodes", "ParsedNPR_majorCodes", nd, function(x) substr(x, 1, 3), 2008:2026))
run("R-NPR-minor", sjekk_aar("NPR allCodes",   "ParsedNPR_allCodes",   nd, identity, 2008:2026))
run("R-KPR-ICPC",  sjekk_aar("KPR ICPC",       "ParsedKPR_ICPCCodes",  kc, identity, 2016:2026))
run("R-KPR-ICD10", sjekk_aar("KPR ICD10",      "ParsedKPR_ICD10Codes", ki, identity, 2016:2026))

# =============================================================================
# S. SmallestDiffDate-allCodes.csv
# =============================================================================
run("S", {
  f  <- list.files(NPRP, "SmallestDiffDate-allCodes.csv$", full.names = TRUE)
  sd <- fread(f[1], colClasses = list(character = "ID_2445"))
  cat("\nKolonner i SmallestDiffDate:", paste(head(names(sd), 12), collapse = ", "), "...\n")
  add("INFO", "SmallestDiffDate: rader x kolonner", paste(dim(sd), collapse = " x "), "-", "INFO")
  pers <- uniqueN(sd$ID_2445); persL <- uniqueN(nd$ID_2445)
  add("S SmallestDiffDate", "Personer i SmallestDiffDate = personer i lang fil", sprintf("%d / %d", pers, persL), "like", okif(pers == persL))
  # Uavhengig: første inndato per person og kode fra ParsedNPR.csv, sammenlignet for et utvalg koder
  pn <- fread(paste0(NPRP, TAG, "ParsedNPR.csv"), select = c("ID_2445", "M_ID_2445", "F_ID_2445", "Diagnosis_ICDCode", "DiffDays_Admission"))
  pn[, ID := fcoalesce(fifelse(as.character(ID_2445) == "", NA_character_, as.character(ID_2445)),
                       fifelse(as.character(M_ID_2445) == "", NA_character_, as.character(M_ID_2445)),
                       fifelse(as.character(F_ID_2445) == "", NA_character_, as.character(F_ID_2445)))]
  pn[, kode := toupper(sub("\\?", "", sub("\\.", "", Diagnosis_ICDCode)))]
  for (k in intersect(c("F840", "F900", "E109", "G409", "F320"), names(sd))) {
    ref <- pn[kode == k & !is.na(DiffDays_Admission), .(ref = min(DiffDays_Admission)), by = ID]
    m <- merge(sd[!is.na(get(k)), .(ID = ID_2445, fil = get(k))], ref, by = "ID", all = TRUE)
    n <- m[is.na(fil) | is.na(ref) | fil != ref, .N]
    add("S SmallestDiffDate", sprintf("%s: første inndato lik ParsedNPR.csv (personer med avvik av %d)", k, nrow(ref)), n, "0", okif(n == 0))
  }
})


# =============================================================================
# V. Rad-for-rad: alle variabler som tekst (fanger byttede verdier og formatendringer)
# =============================================================================
tilTekst <- function(d) { for (j in names(d)) set(d, j = j, value = { v <- as.character(d[[j]]); b <- !is.na(v) & !validUTF8(v); v[b] <- iconv(v[b], "latin1", "UTF-8"); v[is.na(v)] <- ""; trimws(v) }); d }
cmp_rows <- function(navn, raw, par, map, ekstra = character()) {
  setnames(raw, names(map), unname(map), skip_absent = TRUE)
  cols <- intersect(c(ekstra, unname(map)), intersect(names(raw), names(par)))
  r <- tilTekst(raw[, ..cols]); q <- tilTekst(par[, ..cols])
  a <- fsetdiff(q, r, all = TRUE); b <- fsetdiff(r, q, all = TRUE)
  add("V Rader", sprintf("%s: rader i parset ikke i rådata / i rådata ikke i parset (%d kolonner, %d / %d rader)",
                         navn, length(cols), nrow(q), nrow(r)), sprintf("%d / %d", nrow(a), nrow(b)), "0 / 0",
      okif(nrow(a) == 0 && nrow(b) == 0))
  if (nrow(a) || nrow(b)) {
    # Hvilke kolonner forklarer avvikene? Fjern én kolonne om gangen og se om avvikene forsvinner
    forkl <- rbindlist(lapply(setdiff(cols, ekstra), function(cc) {
      k <- setdiff(cols, cc)
      data.table(kolonne = cc, avvik_uten_kolonnen = nrow(fsetdiff(q[, ..k], r[, ..k], all = TRUE)) + nrow(fsetdiff(r[, ..k], q[, ..k], all = TRUE)))
    }))[order(avvik_uten_kolonnen)]
    vis(paste(navn, "- avvik forklart av kolonne (lavest = årsak)"), head(forkl, 10))
    k1 <- forkl$kolonne[1]
    vis(paste(navn, "- eksempelverdier i", k1, "(parset / rådata)"), cbind(unique(a[, .(parset = get(k1))])[1:5], unique(b[, .(raadata = get(k1))])[1:5]))
  }
}
run("V-NPR", {
  par <- fread(paste0(NPRP, TAG, "ParsedNPR.csv"), colClasses = "character",
               select = c("ID_2445", "M_ID_2445", "F_ID_2445", "SourceTableName", unname(npr_map)))
  par[, ID := fifelse(ID_2445 != "", ID_2445, fifelse(M_ID_2445 != "", M_ID_2445, F_ID_2445))]
  for (r in c("AVT", "PHBU", "PHV", "REH", "SOM", "TSB")) {
    a <- fread(list.files(NPRR, paste0("^", r, "aktivitet"), full.names = TRUE), colClasses = "character")
    k <- fread(list.files(NPRR, paste0("^", r, "koder"),     full.names = TRUE), colClasses = "character")
    setnames(a, tolower(names(a))); setnames(k, tolower(names(k))); k[, lopenr := NULL]
    raw <- merge(merge(a, k, by = "nokkel", all = TRUE)[lopenr %in% okLop], idmap, by = "lopenr")
    rm(a, k); raw[, SourceTableName := r]
    pr  <- par[SourceTableName == r]
    raw <- raw[ID %in% pr$ID]
    if (r == "AVT") raw[is.na(diffdager_utdato) | diffdager_utdato == "", diffdager_utdato := diffdager_inndato]
    cmp_rows(paste("NPR ParsedNPR.csv", r), raw, pr, npr_map[names(npr_map) %in% names(raw)], c("ID", "SourceTableName"))
    rm(raw, pr); gc()
  }
  rm(par); gc()
})
run("V-KPR", {
  kd <- fread(paste0(KPRR, "diagnose.csv"), colClasses = "character"); kr <- fread(paste0(KPRR, "regning.csv"), colClasses = "character")
  setnames(kd, tolower(names(kd))); setnames(kr, tolower(names(kr))); kd[, lopenr := NULL]
  raw <- merge(merge(kd, kr, by = "nokkel", all = TRUE)[lopenr %in% okLop], idmap, by = "lopenr")
  raw[, diagnosekode := toupper(sub("\\.", "", diagnosekode))]
  par <- fread(paste0(KPRP, TAG, "ParsedKPR-All.csv"), colClasses = "character")
  par[, ID := fifelse(ID_2445 != "", ID_2445, fifelse(M_ID_2445 != "", M_ID_2445, F_ID_2445))]
  raw <- raw[ID %in% par$ID]
  cmp_rows("KPR ParsedKPR-All.csv", raw, par, kpr_map, "ID")
  rm(raw, par, kd, kr); gc()
})

# =============================================================================
# W. Alle filer i counts/ (generisk etter filnavn)
# =============================================================================
geno <- unique(fread("/tsd/p697/data/durable/genotype/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc-cov.txt",
                     select = "ID_2445", colClasses = "character")$ID_2445)
ndR <- fread(paste0(NPRP, TAG, "ParsedNPR_diagnoses.csv"), select = c("ID_2445", "Role", "Year", "Code"), colClasses = list(character = c("ID_2445", "Code")))
kcR <- fread(paste0(KPRP, TAG, "ParsedKPR_ICPC_diagnoses.csv"),  select = c("ID_2445", "Role", "Year", "Code"), colClasses = list(character = c("ID_2445", "Code")))
kiR <- fread(paste0(KPRP, TAG, "ParsedKPR_ICD10_diagnoses.csv"), select = c("ID_2445", "Role", "Year", "Code"), colClasses = list(character = c("ID_2445", "Code")))
beregn <- function(long) {
  long <- copy(long)[, Year := as.integer(Year)]
  fy <- long[, .(first = min(Year)), by = .(ID_2445, Code)]
  out <- fy[, .(lifetime = .N), by = Code]
  for (y in sort(unique(long$Year))) {
    t1 <- fy[first <= y, .N, by = Code]; setnames(t1, "N", as.character(y))
    t2 <- long[Year == y, .(N = uniqueN(ID_2445)), by = Code]; setnames(t2, "N", paste0("x", y))
    out <- merge(merge(out, t1, by = "Code", all.x = TRUE), t2, by = "Code", all.x = TRUE)
  }
  for (j in names(out)[-1]) set(out, which(is.na(out[[j]])), j, 0L)
  melt(out, id.vars = "Code", variable.name = "kol", value.name = "ny", variable.factor = FALSE)
}
run("W", {
  filer <- c(list.files(paste0(NPRP, "counts"), "\\.csv$", full.names = TRUE), list.files(paste0(KPRP, "counts"), "\\.csv$", full.names = TRUE))
  rolle <- c(Children = "Child", Mothers = "Mother", Fathers = "Father", allSubjects = NA)
  feil <- 0L; detalj <- list()
  for (f in filer) {
    b <- basename(f)
    long <- if (grepl("NPR", b)) ndR else if (grepl("ICPC", b)) kcR else kiR
    if (grepl("Genetics", b)) long <- long[ID_2445 %in% geno]
    r <- rolle[sub(".*-([A-Za-z]+)\\.csv$", "\\1", b)]
    if (!is.na(r)) long <- long[Role == r]
    if (grepl("majorCodes", b)) long <- copy(long)[, Code := substr(Code, 1, 3)]
    g <- fread(f); setnames(g, 1, "Code"); g[, Code := as.character(Code)]
    g <- melt(g, id.vars = "Code", variable.name = "kol", value.name = "fil", variable.factor = FALSE)
    m <- merge(g, beregn(long), by = c("Code", "kol"), all = TRUE); m[is.na(fil), fil := 0L][is.na(ny), ny := 0L]
    n <- m[fil != ny, .N]; feil <- feil + (n > 0)
    detalj[[b]] <- data.table(fil = b, celler = nrow(m), avvik = n)
  }
  d <- rbindlist(detalj)
  add("W counts", sprintf("Alle counts-filer (%d filer, %d celler): filer med avvik", nrow(d), sum(d$celler)), feil, "0", okif(feil == 0))
  vis("counts-filer med avvik", d[avvik > 0])
})

# =============================================================================
# T. Deltakerlister mot SV_INFO
# =============================================================================
run("T", {
  for (dir in c(NPRP, KPRP)) {
    kl <- fread(list.files(dir, "KeepList.csv$", full.names = TRUE)[1], colClasses = "character")
    reg <- if (dir == NPRP) "NPR" else "KPR"
    for (v in c("PREG_ID_2445", "M_ID_2445", "F_ID_2445")) {
      sv <- unique(as.character(svi[[v]])); sv <- sv[!is.na(sv) & sv != ""]
      kk <- unique(kl[[v]]); kk <- kk[!is.na(kk) & kk != ""]
      add("T Deltakere", sprintf("%s KeepList %s: ikke i SV_INFO / i SV_INFO ikke i KeepList", reg, v),
          sprintf("%d / %d", length(setdiff(kk, sv)), length(setdiff(sv, kk))), "0 / 0", okif(length(setdiff(kk, sv)) == 0 && length(setdiff(sv, kk)) == 0))
    }
    n <- sum(grepl("e\\+|^NA$", unlist(kl)))
    add("T Deltakere", paste(reg, "KeepList: ID-er med e+/NA"), n, "0", okif(n == 0))
  }
})

# ---- Oppsummering -----------------------------------------------------------
OUT <- rbindlist(RES)
cat("\n================ OPPSUMMERING QC3 (kontaktfiler) ================\n")
print(OUT[, .(gruppe, sjekk, verdi, krav, status)], nrows = Inf, class = FALSE)
cat("\nAntall sjekker:", OUT[status != "INFO", .N], "  Antall AVVIK:", OUT[status == "AVVIK", .N], "\n")
fwrite(OUT, "/ess/p697/cluster/users/p697-chrd/parsing_2026-10-07/qc3_resultat_2026-11-04.csv")
