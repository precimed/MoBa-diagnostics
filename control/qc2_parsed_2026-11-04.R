# =============================================================================
# QC runde 2: fullstendige sjekker av parset NPR/KPR (parset 2026-11-04).
# Kjør i R-konsollen:  source(".../qc2_2026-11-04.R")   (tar ca. 15-30 min)
#
# G  Hele lange filer gjenbygd uavhengig fra rådata + kobling: identiske?
# H  Brede parquet-filer: ALLE koder og celler mot lang fil
# I  counts/-filer: ALLE koder, år og roller (også genetikk) mot lang fil
# J  Tellertabell (xlsx) mot counts/ og lang fil
# K  Interne ID-sjekker (kolonner, roller, MFR, OppfStopp18aar)
# L  Uavhengige plausibilitetssjekker (fødselsår, kjønn for alle roller, alder)
# M  Tekniske sjekker (år, kodeformat, tegnsett, duplikater)
#
# Skriver kun samletall. Eksempler på avvik vises som (rolle, kode, år) - ingen ID-er.
# =============================================================================
library(data.table); library(haven); library(nanoparquet); library(openxlsx)
options(scipen = 999)

P     <- "/tsd/p697/data/durable/phenotypes/"
SV    <- paste0(P, "sv_infofiles/")
NPRP  <- paste0(P, "npr/2026-10-19/parsed/")
KPRP  <- paste0(P, "kpr/2026-10-19/parsed/")
NPRR  <- paste0(P, "npr/2026-10-19/rawdata/")
KPRR  <- paste0(P, "kpr/2026-10-19/rawdata/")
TAG   <- "2026-11-04_SV-V12_20260618-MoBa-"
LINK  <- "20260605"
XLSX  <- paste0(P, "npr/2026-10-19/MoBa_NPR_KPR_counts_20261104.xlsx")
GENO  <- "/tsd/p697/data/durable/genotype/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc-cov.txt"
MFRFILE <- paste0(P, "mbrn/2025-12-16/P252634_PDB2445_uttrekk_MFR.sav")

RES <- list()
add <- function(gruppe, sjekk, verdi, krav, status)
  RES[[length(RES) + 1]] <<- data.table(gruppe, sjekk, verdi = as.character(verdi), krav, status)
run <- function(navn, expr) { cat(format(Sys.time(), "%H:%M:%S"), "sjekk", navn, "\n"); flush.console()
  tryCatch(expr, error = function(e) {
    add("FEIL", navn, conditionMessage(e), "-", "AVVIK"); message("FEIL i ", navn, ": ", conditionMessage(e)) }) }
okif <- function(x) ifelse(isTRUE(x), "OK", "AVVIK")
vis  <- function(tittel, d) { if (nrow(d)) { cat("\n--", tittel, "--\n"); print(head(d, 15)) } }
clean <- function(x) { bad <- !validUTF8(x); x[bad] <- iconv(x[bad], "latin1", "UTF-8")
  toupper(sub("\\?", "", sub("\\.", "", x))) }

# ---- Data -------------------------------------------------------------------
cat("Leser data ...\n")
readLink <- function(role) {
  f <- list.files(SV, paste0("^", role, "_ID_.*", LINK, "\\.sav$"), full.names = TRUE)
  d <- as.data.table(read_sav(f)); d[, lopenr := as.character(PID_2445)][]
}
barn <- readLink("Barn"); mor <- readLink("Mor"); far <- readLink("Far")
barn[, ID := paste0(PREG_ID_2445, "_", BARN_NR)]
link <- rbind(barn[, .(lopenr, ID, Role = "Child")],
              mor[,  .(lopenr, ID = as.character(M_ID_2445), Role = "Mother")],
              far[,  .(lopenr, ID = as.character(F_ID_2445), Role = "Father")])
svi <- as.data.table(read_sav(paste0(SV, "PDB2445_SV_INFO_V12_20260618.sav")))
okIDs <- unique(c(link[Role == "Child" & sub("_.*", "", ID) %in% as.character(svi$PREG_ID_2445), ID],
                  intersect(as.character(svi$M_ID_2445), link[Role == "Mother", ID]),
                  intersect(as.character(svi$F_ID_2445), link[Role == "Father", ID])))

rdlong <- function(f, extra = character()) {
  d <- fread(f, select = c("ID_2445", "Role", "Year", "Code", extra), colClasses = list(character = c("ID_2445", "Code")))
  d[, Year := as.integer(Year)][]
}
nd <- rdlong(paste0(NPRP, TAG, "ParsedNPR_diagnoses.csv"), c("PREG_ID_2445", "ChildNumber", "WithdrawnConsent18years"))
kc <- rdlong(paste0(KPRP, TAG, "ParsedKPR_ICPC_diagnoses.csv"))
ki <- rdlong(paste0(KPRP, TAG, "ParsedKPR_ICD10_diagnoses.csv"))

# ---- G. Uavhengig gjenbygging fra rådata ------------------------------------
cmp_sets <- function(navn, raw, par) {
  raw <- unique(raw[, .(ID, Role, Year, Code)]); par <- unique(par[, .(ID = ID_2445, Role, Year, Code)])
  a <- fsetdiff(par, raw); b <- fsetdiff(raw, par)
  add("G Rådata", sprintf("%s: rader i parset ikke i rådata / i rådata ikke i parset (av %d / %d)", navn, nrow(par), nrow(raw)),
      sprintf("%d / %d", nrow(a), nrow(b)), "0 / ~0", okif(nrow(a) == 0 && nrow(b) <= 0.0005 * nrow(raw)))
  vis(paste(navn, "- i parset men ikke i rådata (rolle, kode, år)"), a[, .N, by = .(Role, Code, Year)][order(-N)])
  vis(paste(navn, "- i rådata men ikke i parset (rolle, kode, år)"), b[, .N, by = .(Role, Code, Year)][order(-N)])
  if (nrow(b)) add("INFO", paste(navn, "- personer i rådata men ikke parset (forventet: samtykke)"), uniqueN(b$ID), "-", "INFO")
}
run("G-NPR", {
  rn <- rbindlist(lapply(c("AVT", "PHBU", "PHV", "REH", "SOM", "TSB"), function(r) {
    k <- fread(list.files(NPRR, paste0("^", r, "koder"), full.names = TRUE), colClasses = "character")
    k <- if (r == "PHBU") k[, .(lopenr, nokkel, kodeverdi)] else k[kodenavn == "ICD-10", .(lopenr, nokkel, kodeverdi)]
    a <- unique(fread(list.files(NPRR, paste0("^", r, "aktivitet"), full.names = TRUE),
                      select = c("lopenr", "nokkel", "aar"), colClasses = "character"))
    merge(k, a, by = c("lopenr", "nokkel"))[, .(lopenr, Year = as.integer(aar), Code = clean(kodeverdi))]
  }))
  rn <- merge(rn[!is.na(Code) & Code != ""], link, by = "lopenr")[ID %in% okIDs]
  cmp_sets("NPR", rn, nd); rm(rn); gc()
})
run("G-KPR", {
  kd <- fread(paste0(KPRR, "diagnose.csv"), select = c("lopenr", "nokkel", "diagnosetabell", "diagnosekode"), colClasses = "character")
  kr <- unique(fread(paste0(KPRR, "regning.csv"), select = c("lopenr", "nokkel", "aar"), colClasses = "character"))
  rk <- merge(kd, kr, by = c("lopenr", "nokkel"))[, .(lopenr, type = fifelse(diagnosetabell %in% c("ICPC-2", "ICPC-2B"), "ICPC", "ICD"),
                                                    Year = as.integer(aar), Code = toupper(sub("\\.", "", diagnosekode)))]
  rk <- merge(rk[!is.na(Code) & Code != ""], link, by = "lopenr")[ID %in% okIDs]
  cmp_sets("KPR ICPC", rk[type == "ICPC"], kc)
  cmp_sets("KPR ICD10", rk[type == "ICD"], ki); rm(kd, kr, rk); gc()
})

# ---- H. Brede parquet-filer mot lang fil (alle koder, alle celler) ---------
cmp_wide <- function(navn, f, long, suffix, ref) {
  # ref: data.table(ID, kode) som skal ha 1 i kolonnene <kode><suffix>
  sch  <- read_parquet_schema(f)$name
  cols <- grep(paste0(suffix, "$"), sch, value = TRUE)
  ids  <- as.character(read_parquet(f, col_select = "ID_2445")$ID_2445)
  ones <- rbindlist(lapply(split(cols, ceiling(seq_along(cols) / 300)), function(cc) {
    d <- read_parquet(f, col_select = cc)
    rbindlist(lapply(cc, function(c) { v <- d[[c]]; if (any(v == 1, na.rm = TRUE)) data.table(ID = ids[which(v == 1)], kode = sub(paste0(suffix, "$"), "", c)) }))
  }))
  ref <- unique(ref)
  a <- fsetdiff(ones, ref); b <- fsetdiff(ref, ones)
  add("H Parquet", sprintf("%s: 1-celler ikke i lang fil / i lang fil men 0 i parquet (av %d)", navn, nrow(ref)),
      sprintf("%d / %d", nrow(a), nrow(b)), "0 / 0", okif(nrow(a) == 0 && nrow(b) == 0))
  add("H Parquet", sprintf("%s: personer i parquet = personer i lang fil", navn), sprintf("%d / %d", uniqueN(ids), uniqueN(long$ID_2445)),
      "like", okif(setequal(ids, unique(long$ID_2445))))
  vis(paste(navn, "- avvik (kode)"), rbind(a[, .(retning = "parquet ikke lang", kode)], b[, .(retning = "lang ikke parquet", kode)])[, .N, by = .(retning, kode)])
}
firstyr <- function(long, kfun) long[, .(ID = ID_2445, kode = kfun(Code), Year)][, .(first = min(Year)), by = .(ID, kode)]
run("H-NPR-major-lifetime", cmp_wide("NPR majorCodes lifetime", paste0(NPRP, TAG, "ParsedNPR_majorCodes-lifetime.parquet"), nd,
                                     "_lifetime", nd[, .(ID = ID_2445, kode = substr(Code, 1, 3))]))
run("H-NPR-minor-lifetime", cmp_wide("NPR allCodes lifetime", paste0(NPRP, TAG, "ParsedNPR_allCodes-lifetime.parquet"), nd,
                                     "_lifetime", nd[, .(ID = ID_2445, kode = Code)]))
run("H-NPR-major-2023", {
  f <- paste0(NPRP, TAG, "ParsedNPR_majorCodes-2023.parquet")
  fy <- firstyr(nd, function(x) substr(x, 1, 3))
  cmp_wide("NPR majorCodes 2023 kumulativt", f, nd, "_2023",  fy[first <= 2023, .(ID, kode)])
  cmp_wide("NPR majorCodes 2023 tverrsnitt", f, nd, "_x2023", nd[Year == 2023, .(ID = ID_2445, kode = substr(Code, 1, 3))])
})
run("H-NPR-full-folder", {
  parts <- list.files(paste0(NPRP, TAG, "ParsedNPR_majorCodes"), "\\.parquet$", full.names = TRUE)
  fl <- rbindlist(lapply(parts, function(x) as.data.table(read_parquet(x, col_select = c("ID_2445", "F90_lifetime", "F90_2023", "F90_x2023")))))
  lt <- as.data.table(read_parquet(paste0(NPRP, TAG, "ParsedNPR_majorCodes-lifetime.parquet"), col_select = c("ID_2445", "F90_lifetime")))
  add("H Parquet", "NPR full majorCodes-mappe: rader = lifetime-fil, F90 lik", sprintf("%d / %d ; F90 %d / %d", nrow(fl), nrow(lt), sum(fl$F90_lifetime), sum(lt$F90_lifetime)),
      "like", okif(nrow(fl) == nrow(lt) && sum(fl$F90_lifetime) == sum(lt$F90_lifetime)))
})
run("H-KPR-lifetime", {
  cmp_wide("KPR ICPC lifetime",  list.files(KPRP, "ParsedKPR_ICPCCodes-lifetime.parquet$",  full.names = TRUE)[1], kc, "_lifetime", kc[, .(ID = ID_2445, kode = Code)])
  cmp_wide("KPR ICD10 lifetime", list.files(KPRP, "ParsedKPR_ICD10Codes-lifetime.parquet$", full.names = TRUE)[1], ki, "_lifetime", ki[, .(ID = ID_2445, kode = Code)])
})

# ---- I. counts/ mot lang fil (alle koder, alle år, alle roller, genetikk) --
geno <- unique(fread(GENO, select = "ID_2445", colClasses = "character")$ID_2445)
calc_counts <- function(long) {
  fy  <- long[, .(first = min(Year)), by = .(ID_2445, Code)]
  yrs <- sort(unique(long$Year))
  out <- fy[, .(lifetime = .N), by = Code]
  for (y in yrs) {
    t1 <- fy[first <= y, .N, by = Code];                  setnames(t1, "N",  as.character(y))
    t2 <- long[Year == y, .(N = uniqueN(ID_2445)), by = Code]; setnames(t2, "N",  paste0("x", y))
    out <- merge(merge(out, t1, by = "Code", all.x = TRUE), t2, by = "Code", all.x = TRUE)
  }
  for (j in names(out)[-1]) set(out, which(is.na(out[[j]])), j, 0L)
  melt(out, id.vars = "Code", variable.name = "kol", value.name = "ny", variable.factor = FALSE)
}
cmp_counts <- function(navn, dir, mønster, long) {
  f <- tail(sort(list.files(dir, mønster, full.names = TRUE)), 1)
  if (!length(f)) { add("I counts", paste(navn, "- fil ikke funnet"), mønster, "-", "AVVIK"); return() }
  g <- fread(f); setnames(g, 1, "Code"); g[, Code := as.character(Code)]
  g <- melt(g, id.vars = "Code", variable.name = "kol", value.name = "fil", variable.factor = FALSE)
  m <- merge(g, calc_counts(long), by = c("Code", "kol"), all = TRUE)
  m[is.na(fil), fil := 0L][is.na(ny), ny := 0L]
  n <- m[fil != ny, .N]
  add("I counts", sprintf("%s: celler som avviker (av %d)", navn, nrow(m)), n, "0", okif(n == 0))
  vis(paste(navn, "- avvikende celler"), m[fil != ny])
}
run("I", {
  cd <- paste0(NPRP, "counts/"); ck <- paste0(KPRP, "counts/")
  for (r in list(c("Children", "Child"), c("Mothers", "Mother"), c("Fathers", "Father"))) {
    cmp_counts(paste("NPR allCodes", r[1]),             cd, paste0("NPRCounts_allCodes-", r[1], ".csv$"), nd[Role == r[2]])
    cmp_counts(paste("NPR majorCodes", r[1]),           cd, paste0("NPRCounts_majorCodes-", r[1], ".csv$"), nd[Role == r[2]][, Code := substr(Code, 1, 3)])
    cmp_counts(paste("NPR genetikk allCodes", r[1]),    cd, paste0("NPRGeneticsCounts_allCodes-", r[1], ".csv$"), nd[Role == r[2] & ID_2445 %in% geno])
    cmp_counts(paste("KPR ICPC", r[1]),                 ck, paste0("KPRCounts_allICPCCodes-", r[1], ".csv$"), kc[Role == r[2]])
    cmp_counts(paste("KPR ICD10", r[1]),                ck, paste0("KPRCounts_allICDCodes-", r[1], ".csv$"),  ki[Role == r[2]])
    cmp_counts(paste("KPR genetikk ICPC", r[1]),        ck, paste0("KPRGeneticsCounts_allICPCCodes-", r[1], ".csv$"), kc[Role == r[2] & ID_2445 %in% geno])
  }
  cmp_counts("NPR allCodes allSubjects", cd, "NPRCounts_allCodes-allSubjects.csv$", nd)
})

# ---- J. Tellertabell (xlsx) ---------------------------------------------------
run("J", {
  sh <- getSheetNames(XLSX)
  chk <- function(sheet, long, navn) {
    x <- as.data.table(read.xlsx(XLSX, sheet = sheet, check.names = FALSE)); setnames(x, 1, "kode")
    l <- unique(long[, .(ID_2445, Role, kode = substr(Code, 1, 3))])
    ref <- dcast(l[, .N, by = .(kode, Role)], kode ~ Role, value.var = "N", fill = 0)
    refg <- dcast(l[ID_2445 %in% geno, .N, by = .(kode, Role)], kode ~ Role, value.var = "N", fill = 0)
    m <- merge(x, ref, by = "kode", all = TRUE); mg <- merge(x, refg, by = "kode", all = TRUE)
    z <- function(v) fifelse(is.na(v), 0, as.numeric(v))
    n1 <- m[z(Children) != z(Child) | z(Mothers) != z(Mother) | z(Fathers) != z(Father), .N]
    n2 <- mg[z(`Children+Genetics`) != z(Child) | z(`Mothers+Genetics`) != z(Mother) | z(`Fathers+Genetics`) != z(Father), .N]
    n3 <- x[`All Subjects` != Children + Mothers + Fathers, .N]
    add("J Tabell", sprintf("%s: koder som avviker (alle / genetikk / sum)", navn), sprintf("%d / %d / %d", n1, n2, n3), "0 / 0 / 0", okif(n1 + n2 + n3 == 0))
  }
  chk(sh[1], nd, "Fane NPR"); chk(sh[2], ki, "Fane KPR ICD10"); chk(sh[3], kc, "Fane KPR ICPC")
  # NPR-fanen mot counts/ majorCodes (uavhengig kodevei)
  cm <- fread(tail(sort(list.files(paste0(NPRP, "counts/"), "NPRCounts_majorCodes-Children.csv$", full.names = TRUE)), 1)); setnames(cm, 1, "kode")
  x  <- as.data.table(read.xlsx(XLSX, sheet = sh[1], check.names = FALSE)); setnames(x, 1, "kode")
  m  <- merge(x[, .(kode, Children)], cm[, .(kode = as.character(kode), lifetime)], by = "kode", all = TRUE)
  n  <- m[is.na(Children) | is.na(lifetime) | Children != lifetime, .N]
  add("J Tabell", "Fane NPR Children = counts/ majorCodes lifetime (koder som avviker)", n, "0", okif(n == 0))
})

# ---- K. Interne ID-sjekker ----------------------------------------------------
run("K", {
  for (x in list(list("NPR", nd), list("KPR ICPC", kc), list("KPR ICD10", ki))) {
    n <- x[[2]][, uniqueN(Role), by = ID_2445][V1 > 1, .N]
    add("K ID", paste(x[[1]], "- ID med mer enn én rolle"), n, "0", okif(n == 0))
    e <- x[[2]][grepl("e\\+|e-|^NA|_NA$|\\.", ID_2445), uniqueN(ID_2445)]
    add("K ID", paste(x[[1]], "- ID med e+/NA/punktum"), e, "0", okif(e == 0))
  }
  ch <- unique(nd[Role == "Child", .(ID_2445, PREG_ID_2445, ChildNumber, WithdrawnConsent18years)])
  n <- ch[ID_2445 != paste0(PREG_ID_2445, "_", ChildNumber), .N]
  add("K ID", "NPR barn: ID_2445 = PREG_ID_2445 _ ChildNumber", n, "0 avvik", okif(n == 0))
  n <- nd[Role != "Child" & !(is.na(PREG_ID_2445) & is.na(ChildNumber)), .N]
  add("K ID", "NPR foreldre: PREG_ID/ChildNumber er tomme", n, "0 avvik", okif(n == 0))
  w <- merge(ch, barn[, .(ID_2445 = ID, OppfStopp18aar)], by = "ID_2445", all.x = TRUE)
  n <- w[is.na(OppfStopp18aar) | as.integer(WithdrawnConsent18years) != as.integer(OppfStopp18aar), .N]
  add("K ID", "NPR barn: WithdrawnConsent18years = OppfStopp18aar i koblingsfil", n, "0 avvik", okif(n == 0))
})

# ---- L. Uavhengige plausibilitetssjekker -------------------------------------
run("L-MFR", {
  hdr <- names(read_sav(MFRFILE, n_max = 0))
  fa  <- intersect(c("FAAR", "FODSELSAAR", "FAAR_BARN"), hdr)[1]
  mfr <- as.data.table(read_sav(MFRFILE, col_select = c("PREG_ID_2445", "BARN_NR", "KJONN", if (!is.na(fa)) fa)))
  mfr[, ID := paste0(PREG_ID_2445, "_", BARN_NR)]
  for (x in list(list("NPR", nd), list("KPR ICPC", kc))) {
    ids <- unique(x[[2]][Role == "Child", ID_2445])
    add("L Plausibel", paste(x[[1]], "- andel barn som finnes i MFR"), round(mean(ids %in% mfr$ID), 4), ">= 0.99", okif(mean(ids %in% mfr$ID) >= 0.99))
  }
  if (!is.na(fa)) {
    mfr[, faar := as.integer(get(fa))]
    for (x in list(list("NPR", nd), list("KPR ICPC", kc), list("KPR ICD10", ki))) {
      b <- merge(x[[2]][Role == "Child", .(ID = ID_2445, Year)], mfr[, .(ID, faar)], by = "ID")
      n <- b[Year < faar, uniqueN(ID)]
      add("L Plausibel", paste(x[[1]], "- barn med diagnose før fødselsåret"), n, "0", okif(n == 0))
      add("INFO", paste(x[[1]], "- alder ved diagnose, barn (min / median / maks)"),
          paste(b[, c(min(Year - faar), median(Year - faar), max(Year - faar))], collapse = " / "), "-", "INFO")
    }
    s18 <- merge(unique(nd[Role == "Child" & WithdrawnConsent18years == 1, .(ID = ID_2445, Year)]), mfr[, .(ID, faar)], by = "ID")
    n <- s18[Year > faar + 19, uniqueN(ID)]
    add("L Plausibel", "NPR barn med OppfStopp18aar=1: diagnoser etter 19 år", n, "0", okif(n == 0))
    pc <- merge(unique(nd[Role == "Child" & grepl("^P0[5-7]", Code), .(ID = ID_2445, Year)]), mfr[, .(ID, faar)], by = "ID")
    add("INFO", "NPR barn, P05-P07 (lav fødselsvekt/prematur): andel registrert innen 2 år etter fødsel",
        round(pc[, mean(Year <= faar + 2)], 2), "høy for barn født 2008-09", "INFO")
  } else add("INFO", "Fødselsår ikke funnet i MFR-fila - L-alderssjekker hoppet over", paste(head(hdr, 15), collapse = ","), "-", "INFO")
})
run("L-kjønn", {
  kj <- list(
    list("NPR mødre, mannlige koder (N40-N51, Q53-Q55)",   nd, "Mother", "^(N4[0-9]|N5[01]|Q5[3-5])"),
    list("NPR fedre, kvinnelige koder (O, N70-N98, Q50-Q52)", nd, "Father", "^(O|N[7-9][0-9]|Q5[0-2])"),
    list("KPR fedre, kvinnelige koder (X09, X89, W15)",     kc, "Father", "^(X09|X89|W15)"),
    list("KPR mødre, mannlig infertilitet (Y10)",          kc, "Mother", "^Y10"))
  for (x in kj) {
    n <- x[[2]][Role == x[[3]] & grepl(x[[4]], Code), uniqueN(ID_2445)]
    tot <- x[[2]][Role == x[[3]], uniqueN(ID_2445)]
    add("L Plausibel", x[[1]], n, "~0 (< 0.1 % av rollen)", okif(n <= max(3, 0.001 * tot)))
  }
  n <- nd[Role == "Child" & grepl("^(F0[0-3]|G30)", Code), uniqueN(ID_2445)]
  add("L Plausibel", "NPR barn med demens (F00-F03, G30)", n, "~0", okif(n <= 5))
})

# ---- M. Tekniske sjekker -----------------------------------------------------
run("M", {
  for (x in list(list("NPR", nd, 2008:2026), list("KPR ICPC", kc, 2016:2026), list("KPR ICD10", ki, 2016:2026))) {
    d <- x[[2]]
    add("M Teknisk", paste(x[[1]], "- år utenfor", min(x[[3]]), "-", max(x[[3]]), "eller NA"), d[is.na(Year) | !Year %in% x[[3]], .N], "0", okif(d[is.na(Year) | !Year %in% x[[3]], .N] == 0))
    add("M Teknisk", paste(x[[1]], "- duplikater (person, kode, år)"), sum(duplicated(d[, .(ID_2445, Code, Year)])), "0", okif(sum(duplicated(d[, .(ID_2445, Code, Year)])) == 0))
    n <- d[!grepl("^[A-Z][0-9]{2}[0-9A-Z]*$", Code), uniqueN(Code)]
    add("M Teknisk", paste(x[[1]], "- koder med uventet format/tegn"), n, "0", okif(n == 0))
    if (n) vis(paste(x[[1]], "- uventede koder"), d[!grepl("^[A-Z][0-9]{2}[0-9A-Z]*$", Code), .N, by = Code][order(-N)])
    add("M Teknisk", paste(x[[1]], "- tomme verdier i ID/Role/Year/Code"),
        d[is.na(ID_2445) | ID_2445 == "" | is.na(Role) | is.na(Year) | is.na(Code) | Code == "", .N], "0",
        okif(d[is.na(ID_2445) | ID_2445 == "" | is.na(Role) | is.na(Year) | is.na(Code) | Code == "", .N] == 0))
  }
  f <- list.files(c(NPRP, KPRP), recursive = TRUE)
  n <- sum(grepl("2026-10-21|old_|1e05|link20260305", f))
  add("M Teknisk", "parsed/: filer fra gamle kjøringer", n, "0", okif(n == 0))
})

# ---- Oppsummering -----------------------------------------------------------
OUT <- rbindlist(RES)
cat("\n================ OPPSUMMERING QC2 ================\n")
print(OUT[, .(gruppe, sjekk, verdi, krav, status)], nrows = Inf, class = FALSE)
cat("\nAntall sjekker:", OUT[status != "INFO", .N], "  Antall AVVIK:", OUT[status == "AVVIK", .N], "\n")
fwrite(OUT, "/ess/p697/cluster/users/p697-chrd/parsing_2026-10-07/qc2_resultat_2026-11-04.csv")
