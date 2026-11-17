# =============================================================================
# Kvalitetssjekk av parset NPR/KPR (leveranse 2026-10-19, kobling 20260605,
# parset 2026-11-04). Skriver kun samletall til skjermen + en oppsummering.
# Kjør i R-konsollen:  source("qc_2026-11-04.R")
# Hver sjekk gir OK / AVVIK / INFO. Én feil stopper ikke resten.
# =============================================================================
library(data.table); library(haven); library(nanoparquet)
options(scipen = 999)   # samme som i parsingen: 100000 skal ikke bli "1e+05" i ID-ene

P     <- "/tsd/p697/data/durable/phenotypes/"
SV    <- paste0(P, "sv_infofiles/")
NPRP  <- paste0(P, "npr/2026-10-19/parsed/")
KPRP  <- paste0(P, "kpr/2026-10-19/parsed/")
NPRR  <- paste0(P, "npr/2026-10-19/rawdata/")
KPRR  <- paste0(P, "kpr/2026-10-19/rawdata/")
TAG   <- "2026-11-04_SV-V12_20260618-MoBa-"
LINK  <- "20260605"
OLDNPR    <- paste0(P, "npr/2025-05-15/npr_parsed-20250522_SVINFO-20250425/2025-05-22-MoBa-ParsedNPR_majorCodes.csv")
OLDCOUNTS <- "/ess/p697/cluster/users/parekh/2025-05-21_NPR/counts"
MFRFILE   <- paste0(P, "mbrn/2025-12-16/P252634_PDB2445_uttrekk_MFR.sav")

RES <- list()
add <- function(gruppe, sjekk, verdi, krav, status)
  RES[[length(RES) + 1]] <<- data.table(gruppe, sjekk, verdi = as.character(verdi), krav, status)
run <- function(navn, expr) tryCatch(expr, error = function(e) {
  add("FEIL", navn, conditionMessage(e), "-", "AVVIK"); message("FEIL i ", navn, ": ", conditionMessage(e)) })
okif <- function(x) ifelse(x, "OK", "AVVIK")
cat("Leser data ...\n")

# ---- Data -------------------------------------------------------------------
readLink <- function(role) {
  f <- list.files(SV, paste0("^", role, "_ID_.*", LINK, "\\.sav$"), full.names = TRUE)
  d <- as.data.table(read_sav(f)); d[, lopenr := as.character(PID_2445)][]
}
barn <- readLink("Barn"); mor <- readLink("Mor"); far <- readLink("Far")
barn[, ID := paste0(PREG_ID_2445, "_", BARN_NR)]
link <- rbind(barn[, .(lopenr, ID, Role = "Child")],
              mor[,  .(lopenr, ID = as.character(M_ID_2445), Role = "Mother")],
              far[,  .(lopenr, ID = as.character(F_ID_2445), Role = "Father")])

mfr <- as.data.table(read_sav(MFRFILE, col_select = c("PREG_ID_2445", "BARN_NR", "KJONN")))
mfr[, `:=`(ID = paste0(PREG_ID_2445, "_", BARN_NR), gutt = KJONN == 1)]
svi <- as.data.table(read_sav(paste0(SV, "PDB2445_SV_INFO_V12_20260618.sav")))

nd <- fread(paste0(NPRP, TAG, "ParsedNPR_diagnoses.csv"),       select = c("ID_2445", "Role", "Year", "Code"))
kc <- fread(paste0(KPRP, TAG, "ParsedKPR_ICPC_diagnoses.csv"),  select = c("ID_2445", "Role", "Year", "Code"))
ki <- fread(paste0(KPRP, TAG, "ParsedKPR_ICD10_diagnoses.csv"), select = c("ID_2445", "Role", "Year", "Code"))
for (d in list(nd, kc, ki)) d[, ID_2445 := as.character(ID_2445)]

# Rådata NPR (kun ICD-10; PHBU alle akser) og KPR
rk <- rbindlist(lapply(c("AVT", "PHBU", "PHV", "REH", "SOM", "TSB"), function(r) {
  k <- fread(list.files(NPRR, paste0("^", r, "koder"), full.names = TRUE),
             select = c("lopenr", "kodenavn", "kodeverdi"), colClasses = "character")
  if (r != "PHBU") k <- k[kodenavn == "ICD-10"]
  k[, .(lopenr, kode = toupper(sub("\\?", "", sub("\\.", "", kodeverdi))))]
}))
rkpr <- fread(paste0(KPRR, "diagnose.csv"), select = c("lopenr", "diagnosekode"), colClasses = "character")

# ---- A. Koblingsfilene ------------------------------------------------------
run("A", {
  for (x in list(list("Barn", barn, "Bm", "ID"), list("Mor", mor, "Mm", "M_ID_2445"), list("Far", far, "Fm", "F_ID_2445"))) {
    d <- x[[2]]
    add("A Kobling", paste(x[[1]], "- unike løpenumre"), sum(duplicated(d$lopenr)), "0 duplikater", okif(sum(duplicated(d$lopenr)) == 0))
    add("A Kobling", paste(x[[1]], "- unike MoBa-ID"), sum(duplicated(d[[x[[4]]]])), "0 duplikater", okif(sum(duplicated(d[[x[[4]]]])) == 0))
    pre <- mean(startsWith(d$lopenr, x[[3]]))
    add("A Kobling", paste(x[[1]], "- prefiks", x[[3]]), round(pre, 4), "1", okif(pre == 1))
  }
  for (x in list(list("NPR", unique(rk$lopenr)), list("KPR", unique(rkpr$lopenr)))) {
    cov <- mean(x[[2]] %in% link$lopenr)
    add("A Kobling", paste("Dekning løpenumre", x[[1]]), round(cov, 4), ">= 0.999", okif(cov >= 0.999))
  }
})

# ---- B. Riktige personer ----------------------------------------------------
andel_gutt <- function(ids) mfr[ID %in% ids, round(mean(gutt, na.rm = TRUE), 3)]
run("B1-B2", {
  kj <- list(
    list("KPR", kc, "X09|X89|W15", "kvinne"),
    list("NPR", nd, "O|N7[0-6]|N91|N97|Q5[0-2]", "kvinne"),
    list("NPR", nd, "N4[156]|Q5[3-5]", "mann"))
  for (x in kj) {
    ids <- x[[2]][Role == "Child" & grepl(paste0("^(", x[[3]], ")"), Code), unique(ID_2445)]
    g <- andel_gutt(ids)
    krav <- if (x[[4]] == "kvinne") "< 0.05" else "> 0.90"
    pass <- if (x[[4]] == "kvinne") g < 0.05 else g > 0.90
    add("B Personer", sprintf("%s barn, %s-koder (%s): andel gutter, n=%d", x[[1]], x[[4]], x[[3]], length(ids)), g, krav, okif(pass))
  }
})
run("B3", {
  ofar <- nd[Role == "Father" & grepl("^O", Code), uniqueN(ID_2445)]
  omor <- nd[Role == "Mother" & grepl("^O", Code), uniqueN(ID_2445)]
  add("B Personer", sprintf("NPR svangerskapskoder (O): fedre / mødre (%d)", omor), ofar, "~0 fedre", okif(ofar <= max(2, 0.005 * omor)))
  pfar <- kc[Role == "Father" & grepl("^(X09|X89)", Code), uniqueN(ID_2445)]
  pmor <- kc[Role == "Mother" & grepl("^(X09|X89)", Code), uniqueN(ID_2445)]
  add("B Personer", sprintf("KPR premenstruelle koder: fedre / mødre (%d)", pmor), pfar, "~0 fedre", okif(pfar <= max(2, 0.005 * pmor)))
})
run("B4", {
  # Alle 3-tegnskoder med >= 50 personer per rolle i 2025: finnes de igjen?
  hdr  <- names(fread(OLDNPR, nrows = 0))
  lcol <- grep("_lifetime$", hdr, value = TRUE)
  old  <- fread(OLDNPR, select = c("ID_2445", "Role", lcol))
  oldl <- melt(old, id.vars = c("ID_2445", "Role"), variable.name = "k", value.name = "v")[v == 1]
  oldl[, kode := sub("_lifetime$", "", k)][, ID_2445 := as.character(ID_2445)]
  newl <- unique(nd[, .(ID_2445, kode = substr(Code, 1, 3))])
  newl[, i := 1L]
  m <- merge(oldl[, .(ID_2445, Role, kode)], newl, by = c("ID_2445", "kode"), all.x = TRUE)
  s <- m[, .(n = .N, igjen = mean(!is.na(i))), by = .(Role, kode)][n >= 50]
  for (r in c("Child", "Mother", "Father")) {
    ss <- s[Role == r]
    add("B Personer", sprintf("NPR 2025->2026 funnet igjen, %s (%d koder): median / laveste", r, nrow(ss)),
        sprintf("%.3f / %.3f (%s)", median(ss$igjen), min(ss$igjen), ss[which.min(igjen), kode]),
        "median > 0.97", okif(median(ss$igjen) > 0.97))
  }
  lav <- s[igjen < 0.9][order(igjen)]
  if (nrow(lav)) { cat("\nKoder med < 90 % funnet igjen:\n"); print(lav) }
})

# ---- C. Samtykke -------------------------------------------------------------
run("C", {
  okPreg <- as.character(svi$PREG_ID_2445)
  for (x in list(list("NPR", nd), list("KPR ICPC", kc), list("KPR ICD10", ki))) {
    ids <- unique(x[[2]][, .(ID_2445, Role)])
    inn <- ids[, ifelse(Role == "Child", sub("_.*", "", ID_2445) %in% okPreg,
                        ID_2445 %in% c(as.character(svi$M_ID_2445), as.character(svi$F_ID_2445)))]
    add("C Samtykke", paste(x[[1]], "- andel ID i gjeldende SV_INFO"), round(mean(inn), 4), "1", okif(mean(inn) == 1))
  }
  wf <- list.files(NPRP, "WithdrawnList.csv$", full.names = TRUE)
  if (length(wf)) {
    w <- fread(wf[1], colClasses = "character")
    wid <- unique(c(w$M_ID_2445, w$F_ID_2445))
    wid <- wid[!is.na(wid) & wid != "" & !(wid %in% c(svi$M_ID_2445, svi$F_ID_2445))]
    n <- nd[Role != "Child" & ID_2445 %in% wid, uniqueN(ID_2445)]
    add("C Samtykke", "NPR - foreldre fra WithdrawnList i data", n, "0", okif(n == 0))
  }
})

# ---- D. Parsing gjengir rådata ----------------------------------------------
run("D1", {
  for (kk in c("F84", "F90", "E10", "G40", "F32")) {
    lp  <- rk$lopenr[startsWith(rk$kode, kk)]
    raw <- unique(link[lopenr %in% lp, .(ID, Role)])
    par <- unique(nd[startsWith(Code, kk), .(ID = ID_2445, Role)])
    i_par_ikke_raa <- nrow(fsetdiff(par, raw))
    i_raa_ikke_par <- nrow(fsetdiff(raw, par))
    add("D Parsing", sprintf("NPR %s: i parset men ikke i rådata / i rådata men ikke parset (av %d)", kk, nrow(raw)),
        sprintf("%d / %d", i_par_ikke_raa, i_raa_ikke_par), "0 / < 1 % (samtykke)",
        okif(i_par_ikke_raa == 0 && i_raa_ikke_par <= 0.01 * nrow(raw)))
  }
  for (kk in c("P81", "T89", "N88")) {
    lp  <- rkpr$lopenr[startsWith(rkpr$diagnosekode, kk)]
    raw <- unique(link[lopenr %in% lp, .(ID, Role)])
    par <- unique(kc[startsWith(Code, kk), .(ID = ID_2445, Role)])
    a <- nrow(fsetdiff(par, raw)); b <- nrow(fsetdiff(raw, par))
    add("D Parsing", sprintf("KPR %s: i parset men ikke i rådata / i rådata men ikke parset (av %d)", kk, nrow(raw)),
        sprintf("%d / %d", a, b), "0 / < 1 % (samtykke)", okif(a == 0 && b <= 0.01 * nrow(raw)))
  }
})
run("D2", {
  f  <- paste0(NPRP, TAG, "ParsedNPR_majorCodes-lifetime.parquet")
  pq <- as.data.table(read_parquet(f, col_select = c("ID_2445", "Role", "F84_lifetime", "F90_lifetime", "E10_lifetime")))
  for (kode in c("F84", "F90", "E10")) for (r in c("Child", "Mother", "Father")) {
    a <- pq[Role == r, sum(get(paste0(kode, "_lifetime")))]
    b <- nd[Role == r & startsWith(Code, kode), uniqueN(ID_2445)]
    add("D Parsing", sprintf("Parquet lifetime = lang fil: %s %s", kode, r), sprintf("%d / %d", a, b), "like", okif(a == b))
  }
})
run("D3", {
  cdir <- paste0(NPRP, "counts/")
  rd <- function(p) { f <- tail(sort(list.files(cdir, p, full.names = TRUE)), 1); g <- fread(f); setnames(g, 1, "kode"); g }
  ch <- rd("NPRCounts_majorCodes-Children.csv$"); mo <- rd("NPRCounts_majorCodes-Mothers.csv$")
  fa <- rd("NPRCounts_majorCodes-Fathers.csv$");  al <- rd("NPRCounts_majorCodes-allSubjects.csv$")
  gc <- rd("NPRGeneticsCounts_majorCodes-Children.csv$")
  b <- nd[Role == "Child" & startsWith(Code, "F90"), uniqueN(ID_2445)]
  add("D Parsing", "counts/ F90 barn lifetime = lang fil", sprintf("%d / %d", ch[kode == "F90", lifetime], b), "like", okif(ch[kode == "F90", lifetime] == b))
  m <- merge(merge(merge(al[, .(kode, a = lifetime)], ch[, .(kode, c = lifetime)]), mo[, .(kode, m = lifetime)]), fa[, .(kode, f = lifetime)])
  add("D Parsing", "counts/ alle = barn + mødre + fedre (antall koder med avvik)", m[a != c + m + f, .N], "0", okif(m[a != c + m + f, .N] == 0))
  g <- merge(ch[, .(kode, c = lifetime)], gc[, .(kode, g = lifetime)])
  add("D Parsing", "counts/ genetikk <= alle, barn (antall koder med avvik)", g[g > c, .N], "0", okif(g[g > c, .N] == 0))
  add("INFO", "Andel genotypet, barn med F84 / F90 (gammelt ark: 0.66 / 0.65)",
      sprintf("%.2f / %.2f", g[kode == "F84", g / c], g[kode == "F90", g / c]), "~0.65-0.70", "INFO")
})

# ---- E. Sammenligning med 2025-tellingene og plausibilitet -------------------
run("E1", {
  if (dir.exists(OLDCOUNTS)) {
    cdir <- paste0(NPRP, "counts/")
    for (r in c("Children", "Mothers", "Fathers")) {
      o <- fread(tail(sort(list.files(OLDCOUNTS, paste0("NPRCounts_majorCodes-", r, ".csv$"), full.names = TRUE)), 1)); setnames(o, 1, "kode")
      n <- fread(tail(sort(list.files(cdir,      paste0("NPRCounts_majorCodes-", r, ".csv$"), full.names = TRUE)), 1)); setnames(n, 1, "kode")
      m <- merge(o[, .(kode, o = `2024`)], n[, .(kode, n = `2024`)], by = "kode")[o > 0]
      pct <- 100 * (sum(m$n) - sum(m$o)) / sum(m$o)
      add("E Plausibilitet", sprintf("NPR kumulativt t.o.m. 2024 mot 2025-leveransen, %s (%%)", r), round(pct, 2), "-1 til +1.5", okif(pct > -1 & pct < 1.5))
    }
  } else add("E Plausibilitet", "2025-counts ikke funnet", OLDCOUNTS, "-", "INFO")
})
run("E2", {
  py <- nd[, .(personer = uniqueN(ID_2445)), by = .(Role, Year)][order(Role, Year)]
  r  <- py[Year %in% 2025:2026, .(ratio = personer[Year == 2026] / personer[Year == 2025]), by = Role]
  add("E Plausibilitet", "NPR personer 2026/2025 (barn, mødre, fedre)", paste(round(r$ratio, 2), collapse = " / "), "0.6-0.9 (jan-aug)", okif(all(r$ratio > 0.6 & r$ratio < 0.9)))
  hull <- py[Year < 2026, .(min_aar = Year[which.min(personer)], min = min(personer)), by = Role]
  add("E Plausibilitet", "NPR år uten personer 2008-2025", sum(hull$min == 0), "0", okif(all(hull$min > 0)))
  ky <- kc[, .(personer = uniqueN(ID_2445)), by = .(Year)][order(Year)]
  add("INFO", "KPR ICPC personer per år", paste(ky$Year, ky$personer, sep = ":", collapse = "  "), "-", "INFO")
})


# ---- F. ID-format (fanger 1e+05 o.l.) ----------------------------------------
run("F", {
  for (x in list(list("NPR", nd), list("KPR ICPC", kc), list("KPR ICD10", ki))) {
    d <- unique(x[[2]][, .(ID_2445, Role)])
    feil <- d[(Role == "Child" & !grepl("^[0-9]+_[0-9]+$", ID_2445)) | (Role == "Mother" & !grepl("^M[0-9]+$", ID_2445)) |
              (Role == "Father" & !grepl("^F[0-9]+$", ID_2445))]
    add("F ID-format", paste(x[[1]], "- ID med feil format (f.eks. 1e+05)"), nrow(feil), "0", okif(nrow(feil) == 0))
  }
  n <- nd[ID_2445 == "100000_1", .N]
  add("F ID-format", "NPR - barn med PREG_ID 100000 finnes som 100000_1 (rader)", n, "> 0", okif(n > 0))
})

# ---- Oppsummering -----------------------------------------------------------
OUT <- rbindlist(RES)
cat("\n================ OPPSUMMERING ================\n")
print(OUT[, .(gruppe, sjekk, verdi, krav, status)], nrows = Inf, class = FALSE)
cat("\nAntall AVVIK:", OUT[status == "AVVIK", .N], "\n")
