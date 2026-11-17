# =============================================================================
# Sammenligning på personnivå: parset 2025-leveranse mot ny (2026-11-04)
# For personer som finnes i begge, år t.o.m. 2024:
#  1  Diagnoser (person, kilde, år, full kode) - NPR og KPR
#  2  Kontakter (person, kilde, år, inndato) - NPR og KPR
#  3  Referansedato: er DiffDays regnet fra samme dato?
#  4  Variabler på kontakter som finnes i begge (institusjon, nivå, henvisning,
#     kommune, fylke, tjenestetype, hoveddiagnose ...)
# Skriver kun samletall (ingen ID-er).
# Kjør i terminalen:
#   nohup Rscript sammenlign_2025_personniva.R > logs/sammenlign_2025.log 2>&1 &
# =============================================================================
library(data.table); options(scipen = 999)
P   <- "/tsd/p697/data/durable/phenotypes/"
TAG <- "2026-11-04_SV-V12_20260618-MoBa-"
OUT <- "/ess/p697/cluster/users/p697-chrd/parsing_2026-10-07/"
nNPR <- paste0(P, "npr/2026-10-19/parsed/", TAG, "ParsedNPR.csv")
nKPR <- paste0(P, "kpr/2026-10-19/parsed/", TAG, "ParsedKPR-All.csv")
SISTE_AAR <- 2024

# Finn gamle filer uten å søke inn i 2026-mappene (unngår mappeløkka)
finnGammel <- function(reg, monster) {
  d <- list.dirs(paste0(P, reg), recursive = FALSE)
  d <- d[grepl("^2025", basename(d))]
  f <- unlist(lapply(d, function(x) list.files(x, monster, recursive = TRUE, full.names = TRUE)))
  if (!length(f)) stop("Fant ingen 2025-fil for ", reg, " (", monster, ")")
  cat(reg, "- gammel fil:", f[1], "\n"); f[1]
}
gNPR <- finnGammel("npr", "ParsedNPR\\.csv$")
gKPR <- finnGammel("kpr", "ParsedKPR-All\\.csv$")

na0   <- function(x) { x <- as.character(x); x[!is.na(x) & x == ""] <- NA; x }
utf   <- function(x) { b <- !is.na(x) & !validUTF8(x); x[b] <- iconv(x[b], "latin1", "UTF-8"); x }
clean <- function(x) { x <- toupper(sub("\\?", "", sub("\\.", "", utf(x)))); x[!is.na(x) & x == ""] <- NA; x }
uten0 <- function(x) { x <- trimws(utf(as.character(x))); x[is.na(x)] <- ""; sub("^0+(?=.)", "", x, perl = TRUE) }

les <- function(f, vars) {
  h <- names(fread(f, nrows = 0)); v <- intersect(c("ID_2445", "M_ID_2445", "F_ID_2445", vars), h)
  d <- fread(f, select = v, colClasses = "character")
  d[, ID := fcoalesce(na0(ID_2445), na0(M_ID_2445), na0(F_ID_2445))]
  d[, intersect(c("ID_2445", "M_ID_2445", "F_ID_2445"), names(d)) := NULL]
  d[, Year := as.integer(Year)]
  d <- d[!is.na(ID) & Year <= SISTE_AAR]
  if ("DiffDays_Admission" %in% names(d)) d[, DiffDays_Admission := as.integer(DiffDays_Admission)]
  d
}

RES <- list()
sammenlign <- function(navn, o, n, keys, by = NULL) {
  o <- unique(o[, ..keys])[, i_o := TRUE]; n <- unique(n[, ..keys])[, i_n := TRUE]
  m <- merge(o, n, by = keys, all = TRUE)
  s <- m[, .(gammel = sum(!is.na(i_o)), ny = sum(!is.na(i_n)), begge = sum(!is.na(i_o) & !is.na(i_n))), by = by]
  s[, `:=`(gammel_funnet_i_ny = round(begge / gammel, 4), ny_finnes_i_gammel = round(begge / ny, 4))]
  s[, sammenligning := navn]
  if (!is.null(by)) setorderv(s, by)
  cat("\n=====", navn, "=====\n"); print(s, nrows = 100)
  RES[[length(RES) + 1]] <<- s
}
refdato <- function(navn, o, n, by) {
  ro <- o[!is.na(DiffDays_Admission), .(d_o = min(DiffDays_Admission)), by = by]
  rn <- n[!is.na(DiffDays_Admission), .(d_n = min(DiffDays_Admission)), by = by]
  m  <- merge(ro, rn, by = by)[, diff := d_n - d_o]
  cat("\n=====", navn, ": første inndato per person/år, ny minus gammel (dager) =====\n")
  print(m[, .(grupper = .N, andel_lik = round(mean(diff == 0), 4),
              p01 = quantile(diff, .01), median = median(diff), p99 = quantile(diff, .99))])
  cat("Vanligste forskjeller:\n"); print(m[, .N, by = diff][order(-N)][1:5])
}
variabler <- function(navn, o, n, kontakt, vars) {
  kb <- merge(unique(o[, ..kontakt]), unique(n[, ..kontakt]), by = kontakt)   # kontakter i begge
  o <- merge(o, kb, by = kontakt); n <- merge(n, kb, by = kontakt)
  cat("\n=====", navn, ": kontakter i begge =", nrow(kb), "=====\n")
  rad <- rbindlist(lapply(intersect(vars, intersect(names(o), names(n))), function(v) {
    a <- unique(o[, c(kontakt, v), with = FALSE]); b <- unique(n[, c(kontakt, v), with = FALSE])
    setnames(a, v, "x"); setnames(b, v, "x")
    ra <- a[, .(x = uten0(x))]; rb <- b[, .(x = uten0(x))]
    a1 <- copy(a)[, x := trimws(utf(x))][is.na(x), x := ""]; b1 <- copy(b)[, x := trimws(utf(x))][is.na(x), x := ""]
    a2 <- copy(a)[, x := uten0(x)]; b2 <- copy(b)[, x := uten0(x)]
    data.table(variabel = v,
               lik_raa       = round(nrow(fintersect(a1, b1)) / nrow(unique(a1)), 4),
               lik_uten_null = round(nrow(fintersect(a2, b2)) / nrow(unique(a2)), 4))
  }))
  print(rad); RES[[length(RES) + 1]] <<- rad[, sammenligning := paste(navn, "- variabler")]
}

# =============================================================================
# NPR
# =============================================================================
cat("\nLeser NPR ...\n")
nv <- c("SourceTableName", "Year", "DiffDays_Admission", "DiffDays_Discharge", "Diagnosis_ICDCode",
        "Somatic_LevelofCare", "Somatic_TreatmentClinic_Derived", "ReferralType", "ReferralDecision",
        "InstitutionID", "InstitutionID_MentalHealth", "InstitutionID_Psuedoanonymized",
        "Procedures_NCMPNCSPCode", "Diagnosis_Axis")
o <- les(gNPR, nv); n <- les(nNPR, nv)
felles <- intersect(unique(o$ID), unique(n$ID))
cat("NPR personer: gammel", uniqueN(o$ID), " ny", uniqueN(n$ID), " i begge", length(felles), "\n")
o <- o[ID %in% felles]; n <- n[ID %in% felles]
o[, kode := clean(Diagnosis_ICDCode)]; n[, kode := clean(Diagnosis_ICDCode)]
kodeGml <- unique(o[!is.na(kode), kode])        # nye søkte koder i 2026 holdes utenfor
do <- o[!is.na(kode)]; dn <- n[!is.na(kode) & kode %in% kodeGml]
sammenlign("NPR diagnoser per år",   do, dn, c("ID", "SourceTableName", "Year", "kode"), "Year")
sammenlign("NPR diagnoser per kilde", do, dn, c("ID", "SourceTableName", "Year", "kode"), "SourceTableName")
do[, k3 := substr(kode, 1, 3)]; dn[, k3 := substr(kode, 1, 3)]
sammenlign("NPR person x 3-tegnskode t.o.m. 2024", do, dn, c("ID", "k3"))
rm(do, dn); gc()
sammenlign("NPR kontakter per kilde", o, n, c("ID", "SourceTableName", "Year", "DiffDays_Admission"), "SourceTableName")
refdato("NPR", o, n, c("ID", "SourceTableName", "Year"))
variabler("NPR", o, n, c("ID", "SourceTableName", "Year", "DiffDays_Admission"),
          c("DiffDays_Discharge", "Somatic_LevelofCare", "Somatic_TreatmentClinic_Derived", "ReferralType",
            "ReferralDecision", "InstitutionID", "InstitutionID_MentalHealth", "InstitutionID_Psuedoanonymized",
            "Procedures_NCMPNCSPCode", "Diagnosis_Axis"))
rm(o, n); gc()

# =============================================================================
# KPR
# =============================================================================
cat("\nLeser KPR ...\n")
kv <- c("Year", "DiffDays_Admission", "DiagnosisType", "DiagnosisCode", "isMainDiagnosis",
        "MunicipalityNumber", "TreatmentMunicipality", "CountyNumber", "TreatmentCounty", "ServiceType")
o <- les(gKPR, kv); n <- les(nKPR, kv)
felles <- intersect(unique(o$ID), unique(n$ID))
cat("KPR personer: gammel", uniqueN(o$ID), " ny", uniqueN(n$ID), " i begge", length(felles), "\n")
o <- o[ID %in% felles]; n <- n[ID %in% felles]
o[, kode := clean(DiagnosisCode)]; n[, kode := clean(DiagnosisCode)]
kodeGml <- unique(o[!is.na(kode), paste(DiagnosisType, kode)])
do <- o[!is.na(kode)]; dn <- n[!is.na(kode) & paste(DiagnosisType, kode) %in% kodeGml]
sammenlign("KPR diagnoser per år", do, dn, c("ID", "Year", "DiagnosisType", "kode"), "Year")
sammenlign("KPR diagnoser per kodeverk", do, dn, c("ID", "Year", "DiagnosisType", "kode"), "DiagnosisType")
rm(do, dn); gc()
sammenlign("KPR kontakter per år", o, n, c("ID", "Year", "DiffDays_Admission"), "Year")
refdato("KPR", o, n, c("ID", "Year"))
variabler("KPR", o, n, c("ID", "Year", "DiffDays_Admission"),
          c("MunicipalityNumber", "TreatmentMunicipality", "CountyNumber", "TreatmentCounty",
            "ServiceType", "isMainDiagnosis", "DiagnosisType"))
rm(o, n); gc()

fwrite(rbindlist(RES, fill = TRUE), paste0(OUT, "sammenligning_2025_personniva.csv"))
cat("\nFERDIG\n")
