# =============================================================================
# QC runde 4: kontroller som de tidligere rundene ikke dekker
#  A  Rådatafilene er lest inn helt (linjer i fila = rader som fread leser)
#  B  Datoene i 2026 går til ca. 31. august (kontakter 2026 / 2025 ~ 0.67)
#  C  Koblingen for MØDRE og barn mot en hendelse vi kjenner: fødselen.
#     Barn født 2008-09 og mødrene deres bør oftere ha en somatisk kontakt i
#     fødselsåret enn i et kontrollår. Med tilfeldig kobling er andelene like.
# Resultat 06.11.2026: A 15/15 like, ingen advarsler. B: KPR 0.65, SOM 0.66,
#   AVT 0.63, PHV 0.72, TSB 0.77, PHBU 0.42. C: barn 0.46 vs 0.23, mødre 0.999 vs 0.38.
# Kun samletall. Kjør i terminalen:
#   nohup Rscript qc4_siste_kontroller.R > logs/qc4.log 2>&1 &
# =============================================================================
library(data.table); library(haven); options(scipen = 999)
P    <- "/tsd/p697/data/durable/phenotypes/"
TAG  <- "2026-11-04_SV-V12_20260618-MoBa-"
MFR  <- paste0(P, "mbrn/2025-12-16/P252634_PDB2445_uttrekk_MFR.sav")
SVI  <- paste0(P, "sv_infofiles/PDB2445_SV_INFO_V12_20260618.sav")
na0  <- function(x) { x <- as.character(x); x[!is.na(x) & x == ""] <- NA; x }

# ---- A. Rådata lest inn helt -------------------------------------------------
cat("\n===== A. Linjer i rådatafil vs rader lest av fread =====\n")
filer <- c(list.files(paste0(P, "npr/2026-10-19/rawdata"), "\\.csv$", full.names = TRUE),
           list.files(paste0(P, "kpr/2026-10-19/rawdata"), "\\.csv$", full.names = TRUE))
A <- rbindlist(lapply(filer, function(f) {
  linjer <- as.numeric(strsplit(trimws(system(paste("wc -l <", shQuote(f)), intern = TRUE)), " ")[[1]][1]) - 1
  adv <- character()
  rader <- withCallingHandlers(nrow(fread(f, select = 1L, colClasses = "character")),
                               warning = function(w) { adv <<- c(adv, conditionMessage(w)); invokeRestart("muffleWarning") })
  data.table(fil = basename(f), linjer_minus_header = linjer, rader_fread = rader,
             lik = linjer == rader, advarsler = paste(substr(adv, 1, 80), collapse = " | "))
}))
print(A, nrows = 50)
cat("Filer der antallet ikke stemmer:", A[lik == FALSE, .N], "\n")

# ---- Les parsede kontaktfiler -------------------------------------------------
pn <- fread(paste0(P, "npr/2026-10-19/parsed/", TAG, "ParsedNPR.csv"),
            select = c("ID_2445", "M_ID_2445", "F_ID_2445", "SourceTableName", "Year", "DiffDays_Admission", "DiffDays_Discharge"),
            colClasses = "character")
pn[, `:=`(Year = as.integer(Year), inn = as.integer(DiffDays_Admission), ut = as.integer(DiffDays_Discharge))]
pk <- fread(paste0(P, "kpr/2026-10-19/parsed/", TAG, "ParsedKPR-All.csv"),
            select = "Year", colClasses = "character")[, Year := as.integer(Year)]

# ---- B. Siste dato i 2026 ------------------------------------------------------
# DiffDays er regnet fra en dato som er ulik for hver person, så sluttdatoen kan
# ikke regnes ut direkte. I stedet: kontakter i 2026 som andel av 2025.
# Data t.o.m. 31. august gir ca. 243/365 = 0.67 (litt lavere pga. sommerferien).
cat("\n===== B. Kontakter 2026 som andel av 2025 (forventet ca. 0.6-0.7) =====\n")
d <- rbind(pn[Year %in% 2025:2026, .(kilde = SourceTableName, Year)],
           pk[Year %in% 2025:2026, .(kilde = "KPR", Year)])[, .N, by = .(kilde, Year)]
print(dcast(d, kilde ~ Year, value.var = "N")[, andel_2026 := round(`2026` / `2025`, 2)][])
cat("(PHV/TSB vokser og PHBU krymper fordi MoBa-barna blir voksne.)\n")
rm(pk, d); gc()

# ---- C. Fødselstest for barn og mødre -----------------------------------------
cat("\n===== C. Somatisk kontakt i fødselsåret vs kontrollår (barn og mødre) =====\n")
hdr <- names(read_sav(MFR, n_max = 0)); fa <- intersect(c("FAAR", "FODSELSAAR", "FAAR_BARN"), hdr)[1]
if (is.na(fa)) stop("Fant ikke fødselsår i MFR. Kolonner: ", paste(head(hdr, 20), collapse = ", "))
mfr <- as.data.table(read_sav(MFR, col_select = tidyselect::all_of(c("PREG_ID_2445", "BARN_NR", fa))))
setnames(mfr, fa, "faar"); mfr[, `:=`(faar = as.integer(faar), PREG_ID_2445 = as.character(PREG_ID_2445))]
sv  <- as.data.table(read_sav(SVI, col_select = c("PREG_ID_2445", "M_ID_2445")))[, lapply(.SD, as.character)]
fod <- merge(mfr[faar %in% 2008:2009], sv, by = "PREG_ID_2445")
som <- unique(pn[SourceTableName == "SOM", .(ID = fcoalesce(na0(ID_2445), na0(M_ID_2445), na0(F_ID_2445)), Year)])
test <- function(navn, ids_aar, kontroll = 4) {
  d <- copy(ids_aar)
  d[, fodselsaar := paste(ID, faar) %in% som[, paste(ID, Year)]]
  d[, kontrollaar := paste(ID, faar + kontroll) %in% som[, paste(ID, Year)]]
  r <- d[, .(personer = .N, andel_kontakt_fodselsaar = round(mean(fodselsaar), 3),
             andel_kontakt_kontrollaar = round(mean(kontrollaar), 3))]
  cat(navn, "\n"); print(r)
}
test("Barn født 2008-09 (kontrollår = 4 år senere)", unique(fod[, .(ID = paste0(PREG_ID_2445, "_", BARN_NR), faar)]))
test("Mødre med MoBa-fødsel 2008-09 (kontrollår = 4 år senere)", unique(fod[!is.na(M_ID_2445), .(ID = M_ID_2445, faar)]))
cat("\nRiktig kobling: klart høyere andel i fødselsåret enn i kontrollåret.\n",
    "Tilfeldig kobling: omtrent like andeler.\n")
cat("\nFERDIG\n")
