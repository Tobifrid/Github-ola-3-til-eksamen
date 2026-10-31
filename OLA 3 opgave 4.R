# =====================================================================
# OLA 3 – Opgave 4: Stabilitet i min forbrugertillidsindikator
# =====================================================================
# Opgave 4 bygger på data og navne fra opgave 1 (resultater, kombinationskoder,
# forv1ogNKN1, kun_kvt_fra_forv1_og_nkn1, bedste_spm, di_spm og dst_spm).
# source() kører hele "OLA 3 opgave 1.R" først, så alt det findes, før opgave 4 starter.
# Filen skal ligge i samme mappe som projektet.
source("OLA 3 opgave 1.R", encoding = "UTF-8")                                  # kør opgave 1 først (henter data og laver navnene)


# =====================================================================
# Opgave 4.1: Test af model fra opgave 1
# =====================================================================

# ---- Opsætning ----
# Min bedste indikator er valgt ud fra hele perioden. Er den stabil, bør den også klare sig
# godt i kortere perioder. Jeg deler perioden op i blokke på 36 kvartaler, der ikke overlapper:
# kvartal 1-36, 37-72 og 73 til seneste kvartal. Længden følger Judgment-artiklen,
# som bruger vinduer på 36 kvartaler (Gayer, Girardi og Reuter, 2016, s. 12).
# I hver blok finder jeg den bedste kombination og ser, hvor godt min bedste indikator,
# DI's og DST's indikator klarer sig.

blok_laengde <- 36                                                               # antal kvartaler pr. blok – skal kun ændres her

# Saml alle 2047 kombinationer i én stor matrix (kun kvartalerne med forbrugstal)
# do.call(cbind, liste) sætter alle matricerne i listen ved siden af hinanden
alle_indikatorer <- do.call(cbind, resultater)[kun_kvt_fra_forv1_og_nkn1, ]
vaekst           <- forv1ogNKN1$aarlig_realvaekst_i_NKN1                         # den årlige realvækst som en vektor
alle_koder       <- unlist(kombinationskoder)                                    # navnene på alle 2047 kombinationer

# De tre indikatorer: gennemsnittet af de valgte spørgsmål i hvert kvartal
min_bedste_indikator <- rowMeans(forv1[kun_kvt_fra_forv1_og_nkn1, bedste_spm])
di_indikator         <- rowMeans(forv1[kun_kvt_fra_forv1_og_nkn1, di_spm])
dst_indikator        <- rowMeans(forv1[kun_kvt_fra_forv1_og_nkn1, dst_spm])

blok_start <- seq(1, length(vaekst), by = blok_laengde)                          # blokkene starter i kvartal 1, 37 og 73
blok_start

# ---- Loop: blok for blok ----
blok_resultater <- data.frame()                                                  # tom tabel, som fyldes i loopet

for (start in blok_start) {                                                      # loop: blok for blok
  slut    <- min(start + blok_laengde - 1, length(vaekst))                       # min() sikrer, at sidste blok stopper ved seneste kvartal
  raekker <- start:slut                                                          # kvartalerne i denne blok
  r2      <- cor(alle_indikatorer[raekker, ], vaekst[raekker])^2                 # R2 for alle 2047 kombinationer i denne blok
  
  blok_resultater <- bind_rows(blok_resultater, data.frame(
    periode             = paste(forv1ogNKN1$TID[start], "-", forv1ogNKN1$TID[slut]),
    antal_kvartaler     = length(raekker),
    bedste_i_blokken    = alle_koder[which.max(r2)],                             # den kombination med højest R2 i blokken
    R2_bedste_i_blokken = max(r2),
    R2_min_bedste       = cor(min_bedste_indikator[raekker], vaekst[raekker])^2,
    R2_DI               = cor(di_indikator[raekker],         vaekst[raekker])^2,
    R2_DST              = cor(dst_indikator[raekker],        vaekst[raekker])^2
  ))
}

blok_resultater
# 2000K1-2008K4: bedste F2+F3+F9+F10 (0,44). Min 0,36, DI 0,37, DST 0,35
# 2009K1-2017K4: bedste F2+F9+F7+F12 (0,53). Min 0,52, DI 0,48, DST 0,29
# 2018K1-2026K2: bedste F7+F8+F10   (0,68). Min 0,57, DI 0,43, DST 0,34

# ---- Gennemsnitligt R2 på tværs af blokkene ----
blok_resultater |>
  select(R2_min_bedste, R2_DI, R2_DST) |>
  colMeans()
# min bedste 0,48, DI 0,43 og DST 0,33

# Konklusion: min indikator er den mest stabile af de tre i gennemsnit, men ikke bedst i alle blokke
# (DI er lidt bedre i første blok), og den bedste kombination skifter fra blok til blok.
# Forbehold: testen er in-sample. Min indikator er valgt på hele perioden, så den har "set" de
# kvartaler, den testes på. En strengere test ville vælge kombinationen i blok 1 og teste den i
# blok 2 og 3 (out-of-sample). Tre blokke er desuden få.
