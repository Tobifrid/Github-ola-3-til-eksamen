# =====================================================================
# OLA 3 – Forbrugertillid og privatforbrug (opgave 1–4) og DMI's API (opgave 5)
# Periode: 1. kvartal 2000 til seneste kvartal med forbrugstal
# =====================================================================

library(dkstat)
library(tidyverse)   
library(pls)

# |> læses som "og så": resultatet til venstre sendes videre til funktionen til højre.

# aggreger i FORV1 så de bliver kvartaler og fjern f11 og dst indikator og vend fortegnet på F6 og F8
# Hele forberedelsen sker i én pipe, som starter med at hente data. Derfor kan den køres
# igen og igen uden problemer – den starter altid forfra fra DST.
forv1 <- dst_get_data(
  table = "FORV1",
  query = list(INDIKATOR = "*", Tid = "*"), # hent alle spørgsmål og alle tider med "*"
  lang = "da",
  parse_dst_tid = FALSE                     # behold dst tidsteksterne fx 2000M01
) |>
  filter(!grepl("Forbrugertillidsindikatoren|F11", INDIKATOR)) |>               # fjern F11 og forbrugertillidsindikatoren
  filter(TID >= "2000M01") |>                                                    # perioden starter i januar 2000
  pivot_wider(id_cols = TID, names_from = INDIKATOR, values_from = value) |>     # spørgsmålene bliver til kolonner
  mutate(maaned = as.numeric(substr(TID, 6, 7)),                                 # tallene efter M, fx 01
         TID    = paste0(substr(TID, 1, 4), "K", ceiling(maaned / 3))) |>        # måned -> kvartal, fx 2000K1
  select(-maaned) |>                                                             # hjælpekolonnen skal ikke bruges mere
  group_by(TID) |>                                                               # én gruppe pr. kvartal
  summarise(across(everything(), mean)) |>                                       # gennemsnit af de tre måneder for hvert spørgsmål
  as.data.frame()                                                                # tilbage til en almindelig data frame


# ---- Vend fortegnet på de negative spørgsmål ----
# F6 (priserne er steget) og F8 (flere arbejdsløse om et år)
F6_F8 <- grep("^F6|^F8", names(forv1), value = TRUE)
forv1[F6_F8] <- -forv1[F6_F8] #med minus tegn foran ændre vi fortegnet


# =====================================================================
# Opgave 1.1: Alle kombinationer af spørgsmålene
# =====================================================================
resultater <- list() 

for (antal in 1:11) { # jeg vil gerne have den køre i det ydre loop i alle 11 spm
  z <- matrix(0, ncol = choose(11, antal), nrow = nrow(forv1)) #vi laver en tom matrix, hvor den køre en række pr. kvartal og en kolonne pr. kombination
  for (i in 1:nrow(forv1)) {
    indikator_svar <- unlist(forv1[i, 2:ncol(forv1)]) #de 11 svar i et kvartal, så først alle indikator svarene for 2000k1 osv.
    z[i, ] <- colMeans(combn(indikator_svar, antal)) # tager gennemsnit af hver kombination
  }
  resultater[[antal]] <- z
}
resultater |> sapply(ncol) |> sum()   #vil give mig 2047 kombinationer

# aggreger i NKN1, lav en kolonne som viser den årlige realvækst sammenlignet med sig selv for året før den

NKN1 <- dst_get_data(
  table = "NKN1",
  query = list(
    TRANSAKT  = "P.31 Husholdningernes forbrugsudgifter",
    PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
    "SÆSON"   = "Sæsonkorrigeret",
    Tid       = "*"
  ),
  lang = "da",
  parse_dst_tid = FALSE 
) |>
  arrange(TID) |>                                                  # sorter efter tid, så lag() rammer samme kvartal året før
  mutate(aarlig_realvaekst_i_NKN1 = (value / lag(value, 4) - 1) * 100)    # vækst ift. samme kvartal året før, fx 2001K1 mod 2000K1

# ---- Vælg perioden: 2000K1 til seneste kvartal med forbrugstal ----
forv1ogNKN1 <- forv1$TID %in% NKN1$TID #med %in% vil den kun tage de tider med som også findes i begge
#lige nu har vi gemt forv1ogNKN1 som en vector for sig selv, hvor den tjekker TRUE og FALSE på alle de
forv1ogNKN1 <- NKN1 |> filter(TID %in% forv1$TID)
F_spoergsmaalenes_hele_tekst <- names(forv1)[-1] #vælger alle F spm fra forv1
F_koder <- sub(" .*", "", F_spoergsmaalenes_hele_tekst) #laver alle spm til kun deres F nr.
# sub(" .*", "", ) sletter alt fra det første mellemrum og frem
# jeg skal have en vector som kun er med kvartalerne fra mine to df.
kun_kvt_fra_forv1_og_nkn1 <- forv1$TID %in% NKN1$TID
#lav et loops som viser alle korrelationer i alle indikatorer
korrelationer <-  list()
kombinationsnavne <-  list()
kombinationskoder <-  list()
for (antal in 1:11) {
  indikatorer <- resultater[[antal]][kun_kvt_fra_forv1_og_nkn1, , drop=FALSE] #drop=false sørger for, at en tabel eller matrix bliver ved med at være en tabel
  korrelationer[[antal]] <-  cor(indikatorer, forv1ogNKN1$aarlig_realvaekst_i_NKN1) # jeg vil gerne se korrelationen mellem indikatorne og forbruget
  kombinationsnavne[[antal]] <- combn(F_spoergsmaalenes_hele_tekst, antal, paste, collapse= "+") #collapse sætter navnene sammen til én tekst med "+" imellem
  kombinationskoder[[antal]] <- combn(F_koder, antal, paste, collapse= "+")
}

# slut loopet, og saml det i en tabel. Sorter, så den højeste korrelation står øverst,
# giv en placering 1-2047, og regn R2 (med én forklarende variabel er R2 = korrelationen i anden)
alle_kombinationer <- data.frame(
  antal_F_koder=rep(1:11, choose(11, 1:11)),
  F_spoergsmaalenes_hele_tekster=unlist(kombinationsnavne),
  F_koderne=unlist(kombinationskoder),
  korrelationer=unlist(korrelationer)
) |>
  arrange(desc(korrelationer)) |>          # desc() = højeste først
  mutate(placering = row_number(),          # rækkenummeret efter sorteringen = placeringen
         R2        = korrelationer^2)

# =====================================================================
# Opgave 1.2: R2 og forbrugertillidsindikatorer
# =====================================================================

# ---- Den bedste kombination ----
bedste <- alle_kombinationer |> filter(placering==1)                                         # række 1 = den bedste, fordi tabellen er sorteret
bedste |> select(F_koderne, korrelationer, R2)                                  # hvilke spørgsmål, korrelation og R2
# F4+F7+F9+F10+F12+F13: korrelation 0,71 og R2 0,501

# ---- Tjek R2 med en rigtig regression ----
# strsplit deler teksten op igen ved "+", så jeg får de enkelte spørgsmål tilbage
bedste_spm       <- strsplit(bedste$F_spoergsmaalenes_hele_tekster, "+", fixed = TRUE)[[1]]
bedste_indikator <- rowMeans(forv1[kun_kvt_fra_forv1_og_nkn1, bedste_spm, drop = FALSE])       # gennemsnit af de 6 spm pr. kvartal
bedste_model     <- lm(forv1ogNKN1$aarlig_realvaekst_i_NKN1 ~ bedste_indikator)
summary(bedste_model)$r.squared
# skal give det samme som bedste$R2

# ---- Er den bedste indikator bedre end DI's? ----
# funktion der sammenligner en valgt indikator med forbruget
sammenlign_indikatorer <- function(valgte_spm, navn) {
  valgt_indikator  <- rowMeans(forv1[kun_kvt_fra_forv1_og_nkn1, valgte_spm, drop = FALSE])     # gennemsnit af de valgte spm
  model            <- lm(forv1ogNKN1$aarlig_realvaekst_i_NKN1 ~ valgt_indikator)
  samlet_spm_tekst <- paste(F_spoergsmaalenes_hele_tekst[F_spoergsmaalenes_hele_tekst %in% valgte_spm], collapse = "+")
  data.frame(
    indikator          = navn,
    antal_spoergsmaal  = length(valgte_spm),
    korrelation        = cor(valgt_indikator, forv1ogNKN1$aarlig_realvaekst_i_NKN1),
    R2                 = summary(model)$r.squared,
    residualkvadratsum = sum(residuals(model)^2),                                              # RSS: jo lavere, jo bedre
    placering          = alle_kombinationer |> filter(F_spoergsmaalenes_hele_tekster == samlet_spm_tekst) |> pull(placering)
  )
}

# find de lange spørgsmålstekster ud fra F-numrene
dst_spm <- F_spoergsmaalenes_hele_tekst[F_koder == "F2" | F_koder == "F3" | F_koder == "F4" | F_koder == "F5" | F_koder == "F9"]   # DST
di_spm  <- F_spoergsmaalenes_hele_tekst[F_koder == "F2" | F_koder == "F4" | F_koder == "F9" | F_koder == "F10"]                    # DI (Baum, 2016)

sammenligning_af_di_dst_bedste <- bind_rows(                                     # bind_rows er dplyr's udgave af rbind
  sammenlign_indikatorer(bedste_spm, "Bedste kombination"),
  sammenlign_indikatorer(di_spm,     "DI's indikator"),
  sammenlign_indikatorer(dst_spm,    "DST forbrugertillid")
)
sammenligning_af_di_dst_bedste
# bedste 0,501 (plads 1), DI 0,374 (plads 546), DST 0,259 (plads 1.505)
# Svar: ja, den bedste forklarer ca. 13 procentpoint mere af variationen end DI's,
# men den er valgt ud fra de samme data (risiko for overfitting, se opgave 4)


# =====================================================================
# Opgave 1.3: Spørgsmål i indikatoren
# =====================================================================

# ---- De 10 bedste kombinationer (alternative indikatorer) ----
alle_kombinationer |>
  select(placering, antal_F_koder, F_koderne, R2) |>                            # select() = vælg kolonner
  head(10)                                                                       # head() = de første rækker
# R2 fra 0,484 til 0,501. F4, F7 og F12 er med i alle ti.
# Nr. 2 er den samme uden F13 (R2 0,500): et enklere alternativ

# ---- Den bedste kombination med mindst 2 spørgsmål ----
bedste_flere <- alle_kombinationer |>
  filter(antal_F_koder >= 2) |>                                                  # filter() = behold de rækker, hvor betingelsen er TRUE
  slice(1)
bedste_flere |> select(placering, F_koderne, korrelationer, R2)
# det er den samme som den bedste, fordi den bedste allerede har 6 spørgsmål


# =====================================================================
# Opgave 1.4: Forudsigelser med afsæt i min indikator
# =====================================================================
# Formlen fra regressionen: vækst = skæring + hældning * indikator
model_data_1.4 <- data.frame(
  aarlig_realvaekst_i_NKN1 = forv1ogNKN1$aarlig_realvaekst_i_NKN1,
  bedste_indikator         = rowMeans(forv1[kun_kvt_fra_forv1_og_nkn1, bedste_spm, drop = FALSE])
)
forudsigelses_model_til_1.4 <- lm(aarlig_realvaekst_i_NKN1 ~ bedste_indikator, data = model_data_1.4)
coef(forudsigelses_model_til_1.4)
# skæring ca. -0,90 og hældning ca. 0,30

# de nye kvartaler: dem uden forbrugstal (! vender TRUE/FALSE, så vi får kvartalerne UDEN forbrugstal)
nye_kvartaler <- data.frame(
  TID              = forv1$TID[!kun_kvt_fra_forv1_og_nkn1],
  bedste_indikator = rowMeans(forv1[!kun_kvt_fra_forv1_og_nkn1, bedste_spm, drop = FALSE])
)
nye_kvartaler
# min indikator i de nye kvartaler, fx 6,91 = ca. 7 procentpoint flere positive end negative svar

predict(forudsigelses_model_til_1.4, newdata = nye_kvartaler)
# 2026K3: ca. 1,19 % (-0,90 + 0,30 * 6,91)
# 2026K4 (kun oktober): ca. 0,99 %


# =====================================================================
# Opgave 1.5: Sammenlign med en mikroøkonomisk indikator
# =====================================================================
# mikro = spørgsmål om husstanden selv: F2, F3, F9, F10, F12, F13
# makro = spørgsmål om Danmark: F4, F5 (økonomien), F6, F7 (priser), F8 (arbejdsløshed)
# en kombination er kun mikro, hvis den IKKE indeholder nogen af makro-koderne.
# grepl() giver TRUE, hvis teksten indeholder F4, F5, F6, F7 eller F8. ! vender det om.
mikro_kombinationer <- alle_kombinationer |>
  filter(!grepl("F4|F5|F6|F7|F8", F_koderne))
nrow(mikro_kombinationer)
# 63 kombinationer (2^6 - 1)

mikro_kombinationer |>
  select(placering, antal_F_koder, F_koderne, R2) |>
  head(5)
# bedst: F10 alene (R2 0,438), derefter F10+F13 (0,401)

bedste_mikro     <- mikro_kombinationer |> slice(1)                              # den bedste rene mikro-indikator
bedste_mikro_spm <- strsplit(bedste_mikro$F_spoergsmaalenes_hele_tekster, "+", fixed = TRUE)[[1]]

# læg den ind i sammenligningstabellen og sorter efter R2
sammenligning_med_mikro <- bind_rows(
  sammenligning_af_di_dst_bedste,
  sammenlign_indikatorer(bedste_mikro_spm, "Bedste mikro-indikator")
) |>
  arrange(desc(R2))
sammenligning_med_mikro
# bedste 0,501 (plads 1), mikro F10 0,438 (plads 127), DI 0,374 (plads 546), DST 0,259 (plads 1.505)
# flere mikrospørgsmål gør det ikke bedre; det er makrospørgsmålene F4 og F7, der løfter den bedste

