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
negative_spm <- grep("^F6|^F8", names(forv1), value = TRUE)
forv1[negative_spm] <- -forv1[negative_spm] #med minus tegn foran ændre vi fortegnet


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
resultater |> sapply(ncol) |> sum()   # skal være 2047: tæl kolonnerne i hver matrix og læg dem sammen


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
  mutate(aarlig_realvaekst = (value / lag(value, 4) - 1) * 100)    # vækst ift. samme kvartal året før, fx 2001K1 mod 2000K1

# ---- Vælg perioden: 2000K1 til seneste kvartal med forbrugstal ----
forv1ogNKN1 <- forv1$TID %in% NKN1$TID #med %in% vil den kun tage de tider med som også findes i begge
#lige nu har vi gemt forv1ogNKN1 som en vector for sig selv, hvor den tjekker TRUE og FALSE på alle de
#kvartaler som de har tilfældes, men lad os lave det om til en df 
forv1ogNKN1 <- NKN1 |> filter(TID %in% forv1$TID)
spm_navne <- names(forv1)[-1] #vælger alle F spm fra forv1
spm_koder <- sub(" .*", "", spm_navne) #laver alle spm til kun deres F nr.
# sub(" .*", "", ) sletter alt fra det første mellemrum og frem
# jeg skal have en vector som kun er med kvartalerne fra mine to df.
kunkvt <- forv1$TID %in% NKN1$TID
#lav et loops som viser alle korrelationer i alle indikatorer
korrelationer <-  list()
kombinationsnavne <-  list()
kombinationskoder <-  list()
for (antal in 1:11) {
  indikatorer <- resultater[[antal]][kunkvt, , drop=FALSE] #drop=false sørger for, at en tabel eller matrix bliver ved med at være en tabel
  korrelationer[[antal]] <-  cor(indikatorer, forv1ogNKN1$aarlig_realvaekst) # jeg vil gerne se korrelationen mellem indikatorne og forbruget
  kombinationsnavne[[antal]] <- combn(spm_navne, antal, paste, collapse= "+") #collapse sætter navnene sammen til én tekst med "+" imellem
  kombinationskoder[[antal]] <- combn(spm_koder, antal, paste, collapse= "+")
}

# slut loopet, og saml det i en tabel. Sorter, så den højeste korrelation står øverst,
# giv en placering 1-2047, og regn R2 (med én forklarende variabel er R2 = korrelationen i anden)
alle_kombinationer <- data.frame(
  antal_spoergsmaal=rep(1:11, choose(11, 1:11)),
  spoergsmaalene=unlist(kombinationsnavne),
  indikatorne=unlist(kombinationskoder),
  korrelationer=unlist(korrelationer)
) |>
  arrange(desc(korrelationer)) |>          # desc() = højeste først
  mutate(placering = row_number(),          # rækkenummeret efter sorteringen = placeringen
         R2        = korrelationer^2)

#=======neden under kan jeg få vist den bedste kombination svar på opgave 1.2=========#
#jeg døber den bedste kombi bedste
bedste <- alle_kombinationer |> slice(1)   # slice(1) = første række
bedste$indikatorne   # hvilke spørgsmål indgår? (bruges i opgave 1.3)
bedste$R2            # svaret på opgave 1.2: R2 for den bedste indikator
#======================================================================================
# sammenlign den bedste med DI og DST (også en del af opgave 1.2)
# tjek R2 med en rigtig regression
# strsplit deler navnet op igen ved "+", så jeg får de enkelte spørgsmål tilbage
bedste_spm       <- strsplit(bedste$spoergsmaalene, "+", fixed = TRUE)[[1]]
bedste_indikator <- rowMeans(forv1[kunkvt, bedste_spm, drop = FALSE])
bedste_model     <- lm(forv1ogNKN1$aarlig_realvaekst ~ bedste_indikator)
summary(bedste_model)$r.squared   # skal give det samme som bedste$R2

#lad os sammenligne min bedste kombination af spm, med DST og DI
#vi laver en funktion som gør det
sammenlign_indikatorer <- function(valgte_spm, navn){
  valgt_indikator <- rowMeans(forv1[kunkvt, valgte_spm, drop = FALSE])
  model <- lm(forv1ogNKN1$aarlig_realvaekst ~ valgt_indikator)
  samlet_spm_navne <- paste(spm_navne[spm_navne %in% valgte_spm], collapse = "+")
  data.frame(
    indikator          = navn,
    antal_spoergsmaal  = length(valgte_spm),
    korrelation        = cor(valgt_indikator, forv1ogNKN1$aarlig_realvaekst),
    R2                 = summary(model)$r.squared,
    residualkvadratsum = sum(residuals(model)^2),   # RSS: jo lavere, jo bedre
    placering          = alle_kombinationer$placering[alle_kombinationer$spoergsmaalene == samlet_spm_navne]
  )
}

# find de lange navne ud fra F-numrene: "tag de lange navne, hvor F-nummeret er et af disse"
# DST's forbrugertillid: F2, F3, F4, F5 og F9
dst_spm <- spm_navne[spm_koder == "F2" | spm_koder == "F3" | spm_koder == "F4" | spm_koder == "F5" | spm_koder == "F9"]

# DI's indikator: F2, F4, F9 og F10 (Baum, 2016)
di_spm <- spm_navne[spm_koder == "F2" | spm_koder == "F4" | spm_koder == "F9" | spm_koder == "F10"]

# kør funktionen for alle tre og sæt dem under hinanden i én tabel
sammenligning_af_di_dst_bedste <- rbind(
  sammenlign_indikatorer(bedste_spm, "Bedste kombination"),
  sammenlign_indikatorer(di_spm,     "DI's indikator"),
  sammenlign_indikatorer(dst_spm,    "DST forbrugertillid")
)
sammenligning_af_di_dst_bedste   # tabellen til opgave 1.2
# DI har R2 på 0,374 (37,4 %) og DST har R2 på 0,259 (25,9 %)
# DI har korrelation på 0,61 og DST har korrelation på 0,51


# =====================================================================
# Opgave 1.3: Spørgsmål i indikatoren og alternativer
# =====================================================================

# de 10 bedste kombinationer (alternative indikatorer)
alle_kombinationer |>
  select(placering, antal_spoergsmaal, indikatorne, R2) |>   # select() = vælg kolonner
  head(10)

# den bedste kombination med mindst 2 spørgsmål
bedste_flere <- alle_kombinationer |>
  filter(antal_spoergsmaal >= 2) |>   # filter() = behold de rækker, hvor betingelsen er TRUE
  slice(1)
bedste_flere |> select(placering, indikatorne, korrelationer, R2)


# =====================================================================
# Opgave 1.4: Forudsig forbruget i de nyeste kvartaler
# =====================================================================
# Formlen fra regressionen: vækst = skæring + hældning * indikator
model_data_1.4 <- data.frame(
  aarlig_realvaekst = forv1ogNKN1$aarlig_realvaekst,
  bedste_indikator  = rowMeans(forv1[kunkvt, bedste_spm, drop = FALSE])
)
forudsigelses_model_til_1.4 <- lm(aarlig_realvaekst ~ bedste_indikator, data = model_data_1.4)
coef(forudsigelses_model_til_1.4) # skæring og hældning (ca. -0,90 og 0,30)

# de nye kvartaler: dem uden forbrugstal (!kunkvt = 2026K3 og 2026K4)
nye_kvartaler <- data.frame(
  TID              = forv1$TID[!kunkvt],
  bedste_indikator = rowMeans(forv1[!kunkvt, bedste_spm, drop = FALSE])
)
nye_kvartaler   # min bedste indikator i de nye kvartaler = gennemsnittet af nettotallene
# for mine 6 spørgsmål. Fx 6,91 = 7 procentpoint flere positive end negative svar.
# Det er x, som predict() sætter ind: -0,90 + 0,30 * 6,91 = 1,19 %

predict(forudsigelses_model_til_1.4, newdata = nye_kvartaler)
# 107 er 2026K3: forudsiger en vækst på ca. 1,19 %
# 108 er 2026K4: forudsiger en vækst på ca. 0,99 %, altså ca. 1 %


# =====================================================================
# Opgave 1.5: Bedste indikator med KUN mikroøkonomiske spørgsmål
# =====================================================================
# mikro = spørgsmål om husstanden selv: F2, F3, F9, F10, F12, F13
# makro = spørgsmål om Danmark: F4, F5 (økonomien), F6, F7 (priser), F8 (arbejdsløshed)
# en kombination er kun mikro, hvis den IKKE indeholder nogen af makro-koderne.
# grepl() giver TRUE, hvis teksten indeholder F4, F5, F6, F7 eller F8. ! vender det om.
mikro_kombinationer <- alle_kombinationer |>
  filter(!grepl("F4|F5|F6|F7|F8", indikatorne))
nrow(mikro_kombinationer)   # tjek: 2^6 - 1 = 63 kombinationer

mikro_kombinationer |>
  select(placering, antal_spoergsmaal, indikatorne, R2) |>
  head(5)

bedste_mikro     <- mikro_kombinationer |> slice(1)   # den bedste rene mikro-indikator
bedste_mikro_spm <- strsplit(bedste_mikro$spoergsmaalene, "+", fixed = TRUE)[[1]]

# læg den ind i sammenligningstabellen og sorter efter R2
sammenligning_med_mikro <- rbind(
  sammenligning_af_di_dst_bedste,
  sammenlign_indikatorer(bedste_mikro_spm, "Bedste mikro-indikator")
) |>
  arrange(desc(R2))
sammenligning_med_mikro   # tabellen til opgave 1.5


# =====================================================================
# Opgave 2.1: PCA-regression
# =====================================================================
# Y = den årlige realvækst, X = de 11 spørgsmål (korte navne F2, F3, ...)
pca_data <- data.frame(aarlig_realvaekst = forv1ogNKN1$aarlig_realvaekst,
                       forv1[kunkvt, spm_navne])
names(pca_data) <- c("aarlig_realvaekst", spm_koder)   # korte navne, så output er let at læse

# ---- Trin 1: almindelig multipel regression med alle spørgsmål ----
# aarlig_realvaekst ~ . betyder: forklar væksten med ALLE de andre kolonner
model_alle <- lm(aarlig_realvaekst ~ ., data = pca_data)
summary(model_alle)
# mange koefficienter er ikke signifikante, og nogle har mærkelige fortegn.
# det skyldes multikollinearitet: spørgsmålene ligner hinanden.

# korrelation mellem spørgsmålene: mange høje tal = multikollinearitet
pca_data |>
  select(-aarlig_realvaekst) |>   # kun de 11 spørgsmål
  cor() |>
  round(2)

# ---- Trin 2: PCA-regression ----
# scale = TRUE: alle spørgsmål får samme skala, før komponenterne laves
# validation = "CV": krydsvalidering, så vi kan se, hvor mange komponenter der er bedst
# RMSEP = hvor mange procentpoint modellen typisk rammer ved siden af på kvartaler, den ikke har set
set.seed(1)   # samme tilfældige opdeling i krydsvalideringen hver gang
pcr_model <- pcr(aarlig_realvaekst ~ ., data = pca_data, scale = TRUE, validation = "CV")
summary(pcr_model)   # forklaret variation og RMSEP for 1, 2, 3 ... komponenter

# vi vælger 1 komponent: den fanger mest af variationen i svarene (56 %) og kan ses som
# én samlet forbrugertillidsindikator, hvor vægtene er bestemt af PCA i stedet for at være lige store.
# fejlen falder fra 2,77 til 2,34. 2 komponenter giver 2,09, men én indikator er lettest at fortolke.
antal_komponenter <- 1   # bruges i opgave 2.2 og 2.3 – skal kun ændres her


# =====================================================================
# Opgave 2.2: De vigtigste spørgsmål
# =====================================================================
# vægtene (loadings) i 1. komponent: hvor meget hvert spørgsmål fylder i komponenten
vaegte <- pcr_model$loadings[, 1]
round(vaegte, 3)                                           # med fortegn
vaegte |> abs() |> sort(decreasing = TRUE) |> head(3)      # de 3 største vægte (abs fjerner fortegnet)

# koefficienterne med 1 komponent: hvert spørgsmåls effekt på forbrugsvæksten.
# spørgsmålene er skaleret, så koefficienten = effekten af én standardafvigelse højere svar.
koefficienter <- coef(pcr_model, ncomp = antal_komponenter)[, 1, 1]
round(koefficienter, 3)                                          # med fortegn
koefficienter |> abs() |> sort(decreasing = TRUE) |> head(3)     # svaret på 2.2: de 3 højeste vægte

# ---- 11 simple regressioner – ét spørgsmål ad gangen ----
# tom tabel med én række pr. spørgsmål. NA = tom celle, som fyldes ud i loopet
enkelt_regressioner <- data.frame(
  kode      = spm_koder,
  haeldning = NA,
  R2        = NA,
  p_vaerdi  = NA
)

for (i in 1:length(spm_koder)) {                              # loop: spørgsmål for spørgsmål
  svar  <- pca_data[[spm_koder[i]]]                           # svarene på spørgsmål nr. i
  model <- lm(pca_data$aarlig_realvaekst ~ svar)              # væksten forklaret af ét spørgsmål
  
  enkelt_regressioner$haeldning[i] <- coef(model)[2]                     # b1: hældningen
  enkelt_regressioner$R2[i]        <- summary(model)$r.squared           # forklaringsgrad
  enkelt_regressioner$p_vaerdi[i]  <- summary(model)$coefficients[2, 4]  # er hældningen signifikant?
}

# sorter efter R2, så det spørgsmål, der forklarer mest, står øverst
enkelt_regressioner <- enkelt_regressioner |> arrange(desc(R2))
enkelt_regressioner


# =====================================================================
# Opgave 2.3: Forudsig forbruget i 3. og 4. kvartal 2026
# =====================================================================
# opgaven spørger om 2025K3, men vi forudsiger "nuet": de nyeste kvartaler uden forbrugstal.
# 2026K3 = juli-september. 2026K4 bygger kun på oktober.
valgte_kvartaler <- c("2026K3", "2026K4")

nye_svar <- forv1 |>
  filter(TID %in% valgte_kvartaler) |>   # kun de to kvartaler
  select(-TID)                           # de 11 spørgsmål (uden TID)
names(nye_svar) <- spm_koder             # samme korte navne som i pca_data

# forudsig med PCR-modellen med 1 komponent
pcr_forudsigelse <- predict(pcr_model, newdata = nye_svar, ncomp = antal_komponenter)

data.frame(
  TID              = valgte_kvartaler,
  forudsagt_vaekst = round(as.numeric(pcr_forudsigelse), 2)
)

# til sammenligning: samme forudsigelse med 2 komponenter (+1,39 % i 3. kvartal 2026)
predict(pcr_model, newdata = nye_svar, ncomp = 2)


# =====================================================================
# Opgave 2.4: Den faktiske realvækst i 2016
# =====================================================================
# læg de fire kvartaler sammen til et helt år, og beregn væksten fra år til år
forbrug_aar <- NKN1 |>
  mutate(aar = substr(TID, 1, 4)) |>                                  # eks."2016K1" bliver til "2016"
  group_by(aar) |>                                                    # den gruppere pr. år
  summarise(value = sum(value)) |>                                    # samlet forbrug pr. år
  mutate(aarlig_realvaekst = (value / lag(value, 1) - 1) * 100)       # vækst ift. året før

forbrug_aar |> filter(aar == 2015| aar == 2016) 


# =====================================================================
# Opgave 3: Julehandel – detailhandlen i november og december (DETA212)
# =====================================================================
# Julehandel = detailhandlens mængdeindeks i november og december.
# Mængdeindeks: prisstigninger er renset ud, så vi måler mængden, ikke prisen.
# IKKE sæsonkorrigeret, fordi sæsonkorrektion netop fjerner julen.

# ---- Opgave 3.1: Forudsigelse ----

# Hent detailhandlens omsætning (DETA212, månedlig serie fra 2000)
detailhandel <- dst_get_data(
  table = "DETA212",
  query = list(VAREGR = "Detailhandel i alt", INDEKSTYPE = "Mængdeindeks", Tid = "*"),
  lang = "da",
  parse_dst_tid = FALSE #gør så at skrive stilen hos dst er det samme når jeg henter der ned
) |>
  mutate(aar    = as.numeric(substr(TID, 1, 4)),    # "2025M12" bliver til 2025
         maaned = as.numeric(substr(TID, 6, 7)))    # "2025M12" bliver til 12

# Julehandel pr. år: gennemsnit af november og december
julehandel <- detailhandel |>
  filter(maaned ==11|maaned == 12)|>                   # kun november og december
  group_by(aar) |>                                     # én gruppe pr. år
  summarise(value          = mean(value),              # gennemsnit af de to måneder
            antal_maaneder = n()) |>                   # n() tæller rækkerne i gruppen
  filter(antal_maaneder == 2) |>                       # kun år, hvor både november og december er udgivet
  arrange(aar) |>                                      # sorter efter år, før vi bruger lag()
  mutate(vaekst  = (value / lag(value, 1) - 1) * 100,  # vækst i forhold til året før
         hoejere = vaekst > 0) |>                      # det vi vil forudsige: højere end året før? (TRUE/FALSE)
  filter(!is.na(vaekst))                               # første år har intet år før

tail(julehandel, 10)        # de seneste/ældeste 10 år af 2000 til 2026
mean(julehandel$hoejere)    # andel af år med stigning i julehandel (ca. 64 %)

# DST's forbrugertillidsindikator – vi fjernede den i opgave 1, så den hentes igen her
# Gennemsnit af juli, august og september hvert år (det kender vi "nu" for 2026)
# ---- Vælg indikator her: dst_spm, di_spm eller bedste_spm (alle fra opgave 1.2) ----
valgte_spm <- dst_spm   # skift til bedste_spm for at se, hvad min bedste indikator siger om julehandlen

# Tilliden i 3. kvartal (juli-september) hvert år = gennemsnittet af de valgte spørgsmål
# forv1 er allerede lavet om til kvartaler øverst i scriptet, så vi tager bare K3-rækkerne
kun_k3 <- grepl("K3", forv1$TID)   # TRUE for alle 3. kvartaler, fx "2026K3"
tillid_q3 <- data.frame(
  aar       = as.numeric(substr(forv1$TID[kun_k3], 1, 4)),   # "2026K3" bliver til 2026
  tillid_q3 = rowMeans(forv1[kun_k3, valgte_spm, drop = FALSE]),   # gennemsnit af de valgte spørgsmål i juli-september
  row.names = NULL   # nulstil rækkenumrene, så de går 1, 2, 3 ...
)
# Sæt tilliden sammen med julehandlen, så hvert år har begge dele
# inner_join() gør det samme som merge(): sætter tabellerne sammen på året
model_data <- julehandel |>
  select(aar, vaekst, hoejere) |>
  inner_join(tillid_q3, by = "aar")
nrow(model_data)            # 25 år (2001-2025)
sum(model_data$hoejere)     # 16 år med højere julehandel

# Logistisk regression
# family = binomial: y er ja/nej (TRUE/FALSE), så modellen giver en sandsynlighed
jul_model <- glm(hoejere ~ tillid_q3, data = model_data, family = binomial)
summary(jul_model)          # log-odds = 0,583 + 0,056 * tillid

# Forudsig julehandlen 2026 ud fra tilliden i juli-september 2026
tillid_2026 <- tillid_q3 |> filter(aar == 2026)
tillid_2026                                                      # ca. -13,8
predict(jul_model, newdata = tillid_2026, type = "response")     # ca. 45 %

# jeg vil se den tillid, hvor sandsynligheden er præcis 50 % (log-odds = 0) altså på grænsen
-coef(jul_model)[1] / coef(jul_model)[2]                         # ca. -10,4

# ---- Opgave 3.2: Validering ----

# Leave-one-out altså tag ét år ud ad gangen, lav modellen på de andre 24 år, og gæt det udeladte år
gaet <- c() #gæt
for (i in 1:nrow(model_data)) {                     # loop: år for år
  traening      <- model_data[-i, ]                 # alle år undtagen år nr. i
  model         <- glm(hoejere ~ tillid_q3, data = traening, family = binomial)
  sandsynlighed <- predict(model, newdata = model_data[i, ], type = "response")
  gaet[i]       <- sandsynlighed > 0.5              # TRUE = modellen gætter "højere"
}

mean(gaet == model_data$hoejere)                    # andel rigtige gæt (ca. 60 %)
mean(model_data$hoejere)                            # simpelt gæt "altid højere" (ca. 64 %)
table(gaet = gaet, faktisk = model_data$hoejere)    # confusion matrix
model_data |> filter(gaet != hoejere)               # de år, modellen gætter forkert

# Test på julehandlen 2025: modellen laves kun på 2001-2024, og vi gætter 2025
model_til_2024 <- glm(hoejere ~ tillid_q3, data = model_data |> filter(aar <= 2024), family = binomial)
predict(model_til_2024, newdata = model_data |> filter(aar == 2025), type = "response")   # ca. 25 %
model_data |> filter(aar == 2025)                   # faktisk: julehandlen steg


# =====================================================================
# Opgave 4.1: Stabilitet i forbrugertillidsindikatoren
# =====================================================================

# ---- Vores stabilitetstest: blokke på 36 kvartaler ----
# Inspireret af Judgment-artiklen, som bruger vinduer på 36 kvartaler
# (Gayer, Girardi og Reuter, 2016, s. 12).
# Vi deler perioden op i blokke på 36 kvartaler, der ikke overlapper:
# kvartal 1-36, 37-72 og 73 til seneste kvartal.
# I hver blok finder vi den bedste kombination og ser, hvor godt
# vores bedste indikator, DI og DST klarer sig.
# Er en indikator stabil, bør den klare sig godt i alle blokke.

# Saml alle 2047 kombinationer i én stor matrix (kun kvartalerne i analysen)
# do.call(cbind, liste) sætter alle matricerne i listen ved siden af hinanden
alle_indikatorer <- do.call(cbind, resultater)[kunkvt, ]# do.call konstruerer en funktion fra et navn eller en funktion eller en liste af argumenter
vaekst           <- forv1ogNKN1$aarlig_realvaekst      # den årlige realvækst som en vektor
alle_koder       <- unlist(kombinationskoder)          # navnene på alle 2047 kombinationer

# Vores tre indikatorer – lavet ud fra de vektorer, vi allerede har fra opgave 1.2
# rowMeans tager gennemsnittet af de valgte spørgsmål i hvert kvartal
vores_indikator <- rowMeans(forv1[kunkvt, bedste_spm])
di_indikator    <- rowMeans(forv1[kunkvt, di_spm])
dst_indikator   <- rowMeans(forv1[kunkvt, dst_spm])

blok_laengde <- 36
blok_start   <- seq(1, length(vaekst), by = blok_laengde)  # blokkene starter i kvartal 1, 37 og 73

blok_resultater <- data.frame()                             # tom tabel, som fyldes i loopet

for (start in blok_start) {                                 # loop: blok for blok
  slut    <- min(start + blok_laengde - 1, length(vaekst))  # min() sikrer, at sidste blok stopper ved seneste kvartal
  raekker <- start:slut
  r2      <- cor(alle_indikatorer[raekker, ], vaekst[raekker])^2   # R2 for alle 2047 i denne blok
  
  blok_resultater <- rbind(blok_resultater, data.frame(
    periode             = paste(forv1ogNKN1$TID[start], "-", forv1ogNKN1$TID[slut]),
    antal_kvartaler     = length(raekker),
    bedste_i_blokken    = alle_koder[which.max(r2)],        # denne linje gør så vi får vist den bedste r2 i blokken altså perioden
    R2_bedste_i_blokken = max(r2),
    R2_vores_bedste     = cor(vores_indikator[raekker], vaekst[raekker])^2,
    R2_DI               = cor(di_indikator[raekker],    vaekst[raekker])^2,
    R2_DST              = cor(dst_indikator[raekker],   vaekst[raekker])^2
  ))
}

blok_resultater

# få vist R2 for bedste, di og dst R2 
# di og dst er ikke gode længere men hvis man vil se dem
blok_resultater |>
  select(R2_vores_bedste, R2_DI, R2_DST) |>
  colMeans()


# =====================================================================
# Opgave 5: DMI's API – vindstyrke og vindretning i to storme
# =====================================================================
# Stormen i november 2022: Anholt Havn (06079, ude i Kattegat) og Århus Syd (06074, på land)
# Stormen i oktober 2023:  Kegnæs Fyr (06119, ved åbent vand) og Skrydstrup (06110, inde i land)
# Vi bruger middelværdien for den seneste time: wind_speed_past1h (m/s) og wind_dir_past1h (grader)
# DMI kræver ikke API-nøgle længere (siden 2. december 2025).

library(jsonlite)   # fromJSON() henter og læser JSON direkte fra en URL

# ---- 1. Funktion: hent én parameter for én station i en periode ----
hent_dmi <- function(station, parameter, fra, til) {
  url <- paste0("https://opendataapi.dmi.dk/v2/metObs/collections/observation/items",
                "?stationId=", station,
                "&parameterId=", parameter,
                "&datetime=", fra, "/", til,
                "&limit=10000")
  svar <- fromJSON(url)   # GET-request + JSON laves om til R-data
  svar$features$properties |>   # selve målingerne ligger her
    mutate(tid = ymd_hms(observed) |> with_tz("Europe/Copenhagen")) |>   # DMI leverer UTC, vi viser dansk tid
    select(tid, value)
}

# ---- 2. Funktion: vindstyrke og vindretning for én station ----
hent_vind <- function(station, station_navn, fra, til) {
  vindstyrke  <- hent_dmi(station, "wind_speed_past1h", fra, til) |> rename(vindstyrke = value)   # m/s
  vindretning <- hent_dmi(station, "wind_dir_past1h",   fra, til) |> rename(vindretning = value)  # grader, der hvor vinden kommer FRA
  vindstyrke |>
    inner_join(vindretning, by = "tid") |>   # sæt sammen på tidspunktet
    mutate(station = station_navn) |>
    arrange(tid)
}

# ---- 3. Hent de to storme ----
storm_2022 <- bind_rows(
  hent_vind("06079", "Anholt", "2022-11-14T00:00:00Z", "2022-11-22T00:00:00Z"),
  hent_vind("06074", "Århus",  "2022-11-14T00:00:00Z", "2022-11-22T00:00:00Z")
)

storm_2023 <- bind_rows(
  hent_vind("06119", "Kegnæs",     "2023-10-18T00:00:00Z", "2023-10-25T00:00:00Z"),
  hent_vind("06110", "Skrydstrup", "2023-10-18T00:00:00Z", "2023-10-25T00:00:00Z")
)

# ---- 4. Hvornår blæste det mest, og hvorfra kom vinden? ----
# Vindretningen i 8 verdenshjørner á 45 grader. Hos DMI er nord 360, og 0 betyder vindstille.
verdenshjoerner <- c("N", "NØ", "Ø", "SØ", "S", "SV", "V", "NV", "N")

bind_rows(storm_2022, storm_2023) |>
  group_by(station) |>
  slice_max(vindstyrke, n = 1, with_ties = FALSE) |>   # rækken med den højeste vindstyrke pr. station
  mutate(kommer_fra = verdenshjoerner[round(vindretning / 45) + 1])
# Anholt 12,6 m/s fra NØ (19. nov), Århus 8,2 m/s (16. nov), Kegnæs 26,8 m/s fra Ø (20. okt), Skrydstrup 13,4 m/s (20. okt)


# ---- 5. Funktion: graf med vindstyrke og pile for vindretningen ----
# Pilen peger derhen, hvor vinden blæser, altså modsat af hvor den kommer fra (retning + 180 grader).
# Pilens vinkel er kun omtrentlig, fordi x-aksen er tid og y-aksen er m/s.
lav_vindgraf <- function(data, farver, titel, undertitel, kompas_andel = 0.2) {
  hoejeste_vind <- max(data$vindstyrke)
  pil_tid       <- 3 * 3600             # pilens længde vandret: 3 timer (i sekunder)
  pil_vind      <- hoejeste_vind / 25   # pilens længde lodret: en lille del af y-aksen
  
  pile <- data |>
    filter(vindretning > 0,                                      # 0 = vindstille: ingen retning
           as.numeric(format(tid, "%H")) %% 3 == 0) |>           # én pil hver 3. time, så grafen ikke drukner
    mutate(vinkel    = ((vindretning + 180) %% 360) * pi / 180,  # retningen vinden blæser hen, i radianer
           tid_slut  = tid + sin(vinkel) * pil_tid,              # sin = den vandrette del (øst/vest)
           vind_slut = vindstyrke + cos(vinkel) * pil_vind)      # cos = den lodrette del (nord/syd)
  
  # kompasset står øverst i grafen, kompas_andel inde på tidsaksen (0,2 = 20 % fra venstre)
  kompas_x        <- min(data$tid) + kompas_andel * (max(data$tid) - min(data$tid))
  kompas_y        <- 0.85 * hoejeste_vind
  kompas_arm_tid  <- 12 * 3600            # halv bredde: 12 timer
  kompas_arm_vind <- hoejeste_vind / 12   # halv højde
  
  data |>
    ggplot(aes(x = tid, y = vindstyrke, colour = station)) +
    geom_line() +
    geom_segment(data = pile, aes(xend = tid_slut, yend = vind_slut),
                 colour = "black", arrow = arrow(length = unit(0.12, "cm")), show.legend = FALSE) +
    annotate("segment", x = kompas_x - kompas_arm_tid, xend = kompas_x + kompas_arm_tid,
             y = kompas_y, yend = kompas_y, arrow = arrow(ends = "both", length = unit(0.1, "cm"))) +
    annotate("segment", x = kompas_x, xend = kompas_x,
             y = kompas_y - kompas_arm_vind, yend = kompas_y + kompas_arm_vind,
             arrow = arrow(ends = "both", length = unit(0.1, "cm"))) +
    annotate("text", label = c("N", "S", "V", "Ø"), size = 3,
             x = c(kompas_x, kompas_x, kompas_x - 1.4 * kompas_arm_tid, kompas_x + 1.4 * kompas_arm_tid),
             y = c(kompas_y + 1.4 * kompas_arm_vind, kompas_y - 1.4 * kompas_arm_vind, kompas_y, kompas_y)) +
    annotate("text", label = "Pil peger derhen,\nvinden blæser", size = 2.5,
             x = kompas_x, y = kompas_y - 2.8 * kompas_arm_vind) +
    scale_colour_manual(values = farver) +
    scale_x_datetime(date_breaks = "2 days", date_labels = "%b %d") +
    labs(title = titel, subtitle = undertitel,
         x = "Tid", y = "Vindhastighed (m/s)", colour = "Forklaring",
         caption = "Data: DMI - Graf: genereret i RStudio")
}


# ---- 6. De to grafer ----
figur_2022 <- lav_vindgraf(
  storm_2022,
  farver     = c("Anholt" = "#F8766D", "Århus" = "#00BFC4"),
  titel      = "Vind Anholt og Århus i perioden 14-21. november 2022",
  undertitel = paste0("Anholt: stigende vind til hård vind (12,6 m/s) fra nordøst den 19. november.\n",
                      "Århus: op til frisk vind (8,2 m/s) fra øst-sydøst den 16. november.")
)
figur_2022

figur_2023 <- lav_vindgraf(
  storm_2023,
  farver       = c("Kegnæs" = "darkred", "Skrydstrup" = "steelblue"),
  titel        = "Storm Skrydstrup og Kegnæs i perioden 18-24. oktober 2023",
  undertitel   = paste0("Skrydstrup: op til hård vind (13,4 m/s) fra øst-nordøst den 20. oktober.\n",
                        "Kegnæs Fyr: stigende vind til storm (26,8 m/s) fra øst den 20. oktober."),
  kompas_andel = 0.6   # kompasset længere til højre, så det ikke dækker toppen den 20. oktober
)
figur_2023

# Gem graferne (valgfrit)
ggsave("fig_storm_2022.png", figur_2022, width = 9, height = 5.5, dpi = 200)
ggsave("fig_storm_2023.png", figur_2023, width = 9, height = 5.5, dpi = 200)

