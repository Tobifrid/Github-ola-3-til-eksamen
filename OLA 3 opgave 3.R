# =====================================================================
# OLA 3 – Opgave 3: Julehandel
# =====================================================================
# Opgave 3 bygger på data og navne fra opgave 1 (forv1, dst_spm, di_spm og bedste_spm).
# source() kører hele "OLA 3 opgave 1.R" først, så alt det findes, før opgave 3 starter.
# Filen skal ligge i samme mappe som projektet.
source("OLA 3 opgave 1.R", encoding = "UTF-8")                                  # kør opgave 1 først (henter data og laver navnene)

# Julehandel = detailhandlens mængdeindeks i november og december (DST, tabel DETA212).
# Mængdeindeks: prisstigninger er renset ud, så vi måler mængden, ikke prisen.
# IKKE sæsonkorrigeret, fordi sæsonkorrektion netop fjerner julen.


# =====================================================================
# Opgave 3.1: Forudsigelse
# =====================================================================

# ---- Hent detailhandlens omsætning (DETA212, månedlig serie fra 2000) ----
detailhandel <- dst_get_data(
  table = "DETA212",
  query = list(VAREGR = "Detailhandel i alt", INDEKSTYPE = "Mængdeindeks", Tid = "*"),
  lang = "da",
  parse_dst_tid = FALSE                                                          # behold tiden som tekst, fx "2025M12"
) |>
  mutate(aar    = as.numeric(substr(TID, 1, 4)),                                 # "2025M12" bliver til 2025
         maaned = as.numeric(substr(TID, 6, 7)))                                 # "2025M12" bliver til 12

# ---- Julehandel pr. år: gennemsnit af november og december ----
julehandel <- detailhandel |>
  filter(maaned == 11 | maaned == 12) |>                                         # kun november og december
  group_by(aar) |>                                                               # én gruppe pr. år
  summarise(value          = mean(value),                                        # gennemsnit af de to måneder
            antal_maaneder = n()) |>                                             # n() tæller rækkerne i gruppen
  filter(antal_maaneder == 2) |>                                                 # kun år, hvor både november og december er udgivet
  arrange(aar) |>                                                                # sorter efter år, før vi bruger lag()
  mutate(vaekst  = (value / lag(value, 1) - 1) * 100,                            # vækst i forhold til året før
         hoejere = vaekst > 0) |>                                                # det vi vil forudsige: højere end året før? (TRUE/FALSE)
  filter(!is.na(vaekst))                                                         # første år har intet år før

tail(julehandel, 10)
# de seneste 10 år
mean(julehandel$hoejere)
# andel år med stigning: ca. 64 %

# ---- Vælg indikator her: dst_spm, di_spm eller bedste_spm (alle fra opgave 1.2) ----
valgte_spm <- dst_spm                                                            # skift til bedste_spm for at se, hvad min bedste indikator siger

# ---- Tilliden i 3. kvartal (juli-september) hvert år ----
# forv1 er allerede lavet om til kvartaler i opgave 1, så vi tager bare K3-rækkerne
kun_k3 <- grepl("K3", forv1$TID)                                                 # TRUE for alle 3. kvartaler, fx "2026K3"
tillid_q3 <- data.frame(
  aar       = as.numeric(substr(forv1$TID[kun_k3], 1, 4)),                       # "2026K3" bliver til 2026
  tillid_q3 = rowMeans(forv1[kun_k3, valgte_spm, drop = FALSE]),                 # gennemsnit af de valgte spørgsmål i juli-september
  row.names = NULL                                                               # nulstil rækkenumrene, så de går 1, 2, 3 ...
)

# ---- Sæt tilliden sammen med julehandlen, så hvert år har begge dele ----
model_data <- julehandel |>
  select(aar, vaekst, hoejere) |>
  inner_join(tillid_q3, by = "aar")                                              # inner_join: kun år, der findes i begge
nrow(model_data)
# 25 år (2001-2025)
sum(model_data$hoejere)
# 16 år med højere julehandel, 9 år med fald (2007-2013, 2019 og 2022)

# ---- Logistisk regression ----
# family = binomial: y er TRUE/FALSE, så modellen regner i log-odds og giver en sandsynlighed
jul_model <- glm(hoejere ~ tillid_q3, data = model_data, family = binomial)
summary(jul_model)
# log-odds = 0,584 + 0,0558 * tillid. Hældningen er positiv, men ikke signifikant (p = 0,24)

exp(coef(jul_model))                                                             # e opløftet i koefficienterne = odds ratio
# OR ca. 1,057: hver gang tilliden stiger 1 point, bliver odds for højere julehandel ca. 5,7 % højere

# ---- Forudsig julehandlen 2026 ud fra tilliden i juli-september 2026 ----
tillid_2026 <- tillid_q3 |> filter(aar == 2026)
tillid_2026
# ca. -13,8
predict(jul_model, newdata = tillid_2026, type = "response")                     # type = "response" giver sandsynligheden
# ca. 45 % sandsynlighed for, at julehandlen 2026 bliver højere end 2025

-coef(jul_model)[1] / coef(jul_model)[2]                                         # grænsen: den tillid, hvor sandsynligheden er præcis 50 % (log-odds = 0)
# ca. -10,5: tillid under grænsen = modellen gætter "ikke højere"


# =====================================================================
# Opgave 3.2: Validering af model
# =====================================================================

# ---- Leave-one-out: tag ét år ud ad gangen, lav modellen på de andre 24 år, og gæt det udeladte år ----
gaet <- c()                                                                      # tom vektor, som fyldes i loopet
for (i in 1:nrow(model_data)) {                                                  # loop: år for år
  traening      <- model_data[-i, ]                                              # alle år undtagen år nr. i
  model         <- glm(hoejere ~ tillid_q3, data = traening, family = binomial)
  sandsynlighed <- predict(model, newdata = model_data[i, ], type = "response")
  gaet[i]       <- sandsynlighed > 0.5                                           # TRUE = modellen gætter "højere"
}

mean(gaet == model_data$hoejere)
# andel rigtige gæt: 60 %
mean(model_data$hoejere)
# simpelt gæt "altid højere": 64 %, altså er modellen dårligere end at gætte

table(gaet = gaet, faktisk = model_data$hoejere)                                 # confusion matrix: rækker = gæt, kolonner = faktisk
# rigtige: 1 (ikke højere) + 14 (højere) = 15 af 25. Den fanger kun 1 af de 9 år med fald

model_data |> filter(gaet != hoejere)                                            # de år, modellen gætter forkert
# fald i år med positiv tillid (fx 2007, 2010 og 2013) og stigning i år med meget lav tillid (2023 og 2025)

# ---- Opgavens eget spørgsmål: blev julehandlen 2025 større end 2024? ----
# modellen laves kun på 2001-2024, og vi gætter 2025
model_til_2024 <- glm(hoejere ~ tillid_q3, data = model_data |> filter(aar <= 2024), family = binomial)
predict(model_til_2024, newdata = model_data |> filter(aar == 2025), type = "response")
# ca. 25 %, så modellen gætter "ikke højere"
model_data |> filter(aar == 2025)
# faktisk steg julehandlen 4,1 % i 2025: modellen ramte forkert

# Konklusion: DETA212 gør, at vi måler det rigtige, men ikke at modellen forudsiger godt.
# Den er ikke bedre end at gætte "altid højere", tilliden er ikke signifikant,
# og danskerne handler ofte anderledes, end de svarer i spørgeundersøgelsen.