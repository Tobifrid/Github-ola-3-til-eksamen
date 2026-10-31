# =====================================================================
# OLA 3 – Opgave 2: Principal Component Analysis
# =====================================================================
# Opgave 2 bygger på data og navne fra opgave 1 (forv1, NKN1, forv1ogNKN1,
# kun_kvt_fra_forv1_og_nkn1, F_koder osv.). source() kører hele opgave1.R først,
# så alt det findes, før opgave 2 starter. opgave1.R skal ligge i samme mappe.
source("opgave1.R", encoding = "UTF-8")                                         # kør opgave 1 først (henter data og laver navnene)

# =====================================================================
# Opgave 2.1: PCA regression
# =====================================================================
# Y = den årlige realvækst i NKN1, X = de 11 spørgsmål (korte navne F2, F3, ...)
pca_data <- data.frame(aarlig_realvaekst_i_NKN1 = forv1ogNKN1$aarlig_realvaekst_i_NKN1,
                       forv1[kun_kvt_fra_forv1_og_nkn1, F_spoergsmaalenes_hele_tekst])   # kun kvartaler med forbrugstal
names(pca_data) <- c("aarlig_realvaekst_i_NKN1", F_koder)                                 # korte navne, så output er let at læse

# ---- Trin 1: almindelig multipel regression med alle spørgsmål ----
# aarlig_realvaekst_i_NKN1 ~ . betyder: forklar væksten med ALLE de andre kolonner
model_alle <- lm(aarlig_realvaekst_i_NKN1 ~ ., data = pca_data)
summary(model_alle)
# R2 0,586 (justeret 0,537), men kun F9, F10 og F12 er signifikante, og nogle har mærkelige fortegn.
# det skyldes multikollinearitet: spørgsmålene ligner hinanden, så modellen kan ikke skille effekterne ad.

# korrelation mellem spørgsmålene: mange høje tal = multikollinearitet
pca_data |>
  select(-aarlig_realvaekst_i_NKN1) |>                                           # kun de 11 spørgsmål
  cor() |>
  round(2)

# ---- Trin 2: PCA-regression ----
# scale = TRUE: alle spørgsmål får samme skala, før komponenterne laves
# validation = "CV": krydsvalidering, så vi kan se, hvor mange komponenter der er bedst
# RMSEP = hvor mange procentpoint modellen typisk rammer ved siden af på kvartaler, den ikke har set
set.seed(1)                                                                      # samme tilfældige opdeling i krydsvalideringen hver gang
pcr_model <- pcr(aarlig_realvaekst_i_NKN1 ~ ., data = pca_data, scale = TRUE, validation = "CV")
summary(pcr_model)                                                               # forklaret variation og RMSEP for 1, 2, 3 ... komponenter
# 1 komponent: 56 % af variationen i svarene, 31 % af variationen i væksten, RMSEP 2,34 (uden komponenter: 2,77)
# 2 komponenter: RMSEP 2,09. Laveste fejl ved 9 komponenter (1,99), men næsten alle komponenter = multikollineariteten tilbage

# vi vælger 1 komponent: den fanger mest af variationen i svarene og kan ses som én samlet
# forbrugertillidsindikator, hvor vægtene er bestemt af PCA i stedet for at være lige store.
antal_komponenter <- 1                                                           # bruges i opgave 2.2 og 2.3 – skal kun ændres her


# =====================================================================
# Opgave 2.2: De vigtigste indikatorer
# =====================================================================
# vægtene (loadings) i 1. komponent: hvor meget hvert spørgsmål fylder i komponenten
vaegte <- pcr_model$loadings[, 1]
round(vaegte, 3)                                                                 # med fortegn (alle negative = komponenten er bare vendt)
vaegte |> abs() |> sort(decreasing = TRUE) |> head(3)                            # de 3 største vægte (abs fjerner fortegnet)
# svaret på 2.2: F2 (0,373), F10 (0,362) og F3 (0,344). F7 har næsten ingen vægt (0,017)

# koefficienterne med 1 komponent: hvert spørgsmåls effekt på forbrugsvæksten.
# spørgsmålene er skaleret, så koefficienten = effekten af én standardafvigelse højere svar.
koefficienter <- coef(pcr_model, ncomp = antal_komponenter)[, 1, 1]
round(koefficienter, 3)                                                          # med fortegn
koefficienter |> abs() |> sort(decreasing = TRUE) |> head(3)                     # samme top 3 som vægtene, fordi der kun er 1 komponent

# ---- 11 simple regressioner – ét spørgsmål ad gangen ----
# tom tabel med én række pr. spørgsmål. NA = tom celle, som fyldes ud i loopet
enkelt_regressioner <- data.frame(
  F_kode    = F_koder,
  haeldning = NA,
  R2        = NA,
  p_vaerdi  = NA
)

for (i in 1:length(F_koder)) {                                                   # loop: spørgsmål for spørgsmål
  svar  <- pca_data[[F_koder[i]]]                                                # svarene på spørgsmål nr. i
  model <- lm(pca_data$aarlig_realvaekst_i_NKN1 ~ svar)                          # væksten forklaret af ét spørgsmål
  
  enkelt_regressioner$haeldning[i] <- coef(model)[2]                             # b1: hældningen
  enkelt_regressioner$R2[i]        <- summary(model)$r.squared                   # forklaringsgrad
  enkelt_regressioner$p_vaerdi[i]  <- summary(model)$coefficients[2, 4]          # er hældningen signifikant?
}

enkelt_regressioner <- enkelt_regressioner |> arrange(desc(R2))                  # det spørgsmål, der forklarer mest, øverst
enkelt_regressioner
# F10 forklarer mest alene (R2 0,438). F7 har R2 0,122 men næsten ingen vægt i PCA:
# PCA vægter efter variationen i svarene, ikke efter forbruget


# =====================================================================
# Opgave 2.3: Forudsig forbruget
# =====================================================================
# opgaven spørger om 2025K3, men vi forudsiger "nuet": de kvartaler, der endnu ikke har forbrugstal.
# ! vender TRUE/FALSE, så vi får kvartalerne UDEN forbrugstal (nu 2026K3 og 2026K4, til eksamen 2026K4)
valgte_kvartaler <- forv1$TID[!kun_kvt_fra_forv1_og_nkn1]
valgte_kvartaler

nye_svar <- forv1 |>
  filter(TID %in% valgte_kvartaler) |>                                           # kun de nye kvartaler
  select(-TID)                                                                   # de 11 spørgsmål (uden TID)
names(nye_svar) <- F_koder                                                       # samme korte navne som i pca_data

# forudsig med PCR-modellen med 1 komponent
pcr_forudsigelse <- predict(pcr_model, newdata = nye_svar, ncomp = antal_komponenter)

data.frame(
  TID              = valgte_kvartaler,
  forudsagt_vaekst = round(as.numeric(pcr_forudsigelse), 2)
)
# 2026K3: -0,35 %. 2026K4 (kun oktober): -0,91 %
# jeg tror ikke helt på det: forbruget voksede 2,7 % i 2026K2, tilliden er usædvanligt lav,
# og komponenten vægter næsten ikke F7, som er høj i 2026

# til sammenligning: samme forudsigelse med 2 komponenter
predict(pcr_model, newdata = nye_svar, ncomp = 2)
# +1,39 % i 2026K3: fortegnet skifter, når man tilføjer én komponent = usikker model


# =====================================================================
# Opgave 2.4: Forudsigelser fra DI
# =====================================================================
# DI forventede i 2016 en vækst i privatforbruget på lidt over 2 %.
# læg de fire kvartaler sammen til et helt år, og beregn væksten fra år til år
forbrug_aar <- NKN1 |>
  mutate(aar = substr(TID, 1, 4)) |>                                             # "2016K1" bliver til "2016"
  group_by(aar) |>                                                               # én gruppe pr. år
  summarise(value = sum(value)) |>                                               # samlet forbrug pr. år
  mutate(vaekst_hele_aaret = (value / lag(value, 1) - 1) * 100)                  # vækst ift. året før

forbrug_aar |> filter(aar == "2015" | aar == "2016")
# 2015: 952,9 mia. kr. og 2016: 984,5 mia. kr. = 3,3 % vækst
# DI ramte ca. 1 procentpoint for lavt (forbehold: tallene er revideret siden, og DI brugte 2015-priser)