# =====================================================================
# OLA 3 – Opgave 5: Data Science-modellen og DMI's API
# =====================================================================
# Opgave 5 står helt for sig selv: den henter data fra DMI og bruger ikke noget fra opgave 1.

library(tidyverse)   # dplyr, ggplot2 og lubridate (ymd_hms, with_tz)
library(jsonlite)    # fromJSON() henter og læser JSON direkte fra en URL


# =====================================================================
# Opgave 5.1: Dokumentationen
# =====================================================================
# DMI's dokumentation ligger ikke længere på Confluence, men på dmi.dk under Frie data:
# 1. Oversigt over API'erne:        https://www.dmi.dk/friedata/dokumentation/apis
# 2. Basics (ingen API-nøgle):      https://www.dmi.dk/friedata/dokumentation/basics
# 3. Meteorological Observation API: https://www.dmi.dk/friedata/dokumentation/meteorological-observation-api
# 4. Stationsliste (stations-id):    https://www.dmi.dk/friedata/dokumentation/data/meteorological-observation-data-stations
# 5. Parameterliste:                 https://www.dmi.dk/friedata/dokumentation/meteorological-observations-data
# 6. Base-URL for metObs:            https://opendataapi.dmi.dk/v2/metObs
# Swagger: skriv /api efter base-URL'en, vælg GET /collections/observation/items, "Try it out",
# udfyld stationId, parameterId og datetime, klik "Execute" og kopiér Request URL.
# URL = base-URL + collection (observation/items) + filtre (station, parameter og periode)

# Stormen i november 2022: Anholt Havn (06079, ude i Kattegat) og Århus Syd (06074, på land)
# Stormen i oktober 2023:  Kegnæs Fyr (06119, ved åbent vand) og Skrydstrup (06110, inde i land)
# Vi bruger middelværdien for den seneste time: wind_speed_past1h (m/s) og wind_dir_past1h (grader)
# DMI kræver ikke API-nøgle længere (siden 2. december 2025).


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

# =====================================================================
# Opgave 5.2: En graf med mangler
# =====================================================================
# Grafen i opgaven viser kun vindstyrken og mangler retningen. Jeg viser retningen med pile.
# Storm = middelvind på mindst 24,5 m/s (Beaufort 10). wind_speed_past1h er middelvinden for den
# seneste time, så den kan sammenlignes direkte med grænsen.

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
    geom_hline(yintercept = 24.5, linetype = "dashed", colour = "red") +   # stormgrænsen: middelvind på mindst 24,5 m/s
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


# =====================================================================
# Opgave 5.3: Eksempler på anvendelse
# =====================================================================
# 1. Python-biblioteket edu230991/dmi-open-data på GitHub: henter DMI-data med få linjer kode.
#    Business case: sparer udviklingstid – samme idé som min funktion hent_dmi().
# 2. Kort over DMI's klimagitter (DMI Third-party Examples): find klimadata for en bestemt lokation.
#    Business case: klimarisici pr. ejendom eller mark, fx til ESG-rapportering (min egen vurdering).
# 3. Dansk Industri (23. juni 2020): "Virksomheder jubler over gratis vejrdata" (Intellishore, Unity Studios).
#    Business case: vejret påvirker efterspørgsel og drift, fx udeservering eller vindmøller.