library(dkstat)
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)

## Fælles: hent metadata til FORV1 ---------------------------------
forv1_meta <- dst_meta(table = "FORV1", lang = "da")

############################################################
# OPGAVE 4.1 – Forbrugertillidsindikatoren, kvartaler fra 1996Q1
############################################################

# 1) Filtre: kun hovedindikatoren, alle måneder
forv1_filters <- list(INDIKATOR = "Forbrugertillidsindikatoren", Tid  = "*"
      )

# 2) Hent data fra Statistikbanken
forv1_raw <- dst_get_data(table = "FORV1", query = forv1_filters,lang  = "da", 
            meta  = forv1_meta)

# 3) Omdan dato til kvartal
forv1 <- forv1_raw %>%
  mutate(Dato    = as.Date(paste0(substr(TID, 1, 4), "-", substr(TID, 6, 7), "-01")),
    Kvartal = as.yearqtr(Dato)
  )

# 4) Filtrér perioden til 1996Q1 → seneste kvartal
forv1_ny <- forv1 %>%
  filter(Kvartal >= as.yearqtr("1996 Q1"))

# 5) Beregn gennemsnit pr. kvartal  
forbrugK <- forv1_ny %>%
  group_by(Kvartal) %>%
  summarise(
    ForbT = mean(value, na.rm = TRUE)   
  )

library(zoo)    # for scale_x_yearqtr

ggplot(forbrugK, aes(x = as.Date(Kvartal), y = ForbT)) +
  geom_line(color = "blue") +
  scale_x_date(
    expand = c(0, 0)        
  ) +
  labs(
    title = "Forbrugertilliden falder markant - danskerne er blevet væsentligt mere tilbageholdende",
    subtitle = "Store udsving i forbrugertilliden viser, hvordan økonomiske chok - fra finanskrisen til inflation - påvirker danskernes tillid og lyst til at forbruge.",
    caption = "Kilde: Danmarks Statistik",
    x = "År",
    y = "Nettotal"
  ) +
  theme_minimal(base_size = 14)








