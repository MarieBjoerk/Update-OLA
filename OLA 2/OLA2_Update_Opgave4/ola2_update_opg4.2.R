library(dkstat)
library(dplyr)
library(lubridate)
library(zoo)

# Hent metadata
forv1_meta <- dst_meta("FORV1", lang = "da")

# Filtrer til underspørgsmålet
filters <- list(
  INDIKATOR = "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
  Tid = "*"
)

# Hent data
forv_raw <- dst_get_data(
  table = "FORV1",
  query = filters,
  lang  = "da",
  meta  = forv1_meta
)

#
forv_ny <- forv_raw %>%
  mutate(kvartal = as.yearqtr(TID)) %>%
  group_by(kvartal) %>%
  summarise(indeks = mean(value, na.rm = TRUE)) %>%
  filter(
    kvartal >= as.yearqtr("2000 Q1"),
    kvartal <= as.yearqtr("2025 Q3")
  )

###################################################
snit <- mean(forv_ny$indeks, na.rm = TRUE)
maks <- max(forv_ny$indeks, na.rm = TRUE)
min <- min(forv_ny$indeks, na.rm = TRUE)

seneste <- forv_ny %>% 
  slice_max(kvartal)

seneste


snit
maks
min


#nu ggplot
library(ggplot2)

ggplot(forv_ny, aes(x = kvartal, y = indeks)) +
  geom_line(color = "steelblue", linewidth = 1) +
  scale_x_yearqtr(
    format = "%Y",
    expand = c(0, 0)      
  ) +
  labs(
    title = "Danskerne siger nej til store køb - Forbrugertilliden ligger markant under nul siden 2000",
    subtitle = "Fra finanskrise til inflation: Danskernes lyst til store køb styrtdykker i krisetider",
    caption = "Kilde: Danmarks Statistik",
    x = "År",
    y = "Indeksværdi"
  ) +
  theme_minimal(base_size = 14)








