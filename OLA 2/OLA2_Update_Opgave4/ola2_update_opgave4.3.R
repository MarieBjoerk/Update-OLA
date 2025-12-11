library(dkstat)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# OPGAVE 4.3 – De 15 grupper af forbrug, via API
# Kvartaler lægges sammen til år (som din lærer har sagt)

# 1) Hent metadata fra NKHC021
forbrug_meta <- dst_meta("NKHC021", lang = "da")

# 2) Hent ALLE forbrugsgrupper fra metadata
alle_grupper <- forbrug_meta$values$FORMAAAL$text

# 3) Fjern "I alt" → tilbage er de 15 grupper
formaal_15 <- alle_grupper[alle_grupper != "I alt"]

# 4) Definér filtre automatisk som i FORV1-eksemplet
forbrug_filters <- list(
  FORMAAAL = formaal_15,
  PRISENHED = "2020-priser, kædede værdier",
  `SÆSON`   = "Sæsonkorrigeret",
  Tid       = "*"    # alle kvartaler
)

# 5) Hent data via API
forbrug_raw <- dst_get_data(
  table = "NKHC021",
  query = forbrug_filters,
  lang  = "da",
  meta  = forbrug_meta
)


# 5) Lav ÅR og læg kvartalerne sammen til årligt forbrug
df_long <- forbrug_raw %>%
  mutate(
    År = as.integer(substr(TID, 1, 4))
  ) %>%
  group_by(År, Gruppe = FORMAAAL) %>%
  summarise(
    Forbrug = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# Fjern koderne (CPD, CPM osv.)
df_long$Gruppe <- gsub("^[A-Z]{3} ", "", df_long$Gruppe)

# -------------------------------
# SPØRGSMÅL 1: mest forbrug i 2024
# -------------------------------
størst2024 <- df_long %>%
  filter(År == 2024) %>%
  arrange(desc(Forbrug)) %>%
  slice(1)

størst2024

# -------------------------------
# SPØRGSMÅL 2: største stigning 2014–2024
# -------------------------------
årstart <- 2020
årslut  <- 2024

ændring_pct <- df_long %>%
  filter(År %in% c(årstart, årslut)) %>%
  pivot_wider(names_from = År, values_from = Forbrug) %>%
  mutate(
    forskel     = .data[[as.character(årslut)]] - .data[[as.character(årstart)]],
    forskel_pct = (forskel / .data[[as.character(årstart)]]) * 100
  ) %>%
  arrange(desc(forskel))

ændring_pct

ggplot(ændring_pct, aes(x = reorder(Gruppe, forskel),
                        y = forskel,
                        fill = forskel)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient2(low = "firebrick", mid = "grey80", high = "steelblue") +
  scale_y_continuous(breaks = c(-10000, -5000, 0, 5000, 10000, 15000, 20000)) +
  labs(
    title = "Store forskelle i forbrugsudviklingen fra 2020 til 2024 - danskerne prioriterer oplevelser frem for varer",
    subtitle = "Efter coronakrisen vender danskerne tilbage til oplevelser - mens detailforbruget halter efter",
    caption = "Kilde: Danmarks Statistik",
    x = "Forbrugsgruppe",
    y = "Ændring i mio. kr.",
    fill = "Ændring i mio kr."
  ) +
  theme_minimal(base_size = 13)



