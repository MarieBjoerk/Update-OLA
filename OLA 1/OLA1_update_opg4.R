library(dkstat)
library(dplyr)

###Opg 4.1

#Henter Meta Data med henblik på filtrering.
booze_meta <- dst_meta(table = "FU02", lang = "da")

#Definerer filtre for relevante alkoholgrupper
booze_meta_filters <- list(
  KONSUMGRP = c("02.1.1.1 Spiritus og likør",
                "02.1.1.2 Alkoholiske læskedrikke",
                "02.1.2.1 Vin af druer",
                "02.1.2.2 Vin af andre frugter",
                "02.1.2.3 Hedvin",
                "02.1.2.4 Vinbaserede drikkevarer og alkoholfri vin",
                "02.1.3.1 Pilsnerøl, guldøl",
                "02.1.3.2 Andre alkoholholdige øl",
                "02.1.3.3 Øl med lavt alkoholindhold og alkoholfri øl",
                "02.1.3.4 Øl-baserede drikkevarer"),
  PRISENHED = "Faste priser",
  Tid = "*"
)

#Henter data fra Danmarks Statistik
boozedata <- dst_get_data(table = "FU02", query = booze_meta_filters, lang = "da")

library(ggplot2)
library(lubridate)

#Filtrerer datasættet til perioden 2000–2022
boozedf_2000_2022 <- boozedata %>%
  filter(TID >= "2000-01-01", TID <= "2022-01-01")

#Renser kategorinavne - vifjerner kode-tal
boozedf_2000_2022 <- boozedf_2000_2022 %>% 
  mutate(
    KONSUMGRP = gsub("^[0-9[:space:].]+", "", as.character(KONSUMGRP))
  )

#Tilføjer kolonne med årstal
boozedf_2000_2022 <- boozedf_2000_2022 %>% 
  mutate(aar = year(as.Date(TID)))

# 4. Normaliser forbrug pr. gruppe til 0–100 %
boozedf_2000_2022 <- boozedf_2000_2022 %>% 
  group_by(KONSUMGRP) %>%
  mutate(
    value_pct = (value - min(value)) / (max(value) - min(value)) * 100
  ) %>%
  ungroup()


#Plotter udviklingen for ALLE drikkevarer
install.packages("ggh4x")
library(ggh4x)

ggplot(boozedf_2000_2022,
       aes(x = aar, y = value_pct)) +
  geom_line(linewidth = 0.9) +
  facet_wrap2(~ KONSUMGRP, axes = "all") +   # ← viser akser i ALLE paneler
  scale_x_continuous(breaks = seq(2000, 2022, 5)) +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
  labs(
    title = "Alkoholfri forbrug stiger markant, mens traditionelle alkoholtyper generelt falder",
    caption = "Kilde: Danmarks Statistik",
    x = "År",
    y = "Forbrug"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#Opgave 4.2 - korrelqation
library(tidyr)

#Opret hovedgrupper (Vin, Øl, Spiritus, Alkoholfri)
boozedf_2000_2022 <- boozedf_2000_2022 %>% 
  mutate(
    HOVED = case_when(
      
      # Vin
      KONSUMGRP %in% c("Vin af druer",
                       "Vin af andre frugter",
                       "Hedvin") ~ "Vin",
      
      # Øl 
      KONSUMGRP %in% c("Pilsnerøl, guldøl",
                       "Andre alkoholholdige øl",
                       "Øl-baserede drikkevarer") ~ "Øl",
      
      # Spiritus
      KONSUMGRP == "Spiritus og likør" ~ "Spiritus",
      
      # Alkoholfri
      KONSUMGRP %in% c("Øl med lavt alkoholindhold og alkoholfri øl",
                       "Vinbaserede drikkevarer og alkoholfri vin") ~ "Alkoholfri",
      
      TRUE ~ NA
    )
  )

#Fjerner rækker der ikke passer i en hovedgruppe
boozedf_clean <- boozedf_2000_2022 %>% 
  filter(!is.na(HOVED))


#Summerer forbrug pr. år og hovedgruppe
hoveddata <- boozedf_clean %>%
  group_by(aar, HOVED) %>%
  summarise(total = sum(value), .groups = "drop")

#Laver datasæt i bredt format 
hoved_wide <- hoveddata %>%
  pivot_wider(names_from = HOVED, values_from = total)

#Beregner korrelationsmatrix
kor_mat <- cor(hoved_wide[, -1], use = "pairwise.complete.obs")
kor_mat

# Beregner korrelationsmatrix
kor_df <- as.data.frame(as.table(kor_mat))
colnames(kor_df) <- c("Gruppe1", "Gruppe2", "Kor")

#fjerner rækker med NA
kor_df <- kor_df[!is.na(kor_df$Gruppe1) & !is.na(kor_df$Gruppe2), ]

#Plotter heatmap af korrelationer
ggplot(kor_df, aes(x = Gruppe1, y = Gruppe2, fill = Kor)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(Kor, 2)), size = 5) +
  scale_fill_gradient2(
    low = "#4575b4",
    mid = "white",
    high = "#d73027",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Kor."
  ) +
  labs(
    title = "Klar negativ sammenhæng mellem alkoholfri drikke og ølforbrug",
    caption = "Kilde: Danmarks Statistik",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 18)
  )










