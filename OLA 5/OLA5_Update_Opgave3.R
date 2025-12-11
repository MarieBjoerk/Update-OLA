############################################## opgave 3.1
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---------------------------------------------------------
# 1. Læs data
# ---------------------------------------------------------
virk <- read_excel("regnskaber_industri_transport_byg_5_25000_ansatte_anonym(in).xlsx")

#hvis man henter den ned manuelt, bruger man denne:
virk <- regnskaber_industri_transport_byg_5_25000_ansatte_anonym_in_
# ---------------------------------------------------------
# 2. Opret lånekategorier
# ---------------------------------------------------------

laan_svar <- virk[[1]]

virk$laane_kategori <- case_when(
  laan_svar %in% c("Gode", "Meget gode") ~ "Positiv",
  laan_svar == "Neutrale" ~ "Neutral",
  laan_svar %in% c("Dårlige", "Dårlig", "Meget dårlige") ~ "Negativ",
  TRUE ~ NA_character_
)
#plot af svar procentene
plot_data <- virk %>% 
  filter(laane_kategori %in% c("Negativ", "Neutral", "Positiv"))
ggplot(plot_data, aes(x = laane_kategori)) +
  geom_bar(aes(y = after_stat(count / sum(count))),
           fill = "blue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "De fleste virksomheder vurderer deres lånemuligheder som gode",
    x = "Lånekategori",
    y = "Procent"
  ) +
  theme_minimal()
# ---------------------------------------------------------
# 3. Find kolonner automatisk
# ---------------------------------------------------------

solid_cols   <- grep("Soliditetsgrad", colnames(virk), value = TRUE)
afkast_cols  <- grep("Afkast", colnames(virk), value = TRUE)
balance_cols <- grep("Balance", colnames(virk), value = TRUE)

# FIX: vælg kun den tal-baserede kolonne (ikke "kilde")
ansatte_cols <- grep("Antal ansatte Cvr-nr\\.(?! kilde)", colnames(virk), value = TRUE, perl = TRUE)

branche_cols <- grep("Branchebetegnelse primær", colnames(virk), value = TRUE)


# ---------------------------------------------------------
# 4. Long-format (soliditet, afkast, balance)
# ---------------------------------------------------------

# Soliditet
soliditet_long <- virk %>%
  select(laane_kategori, all_of(solid_cols)) %>%
  pivot_longer(
    cols = all_of(solid_cols),
    names_to = "year",
    values_to = "soliditet"
  ) %>%
  mutate(
    year = substr(gsub("[^0-9]", "", year), 1, 4),
    soliditet = as.numeric(gsub(",", ".", soliditet))
  )

# Afkast
afkast_long <- virk %>%
  select(laane_kategori, all_of(afkast_cols)) %>%
  pivot_longer(
    cols = all_of(afkast_cols),
    names_to = "year",
    values_to = "afkast"
  ) %>%
  mutate(
    year = substr(gsub("[^0-9]", "", year), 1, 4),
    afkast = as.numeric(gsub(",", ".", afkast))
  )

# Balance
balance_long <- virk %>%
  select(laane_kategori, all_of(balance_cols)) %>%
  pivot_longer(
    cols = all_of(balance_cols),
    names_to = "year",
    values_to = "balance"
  ) %>%
  mutate(
    year = substr(gsub("[^0-9]", "", year), 1, 4),
    balance = as.numeric(gsub(",", ".", balance))
  )


# ---------------------------------------------------------
# 5. Antal ansatte (uden år)
# ---------------------------------------------------------

antal_ansatte_long <- virk %>%
  select(laane_kategori, all_of(ansatte_cols)) %>%
  rename(antal_ansatte = all_of(ansatte_cols)) %>%
  mutate(antal_ansatte = as.numeric(antal_ansatte))


# ---------------------------------------------------------
# 6. Branchebetegnelse primær
# ---------------------------------------------------------

branche_long <- virk %>%
  select(laane_kategori, all_of(branche_cols)) %>%
  pivot_longer(
    cols = all_of(branche_cols),
    names_to = "variable",
    values_to = "branche"
  ) %>%
  mutate(branche = as.character(branche))


# ---------------------------------------------------------
# 7. SOLIDITETSGRAD (KUN 2020–2016)
# ---------------------------------------------------------

plot_soliditet <- soliditet_long %>%
  filter(
    year %in% c("2020", "2019", "2018", "2017", "2016"),
    !is.na(laane_kategori)
  ) %>%
  group_by(year, laane_kategori) %>%
  summarise(mean_soliditet = mean(soliditet, na.rm = TRUE))

ggplot(plot_soliditet, aes(x = year, y = mean_soliditet, fill = laane_kategori)) +
  geom_col(position = "dodge", width = 0.7) +
  labs(
    title = "Soliditetsgrad fordelt på lånekategorie",
    x = "",
    y = "Gennemsnitlig soliditetsgrad (%)"
  ) +
  scale_fill_manual(
    values = c("Positiv" = "#009DE0", "Neutral" = "#666666", "Negativ" = "#BFBFBF"),
    labels = c(
      "Positiv" = "Positiv = Gode / Meget gode",
      "Neutral" = "Neutral",
      "Negativ" = "Negativ = Dårlige / Meget dårlige"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))


# ---------------------------------------------------------
# 8. AFKAST (KUN 2020–2016)
# ---------------------------------------------------------

plot_afkast <- afkast_long %>%
  filter(
    year %in% c("2020", "2019", "2018", "2017", "2016"),
    !is.na(laane_kategori)
  ) %>%
  group_by(year, laane_kategori) %>%
  summarise(mean_afkast = mean(afkast, na.rm = TRUE))

ggplot(plot_afkast, aes(x = year, y = mean_afkast, fill = laane_kategori)) +
  geom_col(position = "dodge", width = 0.7) +
  labs(
    title = "Afkastningsgrad fordelt på lånekategorier",
    x = "",
    y = "Gennemsnitligt afkast (%)"
  ) +
  scale_fill_manual(
    values = c("Positiv" = "#009DE0", "Neutral" = "#666666", "Negativ" = "#BFBFBF")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))


# ---------------------------------------------------------
# 9. BALANCE (KUN 2020–2016) – log af gennemsnitlig balance
# ---------------------------------------------------------

plot_balance <- balance_long %>%
  filter(
    year %in% c("2020", "2019", "2018", "2017", "2016"),
    !is.na(laane_kategori)
  ) %>%
  group_by(year, laane_kategori) %>%
  summarise(
    mean_balance = mean(balance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    log_mean_balance = log(mean_balance + 1)   # her tager vi log EFTER gennemsnit
  )

ggplot(plot_balance,
       aes(x = year, y = log_mean_balance, fill = laane_kategori)) +
  geom_col(position = "dodge", width = 0.7) +
  labs(
    title = "Virksomheder med større balance har mere positive lånevurderinger",
    subtitle = "Virksomheder, der vurderer deres lånemuligheder positivt, har generelt større balance end andre",
    x = "",
    y = "log(balance)"
  ) +
  scale_fill_manual(
    values = c("Positiv" = "#009DE0", "Neutral" = "#666666", "Negativ" = "#BFBFBF")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))

#opgave 3.2
library(dplyr)
library(ordinal)  

# ---------------------------------------------------------
# 3.2.1 Vælg regnskabsår (2016–2020)
# ---------------------------------------------------------
regn_aar <- "2020"

# Find kolonner til regnskabsåret 2020
balance_col <- grep(paste0("Balance.*", regn_aar), names(virk), value = TRUE)
afkast_col  <- grep(paste0("Afkastningsgrad.*", regn_aar), names(virk), value = TRUE)
solidit_col <- grep(paste0("Soliditetsgrad.*", regn_aar), names(virk), value = TRUE)

# Antal ansatte (typisk uden årstal)
ansatte_col <- grep("^Antal ansatte", names(virk), value = TRUE)[1]

# ---------------------------------------------------------
# 3.2.2 Modeldataset for 2020
# ---------------------------------------------------------
modeldata <- virk %>%
  transmute(
    laane_kategori,
    balance_2020 = as.numeric(gsub(",", ".", .data[[balance_col]])),
    afkast       = as.numeric(gsub(",", ".", .data[[afkast_col]])),
    soliditet    = as.numeric(gsub(",", ".", .data[[solidit_col]])),
    ansatte      = if (!is.na(ansatte_col)) {
      as.numeric(gsub(",", ".", .data[[ansatte_col]]))
    } else NA_real_
  ) %>%
  filter(
    !is.na(laane_kategori),
    !is.na(balance_2020),
    !is.na(afkast),
    !is.na(soliditet)
  )

# Sæt korrekt orden på kategorien
modeldata$laane_kategori <- factor(
  modeldata$laane_kategori,
  levels  = c("Negativ", "Neutral", "Positiv"),
  ordered = TRUE
)

# ---------------------------------------------------------
# 3.2.3 Log-transformering (ingen gennemsnit, ingen skalering)
# ---------------------------------------------------------
modeldata <- modeldata %>%
  mutate(
    log_balance = log(balance_2020),# log af hver virksomheds balance
    log_ansatte = log(ansatte)
    
  )

# ---------------------------------------------------------
# 3.2.4 CLM-model (ordered logit)
# ---------------------------------------------------------
#Sammen
mod_clm <- clm(
  laane_kategori ~ log_balance + afkast + soliditet + log_ansatte,
  data = modeldata,
  link = "logit"
)

summary(mod_clm)
#Hver for sig
#log balance
mod_clm1 <- clm(
  laane_kategori ~ log_balance,
  data = modeldata,
  link = "logit"
)

summary(mod_clm1)
#afkast
mod_clm2 <- clm(
  laane_kategori ~ afkast,
  data = modeldata,
  link = "logit"
)

summary(mod_clm2)
#soliditets
mod_clm3 <- clm(
  laane_kategori ~ soliditet,
  data = modeldata,
  link = "logit"
)

summary(mod_clm3)
#antal ansatte
mod_clm4 <- clm(
  laane_kategori ~ log_ansatte,
  data = modeldata,
  link = "logit"
)

summary(mod_clm4)
##################her laver vi et plot for antal ansatte
ggplot(modeldata, aes(x = log_ansatte)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    bins = 30,
    fill = "grey70",
    color = "black",
    linewidth = 0.3
  ) +
  facet_wrap(~ laane_kategori) +
  scale_y_continuous(labels = scales::percent_format()) +
  
  # ---- X-aksen: vis rigtige antal ansatte ----
scale_x_continuous(
  breaks = c(1, 2, 3, 4, 5, 6),
  labels = round(exp(c(1, 2, 3, 4, 5, 6)))
) +
  # -------------------------------------------

labs(
  title = "Fordeling af antal ansatte (model baseret på log-transformering)",
  x = "Antal ansatte (x-akse viser faktisk antal, men log er anvendt i analysen)",
  y = "Procent"
) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )
###########################opgave 3.3

plot_data <- bind_rows(
  afkast_long %>%
    filter(!is.na(laane_kategori)) %>%
    group_by(laane_kategori) %>%
    summarise(vaerdi = mean(afkast, na.rm = TRUE), .groups = "drop") %>%
    mutate(variable = "Afkastningsgrad"),
  soliditet_long %>%
    filter(!is.na(laane_kategori)) %>%
    group_by(laane_kategori) %>%
    summarise(vaerdi = mean(soliditet, na.rm = TRUE), .groups = "drop") %>%
    mutate(variable = "Soliditetsgrad")
)

plot_data$laane_kategori <- factor(plot_data$laane_kategori,
                                   levels = c("Negativ", "Neutral", "Positiv"))
ggplot(plot_data,
       aes(x = variable, y = vaerdi, fill = laane_kategori)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.55) +
  scale_y_continuous(
    limits = c(0, 45),
    breaks = seq(0, 45, 5),
    expand = c(0, 0)
  ) +
  labs(
    title = "Virksomheder med et højere soliditetsgrad synes bedre om finansieringsklimaet",
    subtitle = "Afkastningsgraden påvirker ikke lånemulighederne",
    x = "",
    y = "Pct."
  ) +
  scale_fill_manual(
    values = c("Negativ" = "#BFBFBF",
               "Neutral" = "#666666",
               "Positiv" = "#009DE0"),
    labels = c(
      "Negativ" = "Meget dårlige/Dårlige",
      "Neutral" = "Neutrale",
      "Positiv" = "Gode/Meget gode"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.margin = margin(t = -5),
    legend.spacing.y = unit(0, "pt"),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
  ) +
  guides(fill = guide_legend(nrow = 1))


