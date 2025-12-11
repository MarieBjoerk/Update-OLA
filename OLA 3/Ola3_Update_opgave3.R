###### Data Retrieval ###########
# Indlæs pakker
############################################
library(dkstat)
library(ggplot2)
library(tidyverse)

############################################
# 1) Hent FORV1 
############################################

forv1_filters <- list(
  INDIKATOR = "*",
  TID       = "*"   
)

forv1_long <- dst_get_data(
  table = "FORV1",
  query = forv1_filters,
  lang  = "da"
)

# forv1_long indeholder kolonnerne: INDIKATOR, TID og value

############################################
# 2) Omstrukturér data: tid lodret og indikatorer vandret
############################################

forv1_wide <- reshape(
  forv1_long,
  idvar     = "TID",        
  timevar   = "INDIKATOR",  
  direction = "wide"
)

# Fjern præfikset "value." i kolonnenavnene
names(forv1_wide) <- sub("^value\\.", "", names(forv1_wide))

# Udsnit med rækker fra linje 304 og nedefter
dataK4 <- forv1_wide[304:nrow(forv1_wide), ]

############################################
# 4) NKN1 – P.31 Privatforbrug,
#    2020-priser, kædede værdier, sæsonkorrigeret
############################################

nkn1_meta <- dst_meta(table = "NKN1", lang = "da")
as.data.frame(nkn1_meta$variables)

nkn1_filters <- list(
  TRANSAKT  = "P.31 Privatforbrug",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  "SÆSON"   = "Sæsonkorrigeret",
  Tid       = "*"
)

nkn1_p31 <- dst_get_data(
  table = "NKN1",
  query = nkn1_filters,
  lang  = "da"
)

# Opret datasæt med kun tidsvariabel og værdier
PF2025 <- nkn1_p31[, c("TID", "value")]

# Behold rækker fra linje 37 og nedefter
PF2025 <- PF2025[37:nrow(PF2025), ]


##################### Før opgave start ###############################

# Udsnit med rækker fra linje 304 og fjerner de sidste 2 rækker
dataK3 <- forv1_wide[304:612, ]
DI <- dataK3

# Vektor 1: realvækst, for privatforbrug 
pfveakst <- c(diff(log(PF2025$`value`), lag = 4) * 100)

# Data.frame af realvækst
pfdf <- as.data.frame(pfveakst)

# Nu skal vi have inddelt vores datasæt fra måneder til kvartaler – vi bruger OLA1, opg 5.2:

# Gør alle datakolonner numeriske
DI1 <- DI[, -1, drop = FALSE]
DI1[] <- lapply(DI1, function(x) as.numeric(gsub(",", ".", x)))

# Lav dataen kvartalsvis (gennemsnit af hver 3 måneder)
fend <- data.frame(matrix(nrow = 0, ncol = ncol(DI1)))
names(fend) <- names(DI1)

for (i in seq(3, nrow(DI1), by = 3)) {
  rows  <- (i - 2):i
  means <- colMeans(DI1[rows, , drop = FALSE], na.rm = TRUE)
  fend  <- rbind(fend, as.data.frame.list(means))
}

# Isoler de forskellige spørgsmål (spg1–spg12)
spg1  <- as.numeric(fend[, 2])
spg2  <- as.numeric(fend[, 3])
spg3  <- as.numeric(fend[, 4])
spg4  <- as.numeric(fend[, 5])
spg5  <- as.numeric(fend[, 6])
spg6  <- -as.numeric(fend[, 7])
spg7  <- as.numeric(fend[, 8])
spg8  <- -as.numeric(fend[, 9])
spg9  <- as.numeric(fend[, 10])
spg10 <- as.numeric(fend[, 11])
spg11 <- as.numeric(fend[, 12])
spg12 <- as.numeric(fend[, 13])

# Korrelationer
cor(pfveakst, spg1)
cor(pfveakst, spg2)
cor(pfveakst, spg3)
cor(pfveakst, spg4)
cor(pfveakst, spg5)
cor(pfveakst, spg6)
cor(pfveakst, spg7)
cor(pfveakst, spg8)
cor(pfveakst, spg9)
cor(pfveakst, spg10, use = "complete.obs")
cor(pfveakst, spg11)
cor(pfveakst, spg12)

####################################################
# 4. kvartal i fend (K4)
kvartal_labels <- rep(c("K1","K2","K3","K4"), times = nrow(fend) / 4)

n <- nrow(fend)
kvartal_mdr <- rep(c("K1","K2","K3","K4"), length.out = n)

# Tving de sidste to måneder til K1 og K2
if (n >= 2) kvartal_mdr[(n - 1):n] <- c("K1","K2")

fend$kvartal <- kvartal_mdr
fend_Q4 <- fend[fend$kvartal == "K4", ]

############################################
# Lav nkn1 (altså pfdf) til den nye fend
kvartal_labels1 <- rep(c("K1","K2","K3","K4"), times = nrow(pfdf) / 4)

n <- nrow(pfdf)
kvartal_mdr1 <- rep(c("K1","K2","K3","K4"), length.out = n)

if (n >= 2) kvartal_mdr1[(n - 1):n] <- c("K1","K2")

pfdf$kvartal <- kvartal_mdr1
pfdf_Q4 <- pfdf[pfdf$kvartal == "K4", ]

# Dummy-variabel for vækst
pfdf_Q4$veakst <- as.integer(pfdf_Q4$pfveakst > 0)
pfdf_Q4$veakst <- factor(pfdf_Q4$veakst, levels = c(0,1), labels = c("ned","op"))
summary(pfdf_Q4$veakst)

##################################################### 
# Nu er det tid til at kigge på måneder 10, 11, 12
# (dette svarer til 4. kvartal i forbrugertillid)
#####################################################

# Udsnit med rækker fra linje 304 og nedefter
FORV1_ <- forv1_wide[304:nrow(forv1_wide), ]

# Træk måned ud som tekst "MM" fra TID (format: "YYYY-MM-DD")
maaned_str <- substr(FORV1_$TID, 6, 7)

# Sidste 3 måneder i hvert år: oktober (10), november (11), december (12)
df_vinter <- FORV1_[maaned_str %in% c("10", "11", "12"), ]

# Tilføj år og måned (som faktor med labels)
df_vinter$aar  <- substr(df_vinter$TID, 1, 4)
df_vinter$maan <- substr(df_vinter$TID, 6, 7)
df_vinter$maan_f <- factor(
  df_vinter$maan,
  levels = c("10","11","12"),
  labels = c("Oktober","November","December")
)

############################################################
## Kolonnediagram for F2 – Familiens økonomiske situation
############################################################

df_vinter$F2_num <- as.numeric(gsub(
  ",", ".",
  df_vinter$`F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden`
))

p_F2 <- ggplot(df_vinter, aes(x = aar, y = F2_num, fill = maan_f)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    x = "År",
    y = "Nettotal - Familiens økonomi i dag vs. for et år siden (F2)",
    fill = "Måned",
    title = "Families økonomiske situation er dårligere i sidste kvartal end i 2024",
    subtitle = "Kun oktober, november og december",
    caption = "Kilde = Danmarks Statistik og egne beregning"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

print(p_F2)

####################
### DST's indikator – KUN F1 Forbrugertillidsindikatoren
####################

df_vinter$F1_num <- as.numeric(gsub(
  ",", ".",
  df_vinter$`F1 Forbrugertillidsindikatoren`
))

df_vinter$DST_snit <- df_vinter$F1_num  # enkel indikator, ikke gennemsnit

df_DST <- data.frame(
  aar    = factor(df_vinter$aar,  levels = sort(unique(df_vinter$aar))),
  maan_f = df_vinter$maan_f,
  vaerdi = df_vinter$DST_snit
)

p_DST <- ggplot(df_DST, aes(x = aar, y = vaerdi, fill = maan_f)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    x = "År",
    y = "Nettotal (F1 Forbrugertillidsindikatoren)",
    fill = "Måned",
    title = "DST's forbrugertillidsindikator er mere negativ omkring julehandlen end sidste år",
    subtitle = " oktober, november og december", 
    caption = "Kilde = Danmarks Statistik og egne beregning"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

print(p_DST)

###############################
### Vores Kombi5 – spg 3, 5, 7, 9, 11
### = F4, F6, F8, F10, F12
###############################
# Træk kun de relevante kolonner ud via deres INDEX
###############################
### Vores Kombi5 – (F4, F6, F8, F10, F12)
###############################
kombi5_mat <- df_vinter[, c(5, 7, 9, 11, 13)]

# Gør dem numeriske
kombi5_mat[] <- lapply(
  kombi5_mat,
  function(x) as.numeric(gsub(",", ".", x))
)

# Gennemsnit for Kombi5
df_vinter$Kombi5_snit <- rowMeans(kombi5_mat, na.rm = TRUE)

df_Kombi5 <- data.frame(
  aar    = factor(df_vinter$aar,  levels = sort(unique(df_vinter$aar))),
  maan_f = df_vinter$maan_f,
  vaerdi = df_vinter$Kombi5_snit
)

# Plot: Kombi5 – okt/nov/dec
p_Kombi5 <- ggplot(df_Kombi5, aes(x = aar, y = vaerdi, fill = maan_f)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    x = "År",
    y = "Nettotal (gennemsnit af spg 3, 5, 7, 9, 11)",
    fill = "Måned",
    title = "Kombi5 er ikke ligeså negativ som DST's forbrugertillidsindikator",
    subtitle = "Gennemsnit af spørgsmål 3, 5, 7, 9 og 11 (F4, F6, F8, F10, F12) i oktober, november, december",
    caption = "Kilde = Danmarks Statistik og egne beregning"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

print(p_Kombi5)
########### OPGAVE 3.1 – Forudsig julehandel (K4) med M10 fra uge43 ###########

########### OPGAVE 3.1 – Logitmodeller og julehandel (K4) ###########
########### OPGAVE 3.1 – Forudsig julehandel (K4) ###########

# 1) Dummy for årlig vækst (2000K1–2025K2)
y <- as.integer(pfveakst > 0)
n <- length(y)

# 2) Lav DI-indikatorer på kvartalsdata (uge41)
sam_DST_spg  <- (spg1[1:n] + spg2[1:n] + spg3[1:n] + spg4[1:n] + spg5[1:n]) / 5   # F1 (DST)
sam_TOP5_spg <- (spg3[1:n] + spg5[1:n] + spg7[1:n] + spg9[1:n] + spg11[1:n]) / 5 # F4,F6,F8,F10,F12

# 3) Samlet datasæt til modellerne
df_mod <- data.frame(
  y           = y,
  sam_DST_spg = sam_DST_spg,
  sam_TOP5_spg = sam_TOP5_spg
)

# 4) Logistiske modeller (trænes på 2000K1–2025K2)
mod_DST  <- glm(y ~ sam_DST_spg,  data = df_mod, family = binomial())
mod_TOP5 <- glm(y ~ sam_TOP5_spg, data = df_mod, family = binomial())

###########################################################
## M10-input: brug de SIDSTE 2 måneder (M11 og M12)
###########################################################

## ---- DST (F1, Uge44) ----
DI_dst <- dataK3
DI1_dst <- DI_dst[, -1, drop = FALSE]
DI1_dst[] <- lapply(DI1_dst, function(x) as.numeric(gsub(",", ".", x)))

# Sidste 2 rækker (= måned 11 og 12)
m10_rows_dst <- tail(DI1_dst, 2)

# Gennemsnit af F1 over de sidste 2 måneder
m10_DST_val <- mean(as.numeric(m10_rows_dst[, 1]), na.rm = TRUE)

## ---- Kombi5 (F4,F6,F8,F10,F12, Uge43) ----
DI_top5 <- dataK3
DI1_top5 <- DI_top5[, -1, drop = FALSE]
DI1_top5[] <- lapply(DI1_top5, function(x) as.numeric(gsub(",", ".", x)))

m10_rows_top5 <- tail(DI1_top5, 2)

# Gennemsnit af F4,F6,F8,F10,F12 over de sidste 2 måneder
m10_TOP5_val <- mean(
  as.numeric(as.matrix(m10_rows_top5[, c(3, 5, 7, 9, 11)])),
  na.rm = TRUE
)

###########################################################
## Forudsig sandsynlighed for "op" i julehandlen 2025 (K4)
###########################################################

# DST-model
p_DST_K4_2025 <- predict(
  mod_DST,
  newdata = data.frame(sam_DST_spg = m10_DST_val),
  type    = "response"
)
retning_DST_K4_2025 <- ifelse(p_DST_K4_2025 >= 0.5, "op", "ned")

# Kombi5-model
p_TOP5_K4_2025 <- predict(
  mod_TOP5,
  newdata = data.frame(sam_TOP5_spg = m10_TOP5_val),
  type    = "response"
)
retning_TOP5_K4_2025 <- ifelse(p_TOP5_K4_2025 >= 0.5, "op", "ned")

###########################################################
## Saml og vis resultat
###########################################################

resultat_Julehandel <- data.frame(
  Model              = c("DST (afgørende)", "Kombi5 (reference)"),
  Input_M10_M11      = c(round(m10_DST_val,  2), round(m10_TOP5_val, 2)),
  Sandsynlighed_for_op = round(c(p_DST_K4_2025,     p_TOP5_K4_2025), 3),
  Retning            = c(retning_DST_K4_2025,       retning_TOP5_K4_2025),
  row.names          = NULL
)

print(resultat_Julehandel)

#####Grafisk###########
###### Kolonnediagram: antal "op" vs "ned" i historikken ######

# Saml historiske klasser (0/1 -> "ned"/"op")
df_hist <- data.frame(
  veakst = factor(ifelse(y == 1, "op", "ned"), levels = c("ned","op"))
)

# Lav en tælling pr. klasse til visning på søjlerne
kvartal_t <- ggplot(df_hist, aes(x = veakst, fill = veakst)) +
  geom_bar(aes(y = (..count..) / sum(..count..)), width = 0.7) +
  geom_text(
    stat = "count",
    aes(
      y = (..count..) / sum(..count..),
      label = scales::percent(..count.. / sum(..count..), accuracy = 1)
    ),
    vjust = -0.2,
    size = 4
  ) +
  scale_fill_manual(values = c("ned" = "Blue", "op" = "Red")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = NULL,
    y = "Andel af kvartaler",
    title = "Flere kvartaler går op end ned ",
    subtitle = "Klasseopdeling af kvartaler i op og ned procentvis, fra 2000K1 til 2025K3",
    caption = "Kilde: Danmarks Statistik og egne beregninger"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(kvartal_t)
#################################################
###############################################################
### Sammenlign: hvor mange "op" og "ned" for DST og vores indikator ###
###############################################################

# Dummy for årlig vækst (1 = op, 0 = ned)
y <- as.integer(pfveakst > 0)
n <- length(y)

### DST’s indikator ###
sam_DST_spg <- (spg1[1:n] + spg2[1:n] + spg3[1:n] + spg4[1:n] + spg5[1:n]) / 5
mod_DST <- glm(y ~ sam_DST_spg, family = binomial())
pred_DST <- ifelse(predict(mod_DST, type = "response") >= 0.5, 1, 0)

### Vores indikator (Top-5) ###
sam_TOP5_spg <- (spg3[1:n] + spg5[1:n] + spg7[1:n] + spg9[1:n] + spg11[1:n]) / 5
mod_TOP5 <- glm(y ~ sam_TOP5_spg, family = binomial())
pred_TOP5 <- ifelse(predict(mod_TOP5, type = "response") >= 0.5, 1, 0)

### Lav samlet dataframe ###
df_plot <- data.frame(
  Model = rep(c("DST's indikator", "Vores indikator"), each = n),
  Forudsigelse = factor(c(pred_DST, pred_TOP5), levels = c(0,1), labels = c("ned","op"))
)

### Tæl hvor mange "op"/"ned" for hver model ###
df_tal <- df_plot %>%
  group_by(Model, Forudsigelse) %>%
  summarise(Antal = n(), .groups = "drop")

# Beregn procent pr. Model
df_tal_pct <- df_tal %>%
  group_by(Model) %>%
  mutate(Procent = Antal / sum(Antal))

dst_vs_kombi5 <- ggplot(df_tal_pct, aes(x = Model, y = Procent, fill = Forudsigelse)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(
    aes(label = scales::percent(Procent, accuracy = 1)),
    position = position_dodge(width = 0.7),
    vjust = -0.3,
    size = 4
  ) +
  scale_fill_manual(values = c("ned" = "Blue", "op" = "Red")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "DST har ét kvartal mere end vores indikator, der går op",
    subtitle = "DST og vores indikator, omregnet til procent  fra 2000K1 til 2025K3",
    x = NULL, y = "Andel af kvartaler",
    fill = "Retning",
    caption = "Kilde: Danmarks Statistik og egne beregninger"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(dst_vs_kombi5)


#### Validering 3.2 ########################################
# Tjek hvor godt DST og Kombi5 rammer "op"/"ned" historisk
###########################################################

# 0) Data til modellerne (y, DST-indikator, Kombi5-indikator)
#    y, sam_DST_spg og sam_TOP5_spg er allerede defineret i 3.1
dat_val <- data.frame(
  y            = y,
  sam_DST_spg  = sam_DST_spg,
  sam_TOP5_spg = sam_TOP5_spg
)

## 1) Logistiske modeller (som i 3.1, men nu til validering)
sam_log_DST   <- glm(y ~ sam_DST_spg,  data = dat_val, family = binomial())
sam_log_komb5 <- glm(y ~ sam_TOP5_spg, data = dat_val, family = binomial())

## 2) Forudsigelser og træfsikkerhed for DST ################

dat_val$pred_DST_prob <- predict(sam_log_DST, type = "response")
dat_val$pred_DST      <- ifelse(dat_val$pred_DST_prob >= 0.5, 1, 0)

table_DST <- table(
  faktisk      = dat_val$y,
  forudsigelse = dat_val$pred_DST
)
acc_DST <- mean(dat_val$pred_DST == dat_val$y)

table_DST
acc_DST

## 3) Forudsigelser og træfsikkerhed for Kombi5 #############

dat_val$pred_komb5_prob <- predict(sam_log_komb5, type = "response")
dat_val$pred_komb5      <- ifelse(dat_val$pred_komb5_prob >= 0.5, 1, 0)

table_komb5 <- table(
  faktisk      = dat_val$y,
  forudsigelse = dat_val$pred_komb5
)
acc_komb5 <- mean(dat_val$pred_komb5 == dat_val$y)

table_komb5
acc_komb5

############################################################
# 4) Figur: PF (pct.) vs. DST og Kombi5 på nettotal-skalaen
############################################################

# Skaler PF (pct.) op på samme skala som nettotal
a <- sd(c(sam_DST_spg, sam_TOP5_spg), na.rm = TRUE) / sd(pfveakst, na.rm = TRUE)
PF_on_NT <- a * pfveakst

# Samlet dataframe til graf
df_plot <- data.frame(
  kvartal  = 1:length(pfveakst),
  PF_on_NT = PF_on_NT,
  DST      = sam_DST_spg,
  Kombi5   = sam_TOP5_spg
)

# Plot: søjler = PF (på nettotal-skala), linjer = indikatorer
dst_kombi5_final <- ggplot(df_plot, aes(x = kvartal)) +
  geom_col(aes(y = PF_on_NT), fill = "blue", width = 0.8) +  # PF som søjler
  geom_line(aes(y = DST,    colour = "DST's indikator"),      size = 1.1) +
  geom_line(aes(y = Kombi5, colour = "Kombi5 (vores) indikator"), size = 1.1) +
  scale_colour_manual(values = c("DST's indikator" = "darkgray",
                                 "Kombi5 (vores) indikator" = "black")) +
  scale_y_continuous(
    name    = "Nettotal (indikatorer)",
    sec.axis = sec_axis(~ . / a, name = "Pct. (realvækst)")
  ) +
  labs(
    x        = NULL,
    title    = "Kombi5-indikator er mere stabil i både opsving og nedture end DST",
    subtitle = "Privat forbrug som søjler (pct., skaleret), indikatorer som linjer (nettotal)",
    caption  = "Kilde: Danmarks Statistik og egne beregninger"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position     = "bottom",
    panel.grid.minor    = element_blank(),
    panel.grid.major.x  = element_blank(),
    axis.title.y.left   = element_text(margin = margin(r = 8)),
    axis.title.y.right  = element_text(margin = margin(l = 8))
  )
print(dst_kombi5_final)
