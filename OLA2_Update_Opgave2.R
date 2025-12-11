##############Gå ned til her starter opgaven for 2000-2025 data###############
#############Første del er sammenling med artikelen Forbruget fortsætter fremgangen i 2016#####################

#vi henter data: 
#datasættet fra statistikbanken: PF- 1999Q1-2016Q2
#datasætter fra statistikbanken: FORV1-2000M01-2016M06

############################################
# Indlæs pakker
############################################
library(dkstat)
library(ggplot2)

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
dataK3 <- forv1_wide[304:501, ]


############################################
# 4) NKN1 – P.31 Privatforbrug,
#    2020-priser, kædede værdier, sæsonkorrigeret
############################################

# Valgfrit: tjek variabelnavne i NKN1
nkn1_meta <- dst_meta(table = "NKN1", lang = "da")
as.data.frame(nkn1_meta$variables)

# Filtre – variabelnavne skal stemme overens med id-kolonnen:
# TRANSAKT, PRISENHED, SÆSON, Tid
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

# Behold rækker fra linje 37 og nedefter (afgrænsning af analyseperiode)
PF2025 <- PF2025[37:106, ]

# Gør alle datakolonner (undtagen dato) numeriske
forvn <- dataK3[ , -1, drop = FALSE]
forvn[] <- lapply(forvn, function(x) as.numeric(gsub(",", ".", x)))

#lav dataen kvatalvis
fend <- data.frame(matrix(nrow = 0, ncol = ncol(forvn)))
names(fend) <- names(forvn)

for(i in seq(3, nrow(forvn), by = 3)){
  rows  <- (i-2):i
  means <- colMeans(forvn[rows, , drop = FALSE], na.rm = TRUE)
  fend  <- rbind(fend, as.data.frame.list(means))  # bevarer numerisk type
}

pfveakst <- c(diff(log(PF2025$value),lag=4)*100)

#Data.frame af realvækst
pfdf <- as.data.frame(pfveakst)

#isoler de forskellige spørgsmål

spg1 <- as.numeric(fend[,2])
spg2 <- as.numeric(fend[,3])
spg3 <- as.numeric(fend[,4])
spg4 <- as.numeric(fend[,5])
spg5 <- as.numeric(fend[,6])
spg6 <- -as.numeric(fend[,7])
spg7 <- as.numeric(fend[,8])
spg8 <- -as.numeric(fend[,9])
spg9 <- as.numeric(fend[,10])
spg10 <- as.numeric(fend[,11])
spg11 <- as.numeric(fend[,12])
spg12 <- as.numeric(fend[,13])



#Kig på korrelation
cor(pfveakst,spg1)
cor(pfveakst,spg2)
cor(pfveakst,spg3)
cor(pfveakst,spg4)
cor(pfveakst,spg5)
cor(pfveakst,spg6)
cor(pfveakst,spg7)
cor(pfveakst,spg8)
cor(pfveakst,spg9)
cor(pfveakst, spg10, use = "complete.obs")
cor(pfveakst,spg11)
cor(pfveakst,spg12)

#Samler DI spg og laver et dataframe
DI_spg <- c(spg1+spg3+spg5+spg9)/4
NDI_spg <- as.data.frame(DI_spg)

#Sammenligner DI spg med pfvækst
samlet <- lm(pfveakst~NDI_spg$DI_spg)
summary(samlet)
fit_samlet <- fitted(samlet)  
cor(pfveakst,fit_samlet)

#Sameler DST spg og laver er dataframe
FTI_spg <- c(spg1+spg2+spg3+spg4+spg5)/5
NFTI_spg <- as.data.frame(FTI_spg)

# Vi sammenliner spg 1 og 3
x  <- 0:(length(spg1) - 1)
df <- data.frame(x = x, spg1 = spg1, spg3 = spg3)

ggplot(df, aes(x = x)) +
  geom_line(aes(y = spg1), linewidth = 1, color = "darkgrey") +
  geom_line(aes(y = spg3), linewidth = 1, color = "blue")+
  labs(x = NULL, y = NULL)

# Vi sammenliner spg 2 og 3
x  <- 0:(length(spg1) - 1)
df <- data.frame(x = x, spg2 = spg2, spg3 = spg3)

ggplot(df, aes(x = x)) +
  geom_line(aes(y = spg2), linewidth = 1, color = "darkgrey") +
  geom_line(aes(y = spg3), linewidth = 1, color = "blue")+
  labs(x = NULL, y = NULL)

# Vi sammenliner spg 4 og 3
x  <- 0:(length(spg4) - 1)
df <- data.frame(x = x, spg4 = spg4, spg3 = spg3)

ggplot(df, aes(x = x)) +
  geom_line(aes(y = spg4), linewidth = 1, color = "darkgrey") +
  geom_line(aes(y = spg3), linewidth = 1, color = "blue")+
  labs(x = NULL, y = NULL)

# Vi sammenliner spg 5 og 3
x  <- 0:(length(spg1) - 1)
df <- data.frame(x = x, spg5 = spg5, spg3 = spg3)

ggplot(df, aes(x = x)) +
  geom_line(aes(y = spg5), linewidth = 1, color = "darkgrey") +
  geom_line(aes(y = spg3), linewidth = 1, color = "blue")+
  labs(x = NULL, y = NULL)

# Vi sammenliner spg 6 og 3
x  <- 0:(length(spg1) - 1)
df <- data.frame(x = x, spg6 = spg6, spg3 = spg3)

ggplot(df, aes(x = x)) +
  geom_line(aes(y = spg9), linewidth = 1, color = "darkgrey") +
  geom_line(aes(y = spg3), linewidth = 1, color = "blue")+
  labs(x = NULL, y = NULL)

#####


# Tallene fra DST er blevet ændret
ftillid2016DI <- data.frame(fend[1:66,1])

indikatorplus2016 <- sum(ftillid2016DI$fend.1.66..1.)/nrow(ftillid2016DI)

vplus2016 <- c(rep(indikatorplus2016,66))

ftillid2016DI$fend.1.66..1. <- (ftillid2016DI$fend.1.66..1.)+(vplus2016*-1)

kvartaler <- seq.Date(from = as.Date("2000-01-01"),
                      to = as.Date("2016-06-30"),
                      by = "quarter")

####Bruger mean til at justere vores data
mean_di <- mean(DI_spg, na.rm = TRUE)

mean_di

mean_pfv <- mean(pfveakst, na.rm=TRUE)

mean_pfv

diff_means <- mean_di


# Beregn hvor meget vi skal justere med
mean_di <- mean(DI_spg, na.rm = TRUE)      
justering <- -mean_di                      

justering_pfv <- mean_pfv    

# Lav et data frame med original og justeret
jdf <- data.frame(
  kvartal    = kvartaler,
  pfveakst   = pfveakst,
  DI_spg     = DI_spg,
  pfveakst_adj = pfveakst + justering_pfv,   
  DI_spg_adj   = DI_spg + justering     
)

##################
# 1) Skaler PF_adj -> DI_adj-skala 
a <- sd(jdf$DI_spg_adj, na.rm = TRUE) / sd(jdf$pfveakst_adj, na.rm = TRUE)
pf_on_DI <- a * jdf$pfveakst_adj  # bruges til søjlerne

# Nettotal (venstre akse) og Procent (søjler)
df_plot <- data.frame(
  kvartal  = jdf$kvartal,
  DI_adj   = jdf$DI_spg_adj,
  PF_on_DI = pf_on_DI         
)

# Tag et kvartal mere med før
one_qtr  <- 91
x_limits <- c(min(df_plot$kvartal) - one_qtr, max(df_plot$kvartal))

# 2) Plot – blå søjler + korrekt 0-justering + kilde nederst
ggplot(df_plot, aes(x = kvartal)) +
  geom_col(aes(y = PF_on_DI,
               fill = "Årlig realvækst pr. kvartal i privat forbruget"),
           width = 80) +  # ~ ét kvartal på Date-akse
  geom_line(aes(y = DI_adj,
                colour = "DI's forbrugertillidsindikator"),
            linewidth = 1.2, lineend = "round") +
  scale_y_continuous(
    name = "Nettotal",
    limits = c(-25, 25),
    breaks = c(-25, -17, -8, 0, 8, 17, 25),
    sec.axis = sec_axis(~ . / a, name = "Pct.",
                        breaks = c(-8, -5, -3, 0, 3, 5, 8))
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%y",
               expand = c(0, 0),
               limits = x_limits) +     # <- ændret til x_limits
  scale_fill_manual(NULL, values = c(
    "Årlig realvækst pr. kvartal i privat forbruget" = "steelblue"
  )) +
  scale_colour_manual(NULL, values = c(
    "DI's forbrugertillidsindikator" = "darkgray"
  )) +
  labs(
    x = NULL,
    title = "DI's forbrugertillidsindikator følger i højere grad privatforbruget",
    subtitle = "DI's forbrugerindikator set i sammenhæng mellem husholdningernes privatforbrug.",
    caption = "Kilde: Danmarks Statistik"
  ) +
  theme_minimal(base_size = 13, base_family = "Arial") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.4, colour = "white"),
    panel.border = element_rect(fill = NA, colour = "darkgray", linewidth = 0.8),
    axis.ticks = element_line(colour = "darkgray"),
    axis.title.y.left  = element_text(colour = "darkgray", margin = margin(r = 8)),
    axis.title.y.right = element_text(colour = "darkgray", margin = margin(l = 8)),
    axis.text = element_text(colour = "darkgray"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(margin = margin(b = 10)),
    plot.caption = element_text(hjust = 0, margin = margin(t = 10))
  )

#####
#kun spg 3 sammenlignet med pfvækst 
a <- sd(spg3, na.rm = TRUE) / sd(pfveakst, na.rm = TRUE)
pf_on_spg3 <- a * pfveakst

df_ts <- data.frame(
  kvartal     = kvartaler,   
  spg3        = spg3,        
  pf_on_spg3  = pf_on_spg3   
)

ggplot(df_ts, aes(x = kvartal)) +
  geom_col(aes(y = pf_on_spg3, fill = "Årlig realvækst pr. kvartal i privat forbruget"), width = 80) +
  geom_line(aes(y = spg3, colour = "Danmark i dag sammenlignet med for et år siden?"), linewidth = 1.2) +
  scale_y_continuous(
    name = "Nettotal",
    limits = c(-50, 30), breaks = seq(-50, 30, 10),
    sec.axis = sec_axis(~ . / a, name = "Pct.", breaks = c(-8,-5,-3,0,3,5,8))
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y",
               expand = c(0,0), limits = range(df_ts$kvartal)) +
  scale_fill_manual(NULL,  values = c("Årlig realvækst pr. kvartal i privat forbruget" = "steelblue")) +
  scale_colour_manual(NULL, values = c("Danmark i dag sammenlignet med for et år siden?" = "#6A6A6A")) +
  labs(title = "Spg3 sammenlignet med privatforbruget", caption = "Kilde: Danmarks Statistik") +
  theme_minimal(base_size = 13, base_family = "Arial") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.4, colour = "white"),
        panel.border = element_rect(fill = NA, colour = "darkgray", linewidth = 0.8),
        legend.position = "bottom")



############################################
library(dkstat)
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

# Valgfrit: tjek variabelnavne i NKN1
nkn1_meta <- dst_meta(table = "NKN1", lang = "da")
as.data.frame(nkn1_meta$variables)

# Filtre – variabelnavne skal stemme overens med id-kolonnen:
# TRANSAKT, PRISENHED, SÆSON, Tid
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


#####################Før opgave start###############################
library("ggplot2")
library("tidyverse")
# Udsnit med rækker fra linje 304 og fjerner de sidste 2 rækker
dataK3 <- forv1_wide[304:612, ]
DI <- dataK3
#Vektor 1: realvækst, for privatforbrug 

pfveakst <- c(diff(log(PF2025$`value`),lag=4)*100)

#Data.frame af realvækst
pfdf <- as.data.frame(pfveakst)

#Nu skal vi have indelt vores datasæt fra måneder til kvartalrt- vi bruger OLA1, opg 5.2:

# Gør alle datakolonner
DI1 <- DI[ , -1, drop = FALSE]
DI1[] <- lapply(DI1, function(x) as.numeric(gsub(",", ".", x)))

#lav dataen kvatalvis
fend <- data.frame(matrix(nrow = 0, ncol = ncol(DI1)))
names(fend) <- names(DI1)

for(i in seq(3, nrow(DI1), by = 3)){
  rows  <- (i-2):i
  means <- colMeans(DI1[rows, , drop = FALSE], na.rm = TRUE)
  fend  <- rbind(fend, as.data.frame.list(means))
}

#isoler de forskellige spørgsmål

spg1 <- as.numeric(fend[,2])
spg2 <- as.numeric(fend[,3])
spg3 <- as.numeric(fend[,4])
spg4 <- as.numeric(fend[,5])
spg5 <- as.numeric(fend[,6])
spg6 <- -as.numeric(fend[,7])
spg7 <- as.numeric(fend[,8])
spg8 <- -as.numeric(fend[,9])
spg9 <- as.numeric(fend[,10])
spg10 <- as.numeric(fend[,11])
spg11 <- as.numeric(fend[,12])
spg12 <- as.numeric(fend[,13])

#Kig på korrelation
cor(pfveakst,spg1)
cor(pfveakst,spg2)
cor(pfveakst,spg3)
cor(pfveakst,spg4)
cor(pfveakst,spg5)
cor(pfveakst,spg6)
cor(pfveakst,spg7)
cor(pfveakst,spg8)
cor(pfveakst,spg9)
cor(pfveakst,spg10)
cor(pfveakst,spg11)
cor(pfveakst,spg12)

####Der var en NA########
cor(pfveakst, spg10, use = "complete.obs")
cor(pfveakst,spg11)
cor(pfveakst,spg12)

#Tag gennemsnittet af DI spg og FTI gennemsnittet spg
DI_spg <- c(spg1+spg3+spg5+spg6)/4
NDI_spg <- as.data.frame(DI_spg)
FTI_spg <- c((spg1+spg2+spg3+spg4+spg5)/5)
NTI_spg <- as.data.frame(FTI_spg)            

# Laver kvartaler om til dato med start 2000
kvartaler <- seq.Date(as.Date("2000-01-01"), by = "quarter", length.out = length(DI_spg))

#Kigger på DI spg
DI_sam <- lm(pfveakst~NDI_spg$DI_spg)
summary(DI_sam)
fit_DI <- fitted(DI_sam)  
cor(pfveakst,fit_DI)

#Kigger på FTI spg
FTI_sam <- lm(pfveakst~NTI_spg$FTI_spg)
summary(FTI_sam)
fit_FTI <- fitted(FTI_sam)  
cor(pfveakst,fit_FTI)

#################

# Tager 2025K1,2025K2 og 2025K3 som de to sidste observationer
DI_K1 <- tail(DI_spg,3)[3]
DI_K2  <- tail(DI_spg, 3)[2]
DI_K3  <- tail(DI_spg, 3)[1]
FTI_K1 <- tail(FTI_spg, 3)[3]
FTI_K2 <- tail(FTI_spg, 3)[2]
FTI_K3 <- tail(FTI_spg, 3)[1]

# 3) Bruger de første 3 kvataler til at regne ud for 2025K4 
DI_K4_hat  <- (DI_K1  + DI_K2 + DI_K3)/3
FTI_K4_hat <- (FTI_K1 + FTI_K2 + FTI_K3)/3

#Bruger estimat til at finde 2025K4
PF_K4_DI  <- 1.56725 +0.22777 * DI_K4_hat
PF_K4_FTI <- 1.23186 + 0.17722 * FTI_K4_hat

PF_K4_DI
PF_K4_FTI

###############################################################################
#Sammenligning mellem DST og DI forbrugertillid

# År og kvartal til x-akse (00, 01, …)
ln   <- length(fit_DI)
i    <- 0:(ln - 1)
year <- 2000 + i %/% 4
qtr  <- (i %% 4) + 1
x    <- year + (qtr - 1) / 4

df <- data.frame(x = x, fit_DI = fit_DI, fit_FTI = fit_FTI)

# Årsmærker som to cifre
break_years <- seq(min(year), max(year), by = 1)
labels      <- sprintf("%02d", break_years %% 100)

ggplot(df, aes(x = x)) +
  # tydelig nul-linje
  geom_hline(yintercept = 0, linewidth = 0.7, colour = "darkgray") +
  # linjer
  geom_line(aes(y = fit_DI,  colour = "DI’s forbrugertillidsindikator"),
            linewidth = 1.2, lineend = "round") +
  geom_line(aes(y = fit_FTI, colour = "DST’s forbrugertillidsindikator"),
            linewidth = 1.2, lineend = "round") +
  # akser
  scale_y_continuous(name = "Pct.") +
  scale_x_continuous(breaks = break_years, labels = labels, expand = c(0, 0)) +
  # farver og forklaring
  scale_colour_manual(
    NULL,
    values = c(
      "DI’s forbrugertillidsindikator"  = "darkgray",  # grå
      "DST’s forbrugertillidsindikator" = "blue"   # blå
    )
  ) +
  # titel, undertekst og kilde
  labs(
    title    = "Danmarks Statistik og Dansk Industi følges med hinanden",
    subtitle = "Sammenligning mellem DST og DI forbrugertillid",
    caption  = "Kilde: Danmarks Statistik"
  ) +
  # DI-lignende tema
  theme_minimal(base_size = 13, base_family = "Arial") +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.4, colour = "white"),
    panel.border       = element_rect(fill = NA, colour = "darkgray", linewidth = 0.8),
    axis.ticks         = element_line(colour = "darkgray"),
    axis.title.y.left  = element_text(colour = "darkgray", margin = margin(r = 8)),
    axis.text          = element_text(colour = "darkgray"),
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold", margin = margin(b = 6)),
    plot.subtitle      = element_text(margin = margin(b = 10)),
    plot.caption       = element_text(hjust = 0, margin = margin(t = 10))
  )

##############################
# Vi skal skaler PF til DI-skala og laver en df_plot
a <- sd(DI_spg, na.rm = TRUE) / sd(pfveakst, na.rm = TRUE)
PF_on_DI <- a * pfveakst

df_plot <- data.frame(
  kvartal  = kvartaler,
  DI_val   = DI_spg,
  PF_on_DI = PF_on_DI
)

# DI  graf 2000–2025
# faste y-grænser (ingen dynamik)
y_limits <- c(-45, 25)                 
y_breaks <- seq(-45, 25, by = 5)

# x-grænser som før: ét ekstra kvartal før
one_qtr  <- 91
x_limits <- c(min(df_plot$kvartal) - one_qtr, max(df_plot$kvartal))

ggplot(df_plot, aes(x = kvartal)) +
  geom_col(aes(y = PF_on_DI,
               fill = "Årlig realvækst pr. kvartal i privat forbruget"),
           width = 80) +
  geom_line(aes(y = DI_val,
                colour = "DI's forbrugertillidsindikator"),
            linewidth = 1.2, lineend = "round") +
  scale_y_continuous(
    name   = "Nettotal",
    limits = y_limits,    #faste grænser
    breaks = y_breaks,    #faste ticks
    sec.axis = sec_axis(~ . / a, name = "Pct.",
                        breaks = c(-8, -6, -4, -2, 0, 2, 4, 6, 8))
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%y",
               expand = c(0, 0),
               limits = x_limits) +
  scale_fill_manual(NULL, values = c(
    "Årlig realvækst pr. kvartal i privat forbruget" = "steelblue"
  )) +
  scale_colour_manual(NULL, values = c(
    "DI's forbrugertillidsindikator" = "darkgray"
  )) +
  labs(
    x = NULL,
    title    = "DI's forbrugertillidsindikator følger i højere grad privatforbruget",
    caption  = "Kilde: Danmarks Statistik"
  ) +
  theme_minimal(base_size = 13, base_family = "Arial") +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.4, colour = "white"),
    panel.border       = element_rect(fill = NA, colour = "darkgray", linewidth = 0.8),
    axis.ticks         = element_line(colour = "darkgray"),
    axis.title.y.left  = element_text(colour = "darkgray", margin = margin(r = 8)),
    axis.title.y.right = element_text(colour = "darkgray", margin = margin(l = 8)),
    axis.text          = element_text(colour = "darkgray"),
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold", margin = margin(b = 6)),
    plot.subtitle      = element_text(margin = margin(b = 10)),
    plot.caption       = element_text(hjust = 0, margin = margin(t = 10))
  )
#############
# Vi skaler PF til DST-skala og laver en ny df_plot_dst
a_dst <- sd(FTI_spg, na.rm = TRUE) / sd(pfveakst, na.rm = TRUE)
PF_on_DST <- a_dst * pfveakst

df_plot_dst <- data.frame(
  kvartal   = kvartaler,
  DST_val   = FTI_spg,
  PF_on_DST = PF_on_DST
)

# Faste aksegrænser og ét ekstra kvartal før
y_limits <- c(-45, 25)
y_breaks <- seq(-45, 25, by = 5)
one_qtr  <- 91
x_limits <- c(min(df_plot_dst$kvartal) - one_qtr, max(df_plot_dst$kvartal))

ggplot(df_plot_dst, aes(x = kvartal)) +
  geom_col(aes(y = PF_on_DST,
               fill = "Årlig realvækst pr. kvartal i privat forbruget"),
           width = 80) +
  geom_line(aes(y = DST_val,
                colour = "DST's forbrugertillidsindikator"),
            linewidth = 1.2, lineend = "round") +
  scale_y_continuous(
    name   = "Nettotal",
    limits = y_limits,
    breaks = y_breaks,
    sec.axis = sec_axis(~ . / a_dst, name = "Pct.",
                        breaks = c(-8, -6, -4, -2, 0, 2, 4, 6, 8))
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%y",
               expand = c(0, 0),
               limits = x_limits) +
  scale_fill_manual(NULL, values = c(
    "Årlig realvækst pr. kvartal i privat forbruget" = "steelblue"
  )) +
  scale_colour_manual(NULL, values = c(
    "DST's forbrugertillidsindikator" = "darkgray"   # skift evt. til "#0089CF" for blå linje
  )) +
  labs(
    x = NULL,
    title    = "DST's forbrugertillidsindikator følger privatforbruget",
    caption  = "Kilde: Danmarks Statistik"
  ) +
  theme_minimal(base_size = 13, base_family = "Arial") +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.4, colour = "white"),
    panel.border       = element_rect(fill = NA, colour = "darkgray", linewidth = 0.8),
    axis.ticks         = element_line(colour = "darkgray"),
    axis.title.y.left  = element_text(colour = "darkgray", margin = margin(r = 8)),
    axis.title.y.right = element_text(colour = "darkgray", margin = margin(l = 8)),
    axis.text          = element_text(colour = "darkgray"),
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold", margin = margin(b = 6)),
    plot.subtitle      = element_text(margin = margin(b = 10)),
    plot.caption       = element_text(hjust = 0, margin = margin(t = 10))
  )



