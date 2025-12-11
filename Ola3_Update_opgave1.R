######Data Retrieval###########
# Indlæs pakker
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

#Tag gennemsnittet af DI spg
DI_spg <- c(spg1+spg3+spg5+spg9)/4
NDI_spg <- as.data.frame(DI_spg)


#Gennemsnittet af DST spg
FTI_spg <- c((spg1+spg2+spg3+spg4+spg5)/5)
NTI_spg <- as.data.frame(FTI_spg)            

#Gennemsnit af Mikro
mikro_spg <- c(spg1+spg2+spg5+spg9+spg11+spg12)/6
dfmikro_spg <- as.data.frame(mikro_spg)

#Gennemsnit af CCI
CCI_spg <- c(spg1+spg2+spg5+spg11)/4
dfCCI_spg <- as.data.frame(CCI_spg)

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

#Kigger på mikro
mikro_sam <- lm(pfveakst~dfmikro_spg$mikro_spg)
summary(mikro_sam)
fit_mikro <- fitted(mikro_sam) 
cor(pfveakst,fit_mikro)

#Kigger på CCI
CCI_sam <- lm(pfveakst~dfCCI_spg$CCI_spg)
summary(CCI_sam)
fit_CCI <- fitted(CCI_sam) 
cor(pfveakst,fit_CCI)

#################################################################################
#################Her starter OLA3 Opgave 1.1 #################

# Antal spørgsmål
antal_spg <- 12

# Sættes ind i matrix (bruger vendte serier)
mat <- cbind(spg1, spg2, spg3, spg4, spg5, spg6, spg7, spg8, spg9, spg10, spg11, spg12)
colnames(mat) <- paste0("spg", 1:12)


# Alle kombinationer 
kombi_spx <- unlist(
  lapply(1:antal_spg, function(k) combn(antal_spg, k, simplify = FALSE)),
  recursive = FALSE
)

length(kombi_spx)

res <- do.call(rbind, lapply(kombi_spx, function(spx){
  rm <- rowMeans(mat[, spx, drop = FALSE])
  fit <- lm(pfveakst ~ rm)
  r2  <- summary(fit)$r.squared
  r2a <- summary(fit)$adj.r.squared
  kor <- cor(pfveakst, rm, use = "complete.obs")
  data.frame(
    k        = length(spx),
    set      = paste0("spg", spx, collapse = "+"),
    r2       = r2,
    adj_r2   = r2a,
    kor      = kor,
    abs_kor  = abs(kor),
    stringsAsFactors = FALSE
  )
}))


# Sorter efter bedste r2
resultat_r2 <- res[order(-res$r2), ]
head(resultat_r2, 5)

# Laver datafram med bedste til værst
df_rank <- res[order(-res$r2, -res$abs_kor, res$k), ]
df_rank
df_rank$rank_r2  <- seq_len(nrow(df_rank))

#Gennemsnit af kombi 6
bedst_spg <- (spg1+spg3+spg5+spg7+spg9+spg11)/6
dfbedst_spg <- as.data.frame(bedst_spg)
bedst_sam <- lm(pfveakst~dfbedst_spg$bedst_spg)
summary(bedst_sam)
fit_bedst <- fitted(bedst_sam) 
cor(pfveakst,fit_bedst)

#Gennemsnit af kombi 5
komb5_spg <- (spg3+spg5+spg7+spg9+spg11)/5
dfkomb5_spg <- as.data.frame(komb5_spg)
komb5_sam <- lm(pfveakst~dfkomb5_spg$komb5_spg)
summary(komb5_sam)
fit_komb5 <- fitted(komb5_sam) 
cor(pfveakst,fit_komb5)

#Gennemsnit af kombi 4
komb4_spg <- (spg3+spg5+spg7+spg11)/4
dfkomb4_spg <- as.data.frame(komb4_spg)
komb4_sam <- lm(pfveakst~dfkomb4_spg$komb4_spg)
summary(komb4_sam)
fit_komb4 <- fitted(komb4_sam) 
cor(pfveakst,fit_komb4)

#####Kigger på det grafisk############
#####Kigger på vores bedste inditikator########

# Vores bedste indikator
vores_nt <- dfbedst_spg$bedst_spg

# 1) Skaler PF (pct.) over på nettotal-skalaen til venstre akse
a <- sd(c(DI_spg, vores_nt), na.rm = TRUE) / sd(pfveakst, na.rm = TRUE) 
PF_on_NT <- a * pfveakst

# 2) Dataframe
df <- data.frame(
  kvartal  = kvartaler,
  PF_on_NT = PF_on_NT,   
  DI       = DI_spg,     
  Vores    = vores_nt    
)

# 3) Plot: venstre akse = nettotal (linjer), højre akse = pct. (søjler)

# Fælles skala til akserne
pct_lim    <- c(-10, 10)
pct_breaks <- seq(-10, 10, by = 5)
net_lim    <- pct_lim * a
net_breaks <- pct_breaks * a

ggplot(df, aes(x = kvartal)) +
  geom_col(aes(y = PF_on_NT), fill = "blue", width = 80) +
  geom_line(aes(y = DI,    colour = "DI's indikator"), size = 1.1) +
  geom_line(aes(y = Vores, colour = "Vores indikator"), size = 1.1) +
  scale_colour_manual(
    name = "",
    values = c("DI's indikator" = "darkgray",
               "Vores indikator" = "black")
  ) +
  scale_y_continuous(
    name   = "Nettotal (indikatorer)",
    limits = net_lim,
    breaks = net_breaks,
    labels = function(x) round(x, 0),
    sec.axis = sec_axis(
      ~ . / a,
      name   = "Pct. (realvækst)",
      breaks = pct_breaks,
      labels = function(x) round(x, 1)
    )
  ) +
  labs(
    x = NULL,
    title = "Vores indikator er bedre til at fange perioder med positiv vækst",
    subtitle = "Kombination 6 indikator sammenlignet med DI i forhold til privatforbrug",
    caption = "Kilde: Danmarks Statistik og egne beregninger"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.y.left  = element_text(margin = margin(r = 8)),
    axis.title.y.right = element_text(margin = margin(l = 8))
  )
#####Kigger på vores anden bedste inditikator########

# Kombination af 5 
vores_nt <- dfkomb5_spg$komb5_spg

# 1) Skaler PF (pct.) over på nettotal-skalaen til venstre akse
a <- sd(c(DI_spg, vores_nt), na.rm = TRUE) / sd(pfveakst, na.rm = TRUE) 
PF_on_NT <- a * pfveakst

# 2) Dataframe
df <- data.frame(
  kvartal  = kvartaler,
  PF_on_NT = PF_on_NT,   
  DI       = DI_spg,     
  Vores    = vores_nt    
)
# 3) Plot: venstre akse = nettotal (linjer), højre akse = pct. (søjler)

pct_lim    <- c(-10, 10)
pct_breaks <- seq(-10, 10, by = 5)
net_lim    <- pct_lim * a
net_breaks <- pct_breaks * a

ggplot(df, aes(x = kvartal)) +
  geom_col(aes(y = PF_on_NT), fill = "blue", width = 80) +
  geom_line(aes(y = DI,    colour = "DI's indikator"), size = 1.1) +
  geom_line(aes(y = Vores, colour = "Vores indikator"), size = 1.1) +
  scale_colour_manual(
    name = "",
    values = c("DI's indikator" = "darkgray",
               "Vores indikator" = "black")
  ) +
  scale_y_continuous(
    name   = "Nettotal (indikatorer)",
    limits = net_lim,
    breaks = net_breaks,
    labels = function(x) round(x, 0),
    sec.axis = sec_axis(
      ~ . / a,
      name   = "Pct. (realvækst)",
      breaks = pct_breaks,
      labels = function(x) round(x, 1)
    )
  ) +
  labs(
    x = NULL,
    title = "Vores indikator er bedre til at fange perioder med positiv vækst",
    subtitle = "Kombination 5 indikator sammenlignet med DI i forhold til privatforbrug",
    caption = "Kilde: Danmarks Statistik og egne beregninger"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.y.left  = element_text(margin = margin(r = 8)),
    axis.title.y.right = element_text(margin = margin(l = 8))
  )

#####Kigger på vores tredje bedste inditikator######

# kombination af 4 
vores_nt <- dfkomb4_spg$komb4_spg

# 1) Skaler PF (pct.) over på nettotal-skalaen til venstre akse
a <- sd(c(DI_spg, vores_nt), na.rm = TRUE) / sd(pfveakst, na.rm = TRUE) 
PF_on_NT <- a * pfveakst

# 2) Dataframe
df <- data.frame(
  kvartal  = kvartaler,
  PF_on_NT = PF_on_NT,   
  DI       = DI_spg,     
  Vores    = vores_nt    
)

# 3) Plot: venstre akse = nettotal (linjer), højre akse = pct. (søjler)

pct_lim    <- c(-10, 10)
pct_breaks <- seq(-10, 10, by = 5)
net_lim    <- pct_lim * a
net_breaks <- pct_breaks * a

ggplot(df, aes(x = kvartal)) +
  geom_col(aes(y = PF_on_NT), fill = "blue", width = 80) +
  geom_line(aes(y = DI,    colour = "DI's indikator"), size = 1.1) +
  geom_line(aes(y = Vores, colour = "Vores indikator"), size = 1.1) +
  scale_colour_manual(
    name = "",
    values = c("DI's indikator" = "darkgray",
               "Vores indikator" = "black")
  ) +
  scale_y_continuous(
    name   = "Nettotal (indikatorer)",
    limits = net_lim,
    breaks = net_breaks,
    labels = function(x) round(x, 0),
    sec.axis = sec_axis(
      ~ . / a,
      name   = "Pct. (realvækst)",
      breaks = pct_breaks,
      labels = function(x) round(x, 1)
    )
  ) +
  labs(
    x = NULL,
    title = "Vores indikator er bedre til at fange perioder med positiv vækst",
    subtitle = "Kombination 4 indikator sammenlignet med DI i forhold til privatforbrug",
    caption = "Kilde: Danmarks Statistik og egne beregninger"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.y.left  = element_text(margin = margin(r = 8)),
    axis.title.y.right = element_text(margin = margin(l = 8))
  )

##############################################################
############## OPGAVE 1.4 – Forudsig forbruget ###############
##############################################################

# Datasæt med de nyeste observationer (dataK4)
# dataK4 indeholder månedstal fra jan 2000 til og med 2025K3
DI <- dataK4

# Gør alle kolonner numeriske
DI1 <- DI[, -1, drop = FALSE]
DI1[] <- lapply(DI1, function(x) as.numeric(gsub(",", ".", x)))

# Omdan månedstal til kvartaler (gennemsnit af hver 3. måned)
fend <- data.frame(matrix(nrow = 0, ncol = ncol(DI1)))
names(fend) <- names(DI1)

for (i in seq(3, nrow(DI1), by = 3)) {
  rows  <- (i - 2):i
  means <- colMeans(DI1[rows, , drop = FALSE], na.rm = TRUE)
  fend  <- rbind(fend, as.data.frame.list(means))
}

# Isoler spørgsmål (samme struktur som tidligere)
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

##############################################################
### T5-INDIKATOR (spg3 + spg5 + spg7 + spg9 + spg11) #########
##############################################################

# T5-indikator: samme sammensætning som i opg. 1.3
# Vi bruger rowMeans med na.rm=TRUE for at undgå NA i kvartaler med få manglende svar
kombi5_spg <- rowMeans(
  cbind(spg3, spg5, spg7, spg9, spg11),
  na.rm = TRUE
)

# Sørg for at T5 og pfveakst har samme længde (fælles periode)
n_faelles <- min(length(pfveakst), length(kombi5_spg))

df_T5 <- data.frame(
  pfveakst = pfveakst[1:n_faelles],   # årlig realvækst, 2000K1–2025K3
  T5       = kombi5_spg[1:n_faelles]  # T5-indikator, 2000K1–2025K3
)

# Estimer lineær model mellem privatforbrug og T5
mod_T5 <- lm(pfveakst ~ T5, data = df_T5)
summary(mod_T5)

##############################################################
### FORUDSIGELSE FOR 2025K4 ##################################
##############################################################

# Beregn T5-værdi for 2025K4 som gennemsnit af de sidste 4 observationer
# (bruger na.rm=TRUE så enkelte NA ikke giver NaN)
T5_2025K4 <- mean(tail(kombi5_spg, 4), na.rm = TRUE)

# Forudsig realvækst i privatforbruget for 2025K4 (kvartal nr. 104)
forudsig_2025K4 <- predict(mod_T5, newdata = data.frame(T5 = T5_2025K4))

# Saml resultatet
forudsigelser <- data.frame(
  Kvartal          = "2025K4",
  T5_værdi         = round(T5_2025K4, 2),
  Forudsiget_vækst = round(forudsig_2025K4, 2)
)

print(forudsigelser)


#################################################################################
################# OLA3 Opgave 1.5 – Bedste mikro-indikator ######################
DI <- dataK3
#Vektor 1: realvækst, for privatforbrug 
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

# Vælger mikro-spørgsmål (1,2,5,9,11,12)
mikro_idx  <- c(1, 2, 5, 9, 11, 12)
mat_mikro  <- cbind(spg1, spg2, spg5, spg9, spg11, spg12)
colnames(mat_mikro) <- paste0("spg", mikro_idx)

# Alle kombinationer K = 1..6
kombi_mikro <- unlist(
  lapply(1:ncol(mat_mikro), function(k) combn(ncol(mat_mikro), k, simplify = FALSE)),
  recursive = FALSE
)
length(kombi_mikro)   #63

# Finder r2, adj_r2 og kor for hver kombination
res_mikro <- do.call(rbind, lapply(kombi_mikro, function(idxs){
  rm  <- rowMeans(mat_mikro[, idxs, drop = FALSE])      
  fit <- lm(pfveakst ~ rm)
  data.frame(
    k       = length(idxs),
    set     = paste0(colnames(mat_mikro)[idxs], collapse = "+"),
    r2      = summary(fit)$r.squared,
    adj_r2  = summary(fit)$adj.r.squared,
    kor     = cor(pfveakst, rm, use = "complete.obs"),
    abs_kor = abs(cor(pfveakst, rm, use = "complete.obs")),
    stringsAsFactors = FALSE
  )
}))

# Sorter efter bedste r2 (som i opg. 1)
resultat_mikro_r2 <- res_mikro[order(-res_mikro$r2, -res_mikro$abs_kor, res_mikro$k), ]
head(resultat_mikro_r2, 10)

# Konstruerer serie for bedste mikro-kombination
best_row_mikro    <- resultat_mikro_r2[1, ]
best_names_mikro  <- unlist(strsplit(best_row_mikro$set, "\\+"))
mikro_bedst_spg   <- rowMeans(mat_mikro[, match(best_names_mikro, colnames(mat_mikro)), drop = FALSE])
dfmikro_bedst_spg <- data.frame(mikro_bedst_spg = mikro_bedst_spg)


# Model for bedste mikro-indikator
mikro_bedst_sam  <- lm(pfveakst ~ dfmikro_bedst_spg$mikro_bedst_spg)
summary(mikro_bedst_sam)
fit_mikro_bedst  <- fitted(mikro_bedst_sam)
cor(pfveakst, fit_mikro_bedst)

