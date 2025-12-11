############################################
# 1) Hent NKHC021 fra API
############################################
library(dkstat)

nkhc_filters <- list(
  FORMAAAL  = "*",                          # alle formål
  PRISENHED = "2020-priser, kædede værdier",
  "SÆSON"   = "Sæsonkorrigeret",
  TID       = "*"
)

nkhc021_long <- dst_get_data(
  table = "NKHC021",
  query = nkhc_filters,
  lang  = "da"
)

############################################
# 2) Omstrukturér: TID lodret, FORMÅL vandret
############################################
nkhc021_long2 <- nkhc021_long[, c("TID", "FORMAAAL", "value")]

nkhc021_wide <- reshape(
  nkhc021_long2,
  idvar   = "TID",
  timevar = "FORMAAAL",
  v.names = "value",
  direction = "wide"
)

names(nkhc021_wide) <- sub("^value\\.", "", names(nkhc021_wide))

# Udsnit fra række 37 og frem
nkhc_T <- nkhc021_wide[37:nrow(nkhc021_wide), ]

# nkhc_T findes allerede herover

# 1) Sørg for at alle forbrugskolonner (alt undtagen TID) er numeriske
nkhc_T_num <- nkhc_T
nkhc_T_num[ , -1] <- lapply(nkhc_T_num[ , -1], function(x) as.numeric(x))

# 2) Lav årlige vækstrater i procent for ALLE kolonner (undtagen TID)
nkhc_pct <- data.frame(
  TID = nkhc_T_num$TID[-(1:4)],  # vi mister de første 4 kvartaler pga. lag = 4
  lapply(nkhc_T_num[ , -1], function(x) diff(log(x), lag = 4) * 100)
)

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

library("ggplot2")
library("tidyverse")
# Udsnit med rækker fra linje 304 og fjerner de sidste 2 rækker
dataK3 <- forv1_wide[304:612, ]
DI <- dataK3

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


#Tag gennemsnittet af DI spg
di_samlet <- c(spg1+spg3+spg5+spg9)/4

dst_samlet <- c(spg1+spg2+spg3+spg4+spg5)/5

#di reg
di1 <- lm(nkhc_pct$CPA.Fødevarer.mv.~ di_samlet)
summary(di1)

di2 <- lm(nkhc_pct$CPB.Drikkevarer.og.tobak.mv.~ di_samlet)
summary(di2)

di3 <- lm(nkhc_pct$CPC.Beklædning.og.fodtøj~ di_samlet)
summary(di3)

di4 <- lm(nkhc_pct$CPD.Boligbenyttelse~ di_samlet)
summary(di4)

di5 <- lm(nkhc_pct$CPE.Elektricitet..fjernvarme.og.andet.brændsel~ di_samlet)
summary(di5)

di6 <- lm(nkhc_pct$CPF.Boligudstyr..husholdningstjenester.mv.~ di_samlet)
summary(di6)

di7 <- lm(nkhc_pct$CPG.Medicin..lægeudgifter.o.l.~ di_samlet)
summary(di7)

di8 <- lm(nkhc_pct$CPH.Køb.af.køretøjer~ di_samlet)
summary(di8)

di9 <- lm(nkhc_pct$CPI.Drift.af.køretøjer.og.transporttjenester~ di_samlet)
summary(di9)

di10 <- lm(nkhc_pct$CPJ.Information.og.kommunikation~ di_samlet)
summary(di10)

di11 <- lm(nkhc_pct$CPK.Fritid..sport.og.kultur~ di_samlet)
summary(di11)

di12 <- lm(nkhc_pct$CPL.Undervisning~ di_samlet)
summary(di12)

di13 <- lm(nkhc_pct$CPM.Restauranter.og.hoteller~ di_samlet)
summary(di13)

di14 <- lm(nkhc_pct$CPN.Forsikring.og.finansielle.tjenester~ di_samlet)
summary(di14)

di15 <- lm(nkhc_pct$CPO.Andre.varer.og.tjenester~ di_samlet)
summary(di15)
########################################################

#dstn reg

dst1 <- lm(nkhc_pct$CPA.Fødevarer.mv.~ dst_samlet)
summary(dst1)

dst2 <- lm(nkhc_pct$CPB.Drikkevarer.og.tobak.mv.~ dst_samlet)
summary(dst2)

dst3 <- lm(nkhc_pct$CPC.Beklædning.og.fodtøj~ dst_samlet)
summary(dst3)

dst4 <- lm(nkhc_pct$CPD.Boligbenyttelse~ dst_samlet)
summary(dst4)

dst5 <- lm(nkhc_pct$CPE.Elektricitet..fjernvarme.og.andet.brændsel~ dst_samlet)
summary(dst5)

dst6 <- lm(nkhc_pct$CPF.Boligudstyr..husholdningstjenester.mv.~ dst_samlet)
summary(dst6)

dst7 <- lm(nkhc_pct$CPG.Medicin..lægeudgifter.o.l.~ dst_samlet)
summary(dst7)

dst8 <- lm(nkhc_pct$CPH.Køb.af.køretøjer~ dst_samlet)
summary(dst8)

dst9 <- lm(nkhc_pct$CPI.Drift.af.køretøjer.og.transporttjenester~ dst_samlet)
summary(dst9)

dst10 <- lm(nkhc_pct$CPJ.Information.og.kommunikation~ dst_samlet)
summary(dst10)

dst11 <- lm(nkhc_pct$CPK.Fritid..sport.og.kultur~ dst_samlet)
summary(dst11)

dst12 <- lm(nkhc_pct$CPL.Undervisning~ dst_samlet)
summary(dst12)

dst13 <- lm(nkhc_pct$CPM.Restauranter.og.hoteller~ dst_samlet)
summary(dst13)

dst14 <- lm(nkhc_pct$CPN.Forsikring.og.finansielle.tjenester~ dst_samlet)
summary(dst14)

dst15 <- lm(nkhc_pct$CPO.Andre.varer.og.tjenester~ dst_samlet)
summary(dst15)




