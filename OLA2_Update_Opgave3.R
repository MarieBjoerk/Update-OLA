
############################################
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
dataK3 <- forv1_wide[304:nrow(forv1_wide), ]


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
PF2025 <- PF2025[37:nrow(PF2025), ]



###########OPGAVE3.1#####################
#Er der vækst ned eller op
PF2 <- PF2025[-(1:4), ]
PF2$pfveakst <- diff(log(PF2025$value), lag = 4) * 100
PF2$veakst <- as.integer(PF2$pfveakst>0)
PF2$veakst <- factor(PF2$veakst, levels = c(0,1), labels = c("ned","op"))
summary(PF2$veakst)

######################OPG3.2###############
# 1) Dummy for årlig vækst (1 = op, 0 = ned)
y <- as.integer(pfveakst > 0)

# 2) Forklarende variable: DI's spørgsmål og DST's spørgsmål
n <- length(y)
dat <- data.frame(
  spg1 = spg1[1:n],
  spg2 = spg2[1:n],
  spg3 = spg3[1:n],
  spg4 = spg4[1:n],
  spg5 = spg5[1:n],
  spg9 = spg9[1:n],
  y    = y
)

# 3) Logistisk regression for DI
sam_DI_spg <- c(spg1+spg3+spg5+spg9)/4
sam_log_DI <- glm(y ~ sam_DI_spg, data = dat, family = binomial())
PF2$pred_DI <- predict(sam_log_DI, type = "response")
pred_DI <- ifelse(PF2$pred_DI >= 0.5, 1, 0)

table_DI <- table(faktisk = dat$y, forudsigelse = pred_DI)
acc_DI <- mean(pred_DI == dat$y)

table_DI
acc_DI
####################

# 3) Logistisk regression for DST
sam_DST_spg <- (spg1 + spg2 + spg3 + spg4 + spg5) / 5
sam_log_DST <- glm(y ~ sam_DST_spg, data = dat, family = binomial())
PF2$pred_DST <- predict(sam_log_DST, type = "response")
pred_DST <- ifelse(PF2$pred_DST >= 0.5, 1, 0)

table_DST <- table(faktisk = dat$y, forudsigelse = pred_DST)
acc_DST <- mean(pred_DST == dat$y)

table_DST
acc_DST

##########NØGLETAL######
# DI
acc_DI  <- mean(pred_DI == dat$y)
prec_DI <- sum(pred_DI==1 & dat$y==1) / sum(pred_DI==1)
rec_DI  <- sum(pred_DI==1 & dat$y==1) / sum(dat$y==1)         # recall/sensitivitet
spec_DI <- sum(pred_DI==0 & dat$y==0) / sum(dat$y==0)         # specificitet
f1_DI   <- 2*prec_DI*rec_DI/(prec_DI+rec_DI)

# DST (samme mønster)
acc_DST  <- mean(pred_DST == dat$y)
prec_DST <- sum(pred_DST==1 & dat$y==1) / sum(pred_DST==1)
rec_DST  <- sum(pred_DST==1 & dat$y==1) / sum(dat$y==1)
spec_DST <- sum(pred_DST==0 & dat$y==0) / sum(dat$y==0)
f1_DST   <- 2*prec_DST*rec_DST/(prec_DST+rec_DST)


#####Her kigge på nøgletal for DI
acc_DI
prec_DI
rec_DI
spec_DI
f1_DI 
#####Her kigge på nøgletal for dst
acc_DST
prec_DST
rec_DST
spec_DST
f1_DST 


#####
DIK4 <- forv1_wide[613:nrow(forv1_wide), ]

#Kvartalsgennemsnit for de 2 måneder i DIK4

DIK4_means <- colMeans(DIK4[sapply(DIK4, is.numeric)], na.rm = TRUE)

DIK4_K4 <- as.data.frame(as.list(DIK4_means))

DIK4_spg1 <- as.numeric(DIK4_K4[[1]])
DIK4_spg2 <- as.numeric(DIK4_K4[[2]])
DIK4_spg3 <- as.numeric(DIK4_K4[[3]])
DIK4_spg4 <- as.numeric(DIK4_K4[[4]])
DIK4_spg5 <- as.numeric(DIK4_K4[[5]])
DIK4_spg9 <- as.numeric(DIK4_K4[[6]])

# 4) Forudsig for 2025K4 ved kun at give K4-værdierne
new_K4 <- data.frame(
  spg1 = DIK4_spg1,
  spg2 = DIK4_spg2,
  spg3 = DIK4_spg3,
  spg4 = DIK4_spg4,
  spg5 = DIK4_spg5,
  spg9 = DIK4_spg9
)

## DI-forudsigelse for 2025K4
ny_DI_K4 <- data.frame(
  sam_DI_spg = (DIK4_spg1 + DIK4_spg3 + DIK4_spg5 + DIK4_spg9) / 4
)
p_DI_K4 <- predict(sam_log_DI, newdata = ny_DI_K4, type = "response")
retning_DI <- ifelse(p_DI_K4 >= 0.5, "op", "ned")
retning_DI

## DST-forudsigelse for 2025K4
ny_DST_K4 <- data.frame(
  sam_DST_spg = (DIK4_spg1 + DIK4_spg2 + DIK4_spg3 + DIK4_spg4 + DIK4_spg5) / 5
)
p_DST_K4 <- predict(sam_log_DST, newdata = ny_DST_K4, type = "response")
retning_DST <- ifelse(p_DST_K >= 0.5, "op", "ned")
retning_DST

###################### OPGAVE 3.4 ##################
########################################
# Finder bedste spørgsmål
sam_bedst_spg <- (spg3) / 1
sam_log_bedst <- glm(y ~ sam_bedst_spg, data = dat, family = binomial())
PF2$pred_bedst <- predict(sam_log_bedst, type = "response")
pred_bedst <- ifelse(PF2$pred_bedst >= 0.5, 1, 0)

table_bedst <- table(faktisk = dat$y, forudsigelse = pred_bedst)
acc_bedst <- mean(pred_bedst == dat$y)

table_bedst
acc_bedst

######Ændre i vores tærskel i DI Forbrugertillid så rammer alle op

sam_DI_spg <- c(spg1+spg3+spg5+spg9)/4
sam_log_DI <- glm(y ~ sam_DI_spg, data = dat, family = binomial())
PF2$pred_DI <- predict(sam_log_DI, type = "response")
pred_DI <- ifelse(PF2$pred_DI >= 0.45, 1, 0)

table_DI <- table(faktisk = dat$y, forudsigelse = pred_DI)
acc_DI <- mean(pred_DI == dat$y)

table_DI
acc_DI

#####Prøver at lave den mest optimale kombinations model

sam_opti_spg <- (spg3+ spg9) / 2
sam_log_opti <- glm(y ~ sam_opti_spg, data = dat, family = binomial())
PF2$pred_opti <- predict(sam_log_opti, type = "response")
pred_opti <- ifelse(PF2$pred_opti >= 0.5, 1, 0)

table_opti <- table(faktisk = dat$y, forudsigelse = pred_opti)
acc_opti <- mean(pred_opti == dat$y)

table_opti
acc_opti
