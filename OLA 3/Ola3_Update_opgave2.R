#### PCR.fit ####
library(pls)
library(ggplot2)

############################################
# 1) FORV1 – Forventningsindikatorer
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

#  forv1_long indeholder kolonnerne: INDIKATOR, TID og value

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

# Udsnit med rækker fra linje 304 og nedefter (2000 og frem)
dataK4 <- forv1_wide[304:nrow(forv1_wide), ]

############################################
# 3) NKN1 – P.31 Privatforbrug
############################################

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

# Behold rækker fra linje 37 og nedefter (så vi får 2000K1–2025K2-ish)
PF2025 <- PF2025[37:nrow(PF2025), ]

# Udsnit med rækker fra linje 304 og fjerner de sidste 2 rækker
dataK3 <- forv1_wide[304:612, ]
DI     <- dataK3

# Vektor: årlig realvækst i privatforbrug (kvartalsvis)
pfveakst <- c(diff(log(PF2025$value), lag = 4) * 100)

# Gør alle DI-kolonner numeriske (komma -> punktum)
DI1 <- DI[, -1, drop = FALSE]
DI1[] <- lapply(DI1, function(x) as.numeric(gsub(",", ".", x)))

# Lav dataen kvartalsvis ved at tage gennemsnit af 3 måneder
fend <- data.frame(matrix(nrow = 0, ncol = ncol(DI1)))
names(fend) <- names(DI1)

for (i in seq(3, nrow(DI1), by = 3)) {
  rows  <- (i - 2):i
  means <- colMeans(DI1[rows, , drop = FALSE], na.rm = TRUE)
  fend  <- rbind(fend, as.data.frame.list(means))
}

# Isolér de forskellige spørgsmål (spg1–spg12)
spg1  <- as.numeric(fend[,  2])
spg2  <- as.numeric(fend[,  3])
spg3  <- as.numeric(fend[,  4])
spg4  <- as.numeric(fend[,  5])
spg5  <- as.numeric(fend[,  6])
spg6  <- -as.numeric(fend[, 7])   # vendt fortegn
spg7  <- as.numeric(fend[,  8])
spg8  <- -as.numeric(fend[, 9])   # vendt fortegn
spg9  <- as.numeric(fend[, 10])
spg10 <- as.numeric(fend[, 11])
spg11 <- as.numeric(fend[, 12])
spg12 <- as.numeric(fend[, 13])

############################################
# 4) PCR på alle 12 spørgsmål
############################################

pcr.fit <- pcr(
  pfveakst ~ spg1+spg2+spg3+spg4+spg5+spg6+
    spg7+spg8+spg9+spg10+spg11+spg12,
  validation = "CV", 
  scale = TRUE
)
summary(pcr.fit)

loadings.pcr.fit <- pcr.fit$loadings

validationplot(pcr.fit, val.type = "MSEP")

w.indicators1 <- as.data.frame(loadings.pcr.fit[1:12, 2]^2)
names(w.indicators1)[1] <- "Vægt"

sum(w.indicators1$Vægt)

w.indicators1$spg <- c("Spg1", "Spg2", "Spg3", "Spg4", "Spg5", "Spg6",
                       "Spg7", "Spg8", "Spg9", "Spg10", "Spg11", "Spg12")
w.indicators1$spg <- factor(w.indicators1$spg,
                            levels = paste0("Spg", 1:12))

# Oliver-datasæt med navngivne kolonner
Oliver <- data.frame(
  spg1  = spg1  * as.numeric(w.indicators1[1,1]),
  spg2  = spg2  * as.numeric(w.indicators1[2,1]),
  spg3  = spg3  * as.numeric(w.indicators1[3,1]),
  spg4  = spg4  * as.numeric(w.indicators1[4,1]),
  spg5  = spg5  * as.numeric(w.indicators1[5,1]),
  spg6  = spg6  * as.numeric(w.indicators1[6,1]),
  spg7  = spg7  * as.numeric(w.indicators1[7,1]),
  spg8  = spg8  * as.numeric(w.indicators1[8,1]),
  spg9  = spg9  * as.numeric(w.indicators1[9,1]),
  spg10 = spg10 * as.numeric(w.indicators1[10,1]),
  spg11 = spg11 * as.numeric(w.indicators1[11,1]),
  spg12 = spg12 * as.numeric(w.indicators1[12,1]),
  pfv   = pfveakst
)

lmvægt <- lm(data=Oliver,
             pfv ~ spg1+spg2+spg3+spg4+spg5+spg6+
               spg7+spg8+spg9+spg10+spg11+spg12)
summary(lmvægt)

# Plot med vægte (alle 12)
ggplot(data = w.indicators1, aes(x = spg, y = Vægt))+
  geom_bar(fill = "darkolivegreen4", stat = "identity")+
  ylab("Procent %")+xlab("Spørgsmål")+
  labs(title = "Spørgsmål 7 er højest vægtet",
       subtitle = "Vægtning af spørgsmål 1 til 12",
       caption = "Danmarks statistik og egne beregning") +
  theme_minimal()

############################################
# 5) PCR på de 5 "optimale" spørgsmål
############################################

pcr.fit.opt <- pcr(pfveakst ~
                     spg3+
                     spg5+
                     spg7+
                     spg9+
                     spg11,
                   validation = "CV", scale = TRUE)
summary(pcr.fit.opt)

loadings.pcr.fit.opt <- pcr.fit.opt$loadings
validationplot(pcr.fit.opt, val.type = "MSEP")

w.indicators1opt <- as.data.frame(loadings.pcr.fit.opt[1:5, 2]^2)
names(w.indicators1opt)[1] <- "Vægt"

sum(w.indicators1opt$Vægt)

w.indicators1opt$spg <- c("spg3", "spg5", "spg7", "spg9", "spg11")
w.indicators1opt$spg <- factor(w.indicators1opt$spg,
                               levels = paste0("spg", 1:12))

ggplot(data = w.indicators1opt, aes(x = spg, y = Vægt))+
  geom_bar(fill = "darkolivegreen4", stat = "identity")+
  ylab("Procent %")+xlab("Spørgsmål")+
  labs(title = "Spørgsmål 7 har markant størst vægt i indikatoren",
subtitle = "Vægtning af spørgsmål 1 til 12",
caption = "Danmarks statistik og egne beregning") +
  theme_minimal()

# Optimal indikator (5 spørgsmål)
optimalvektor <- as.data.frame(
  spg3  * as.numeric(w.indicators1opt[1,1])+
    spg5  * as.numeric(w.indicators1opt[2,1])+
    spg7  * as.numeric(w.indicators1opt[3,1])+
    spg9  * as.numeric(w.indicators1opt[4,1])+
    spg11 * as.numeric(w.indicators1opt[5,1])
)
names(optimalvektor)[1] <- "opt5"

lm.opt.vægt <- lm(pfveakst ~ opt5, data = optimalvektor)
summary(lm.opt.vægt)

############################################################
# 6) Fælles længde så alt matcher (pfveakst vs spørgsmål)
############################################################
n_common <- min(length(pfveakst), nrow(fend))

pfv_common <- pfveakst[1:n_common]

opt5_common <- optimalvektor$opt5[1:n_common]

# vægte til alle 12 spørgsmål fra pcr.fit
w_all <- as.numeric(w.indicators1$Vægt[1:12])

X_all <- cbind(spg1, spg2, spg3, spg4, spg5, spg6,
               spg7, spg8, spg9, spg10, spg11, spg12)

X_all_common <- X_all[1:n_common, ]

# Indikator med alle 12 spørgsmål
indicator_all <- as.numeric(X_all_common %*% w_all)

############################################################
# 7) Regressioner til prognoser
############################################################

# MODEL 1 – 5 optimale spørgsmål
df_opt <- data.frame(
  pfv   = pfv_common,
  opt5  = opt5_common
)
df_opt <- na.omit(df_opt)

lm_opt2 <- lm(pfv ~ opt5, data = df_opt)

beta0_opt <- coef(lm_opt2)[1]
beta1_opt <- coef(lm_opt2)[2]

# antag sidste obs = 2025K4
opt_2025K4_opt <- tail(df_opt$opt5, 1)

prognose_2025K4_opt <- beta0_opt + beta1_opt * opt_2025K4_opt


# MODEL 2 – alle 12 spørgsmål
df_all <- data.frame(
  pfv  = pfv_common,
  ind  = indicator_all
)
df_all <- na.omit(df_all)

lm_all <- lm(pfv ~ ind, data = df_all)

beta0_all <- coef(lm_all)[1]
beta1_all <- coef(lm_all)[2]

ind_2025K4_all <- tail(df_all$ind, 1)

prognose_2025K4_all <- beta0_all + beta1_all * ind_2025K4_all


############################################################
# 8) RESULTATER – prognose for 3. opg. (2025K4)
############################################################

prognose_2025K4_opt   # prognose med de 5 optimale spørgsmål
prognose_2025K4_all   # prognose med alle 12 spørgsmål

