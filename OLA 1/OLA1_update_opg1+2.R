#OPGAVE 1: 

#Opgave 1.1- Find data: 

library(readr)
newhomes <- read_csv("newhomes.csv")
View(newhomes)


newhome <- newhomes[-1,]

Frigrunden9 <- newhome[newhome$vej=="Frigrunden 96270 Tønder",]

Hostrupvej13A <- newhome[newhome$vej=="Hostrupvej 13AJejsing, 6270 Tønder",]

#Opgave 1.2- Vælg

Valg1 <- newhome[newhome$vej=="Skodborg Stadionvej 106630 Rødding",]
Valg2 <- newhome[newhome$vej=="Nymarks Allé 2588320 Mårslet",]

#Opgave 1.3- NA værdi 
#- Se PDF 

#Opgave 1.4- Rescearch goal 
#- se PDF

#OPGAVE 2- Korrelation og simple lineær regression 

#opgave 2.1- se både pdf og opgave 2.2- Korrelation 



#opgave 2.3
library(readr)
View(newhomes)
newhome <- newhomes[-1,]

#Vi renser datasættet: 

newhome[newhome=="- m²"] <- NA
newhome[newhome=="Helårsgrund"] <-NA
newhome[newhome=="Fritidsgrund"] <-NA
newhome[newhome=="Helårsgrund/Fritidsgrund"] <-NA
newhome[newhome=="Fritidsgrund/Fritidsbolig"] <-NA

newhome2 <- na.omit(newhome)

#Vi fjerne NA værdier, men finder ydereligere NA værdier: 

newhome2[newhome2=="-"] <- NA
newhome2[newhome2==" - "] <- NA
newhome2[newhome2=="- "] <- NA
newhome2[newhome2=="Landejendom"] <-NA
newhome2[newhome2=="Landejendom/Landejendom"] <- NA

newhome2<- na.omit(newhome2)

#Her fjerner strings, så vi kun har med nummeriske tal at arbejde med: 

newhome2$kvm <- gsub("m²","",newhome2$kvm)
newhome2$pris <- gsub("kr.","",newhome2$pris)
newhome2$alder <- gsub("Opført","",newhome2$alder)
newhome2$grund <- gsub("Grund","",newhome2$grund)
newhome2$grund <- gsub("m²","",newhome2$grund)
newhome2$ejerudg <- gsub("Ejerudg.","",newhome2$ejerudg)
newhome2$ejerudg <- gsub("kr./md","",newhome2$ejerudg)
newhome2$vaer <- gsub("Vær.","",newhome2$vaer)
newhome2$liggetid <- gsub("dag","",newhome2$liggetid)
newhome2$liggetid <- gsub("e","",newhome2$liggetid)
newhome2$ejerudg <- gsub("Ydelse ","", newhome2$ejerudg)

#Vi laver 7 vektorer: 

prisV <- c(newhome2$pris)
kvmV <- c(newhome2$kvm)
alderV <- c(newhome2$alder)
grundV <- c(newhome2$grund)
ejerudgV <- c(newhome2$ejerudg)
vaerV <- c(newhome2$vaer)
liggetidV <- c(newhome2$liggetid)

#Vi transformerer fra charater til numerisk værdi:  

prisV <- as.numeric(gsub("[.]", "",prisV))
kvmV<- as.numeric(gsub("[.]", "",kvmV))
alderV <- as.numeric(gsub("[.]","",alderV))
grundV <- as.numeric(gsub("[.]","",grundV))
ejerudgV <- as.numeric(gsub("[.]","",ejerudgV))
vaerV <- as.numeric(gsub("[.]","",vaerV))
liggetidV <- as.numeric(gsub("[.]","",liggetidV))

# Samlet data frame med alle variabler
df_all <- data.frame(
  pris      = prisV,
  kvm       = kvmV,
  alder     = alderV,
  grund     = grundV,
  ejerudg   = ejerudgV,
  vaer      = vaerV,
  liggetid  = liggetidV
)

# Pris pr. m² 
df_all$pris_pr_m2 <- df_all$pris / df_all$kvm
View(df_all)
summary(df_all)

# Funktion der fjerner outliers efter 1% og 99% percentil
remove_outliers_q <- function(x, lower = 0.002, upper = 0.99) {
  qs <- quantile(x, c(lower, upper), na.rm = TRUE)
  x >= qs[1] & x <= qs[2]
}

# Logisk maske: behold kun observationer der ikke er outliers på nogen af variablerne
mask <- with(df_all,
             remove_outliers_q(pris_pr_m2) &
               remove_outliers_q(alder) &
               remove_outliers_q(grund) &
               remove_outliers_q(ejerudg) &
               remove_outliers_q(vaer) &
               remove_outliers_q(liggetid))

# Nyt datasæt uden outliers
df_no_out <- df_all[mask, ]

View(df_no_out)

# Simple regressioner
reg_alder    <- lm(pris_pr_m2 ~ alder,    data = df_all)
reg_grund    <- lm(pris_pr_m2 ~ grund,    data = df_all)
reg_ejerudg  <- lm(pris_pr_m2 ~ ejerudg,  data = df_all)
reg_kvm     <- lm(pris_pr_m2 ~ kvm,     data = df_all)
reg_liggetid <- lm(pris_pr_m2 ~ liggetid, data = df_all)

summary(reg_alder)
summary(reg_grund)
summary(reg_ejerudg)
summary(reg_kvm)
summary(reg_liggetid)


# Data til korrelationsmatrix (pris pr. m² + de 5 forklarende)
kordata <- df_all[, c("pris_pr_m2", "alder", "grund", "ejerudg", "kvm", "liggetid")]
cor_mat <- cor(kordata, use = "complete.obs")
cor_mat

library(ggplot2)

ggplot(df_no_out, aes(x = grund, y = pris_pr_m2)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Sammenhæng mellem grundareal og pris pr. m²",
    x = "Grundareal (m²)",
    y = "Pris pr. m² (kr.)"
  ) +
  theme_minimal()


# 1) Alder
ggplot(df_no_out, aes(x = alder, y = pris_pr_m2)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Sammenhæng mellem alder og pris pr. m²",
    x = "Opførelsesår",
    y = "Pris pr. m² (kr.)"
  ) +
  theme_minimal()

# 2) Grundareal
ggplot(df_no_out, aes(x = grund, y = pris_pr_m2)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Sammenhæng mellem grundareal og pris pr. m²",
    x = "Grundareal (m²)",
    y = "Pris pr. m² (kr.)"
  ) +
  theme_minimal()

# 3) Ejerudgift
ggplot(df_no_out, aes(x = ejerudg, y = pris_pr_m2)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Sammenhæng mellem ejerudgift og pris pr. m²",
    x = "Ejerudgift (kr./md.)",
    y = "Pris pr. m² (kr.)"
  ) +
  theme_minimal()

# 4) kvadrameter
ggplot(df_no_out, aes(x = kvm, y = pris_pr_m2)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Sammenhæng mellem kvadratmeter og pris pr. m²",
    x = "Kvadratmeter",
    y = "Pris pr. m² (kr.)"
  ) +
  theme_minimal()

# 5) Liggetid
ggplot(df_no_out, aes(x = liggetid, y = pris_pr_m2)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Sammenhæng mellem liggetid og pris pr. m²",
    x = "Liggetid (dage)",
    y = "Pris pr. m² (kr.)"
  ) +
  theme_minimal()

