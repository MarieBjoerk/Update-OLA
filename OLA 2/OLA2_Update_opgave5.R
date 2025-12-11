#Opgave 5:

#5.1 årlig realvækst fra eurostat

install.packages("eurostat")
library("tidyverse")
library("eurostat")

#hent data fra Eurostat 


hushold <- get_eurostat("namq_10_fcs", type = "label")


###################################################################################################################


hus_lande <- hushold %>%
  filter(geo %in% c("Germany", "Denmark", "Sweden", "Belgium", "Spain", "France", "Italy", "Netherlands", "Austria"))


hus_tid <- hushold %>%
  filter(TIME_PERIOD >= as.Date("1999-01-01"),
         TIME_PERIOD <= as.Date("2025-09-30"))

hus_enhed <- hushold %>%
  filter(unit == "Chain linked volumes (2020), million euro",
         s_adj == "Seasonally and calendar adjusted data",
         na_item == "Final consumption expenditure of households"
  )


#vi joiner på vores valgte variabler 

library(dplyr)

hus_merge1 <- semi_join(hus_lande, hus_tid, by = c("geo", "TIME_PERIOD"))

hus_final <- semi_join(hus_merge1, hus_enhed, by = c("geo", "TIME_PERIOD", "unit", "s_adj", "na_item"))


######################## DEL DATASÆTTET OP I LANDE ####################################

df_denmark     <- hus_final %>% filter(geo == "Denmark")
df_sweden      <- hus_final %>% filter(geo == "Sweden")
df_germany     <- hus_final %>% filter(geo == "Germany")
df_belgium     <- hus_final %>% filter(geo == "Belgium")
df_spain       <- hus_final %>% filter(geo == "Spain")
df_france      <- hus_final %>% filter(geo == "France")
df_italy       <- hus_final %>% filter(geo == "Italy")
df_netherlands <- hus_final %>% filter(geo == "Netherlands")
df_austria     <- hus_final %>% filter(geo == "Austria")


#Realvækst for de relevante lande: 

DK_realvækst <- c(diff(log(df_denmark$values),lag=4)*100)
SE_realvækst <- c(diff(log(df_sweden$values),lag=4)*100)
DE_realvækst <- c(diff(log(df_germany$values),lag=4)*100)
BE_realvækst <- c(diff(log(df_belgium$values),lag=4)*100)
ES_realvækst <- c(diff(log(df_spain$values),lag=4)*100)
FR_realvækst <- c(diff(log(df_france$values),lag=4)*100)
IT_realvækst <- c(diff(log(df_italy$values),lag=4)*100)
NL_realvækst <- c(diff(log(df_netherlands$values),lag=4)*100)
AT_realvækst <- c(diff(log(df_austria$values),lag=4)*100)


#Datasæt for realvækst alle lande: 

realdata <- data.frame(DK_realvækst, SE_realvækst, DE_realvækst, BE_realvækst,
                       ES_realvækst, FR_realvækst, IT_realvækst,
                       NL_realvækst, AT_realvækst )


#Kvartaldata: 
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2025-09-30")

kvartaler <- seq.Date(from = start_date, to = end_date, by = "quarter")
kvartal_labels <- paste(format( kvartaler, "%Y"),
                        paste0("Q", (as.numeric(format(kvartaler,"%m")) - 1) %/% 3+1))


realdata$kvartaler <- kvartal_labels

# Flyt 'kvartaler' til første kolonne
realdata <- realdata[, c("kvartaler", setdiff(names(realdata), "kvartaler"))]


############Spørgsmål 5.2- hvilket land har haft højeste kvartalvis årlig vækst###################

#Vi laver format lang, så det er nemmere at regne med

library(tidyr)
library(dplyr)

realdata_long <- realdata %>%
  pivot_longer(cols = ends_with("_realvækst"),
               names_to = "Land",
               values_to = "Vækst")

#Derefter omregner vi til gennemsnitlig kvartalvis vækst: 

gem_vækst <- realdata_long %>%                                              #Piper fra realdata_long til gem_vækst
  group_by(Land) %>%                                                        #Grupperer efter værdierne i "Land"
  summarise(Gennemsnit = mean(Vækst, na.rm = TRUE)) %>%                     #Beregner gennemsnittet efter kolonnen "Vækst"for hver gruppe land. na.rm=TRUE_: ignorer NA.værdier 
  arrange(desc(Gennemsnit))                                                 #sorterer i falende rækkefølge, så landet med højest gennemsnit er  øverst 

#Vi plotter: 
plotreal <- ggplot(gem_vækst, aes(x = Land, y = Gennemsnit)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Sverige ligger øverste, mens Italien ligger i bunden i realvæksten (Q1 2000-2025 Q3)",
       x = "Land",
       y = "Vækst (%)",
       caption = "Kilde: Vores egne beregninger på baggrund af Eurostat (tps00028)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 90, size = 12, face = "bold",
                                   color = "black", vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(size = 10, hjust = 0))

print(plotreal)
#######################Opgave 5.3##############################################

#Corona-krisen som outlier


RvækstCOVID <- paste(rep(2020:2022, each = 4), paste0("Q", 1:4), sep = " ")

Rvækst_COVID <- realdata_long %>% 
  filter(kvartaler %in% RvækstCOVID)

RvækstUden_COVID <- realdata_long %>% 
  filter(! kvartaler %in% RvækstCOVID)


#Gennemsnit COVID-19 realvækst

gem_vækst_COVID <- Rvækst_COVID %>%
  group_by(Land) %>%
  summarise(Gennemsnit = mean(Vækst, na.rm = TRUE)) %>% 
  arrange(desc(Gennemsnit))

#Der plottes COVID: 

plot_covid <- ggplot(gem_vækst_COVID, aes(x = Land, y = Gennemsnit))+
  geom_bar(stat = "identity", fill = "orange")+
  labs(title = "COVID realvækst (2020-2022)",
       subtitle = "Spainen var mest ramt under Corona årene",
       x = "Land",
       y = "Vækst (%)",
       caption = "Kilde: Vores egne beregninger på baggrund af Eurostat (tps00028)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 90, size = 12, face = "bold",
                                   color = "black", vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(size = 10, hjust = 0))
print(plot_covid)
#Realvækst uden COVID-19

gem_vækst_UDEN_COVID <- RvækstUden_COVID %>%
  group_by(Land) %>%
  summarise(Gennemsnit = mean(Vækst, na.rm = TRUE)) %>% 
  arrange(desc(Gennemsnit))

#Der plottes uden COVID-19

plot_uden_covid <-  ggplot(gem_vækst_UDEN_COVID, aes(x = Land, y = Gennemsnit))+
  geom_bar(stat = "identity", fill ="blue")+
  labs(title = "Italien har haft den laveste realvækst, når COVID-perioden udelades",
       x = "Land",
       y = "Vækst (%)",
       caption = "Kilde: Vores egne beregninger på baggrund af Eurostat (tps00028)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 90, size = 12, face = "bold",
                                   color = "black", vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(size = 10, hjust = 0))
print(plot_uden_covid)
plot_uden_covid
#Vi kigger på flere plots  (install.packages("gridExtra"))

install.packages("gridExtra")
library(gridExtra)

grid.arrange(plotreal, plot_uden_covid, ncol = 2)  # side om side


#Opgave 5.4

#Vi finder nu realvæksten fra 2020 til 2025

Rvækst_2020_2025 <- paste(rep(2020:2025, each = 4), paste0("Q", 1:4), sep = " ")

Realv_2020_2025 <- realdata_long %>% 
  filter(kvartaler %in% Rvækst_2020_2025)


#Gennemsnit fra 2000 til 2025

Gem_RV_2020_2025 <- Realv_2020_2025 %>%                                              
  group_by(Land) %>%                                                       
  summarise(Gennemsnit = mean(Vækst, na.rm = TRUE)) %>%  
  arrange(desc(Gennemsnit))     

#Der plottes: 

ggplot(Gem_RV_2020_2025, aes(x = Land, y = Gennemsnit))+
  geom_bar(stat = "identity", fill = "red")+
  labs(title = "Spanien havde den højeste realvækst, mens Østrig lå lavest i perioden 2020–2025",
       x = "Vækst (%)", 
       y = "Vækst (%)",
       caption = "Kilde: Vores egne beregninger på baggrund af Eurostat (tps00028)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 90, size = 12, face = "bold",
                                   color = "black", vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.caption = element_text(size = 10, hjust = 0))

