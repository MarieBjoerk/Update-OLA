##### OLA 3: Opgave 5 ######
# Opg 5.1: det første skridt #

library(jsonlite)
library(dplyr)
library(ggplot2)


################## Opgave 5.2 ###############

#### Api nøgle og base-URL

api_key <- "5be2c143-7158-4086-bed1-42c92a9be002"
obs_url <-  "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?parameterId=pressure_at_sea"


### 5.2.1 #############
#NAVNET PÅ HEADER: "X-Gravitee-Api-Key: 5be2c143-7158-4086-bed1-42c92a9be002"

## Funktion til API-kald

response <- GET(
  obs_url, 
  add_headers("X-Gravitee-Api-Key" = api_key)
)

#Status kode:

response$status_code

#Her henter vi indholdet fra API-serveren (response) og gemmer det som ren tekst 

data_raw <- content(response, as = "text", endcoding = "UTF-8")

# Her konveterer vi en JSON-teksten til R-struktur 

dobs <- fromJSON(data_raw, flatten = TRUE)
dobs

############################## nu skal vi finde "visiblity" #########################

vis_url <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?parameterId=visibility"
vis <- GET( 
  vis_url, 
  add_headers("X-Gravitee-Api-Key" = api_key)
  )
vis$status_code

vis_b <- content(vis, as = "text")
vis_b <- fromJSON(vis_b, flatten= TRUE)
names(vis_b)

#Dataframe over visibility: 
visdf <-as.data.frame(vis_b$features)
visdf

################################ Nu skal vi finde stationen: 05272 ########################

#stationId = 05272

sta_url <-"https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?stationId=05272"
station <- GET( 
  sta_url, 
  add_headers("X-Gravitee-Api-Key" = api_key)
)
station$status_code

stat_b <- content(station, as = "text")
stat_b <- fromJSON(stat_b, flatten= TRUE)
names(stat_b)

#Dataframe over visibility: 
statdf <-as.data.frame(stat_b$features)
statdf


#####Endpont til stationsoversigt 
url1 <- paste0("https://dmigw.govcloud.dk/v2/metObs/collections/station/items?api-key=", api_key)

#Hent Data:
response1 <- GET(url1)
stations <- fromJSON(content(response1, "text", encoding = "UTF-8"), flatten = TRUE)

str(stations)

#Se alle stationnavne 
str(stations$features)


idx <- grep("Assens", stations$features$properties.name, ignore.case = TRUE)  
stations$features$properties.name[idx]  


lillebælt_navn <- stations$features$properties.name[idx]
lillebælt_id <- stations$features$properties.stationId[idx]

lillebælt <- as.data.frame(lillebælt_id, lillebælt_navn)



########################### Lav en graf over stormen i 2023 #####################

##################### ANHOLT ########################
### wind_speed: 
anholt_speed_url <-paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?stationId=06079&datetime=2023-10-20T00:00:00Z/2023-10-21T23:59:59Z&parameterId=wind_speed")

res_anspeed <- GET(
  anholt_speed_url, add_headers("X-Gravitee-Api-Key" = api_key))

res_anspeed$status_code


anholt_raw_speed <- content(res_anspeed, as = "text", encoding = "UTF-8")
anholt__data_speed <- fromJSON(anholt_raw_speed, flatten = TRUE)

anholt_df_speed <-as.data.frame(anholt__data_speed$features)
anholt_df_speed

#### Wind_dir


anholt_dir_url <-paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?stationId=06079&datetime=2023-10-20T00:00:00Z/2023-10-21T23:59:59Z&parameterId=wind_dir")

res_andir <- GET(
  anholt_dir_url, add_headers("X-Gravitee-Api-Key" = api_key))

res_andir$status_code

anholt_raw_dir <- content(res_andir, as = "text", encoding = "UTF-8")
anholt_data_dir <- fromJSON(anholt_raw_dir, flatten = TRUE)

anholt_df_dir <-as.data.frame(anholt_data_dir$features)
anholt_df_dir

############################## AARHUS ###############################

### wind_speed: 

aarhus_speed_url <-paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?stationId=06070&datetime=2023-10-20T00:00:00Z/2023-10-21T23:59:59Z&parameterId=wind_speed")

res_aaspeed <- GET(
  aarhus_speed_url, add_headers("X-Gravitee-Api-Key" = api_key))

res_anspeed$status_code


aarhus_raw_speed <- content(res_aaspeed, as = "text", encoding = "UTF-8")
aarhus__data_speed <- fromJSON(aarhus_raw_speed, flatten = TRUE)

aarhus_df_speed <-as.data.frame(aarhus__data_speed$features)
aarhus_df_speed

#### Wind_dir

aarhus_dir_url <-paste0("https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?stationId=06070&datetime=2023-10-20T00:00:00Z/2023-10-21T23:59:59Z&parameterId=wind_dir")

res_aadir <- GET(
  aarhus_dir_url, add_headers("X-Gravitee-Api-Key" = api_key))

res_aadir$status_code


aarhus_raw_dir <- content(res_aadir, as = "text", encoding = "UTF-8")
aarhus_data_dir <- fromJSON(aarhus_raw_dir, flatten = TRUE)

aarhus_df_dir <-as.data.frame(aarhus_data_dir$features)
aarhus_df_dir


###### Merge #########
anholt_samlet <- merge(anholt_df_dir, anholt_df_speed, by = "properties.observed")
aarhus_samlet <- merge(aarhus_df_dir, aarhus_df_speed, by = "properties.observed")


########################## Nu laver vi et datasæt med relevant data: 

anholt_speed <- anholt_samlet %>% 
  select("properties.stationId.x","properties.observed", "geometry.coordinates.x","properties.parameterId.y","properties.value.y")

colnv <- c("StationsId", "Observation", "Koordinater", "ParameterId.y", "Value.y")

colnames(anholt_speed) <- colnv

aarhus_speed <- aarhus_samlet %>% 
  select("properties.stationId.x","properties.observed", "geometry.coordinates.x","properties.parameterId.y","properties.value.y")

colnv <- c("StationsId", "Observation", "Koordinater", "ParameterId.y", "Value.y")

colnames(aarhus_speed) <- colnv


###### speed for aarhus og anholt 
speed_df <- rbind(aarhus_speed, anholt_speed)

##################### Der plottes vind hastighed  ######### 

speed_df$Observation <- as.POSIXct(speed_df$Observation, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

####Plot

ggplot(speed_df, aes(x = Observation, y = Value.y, color = StationsId)) +
  geom_line(size = 1) +
  labs(
    title = "Oktober 2023: stormy vejr over Anholt og Aarhus",
    subtitle = "Blå = Anholt  Rød = Aarhus",
    caption = "Vindretning: e(Øst)",
    x = "Observations tidspunkt",
    y = "Vindhastighed (m/s)",
    color = "Stations-ID"
  ) +
  scale_y_continuous(breaks = seq(0,19, by = 2))+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 17, face = "bold"),
    plot.caption = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold")
  )


############ Wind direction ##############

install.packages("metR")
library(metR)

anholt_wind <- anholt_samlet %>% 
  select("properties.stationId.x","properties.observed", "geometry.coordinates.x","properties.parameterId.x","properties.value.x")

colnv <- c("StationsId", "Observation", "Koordinater", "ParameterId.x", "Value.x")

colnames(anholt_wind) <- colnv

aarhus_wind <- aarhus_samlet %>% 
  select("properties.stationId.x","properties.observed", "geometry.coordinates.x","properties.parameterId.x","properties.value.x")

colnv <- c("StationsId", "Observation", "Koordinater", "ParameterId.x", "Value.x")

colnames(aarhus_wind) <- colnv

###### wind  for aarhus og anholt 
wind_df <- rbind(aarhus_wind, anholt_wind)


wind_df$Observation <- as.POSIXct(wind_df$Observation, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

######## Der plottes vind retning


# Antag at du har x = Observation, y = Value (vindhastighed), og wind_direction i grader

wind_df$dx <- cos(wind_df$Value.x * pi / 180)  # østlig komponent
wind_df$dy <- sin(wind_df$Value.x * pi / 180)  # nordlig komponent

ggplot(wind_df, aes(x = Observation, y = Value.x)) +
  geom_segment(aes(xend = Observation + dx, yend = Value.x + dy),
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  labs(title = "Vindretning", x = "Tidspunkt", y = "Østgående(67,5° og 112,5°)") +
  theme_minimal()




