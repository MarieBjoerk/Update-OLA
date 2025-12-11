###############################################################Opgave_3.1
library(httr)
library(jsonlite)

baseurl <- "https://opensky-network.org/api/states/all"
lamin <- 52.055129
lamax <- 56.196869
lomin <- -6.065140
lomax <- 4.305954

fullurl <- paste0(baseurl, "?lamin=", lamin, "&lomin=", lomin, "&lamax=", lamax, "&lomax=", lomax)

res <- httr::GET(fullurl)
rescontent <- httr::content(res, as="text")
resretval <- jsonlite::fromJSON(rescontent)
statedf <- as.data.frame(resretval$states)


colnames(statedf) <- c("icao24","callsign","origin_country","time_position","last_contact",
                       "longitude","latitude","baro_altitude","on_ground","velocity",
                       "heading","vertical_rate","sensors","geo_altitude","squawk",
                       "spi","position_source")

library(dplyr)

fly_pr_land <- statedf %>%
  group_by(origin_country) %>%
  summarise(antal = n()) %>%
  arrange(desc(antal))

library(ggplot2)

ggplot(fly_pr_land, aes(x = reorder(origin_country, antal), y = antal)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Der er flest fly fra UK over Nordsøen",
       x = "Land",
       caption = "Kilde:Opensky",
       y = "Antal fly") +
  theme_minimal()
#############################################################################Opgave_3.2
# Vælg ét fly fra din liste
icao <- statedf$icao24[3]
icao

# Hent track for flyet
baseurl <- "https://opensky-network.org/api/tracks/all"

clientId="oliverandersen-api-client"
clientSecret="GLaSqgfWobspT6uSxt6VRcGmuJd6xVsh"


# Request an access token/ kig på util.R

getToken <- function() {
  token=NULL
  response <- POST(
    url = "https://auth.opensky-network.org/auth/realms/opensky-network/protocol/openid-connect/token",
    body = list(
      grant_type = "client_credentials",
      client_id = clientId,
      client_secret = clientSecret
    ),
    encode = "form"
  )
  
  # Parse the JSON response
  response$status_code
  if (response$status_code == 200) {
    content <- content(response, as = "parsed", type = "application/json")
    token <- content$access_token
  }
  return(token)
}

token <- getToken()  # Hvis du har funktionen fra util.R – ellers kan du droppe Authorization

turl <- paste0(baseurl, "?icao24=", icao, "&time=0")

res <- httr::GET(turl)
track_raw <- httr::content(res, as = "text")
track_json <- jsonlite::fromJSON(track_raw)
trackdf <- as.data.frame(track_json)

# Her laves kolonnenavnene om
colnames(trackdf) <- c("icao24", "callsign", "startTime", "endTime", 
                       "time", "lat", "lng", "alt", "crs", "grd")

# Hent et cirklende fly fra .rds filer det bliver circjet7.rds
circdf <- readRDS("circjet7.rds")

#Plot begge fly
library(leaflet)

leaflet() %>% 
  addTiles() %>% 
  addPolylines(data = trackdf, lng = ~lng, lat = ~lat, color = "blue", weight = 5, label = "Normalt fly") %>%
  addPolylines(data = circdf, lng = ~lng, lat = ~lat, color = "red", weight = 2, label = "Cirklende fly")
##################################################################################opgave 3.3
library(dplyr)

# Sørg for at statedf har de rigtige kolonner:
colnames(statedf) <- c("icao24","callsign","origin_country","time_position","last_contact",
                       "longitude","latitude","baro_altitude","on_ground","velocity",
                       "heading","vertical_rate","sensors","geo_altitude","squawk",
                       "spi","position_source")

# Fjern rækker uden ICAO-kode
statedf <- statedf %>%
  filter(!is.na(icao24), icao24 != "")

# Unikke ICAO-koder
friske_icao <- unique(statedf$icao24)

# Tag de første 10 til test
friske_icao <- friske_icao[1:10]

friske_icao   # Tjek: skal ligne "407e67" osv.

#start 
hent_track_metrics <- function(icao) 
  baseurl <- "https://opensky-network.org/api/tracks/all"
  
  # Brug et tidspunkt tæt på nu (fx nu minus 5 min)
  t_now <- as.integer(Sys.time())
  turl <- paste0(baseurl, "?icao24=", icao, "&time=", t_now - 300)
  
  # hvis I har login til OpenSky, så skal der authenticate på:
  # res <- httr::GET(turl, httr::authenticate("brugernavn", "password"))
  res <- httr::GET(turl)
  
  raw <- httr::content(res, as = "text", encoding = "UTF-8")
  
  # Fang JSON-fejl
  js  <- tryCatch(
    jsonlite::fromJSON(raw),
    error = function(e) return(NULL)
  )
  
  # Hvis der ikke er gyldige tracks → returnér NA’er
  if (is.null(js) || is.null(js$path) || length(js$path) == 0) {
    return(data.frame(
      icao24 = icao,
      crs_sd = NA_real_,
      r2     = NA_real_
    ))
  }
  
  df <- as.data.frame(js$path)
  names(df) <- c("time","lat","lng","alt","crs","grd")
  
  # Standardafvigelse af kurs
  crs_sd <- sd(df$crs, na.rm = TRUE)
  
  # Regression (lat ~ lng)
  model <- tryCatch(
    lm(lat ~ lng, data = df),
    error = function(e) NULL
  )
  
  if (is.null(model)) {
    r2 <- NA_real_
  } else {
    r2 <- summary(model)$r.squared
  }
  
  data.frame(
    icao24 = icao,
    crs_sd = crs_sd,
    r2     = r2
  )

resultater_frisk <- do.call(rbind, lapply(friske_icao, hent_track_metrics))
###############
library(httr)
library(jsonlite)
library(dplyr)

############################
# 1. Læs træningsfly (circjet*.rds)
############################

df1 <- readRDS("circjet.rds")
df2 <- readRDS("circjet2.rds")
df3 <- readRDS("circjet3.rds")
df4 <- readRDS("circjet4.rds")
df5 <- readRDS("circjet5.rds")
df6 <- readRDS("circjet6.rds")
df7 <- readRDS("circjet7.rds")

# Ens kolonnenavne
std_names <- c("icao24", "callsign", "startTime", "endTime", 
               "time", "lat", "lng", "alt", "crs", "grd")

colnames(df1) <- std_names
colnames(df2) <- std_names
colnames(df3) <- std_names
colnames(df4) <- std_names
colnames(df5) <- std_names
colnames(df6) <- std_names
colnames(df7) <- std_names


############################
# 2. Hjælpefunktioner (afstand + loiter index)
############################

# Haversine-afstand i meter
haversine_vec <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000  # jordens radius i meter
  to_rad <- function(x) x * pi / 180
  
  phi1 <- to_rad(lat1)
  phi2 <- to_rad(lat2)
  dphi <- to_rad(lat2 - lat1)
  dlambda <- to_rad(lon2 - lon1)
  
  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlambda / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c
}

# Loiter-index = total distance / lige-linje distance
compute_loiter_index <- function(df) {
  if (nrow(df) < 2) return(NA_real_)
  
  # total distance (sum af segmenter)
  lat1 <- head(df$lat, -1)
  lon1 <- head(df$lng, -1)
  lat2 <- tail(df$lat, -1)
  lon2 <- tail(df$lng, -1)
  
  total_dist <- sum(haversine_vec(lat1, lon1, lat2, lon2), na.rm = TRUE)
  
  # lige-linje distance mellem første og sidste punkt
  straight_dist <- haversine_vec(df$lat[1], df$lng[1],
                                 df$lat[nrow(df)], df$lng[nrow(df)])
  
  if (straight_dist == 0) return(NA_real_)
  
  total_dist / straight_dist
}


############################
# 3. Metrics for træningsfly (circjet*.rds)
############################

training_metrics2 <- function(df, navn) {
  crs_sd  <- sd(df$crs, na.rm = TRUE)
  r2      <- summary(lm(lat ~ lng, data = df))$r.squared
  loiter  <- compute_loiter_index(df)
  
  data.frame(
    icao24       = navn,
    crs_sd       = crs_sd,
    r2           = r2,
    loiter_index = loiter
  )
}

resultater_træning <- rbind(
  training_metrics2(df1, "circjet"),
  training_metrics2(df2, "circjet2"),
  training_metrics2(df3, "circjet3"),
  training_metrics2(df4, "circjet4"),
  training_metrics2(df5, "circjet5"),
  training_metrics2(df6, "circjet6"),
  training_metrics2(df7, "circjet7")
)

resultater_træning


############################
# 4. Metrics for friske fly (fra friske_icao)
#    (forudsætter at 'friske_icao' allerede er lavet ud fra statedf)
############################

hent_track_metrics_alg <- function(icao) {
  baseurl <- "https://opensky-network.org/api/tracks/all"
  t_now <- as.integer(Sys.time())
  turl <- paste0(baseurl, "?icao24=", icao, "&time=", t_now - 300)
  
  res <- httr::GET(turl)
  raw <- httr::content(res, as = "text", encoding = "UTF-8")
  
  js  <- tryCatch(
    jsonlite::fromJSON(raw),
    error = function(e) return(NULL)
  )
  
  if (is.null(js) || is.null(js$path) || length(js$path) == 0) {
    return(data.frame(
      icao24       = icao,
      crs_sd       = NA_real_,
      r2           = NA_real_,
      loiter_index = NA_real_
    ))
  }
  
  df <- as.data.frame(js$path)
  names(df) <- c("time","lat","lng","alt","crs","grd")
  
  crs_sd <- sd(df$crs, na.rm = TRUE)
  model  <- tryCatch(lm(lat ~ lng, data = df), error = function(e) NULL)
  r2     <- if (is.null(model)) NA_real_ else summary(model)$r.squared
  
  loiter <- compute_loiter_index(df)
  
  data.frame(
    icao24       = icao,
    crs_sd       = crs_sd,
    r2           = r2,
    loiter_index = loiter
  )
}

resultater_frisk <- do.call(rbind, lapply(friske_icao, hent_track_metrics_alg))
resultater_frisk


############################
# 5. Saml det hele og marker cirklende fly (isOff)
############################

alle_resultater <- bind_rows(
  resultater_træning,
  resultater_frisk
)

# sørg for numerisk loiter_index
alle_resultater$loiter_index <- as.numeric(alle_resultater$loiter_index)

# Regel: loiter_index > 1.1 → cirkler (isOff = 1)
alle_resultater$isOff <- ifelse(
  !is.na(alle_resultater$loiter_index) & alle_resultater$loiter_index > 1.1,
  1L,
  0L
)

alle_resultater

