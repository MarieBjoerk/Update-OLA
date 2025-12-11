##### FOODWAST opgaven 

install.packages("httr")
install.packages("jsonlite")
library(httr)
library(jsonlite)

################################################################################

api_key <- "SG_APIM_0JQAMC41TBEVB5D2Y4AVVM100K1HCG5Z4X26RPGA4XZ0B3F100N0"
oest_url <- "https://api.sallinggroup.com/v1/food-waste/?zip=2100"


###################### Hent butikker i et postnummer ###########################

resp_oest <- GET(oest_url, add_headers(Authorization = paste("Bearer", api_key)))


print(status_code(resp_oest))


data_oest <-  fromJSON(flatten = TRUE,content(resp_oest, "text", encoding = "UTF-8"))

str(data_oest)

df_oesterbro <- as.data.frame(data_oest)

str(df_oesterbro)

########## Netto ved blegdamsvej 

net_bleg <- subset(df_oesterbro,store.id == "c41aaac9-b98d-4eee-a898-83226db622ff")

net_bleg_rens <- net_bleg$clearances[[1]]
net_bleg_rens$store.id = "c41aaac9-b98d-4eee-a898-83226db622ff"
net_bleg_rens$store.name = "Netto Blegdamsvej 124"



################################################################################

api_key <- "SG_APIM_0JQAMC41TBEVB5D2Y4AVVM100K1HCG5Z4X26RPGA4XZ0B3F100N0"
fred_url <- "https://api.sallinggroup.com/v1/food-waste/?zip=2000"


###################### Hent butikker i et postnummer ###########################

resp_fred <- GET(fred_url, add_headers(Authorization = paste("Bearer", api_key)))


print(status_code(resp_fred))


data_fred <-  fromJSON(flatten = TRUE,content(resp_fred, "text", encoding = "UTF-8"))

str(data_fred)

df_frederiksberg <- as.data.frame(data_fred)

str(df_frederiksberg)

########## Føtex ved ndr fasanvej

ftx_ndrf <- subset(df_frederiksberg, store.id == "de063c86-ef9b-4ab0-a855-9cee522e02ba")

ftx_ndr_rens <- ftx_ndrf$clearances[[1]]
ftx_ndr_rens$store.id = "de063c86-ef9b-4ab0-a855-9cee522e02ba"
ftx_ndr_rens <- ftx_ndr_rens[ftx_ndr_rens$offer.stockUnit != "kg", ]
ftx_ndr_rens$store.name = "føtex Nordre Fasanvej"

################################################################################

api_key <- "SG_APIM_0JQAMC41TBEVB5D2Y4AVVM100K1HCG5Z4X26RPGA4XZ0B3F100N0"
ball_url <- "https://api.sallinggroup.com/v1/food-waste/?zip=2750"


###################### Hent butikker i et postnummer ###########################

resp_ball <- GET(ball_url, add_headers(Authorization = paste("Bearer", api_key)))


print(status_code(resp_ball))


data_ball <-  fromJSON(flatten = TRUE,content(resp_ball, "text", encoding = "UTF-8"))

str(data_ball)

df_ballerup <- as.data.frame(data_ball)

str(df_ballerup)

########## Føtex ved Baltropvej
ftx_ball <- subset(df_ballerup, store.id == "c891c2a5-e302-4204-a7f5-8132ff5f5856")

ftx_ball_rens <- ftx_ball$clearances[[1]]
ftx_ball_rens$store.id = "c891c2a5-e302-4204-a7f5-8132ff5f5856"
ftx_ball_rens <- ftx_ball_rens[ftx_ball_rens$offer.stockUnit != "kg", ]
ftx_ball_rens$store.name = "føtex Ballerup"

##################################################################################


api_key <- "SG_APIM_0JQAMC41TBEVB5D2Y4AVVM100K1HCG5Z4X26RPGA4XZ0B3F100N0"
syd_url <- "https://api.sallinggroup.com/v1/food-waste/?zip=2450"


###################### Hent butikker i et postnummer ###########################

resp_syd <- GET(syd_url, add_headers(Authorization = paste("Bearer", api_key)))


print(status_code(resp_syd))


data_syd <- fromJSON(flatten = TRUE,content(resp_syd, "text", encoding = "UTF-8"))

str(data_syd)

df_sydhavn <- as.data.frame(data_syd)

str(df_sydhavn)

########## Netto ved Vestre Teglgade 

net_vest <- subset(df_sydhavn, store.id == "550c219f-55ff-4d6c-b662-24a2300d1908")

net_vest_rens <- net_vest$clearances[[1]]
net_vest_rens$store.id = "550c219f-55ff-4d6c-b662-24a2300d1908"
net_vest_rens$store.name = "Netto Vestre Teglgade"



###############################################################################

alle_butikker <- rbind(ftx_ball_rens,ftx_ndr_rens,net_bleg_rens,net_vest_rens)

store_id_zip <- rbind(df_ballerup,df_frederiksberg,df_oesterbro,df_sydhavn)