library("rvest")
library("stringr")
library("httr")
library("dplyr")

########################################
### Opgave 1.1 – Hente data fra Bilbasen
########################################

eurl <- "https://www.bilbasen.dk/brugt/bil/toyota/aygo?gear=automatic&includeengroscvr=true&includeleasing=false&sellertypes=dealer"
header <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36"

# Hent søgeresultatsiden
respo <- GET(eurl, add_headers("User-Agent" = header))
respo$status_code

rawcontent <- content(respo, as = "text", encoding = "UTF-8")
page <- read_html(rawcontent)

# Alle bil-annoncer (articles inde i srp_results)
itemlist <- page %>%
  html_elements("section[class^='srp_results'] article")

length(itemlist)

##########################
# CSS-tags til felter
Price_tag        <- "[class^='Listing_price']"
Makemodel_tag    <- "[class^='Listing_makeModel']"
detail_tag       <- "[class^='Listing_details']"
Properties_tag   <- "[class^='Listing_properties']"
location_tag     <- "[class^='Listing_location']"
Link_tag         <- "[class^='Listing_link']"

## Forhandler-tags (bruges først på selve annoncesiden)
Seller_name_tag    <- "div[aria-label='bil sælger']"
Seller_address_tag <- "[data-e2e='seller-address']"
Seller_cvr_tag     <- "[class^='bas-MuiSellerInfoComponent-cvr']"

##############
# Data fra søgeresultatsiden
biler_base <- tibble(
  link = itemlist %>%
    html_element(Link_tag) %>%
    html_attr("href"),
  
  makemodel = itemlist %>%
    html_element(Makemodel_tag) %>%
    html_text2(),
  
  price = itemlist %>%
    html_element(Price_tag) %>%
    html_text2(),
  
  details = itemlist %>%
    html_element(detail_tag) %>%
    html_text2(),
  
  properties = itemlist %>%
    html_element(Properties_tag) %>%
    html_text2(),
  
  location = itemlist %>%
    html_element(location_tag) %>%
    html_text2()
)

# Gør links fulde (hvis de er relative)
biler_base$link <- url_absolute(biler_base$link, eurl)

########################################
### Hent forhandler-info for hver bil
########################################

get_seller_info <- function(car_url) {
  res <- GET(car_url, add_headers("User-Agent" = header))
  pg  <- read_html(content(res, as = "text", encoding = "UTF-8"))
  
  seller_name <- pg %>%
    html_element(Seller_name_tag) %>%
    html_text2()
  
  seller_address <- pg %>%
    html_element(Seller_address_tag) %>%
    html_text2()
  
  seller_cvr <- pg %>%
    html_element(Seller_cvr_tag) %>%
    html_text2()
  
  tibble(
    seller_name    = seller_name,
    seller_address = seller_address,
    seller_cvr     = seller_cvr
  )
}

# Test på én bil (valgfrit)
# get_seller_info(biler_base$link[1])

seller_list <- vector("list", nrow(biler_base))
for (i in seq_len(nrow(biler_base))) {
  cat("Henter sælger for bil", i, "af", nrow(biler_base), "\n")
  seller_list[[i]] <- get_seller_info(biler_base$link[i])
}
sellers <- bind_rows(seller_list)

# Samlet datasæt fra opgave 1.1
Toyfin <- bind_cols(biler_base, sellers)


########################################
### Opgave 1.2 – Rensning af tekster mv.
########################################

rens_salgstekst <- function(x) {
  # 1) Vi erstatter newline-tegn (\r og \n) med punktum + mellemrum
  x <- gsub("[\r\n]+", ". ", x)
  
  # 2) Vi fjerner alle tegn, der IKKE er bogstaver, tal, komma, punktum eller mellemrum
  x <- gsub("[^[:alnum:],\\. ]", " ", x)
  
  # 3) Vi erstatter mange mellemrum med ét enkelt mellemrum
  x <- gsub(" +", " ", x)
  
  # 4) Vi fjerner mellemrum i starten og slutningen af teksten
  x <- gsub("^\\s+|\\s+$", "", x)
  
  x
}

Toyfin_renset <- Toyfin %>%
  # Rens alle relevante tekstfelter
  mutate(
    across(
      c(makemodel,
        details,
        properties,
        location,
        seller_name,
        seller_address),
      rens_salgstekst
    )
  ) %>%
  # Lav pris og CVR om til tal + udtræk bil-id fra link
  mutate(
    bil_id = str_extract(link, "[0-9]+$") %>%
      as.integer()
  )

Toyfin_renset
