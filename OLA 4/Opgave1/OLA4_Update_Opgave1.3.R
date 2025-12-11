########################################
### Opgave 1.3 – Hente nye data (simuleret)
### Udgangspunkt: Toyfin_renset (Toyota Aygo)
########################################

# Vi tager udgangspunkt i datasættet fra 1.2 og kalder første kørsel dag1
dag1 <- Toyfin_renset

# Vi har valgt at lave price om til et numerisk felt (fjerner " kr." osv.)
dag1$price <- gsub("[^0-9]", "", dag1$price)  
dag1$price <- as.integer(dag1$price)


# Vi har valgt at sikre, at der findes en scrapedate på første kørsel
if (!"scrapedate" %in% names(dag1)) {
  dag1$scrapedate <- Sys.Date() 
}

# Vi gemmer dag1 som RDS-fil i den nuværende mappe
saveRDS(dag1, file = "dag1.rds")

# Vi gemmer dag1 som CSV-fil
write.csv(dag1, file = "dag1.csv", row.names = FALSE)

# Vi sætter en seed, så det tilfældige valg kan genskabes
set.seed(1811)

# Vi har valgt at fjerne 5 biler, som vi simulerer er solgt i dag2
sold_ids <- sample(dag1$bil_id, 5)

# Vi laver dag2 ved at fjerne de solgte biler
dag2 <- dag1[!(dag1$bil_id %in% sold_ids), ]

# Vi har valgt at ændre prisen på 3 biler i dag2 (her sat til -5 %)
change_ids <- sample(dag2$bil_id, 3)
dag2$price[dag2$bil_id %in% change_ids] <-
  round(dag2$price[dag2$bil_id %in% change_ids] * 0.95)

# Vi finder det højeste bil_id, så nye biler får unikke id'er
max_id <- max(dag1$bil_id, na.rm = TRUE)

# Vi har valgt at tilføje 2 nye Toyota Aygo-biler til dag2
nye_biler <- data.frame(
  link       = c(
    "https://www.bilbasen.dk/brugt/bil/toyota/aygo/10-vvt-i-x-play-5d/9999999",
    "https://www.bilbasen.dk/brugt/bil/toyota/aygo/10-vvt-i-x-press-5d/8888888"
  ),
  makemodel  = c(
    "Toyota Aygo 1,0 VVT-i x-play 5d",
    "Toyota Aygo 1,0 VVT-i x-press 5d"
  ),
  price      = c(114900L, 94900L),
  details    = c(
    "5 2022. 25.000 km. 23,5 km l. automatgear, 5 døre",
    "1 2025. 3.000 km. 25 km l. automatgear, 5 døre"
  ),
  properties = c("SIMULERET udstyrspakke", "SIMULERET udstyrspakke"),
  location   = c("Nørreport, København", "Tivolifrihden, Aarhus"),
  seller_name    = c("Karsten biler", "Jeppes Biler"),
  seller_address = c("SQLvej 1, 1000 København K",
                     "Workbenchvej 2, 8000 Aarhus C"),
  seller_cvr     = c(12345678L, 87654321L),
  bil_id         = c(max_id + 1L, max_id + 2L),
  scrapedate     = dag1$scrapedate[1] + 1  # dag1 + 1 dag
)

# Vi lægger de nye biler oven i datasættet for dag2 (base R, ingen dplyr)
dag2 <- rbind(dag2, nye_biler)

# Vi har valgt at sætte scrapedate for hele dag2 til scrapedate for dag1 + 1 dag
dag2$scrapedate <- dag1$scrapedate[1] + 1

# Vi gemmer dag2 som RDS og CSV
saveRDS(dag2, file = "dag2.rds")
write.csv(dag2, file = "dag2.csv", row.names = FALSE)

# Lille kontrol
nrow(dag1)    
nrow(dag2)        
length(sold_ids)  
length(change_ids)
