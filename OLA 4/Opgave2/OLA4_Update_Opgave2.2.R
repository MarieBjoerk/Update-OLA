
##### FRA R TIL SQL 
## Opgave 2.2 

aygo_df <- `dag1 (2)`
library(DBI)
library(RMariaDB)
con <- dbConnect(MariaDB(),
                 host="localhost",
                 db= "bilbasen", 
                 user= "root", 
                 password="Marie0907006212"
)

library(dplyr)

seller_df <- aygo_df %>% 
  distinct(seller_cvr, seller_address, seller_name)

car_df <- aygo_df %>% 
  distinct(bil_id, link, makemodel, properties, location, seller_cvr)

obs_df1 <- aygo_df %>% 
  distinct(scrapedate, bil_id, price)



##### Vi sender til SQL 
dbWriteTable(con, "Seller", seller_df, append= TRUE, row.names= FALSE)
dbWriteTable(con, "Car", car_df, append= TRUE, row.names= FALSE)
dbWriteTable(con, "Observation1", obs_df1, append=TRUE)



######## Opgave 2.3
aygo_df2 <- dag2

obs_df2 <- aygo_df2 %>% 
  distinct(scrapedate,bil_id, price)

dbWriteTable(con, "Observation1", obs_df2, append = TRUE)

