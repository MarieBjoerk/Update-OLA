########################################
### Opgave 2.3 – Sammenlign dag1 og dag2
########################################
library(dplyr)

dag1 <- `dag1 (2)`
dag2 <- dag2
# a) Nyedag2# a) Nye records (biler der kun findes i dag2)
diffnye <- anti_join(dag2, dag1, by = "bil_id")

# b) Missing records / solgte biler (biler der kun findes i dag1)
diffgamle <- anti_join(dag1, dag2, by = "bil_id")

# c) Ændrede records (på prisen)
#    Samme bil_id i begge dage, men forskellig price
pris_sammen <- inner_join(
  dag1, dag2,
  by = "bil_id",
  suffix = c("_dag1", "_dag2")
)

diffpris <- pris_sammen %>%
  filter(price_dag1 != price_dag2) %>%
  select(bil_id, price_dag1, price_dag2, scrapedate_dag1, scrapedate_dag2)

