library(danstat)
library(tidyverse)

# Hent metadata og data
meta <- get_table_metadata(table_id = "POSTNR2", variables_only = TRUE)

variliste <- list(
  list(code = "PNR20", values = NA),
  list(code = "Tid", values = "2024")
)

postdata <- get_data(table_id = "POSTNR2", variables = variliste, language = "da")

# Fjern postnumre med under 200 indbyggere
post_filter <- postdata %>% filter(INDHOLD >= 200)


# Split område og by ud
post_split <- separate(post_filter, col = PNR20, into = c("område", "efter"), sep = " - ", extra = "merge")
post_final <- separate(post_split, col = efter, into = c("postnr", "by"), sep = " ", extra = "merge")

# Konverter postnr til numerisk
post_final$postnr <- as.numeric(post_final$postnr)

post_final_unique <- post_final %>%
  group_by(postnr) %>%
  slice_max(order_by = INDHOLD, n = 1) %>%
  ungroup()



# Konverter postnr i boligdata
boligcl2$mzip <- as.numeric(boligcl2$mzip)

# Merge med renset postnummerdata
post_merge <- merge(boligcl2, post_final_unique, by.x = "mzip", by.y = "postnr", all.x = TRUE)

# Fjern rækker med NA (postnumre under 200 indbyggere)
post_mfinal <- na.omit(post_merge)


bycat <- c("Landsby" = 200, "Lille by" = 1000, "Almindelig by" = 2500, "Større by" = 10000, "Storby" = 50000)
grænser <- unique(c(bycat, Inf))
labels1 <- names(bycat)

post_mfinal$bystørrelse <- cut(post_mfinal$INDHOLD,
                               breaks = grænser,
                               labels = labels1,
                               right = TRUE,
                               include.lowest = TRUE)




post_mfinal$pris <- as.numeric(gsub("\\.", "", gsub(" kr", "", post_mfinal$pris)))
post_mfinal$kvm2 <- as.numeric(post_mfinal$kvm2)
post_mfinal$kvmpris <- post_mfinal$pris / post_mfinal$kvm2

post_mfinal_unique <- post_mfinal %>%
  distinct(vejname, kvm2, pris, .keep_all = TRUE)


bydf <- post_mfinal_unique[, c("by", "pris", "kvmpris", "bystørrelse")]

#Der plottes: 

ggplot(bydf, aes(x = bystørrelse, y = kvmpris, fill = bystørrelse)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(
    x = "Bystørrelse",
    y = "kr/m2",
    title = "Kvadratmeterpris efter bytype"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 12, face = "bold", color = "black", vjust = 0.5, hjust = 1)
  )


        