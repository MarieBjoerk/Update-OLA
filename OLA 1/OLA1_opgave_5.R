# opgave 1
Klasse <- rep(c("A", "B", "C", "D"), each = 9)
Uge <- rep(1:9, times = 4)

# Brug seq() og bland rækkefølgen tilfældigt
Score <- sample(seq(1, 36), 36, replace = FALSE)

df_5_1 <- data.frame(Klasse, Uge, Score)
df_5_1


# opgave 2
df_5_2 <- data.frame(Klasse = character(),
                     Uge = numeric(),
                     Score = numeric())

for (i in 1:nrow(df_5_1)) {
  
  # stop efter 27 rækker → 9 kvartaler
  if (i > 27) break 
  
  if (i %% 3 == 0) {
    mean_score <- mean(df_5_1$Score[(i-2):i])
    new_row <- data.frame(
      Klasse = df_5_1$Klasse[i],
      Uge = df_5_1$Uge[i],
      Score = mean_score
    )
    df_5_2 <- rbind(df_5_2, new_row)
  }
}

df_5_2
#opgave 3
library(tidyr)

df_5_wide <- df_5_2 %>%
  pivot_wider(
    names_from = Klasse,   # A, B, C, D bliver kolonnenavne
    values_from = Score    # værdierne som skal fylde cellerne
  )

df_5_wide

