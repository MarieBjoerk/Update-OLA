###### Data Retrieval ###########
# Indlæs pakker
############################################
library(dkstat)
library(ggplot2)

############################################
# 1) Hent FORV1 
############################################

forv1_filters <- list(
  INDIKATOR = "*",
  TID       = "*"   
)

forv1_long <- dst_get_data(
  table = "FORV1",
  query = forv1_filters,
  lang  = "da"
)

# forv1_long indeholder kolonnerne: INDIKATOR, TID og value

############################################
# 2) Omstrukturér data: tid lodret og indikatorer vandret
############################################

forv1_wide <- reshape(
  forv1_long,
  idvar     = "TID",        
  timevar   = "INDIKATOR",  
  direction = "wide"
)

# Fjern præfikset "value." i kolonnenavnene
names(forv1_wide) <- sub("^value\\.", "", names(forv1_wide))

# Udsnit med rækker fra linje 304 og nedefter
dataK4 <- forv1_wide[304:nrow(forv1_wide), ]

############################################
# 3) NKN1 – P.31 Privatforbrug,
#    2020-priser, kædede værdier, sæsonkorrigeret
############################################

nkn1_meta <- dst_meta(table = "NKN1", lang = "da")

nkn1_filters <- list(
  TRANSAKT  = "P.31 Privatforbrug",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  "SÆSON"   = "Sæsonkorrigeret",
  Tid       = "*"
)

nkn1_p31 <- dst_get_data(
  table = "NKN1",
  query = nkn1_filters,
  lang  = "da"
)

# Opret datasæt med kun tidsvariabel og værdier
PF2025 <- nkn1_p31[, c("TID", "value")]

# Behold rækker fra linje 37 og nedefter
PF2025 <- PF2025[37:nrow(PF2025), ]

##################### Før opgave start ###############################

# Udsnit med rækker fra linje 304 og fjerner de sidste 2 rækker
dataK3 <- forv1_wide[304:612, ]
DI <- dataK3

# Vektor 1: realvækst, for privatforbrug 
pfveakst <- diff(log(PF2025$value), lag = 4) * 100

# antal observationer og kvartal-akse (bare 1,2,3,...)
n <- length(pfveakst)
kvartaler <- seq_len(n)

# Data.frame af realvækst
pfdf <- as.data.frame(pfveakst)

# Gør alle datakolonner numeriske
DI1 <- DI[ , -1, drop = FALSE]
DI1[] <- lapply(DI1, function(x) as.numeric(gsub(",", ".", x)))

# Lav dataen kvartalsvis (gennemsnit af hver 3. måned)
fend <- data.frame(matrix(nrow = 0, ncol = ncol(DI1)))
names(fend) <- names(DI1)

for(i in seq(3, nrow(DI1), by = 3)){
  rows  <- (i-2):i
  means <- colMeans(DI1[rows, , drop = FALSE], na.rm = TRUE)
  fend  <- rbind(fend, as.data.frame.list(means))
}

# Isoler de forskellige spørgsmål
spg1  <- as.numeric(fend[,2])
spg2  <- as.numeric(fend[,3])
spg3  <- as.numeric(fend[,4])
spg4  <- as.numeric(fend[,5])
spg5  <- as.numeric(fend[,6])
spg6  <- -as.numeric(fend[,7])
spg7  <- as.numeric(fend[,8])
spg8  <- -as.numeric(fend[,9])
spg9  <- as.numeric(fend[,10])
spg10 <- as.numeric(fend[,11])
spg11 <- as.numeric(fend[,12])
spg12 <- as.numeric(fend[,13])

############################################
# 4) Opsætning til rank over tid
############################################

antal_spg <- 12

mat <- cbind(
  spg1, spg2, spg3, spg4, spg5, spg6,
  spg7, spg8, spg9, spg10, spg11, spg12
)
colnames(mat) <- paste0("spg", 1:12)

# Alle kombinationer som i opgave 1.1
kombi_spx <- unlist(
  lapply(1:antal_spg, function(k) combn(antal_spg, k, simplify = FALSE)),
  recursive = FALSE
)

window <- 25   # vindueslængde

# Definér de 5 indikatorer
DI_idx     <- c(1, 3, 5, 9)          # DI
DST_idx    <- c(1, 2, 3, 4, 5)       # DST/FTI
kombi6_idx <- c(1, 3, 5, 7, 9, 11)   # kombi 6
kombi5_idx <- c(3, 5, 7, 9, 11)      # kombi 5
kombi4_idx <- c(3, 5, 7, 11)         # kombi 4

set_DI     <- paste0("spg", DI_idx,     collapse = "+")
set_DST    <- paste0("spg", DST_idx,    collapse = "+")
set_kombi6 <- paste0("spg", kombi6_idx, collapse = "+")
set_kombi5 <- paste0("spg", kombi5_idx, collapse = "+")
set_kombi4 <- paste0("spg", kombi4_idx, collapse = "+")

# Funktion der laver rank-tabel for ét vindue (25 kvartaler)
lav_rank_for_window <- function(idx) {
  res_win <- do.call(rbind, lapply(kombi_spx, function(spx) {
    rm  <- rowMeans(mat[idx, spx, drop = FALSE])
    y   <- pfveakst[idx]
    fit <- lm(y ~ rm)
    r2  <- summary(fit)$r.squared
    kor <- cor(y, rm, use = "complete.obs")
    data.frame(
      k       = length(spx),
      set     = paste0("spg", spx, collapse = "+"),
      r2      = r2,
      kor     = kor,
      abs_kor = abs(kor),
      stringsAsFactors = FALSE
    )
  }))
  
  res_win <- res_win[order(-res_win$r2, -res_win$abs_kor, res_win$k), ]
  res_win$rank_r2 <- seq_len(nrow(res_win))
  res_win
}

# Helper til at hive rank ud for en bestemt indikator
get_rank_from_res <- function(res_win, set_name, label, tid) {
  r <- res_win$rank_r2[res_win$set == set_name]
  if (length(r) == 0) r <- NA
  data.frame(
    TID   = tid,
    model = label,
    rank  = r
  )
}

############################################
# 5) Scenario A: uden de sidste 25 kvartaler
############################################

ends_uden_last25 <- window:(n - 25)  # vinduer slutter senest i n-25

rows_A <- list()
for (t in ends_uden_last25) {
  idx   <- (t - window + 1):t
  res_t <- lav_rank_for_window(idx)
  tid_t <- kvartaler[t]
  
  rows_A[[length(rows_A) + 1]] <- get_rank_from_res(res_t, set_DI,     "DI",      tid_t)
  rows_A[[length(rows_A) + 1]] <- get_rank_from_res(res_t, set_DST,    "DST",     tid_t)
  rows_A[[length(rows_A) + 1]] <- get_rank_from_res(res_t, set_kombi6, "Kombi 6", tid_t)
  rows_A[[length(rows_A) + 1]] <- get_rank_from_res(res_t, set_kombi5, "Kombi 5", tid_t)
  rows_A[[length(rows_A) + 1]] <- get_rank_from_res(res_t, set_kombi4, "Kombi 4", tid_t)
}

ranks_uden_last25 <- do.call(rbind, rows_A)

############################################
# 6) Scenario B: uden de første 25 kvartaler
############################################

starts_uden_first25 <- 26:(n - window + 1)

rows_B <- list()
for (start in starts_uden_first25) {
  idx   <- start:(start + window - 1)
  end_t <- start + window - 1
  res_t <- lav_rank_for_window(idx)
  tid_t <- kvartaler[end_t]
  
  rows_B[[length(rows_B) + 1]] <- get_rank_from_res(res_t, set_DI,     "DI",      tid_t)
  rows_B[[length(rows_B) + 1]] <- get_rank_from_res(res_t, set_DST,    "DST",     tid_t)
  rows_B[[length(rows_B) + 1]] <- get_rank_from_res(res_t, set_kombi6, "Kombi 6", tid_t)
  rows_B[[length(rows_B) + 1]] <- get_rank_from_res(res_t, set_kombi5, "Kombi 5", tid_t)
  rows_B[[length(rows_B) + 1]] <- get_rank_from_res(res_t, set_kombi4, "Kombi 4", tid_t)
}

ranks_uden_first25 <- do.call(rbind, rows_B)

############################################
# 7) Grafer
############################################

# Graf 1: rank over tid – uden de sidste 25 kvartaler
ggplot(ranks_uden_last25, aes(x = TID, y = rank, color = model)) +
  geom_line(linewidth = 1) +
  scale_y_reverse() +  # rank 1 øverst
  labs(
    title = "Kombi 4, kombi 5 og kombi 6 følger hinanden tæt, DST ligger langt fra både DI og Kombinationerne",
    subtitle = "Rank r² over tid- uden de sidste 25 kvartaler", 
    caption = "Danmarks Statitstik og egne beregninger",
    x     = "Kvartal (indeks)",
    y     = "Rank (lavere er bedre)",
    color = "Indikator"
  ) +
  theme_minimal()

# Graf 2: rank over tid – uden de første 25 kvartaler
ggplot(ranks_uden_first25, aes(x = TID, y = rank, color = model)) +
  geom_line(linewidth = 1) +
  scale_y_reverse() +
  labs(
    title = "Rank r², DST følger hverken DI eller vores kombinationer ",
    subtitle = "r² over tid – uden de første 25 kvartaler",
    caption = "Danmarks Statistik og egne beregninger",
    x     = "Kvartal (indeks)",
    y     = "Rank (lavere er bedre)",
    color = "Indikator"
  ) +
  theme_minimal()
#######################Ekstra plots###########
############################################
# 8) Samlet rank uden første/sidste 25 kvartaler
############################################

# Indeks til hele perioden uden sidste 25 og uden første 25
idx_uden_last25  <- 1:(n - 25)
idx_uden_first25 <- 26:n

# Brug den samme funktion som før (virker også uden fast vindueslængde)
res_uden_last25  <- lav_rank_for_window(idx_uden_last25)
res_uden_first25 <- lav_rank_for_window(idx_uden_first25)

# Helper: hent én samlet rank for en given indikator
get_rank_single <- function(res_tab, set_name, label) {
  r <- res_tab$rank_r2[res_tab$set == set_name]
  if (length(r) == 0) r <- NA
  data.frame(
    model = label,
    rank  = r
  )
}

# Samlet rank når de SIDSTE 25 kvartaler er fjernet
samlet_uden_last25 <- rbind(
  get_rank_single(res_uden_last25, set_DI,     "DI"),
  get_rank_single(res_uden_last25, set_DST,    "DST"),
  get_rank_single(res_uden_last25, set_kombi6, "Kombi 6"),
  get_rank_single(res_uden_last25, set_kombi5, "Kombi 5"),
  get_rank_single(res_uden_last25, set_kombi4, "Kombi 4")
)

# Samlet rank når de FØRSTE 25 kvartaler er fjernet
samlet_uden_first25 <- rbind(
  get_rank_single(res_uden_first25, set_DI,     "DI"),
  get_rank_single(res_uden_first25, set_DST,    "DST"),
  get_rank_single(res_uden_first25, set_kombi6, "Kombi 6"),
  get_rank_single(res_uden_first25, set_kombi5, "Kombi 5"),
  get_rank_single(res_uden_first25, set_kombi4, "Kombi 4")
)

############################################
# Plot 1: samlet rank – uden de sidste 25 kvartaler
ggplot(samlet_uden_last25, aes(x = model, y = rank, fill = model)) +
  geom_col() +
  geom_text(
    aes(x = model, y = 0, label = rank),  # fast y = 0 = øverst
    vjust = -0.5
  ) +
  scale_y_reverse() +  # rank 1 øverst
  scale_fill_manual(
    values = c(
      "DI"      = "#1b9e77",
      "DST"     = "#d95f02",
      "Kombi 4" = "#66a61e",
      "Kombi 5" = "#e7298a",
      "Kombi 6" = "#7570b3"
    ),
    name = "Indikator"
  ) +
  labs(
    title = "Kombi 6 ligger på en 1 plads, efterfulgt af DI  og kombi 5",
    subtitle = "Samlet rank r², uden de SIDSTE 25 kvartaler",
    x     = "Indikator",
    y     = "Rank (lavere er bedre)"
  ) +
  theme_minimal()

# Plot 2: samlet rank – uden de første 25 kvartaler
ggplot(samlet_uden_first25, aes(x = model, y = rank, fill = model)) +
  geom_col() +
  geom_text(
    aes(x = model, y = 0, label = rank),  # fast y = 0 = øverst
    vjust = -0.5
  ) +
  scale_y_reverse() +
  scale_fill_manual(
    values = c(
      "DI"      = "#1b9e77",
      "DST"     = "#d95f02",
      "Kombi 4" = "#66a61e",
      "Kombi 5" = "#e7298a",
      "Kombi 6" = "#7570b3"
    ),
    name = "Indikator"
  ) +
  labs(
    title = "Kombi 4 ligger på 1 plads, efterfulgt af kombi 5 og 6",
    subtitle ="Samlet rank (r²) – uden de FØRSTE 25 kvartaler",
    caption = "Danmarks Statistik og egne beregninger",
    x     = "Indikator",
    y     = "Rank (lavere er bedre)"
  ) +
  theme_minimal()
