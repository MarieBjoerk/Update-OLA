library(serial)
library(DBI)
library(RMariaDB)
library(stringr)
library(stringi)
library(logr)

##Opret seriel forbindelse til Arduino ----
  conarduino <- serialConnection(
    name     = "arduino",
    port     = "COM3",        # <-- RET hvis din port er en anden (fx "COM4")
    mode     = "9600,n,8,1",
    newline  = 1              # vi håndterer selv linjeskift med strsplit()
  )

open(conarduino)
close(conarduino)   # sørger for at porten lukkes når scriptet stopper

flush(conarduino)

##Dataframe til at gemme målinger ----
  df <- data.frame(
    person_id = numeric(0),
    distance  = numeric(0),
    timestamp = as.POSIXct(character(0))
  )





##While-loop der læser fra Arduino ----
  stoptime <- Sys.time() + 60   # kør i 60 sekunder (ret hvis du vil)


while (Sys.time() < stoptime) {
  
 # Læs hvad der lige nu ligger i bufferen fra Arduino
  line <- read.serialConnection(conarduino)
  cat("LINE:[", line, "]\n")   # debug: så du kan se, hvad der kommer
  
 # Nogle gange kommer der flere linjer på én gang, fx "1,80\n2,95\n3,110"
  lines_vec <- strsplit(line, "\n")[[1]]
  
  for (ln in lines_vec) {
    ln <- trimws(ln)
    if (nchar(ln) == 0) next      # spring tomme linjer over
    
   # Forventer formatet: person_id,distance
    parts <- strsplit(ln, ",")[[1]]
    if (length(parts) != 2) next  # ignorer alt der ikke matcher
    
    person_id <- suppressWarnings(as.numeric(parts[1]))
    dist      <- suppressWarnings(as.numeric(parts[2]))
    
    if (is.na(person_id) || is.na(dist)) next  # ignorer hvis noget ikke er tal
    
    df <- rbind(
      df,
      data.frame(
        person_id = person_id,
        distance  = dist,
        timestamp = Sys.time()
      )
    )
    
    print(df[nrow(df), ])        # vis seneste række i konsollen
  }
  
  Sys.sleep(0.1)   # lille pause, så vi ikke hammer CPU'en
}

##Kig på resultaterne ----
  View(df)
print(df)

####################################################### Vi gemmer data i en CSV fil 

#write.csv(df, "C:/Users/marol/Dataanalyse 1. sem/R/OLA/OLA 5/data_lego.csv", row.names = FALSE)

library(ggplot2)


# Lav søjlediagram

ggplot(data_lego, aes(x = timestamp, y = distance, fill = factor(person_id))) +
  geom_col() +
  labs(title = "Afstand over tid pr. person",
       x = "Tidspunkt", y = "Afstand") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "5 sec") +
  scale_fill_manual(values = c("red", "blue", "green", "orange", "purple", "black"))+
  plot.title = element_text(size = 20, face = "bold"),
axis.title = element_text(size = 16),
axis.text  = element_text(size = 14),
axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
legend.title = element_text(size = 14),
legend.text  = element_text(size = 12)
)


