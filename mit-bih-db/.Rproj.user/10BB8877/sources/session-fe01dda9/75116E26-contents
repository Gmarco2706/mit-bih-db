#caricamento dataset
df <- read.csv("/Users/marcogreco/Documents/VSC/SAD_dataset/mit-bih-db.csv", stringsAsFactors = FALSE)
library(dplyr)   
#funzione per cercare celle vuote, valori Na/Nan e 0 per ogni colonna
check_valori<-function(data) {
  sapply(data, function(col){
    zero <- sum(col == 0, na.rm = TRUE)
    Stringhe_vuote<-sum(col== "", na.rm = TRUE)
    valori_na<- sum(is.na(col))
    valori_nan<-sum(is.nan(as.numeric(col)))
    c(valori_zero = zero, na = valori_na, nan = valori_nan, vuote = Stringhe_vuote)
  })
}

risultati <- check_valori(df)
print(t(risultati))



pazienti_na <- sort(unique(pull(filter(df, is.na(MLII)), Partecipante)))

na_per_paziente <- data.frame(
  Partecipante = character(),
  total_samples = numeric(),
  na_MLII_count = numeric(),
  na_MLII_percent = numeric(),
  stringsAsFactors = FALSE
)

for (paz in pazienti_na) {
  subset_paz <- df[df$Partecipante == paz, ]
  total <- nrow(subset_paz)
  na_count <- sum(is.na(subset_paz$MLII))
  na_perc <- round(na_count / total * 100, 2)
  
  na_per_paziente <- rbind(na_per_paziente, data.frame(
    Partecipante = paz,
    total_samples = total,
    na_MLII_count = na_count,
    na_MLII_percent = na_perc,
    stringsAsFactors = FALSE
  ))
}


cat("pazienti unici na", length(pazienti_na))
print((pazienti_na))

cat("Conteggio NA V1 solo per pazienti con NA:\n")
print(na_per_paziente)


#conteggio V1
# Id dei pazienti che hanno almeno un NA in V1
pazienti_na_V1 <- sort(unique(pull(filter(df, is.na(V1)), Partecipante)))

# Data frame riassuntivo per V1
na_per_paziente_V1 <- data.frame(
  Partecipante = character(),
  total_samples = numeric(),
  na_V1_count = numeric(),
  na_V1_percent = numeric(),
  stringsAsFactors = FALSE
)

for (paz in pazienti_na_V1) {
  subset_paz <- df[df$Partecipante == paz, ]
  total <- nrow(subset_paz)
  na_count <- sum(is.na(subset_paz$V1))
  na_perc <- round(na_count / total * 100, 2)
  
  na_per_paziente_V1 <- rbind(na_per_paziente_V1, data.frame(
    Partecipante = paz,
    total_samples = total,
    na_V1_count = na_count,
    na_V1_percent = na_perc,
    stringsAsFactors = FALSE
  ))
}

cat("Pazienti con almeno un NA in V1:", length(pazienti_na_V1), "\n")
print(pazienti_na_V1)

cat("Conteggio NA V1 per paziente (e percentuale):\n")
print(na_per_paziente_V1)
