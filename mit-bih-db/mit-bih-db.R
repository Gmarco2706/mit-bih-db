#caricamento dataset
df <- read.csv("/Users/marcogreco/Documents/VSC/SAD_dataset/mit-bih-db.csv", stringsAsFactors = FALSE)


#Dalla documentazione del dataset sono state scoperte diverse anomalie nei dati, per la maggior parte dei pazienti il lead superiore dell'ECG è il MLII fatta eccezione per i
# pazienti 102 e 104 in cui il valore di MLII va imputato con il valore della colonna V5 corrispondente al paziente, il lead inferiore per i pazienti rimane tipicamente il V1,
# per quanto riguarda invece il paziente 114 i valori del lead superiore (MLII) e quello inferiore (V1) sono stati invertiti.
# È stato annotato inoltre che i pazienti 102 104 107 e 217 sono pazienti con pacemaker. e che il paziente 106 ha ectopici prominenti mostrando battiti anomali



#Normalizzazione del dataset per i pazienti 102 e 104, come documentazione riporta come upper lead è stato usato V5 dato che sono mancanti di MLII quindi il valore V5 relativo ai pazienti
#verrà sostituito nella cella inerente a MLII


#Modifichiamo i record di 102 e 104 imputando i valori con V5->MLII
#carichiamo i pazienti 
p102_104<- c(102,104) 
df$MLII[df$Partecipante %in% p102_104]<-df$V5[df$Partecipante %in% p102_104]
df$V5[df$Partecipante %in% p102_104]<- NA

#Modifica del record 114, invertendo il valore dei segnali del lead inferiore e superiore
p114<- 114
MLII_temp <- df$MLII[df$Partecipante == p114]
df$MLII[df$Partecipante == p114] <- df$V5[df$Partecipante == p114]
df$V5[df$Partecipante == p114]   <- MLII_temp
rm(MLII_temp)


cat("modifiche apportate ai pazienti 102 104 e 114")

