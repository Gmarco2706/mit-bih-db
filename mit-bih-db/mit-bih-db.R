library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggrepel)

# ==============================================================================
# 1. CARICAMENTO E PULIZIA STRUTTURALE
# ==============================================================================

file_path <- "../mit-bih-db.csv"

if (!file.exists(file_path)) {
  stop("Errore: Il file non esiste. Controlla il percorso: ", file_path)
}

print("Caricamento dataset in corso...")
raw_data <- read_csv(file_path, show_col_types = FALSE) %>%
  select(-`EtÃ`) %>%
  rename(Sample = `#Sample`)

print("Applicazione correzioni (102, 104, 114) e riordinamento colonne...")

clean_data <- raw_data %>%
  # 1. Logica di correzione valori (Swap e Imputazione)
  mutate(
    MLII_new = case_when(
      Partecipante %in% c(102, 104) ~ V5,
      Partecipante == 114 ~ V5,
      TRUE ~ MLII
    ),
    V5_new = case_when(
      Partecipante %in% c(102, 104) ~ NA_real_,
      Partecipante == 114 ~ MLII,
      TRUE ~ V5
    )
  ) %>%
  
  # 2. Rimozione vecchie e rinomina nuove
  select(-MLII, -V5) %>%
  rename(MLII = MLII_new, V5 = V5_new) %>%
  
  # 3. RIORDINAMENTO COLONNE
  relocate(MLII, .before = 1) %>%   # Sposta MLII in prima posizione assoluta
  relocate(V5, .after = V4)         # Sposta V5 subito dopo V4

# Pulizia memoria
rm(raw_data)
gc()

# Verifica visiva dell'ordine
print("Prime righe e nomi colonne per verifica ordine:")
print(names(clean_data)) # Ti stampa l'elenco delle colonne nell'ordine nuovo

# ==============================================================================
# 2. FILTRAGGIO "WHITE LIST" (Rimozione Rumore e Non-Beat)
# ==============================================================================

print("Filtraggio delle annotazioni: mantenimento solo dei battiti reali...")

# Lista ufficiale dei simboli che corrispondono a un battito cardiaco fisico
battiti_reali <- c(
  "N",  # Normal beat
  "L",  # Left bundle branch block
  "R",  # Right bundle branch block
  "B",  # Bundle branch block unspecified
  "A",  # Atrial premature beat
  "a",  # Aberrated atrial premature beat
  "J",  # Nodal (junctional) premature beat
  "S",  # Supraventricular premature beat
  "V",  # Premature ventricular contraction
  "r",  # R-on-T premature ventricular contraction
  "F",  # Fusion of ventricular and normal beat
  "e",  # Atrial escape beat
  "j",  # Nodal (junctional) escape beat
  "n",  # Supraventricular escape beat
  "E",  # Ventricular escape beat
  "/",  # Paced beat
  "f",  # Fusion of paced and normal beat
  "Q",  # Unclassifiable beat
  "?",   # Beat not classified during learning
  "!"   # Fusion Beat
)

# Applichiamo il filtro: teniamo SOLO le righe dove la Label è nella lista
data_filtrati <- clean_data %>%
  filter(Label %in% battiti_reali)

table(data_filtrati$Label)

# Vediamo quante righe abbiamo eliminato
n_rimossi <- nrow(clean_data) - nrow(data_filtrati)
print(paste("Rimossi", n_rimossi, "record di annotazioni non-beat (rumore, commenti, ecc.)"))

# ==============================================================================
# 3. ANALISI DURATA BATTITI
# ==============================================================================

print("Calcolo durata dei battiti...")

timeline_tutti <- data_filtrati %>%
  group_by(Partecipante, Sample) %>%
  summarise(
    Lunghezza_Righe = n(),
    .groups = "drop"
  )

# Tabella riepilogativa per il Clustering
riepilogo_battiti <- timeline_tutti %>%
  group_by(Partecipante) %>%
  summarise(
    Totale_Battiti = n(),
    Durata_Media = round(mean(Lunghezza_Righe), 2),
    Dev_Std_Durata = round(sd(Lunghezza_Righe), 2), # Utile per vedere chi è aritmico
    Min_Durata = min(Lunghezza_Righe),
    Max_Durata = max(Lunghezza_Righe)
  ) %>%
  arrange(desc(Totale_Battiti))

print("Riepilogo statistiche per paziente (Pronto per Clustering):")
print(head(riepilogo_battiti, 10))

# ==============================================================================
# 4. VISUALIZZAZIONE (TIMELINE)
# ==============================================================================

print("Generazione grafico...")

ggplot(timeline_tutti, aes(x = Sample, y = Lunghezza_Righe)) +
  geom_point(alpha = 0.6, size = 0.8, color = "dodgerblue") +
  
  # Linea di riferimento (360 Hz = 1 secondo)
  geom_hline(yintercept = 360, color = "red", linetype = "dashed", alpha = 0.8) +
  
  facet_wrap(~Partecipante, scales = "free") + 
  
  # Zoomiamo per vedere il ritmo (tagliamo le pause > 800)
  coord_cartesian(ylim = c(0, 800)) +
  
  labs(
    title = "Analisi Temporale del Ritmo Cardiaco (Solo Battiti Reali)",
    subtitle = "Filtro 'White List' applicato. Asse Y = Durata in campioni (360 = 1 sec).",
    x = "Numero Progressivo Battito",
    y = "Durata Battito (Campioni)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 6)
  )

print("Elaborazione di tutti i pazienti...")

vettore_finale_tutti <- data_filtrati %>%
  # 1. NON filtriamo più un solo paziente. Li prendiamo tutti.
  
  # 2. Raggruppiamo per Paziente E per Sample.
  # Questo è fondamentale: tiene separati i dati di persone diverse.
  group_by(Partecipante, Sample) %>%
  
  # 3. Estraiamo la Label unica per quella posizione
  summarise(
    Label = first(Label),
    .groups = "drop"
  ) %>%
  
  # 4. Ordiniamo tutto per pulizia (prima per paziente, poi per tempo)
  arrange(Partecipante, Sample)

# ==============================================================================
# 4. VERIFICA
# ==============================================================================

# Mostra quante righe totali abbiamo (saranno milioni)
print(paste("Dimensione totale (righe):", nrow(vettore_finale_tutti)))

# Mostra le prime righe (vedrai il primo paziente)
print("--- Inizio (Paziente 100...) ---")
print(head(vettore_finale_tutti))

# Mostra le ultime righe (vedrai l'ultimo paziente)
print("--- Fine (Ultimo Paziente...) ---")
print(tail(vettore_finale_tutti))

# ==============================================================================
# 5. DEFINIZIONE DELLE FAMIGLIE VENTRICOLARI
# ==============================================================================

# Aggiungiamo un indice di riga per calcolare la distanza
dati_completi <- vettore_finale_tutti %>%
  arrange(Partecipante, Sample) %>%
  group_by(Partecipante) %>%
  mutate(
    Indice_Battito = row_number(),
    
    # DEFINIAMO CHI FA PARTE DEL GRUPPO DA ANALIZZARE
    # V = PVC, F = Fusion, E = Escape, r = R-on-T, ! = Flutter
    Famiglia_Ventricolare = Label %in% c("V", "F", "E", "r", "!")
  ) %>%
  ungroup()

# ==============================================================================
# 6. CALCOLO DISTANZE E CLASSIFICAZIONE
# ==============================================================================

analisi_periodica <- dati_completi %>%
  # Questo permette di vedere sequenze miste (es. V-F-V).
  filter(Famiglia_Ventricolare == TRUE) %>%
  
  group_by(Partecipante) %>%
  mutate(
    # Calcolo distanza basata sull'indice originale
    Distanza_V_Prev = Indice_Battito - lag(Indice_Battito, default = -999),
    Distanza_V_Next = lead(Indice_Battito, default = -999) - Indice_Battito
  ) %>%
  ungroup() %>%
  
  # 3. CLASSIFICAZIONE AVANZATA
  mutate(
    Classe_Pattern = case_when(
      # --- A. SEQUENZE PERICOLOSE (Vincono su tutto) ---
      # Run (3 o più consecutivi, Distanza = 1)
      (Distanza_V_Next == 1 & lead(Distanza_V_Next) == 1) | 
        (Distanza_V_Prev == 1 & Distanza_V_Next == 1)       | 
        (Distanza_V_Prev == 1 & lag(Distanza_V_Prev) == 1)  ~ "Run (V-Tach)",
      
      # Coppia (2 consecutivi, Distanza = 1)
      (Distanza_V_Next == 1) | (Distanza_V_Prev == 1)     ~ "Coppia (Couplet)",
      
      # --- B. PATTERN PERIODICI (Ritmo regolare) ---
      # Bigeminismo (Alternanza 1 a 1 -> Distanza = 2)
      (Distanza_V_Next == 2 & lead(Distanza_V_Next) == 2) | 
        (Distanza_V_Prev == 2 & Distanza_V_Next == 2)       | 
        (Distanza_V_Prev == 2 & lag(Distanza_V_Prev) == 2)  ~ "Bigeminismo (N-V-N-V)",
      
      # Trigeminismo (1 V ogni 2 N -> Distanza = 3)
      (Distanza_V_Next == 3 & lead(Distanza_V_Next) == 3) | 
        (Distanza_V_Prev == 3 & Distanza_V_Next == 3)       | 
        (Distanza_V_Prev == 3 & lag(Distanza_V_Prev) == 3)  ~ "Trigeminismo (N-N-V)",
      
      # --- C. ISOLATE DETTAGLIATE (Recuperiamo l'etichetta originale) ---
      Label == "V" ~ "Isolata (PVC Classica)",
      Label == "F" ~ "Isolata (Fusion Beat)",
      Label == "r" ~ "Isolata (R-on-T)",
      Label == "E" ~ "Isolata (Escape Beat)",
      Label == "!" ~ "Isolata (Flutter)",
      
      TRUE ~ "Altro Ventricolare"
    )
  )

# ==============================================================================
# 7. RISULTATI FINALI
# ==============================================================================

# 1. Conteggio (Percentuali relative ai soli eventi ventricolari)
print("--- DISTRIBUZIONE PATTERN ---")
conteggio <- analisi_periodica %>% 
  count(Classe_Pattern) %>% 
  mutate(Perc = round(n/sum(n)*100, 1)) %>% 
  arrange(desc(Perc))
print(conteggio)

# 2. Grafico per Paziente (Aggiornato con nuovi colori)
ggplot(analisi_periodica, aes(x = factor(Partecipante), fill = Classe_Pattern)) +
  geom_bar() +
  scale_fill_manual(values = c(
    # Isolate
    "Isolata (PVC Classica)" = "#3498db",  # Blu
    "Isolata (Fusion Beat)"  = "#9b59b6",  # Viola
    "Isolata (R-on-T)"       = "#8e44ad",  # Viola Scuro
    "Isolata (Escape Beat)"  = "#2980b9",  # Blu scuro
    "Isolata (Flutter)"      = "#2c3e50",  # Grigio scuro
    "Altro Ventricolare"     = "#95a5a6",
    
    # Periodiche
    "Bigeminismo (N-V-N-V)" = "#2ecc71",   # Verde
    "Trigeminismo (N-N-V)"  = "#1abc9c",   # Turchese
    
    # Sequenze
    "Coppia (Couplet)"      = "#f1c40f",   # Giallo
    "Run (V-Tach)"          = "#e74c3c"    # Rosso
  )) +
  labs(
    title = "Classificazione Completa PVC per Paziente",
    subtitle = "Dettaglio tipologia per battiti isolati (Fusion, R-on-T, ecc.)",
    x = "Paziente", y = "Conteggio Eventi"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8))

# ==============================================================================
# 8. GRAFICO PERCENTUALI GLOBALI (Risposta RQ1 Aggiornata)
# ==============================================================================

# 1. Calcoliamo le percentuali
df_percentuali <- analisi_periodica %>%
  count(Classe_Pattern) %>%
  mutate(
    Percentuale = round(n / sum(n) * 100, 1)
  ) %>%
  arrange(desc(Percentuale))

# 2. Generiamo il Barplot
ggplot(df_percentuali, aes(x = reorder(Classe_Pattern, -Percentuale), y = Percentuale, fill = Classe_Pattern)) +
  geom_bar(stat = "identity") +
  
  geom_text(aes(label = paste0(Percentuale, "%")), vjust = -0.5, size = 3.5, fontface = "bold") +
  
  # COLORI AGGIORNATI PER TUTTE LE CLASSI
  scale_fill_manual(values = c(
    "Isolata (PVC Classica)" = "#3498db",  
    "Isolata (Fusion Beat)"  = "#9b59b6",  
    "Isolata (R-on-T)"       = "#8e44ad",  
    "Isolata (Escape Beat)"  = "#2980b9",  
    "Isolata (Flutter)"      = "#2c3e50",
    "Altro Ventricolare"     = "#95a5a6",
    "Bigeminismo (N-V-N-V)" = "#2ecc71",   
    "Trigeminismo (N-N-V)"  = "#1abc9c",   
    "Coppia (Couplet)"      = "#f1c40f",   
    "Run (V-Tach)"          = "#e74c3c"    
  )) +
  
  labs(
    title = "Frequenza Globale dei Pattern PVC",
    subtitle = "Qual è la classe più comune (includendo sottoclassi)?",
    x = "Classe Identificata",
    y = "Percentuale sul Totale (%)",
    fill = "Legenda"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, face = "bold", angle = 20, hjust = 1), # Inclinato per leggere meglio i nomi lunghi
    plot.title = element_text(face = "bold", size = 14)
  ) +
  ylim(0, max(df_percentuali$Percentuale) + 5)