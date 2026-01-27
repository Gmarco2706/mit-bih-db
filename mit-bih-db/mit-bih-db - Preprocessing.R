library(data.table)
library(ggplot2)

# ==============================================================================
# 1. FUNZIONI DI UTILITÀ
# ==============================================================================

theme_ecg <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_line(color = "#ff9999", linewidth = 0.5),
    panel.grid.minor = element_line(color = "#ffcccc", linewidth = 0.25),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    strip.background = element_rect(fill = "#f0f0f0", color = "black"),
    strip.text = element_text(face = "bold", size = 9),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(color = "black")
  )

esegui_correzioni_canali <- function(dt) {
  ids_gruppo1 <- c(102, 104)
  dt <- dt[!(dt$Paziente %in% ids_gruppo1)]
  id_gruppo2 <- 114
  dt[Paziente == id_gruppo2, ':=' (V5 = MLII, MLII = V5)]
  return(dt)
}

correggi_picchi_locali <- function(df, raggio) {
  mlii_vals <- df$MLII
  n_rows <- nrow(df)
  
  # Selezione indici delle annotazioni per il riallineamento
  indici_picchi <- which(df$Picco != "" & !is.na(df$Picco))
  labels_originali <- df$Picco[indici_picchi]
  
  indici_corretti <- integer(length(indici_picchi))
  
  for(i in seq_along(indici_picchi)) {
    idx_centro <- indici_picchi[i]
    inizio <- max(1, idx_centro - raggio)
    fine   <- min(n_rows, idx_centro + raggio)
    
    # Ricerca del massimo valore assoluto nel range locale (Z-score)
    segmento <- abs(mlii_vals[inizio:fine])
    indici_corretti[i] <- inizio + which.max(segmento) - 1
  }
  
  # Reset colonna e riposizionamento etichette sui nuovi indici
  df[, Picco := ""] 
  if(length(indici_corretti) > 0) {
    df[indici_corretti, Picco := labels_originali]
  }
  return(df)
}

genera_plot_segmento <- function(df, id_paziente, samples, titolo) {
  dati_plot <- df[Paziente %in% id_paziente & sample_index %in% samples]
  if (nrow(dati_plot) == 0) return(NULL)
  dati_plot[, X := 1:.N]
  p <- ggplot(dati_plot, aes(x = X/360, y = MLII)) +
    theme_ecg +
    geom_line(color = "#00441b", linewidth = 0.7) +
    labs(title = titolo, subtitle = paste("Paziente:", paste(id_paziente, collapse=", "), "| Visualizzazione ECG"), x = "Tempo (secondi)", y = "Ampiezza (Z-Score)")
  
  if ("Picco" %in% names(dati_plot)) {
    picchi_presenti <- dati_plot[!is.na(Picco) & !(Picco == "")]
    if (nrow(picchi_presenti) > 0) {
      p <- p + geom_point(data = picchi_presenti, aes(x = X/360, y = MLII), shape = 21, fill = "red", color = "white", stroke = 1.5, size = 3)
      p <- p + geom_text(data = picchi_presenti, aes(x = X/360, y = MLII, label = Picco), vjust = -1.5, fontface = "bold", size = 3.5, color = "darkred")
    }
  }
  return(p)
}

estrai_segmenti_battiti <- function(df, target_labels, win_pre = 90, win_post = 269) {
  idxs_centri <- which(df$Picco %in% target_labels)
  
  # Stampa informativa sulla tipologia e quantità di battiti individuati
  cat(paste0("\n--- ESTRAZIONE FINESTRE PER BATTITI (Trovati: ", length(idxs_centri), "): ", 
             paste(target_labels, collapse=","), " ---\n"))
  
  if(length(idxs_centri) == 0) return(NULL)
  
  lista_segmenti <- list()
  pb <- txtProgressBar(min = 0, max = length(idxs_centri), style = 3)
  
  for(i in seq_along(idxs_centri)) {
    idx <- idxs_centri[i]
    idx_start <- idx - win_pre
    idx_end   <- idx + win_post
    
    if(idx_start < 1 || idx_end > nrow(df)) next
    if (df$Paziente[idx] != df$Paziente[idx_start]) next
    
    segmento <- df[idx_start:idx_end, .(Paziente, sample_index, MLII, V1, Picco)]
    segmento[, Beat_ID := paste0(Paziente[1], "_", i)]
    segmento[, Tempo_Locale := -win_pre:win_post]
    segmento[, Label_Battito := df$Picco[idx]]
    
    lista_segmenti[[i]] <- segmento
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(rbindlist(lista_segmenti))
}

genera_plot_sovrapposto <- function(df_segmentato, n_max = 200, titolo = "Sovrapposizione Battiti") {
  if (is.null(df_segmentato) || nrow(df_segmentato) == 0) return(NULL)
  ids_all <- unique(df_segmentato$Beat_ID)
  n_effettivi <- min(length(ids_all), n_max)
  ids_plot <- head(ids_all, n_effettivi)
  dati_plot <- df_segmentato[Beat_ID %in% ids_plot]
  p <- ggplot(dati_plot, aes(x = (Tempo_Locale+W_PRE)/360, y = MLII, group = Beat_ID)) +
    theme_ecg +
    geom_line(aes(color = factor(Paziente)), alpha = 0.3, linewidth = 0.6) + 
    geom_vline(xintercept = 0, linetype="dashed", color="blue", alpha=0.5) +
    labs(title = titolo, subtitle = paste("Visualizzati:", n_effettivi, "battiti sovrapposti"), x = "Tempo (Secondi)", y = "Ampiezza (Z-Score)", color = "Paziente") +
    theme(legend.position = "bottom")
  return(p)
}

genera_plot_griglia <- function(df_segmentato, n_battiti = 6, titolo = "Dettaglio Singoli Battiti") {
  if (is.null(df_segmentato) || nrow(df_segmentato) == 0) return(NULL)
  ids_plot <- head(unique(df_segmentato$Beat_ID), n_battiti)
  dati_plot <- df_segmentato[Beat_ID %in% ids_plot]
  dati_plot[, Label_Estesa := paste0("Paz: ", Paziente, "\n(", Beat_ID, ")")]
  p <- ggplot(dati_plot, aes(x = (Tempo_Locale+W_PRE)/360, y = MLII)) +
    theme_ecg +
    geom_line(color = "#00441b", linewidth = 0.8) +
    geom_point(data = dati_plot[Tempo_Locale == 0], aes(x = (Tempo_Locale+W_PRE)/360, y = MLII), shape = 21, fill = "red", color = "white", stroke = 1.5, size = 3) +
    facet_wrap(~Label_Estesa, scales = "free_y") +
    labs(title = titolo, subtitle = "Dettaglio Battiti", x = "Secondi", y = "Z-Score")
  return(p)
}

conta_occorrenze_leads <- function(dt) {
  target_cols <- c("MLII", "V1", "V2", "V4", "V5")
  cols_presenti <- intersect(target_cols, names(dt))
  cols_mancanti <- setdiff(target_cols, names(dt))
  
  report <- dt[, lapply(.SD, function(x) sum(!is.na(x))), 
               by = Paziente, 
               .SDcols = cols_presenti]
  
  if(length(cols_mancanti) > 0) {
    report[, (cols_mancanti) := 0]
  }
  
  setcolorder(report, c("Paziente", target_cols))
  setorder(report, Paziente) 
  
  return(report)
}

# ==============================================================================
# 2. ESECUZIONE MAIN 
# ==============================================================================

path_dataset <- "../../mit_bih_dataset_puro.csv"
id_target <- 219
samples_target <- 20000:25000

# Caricamento dati
df_signal <- fread(path_dataset)
setDT(df_signal); 
setorder(df_signal, Paziente, sample_index)

# Preview e Conteggi iniziali
cat("\n--- HEAD SIGNAL DATASET ---\n"); 
print(head(df_signal))

cat("Conteggio occorrenze per canale in corso...\n")
tabella_leads <- conta_occorrenze_leads(df_signal)
print(tabella_leads)

# Filtro pazienti con lead necessari
df_signal <- df_signal[Paziente %in% df_signal[, .(keep = any(!is.na(MLII)) & any(!is.na(V1))), by = Paziente][(keep), Paziente]]
cat("Conteggio occorrenze dopo filtro lead...\n")
tabella_leads <- conta_occorrenze_leads(df_signal)
print(tabella_leads)


# Normalizzazione Globale Z-Score
cat("\n--- ESECUZIONE NORMALIZZAZIONE Z-SCORE SU INTERO DATASET ---\n")
#df_signal[, MLII := MLII - median(MLII, na.rm = TRUE), by = Paziente]
#df_signal[, V1 := V1 - median(V1, na.rm = TRUE), by = Paziente]
df_signal[, MLII := as.numeric(scale(MLII)), by = Paziente]
df_signal[, V1 := as.numeric(scale(V1)), by = Paziente]
cat("Normalizzazione Z-Score completata.\n")

print(genera_plot_segmento(df_signal, id_target, samples_target, "Visualizzazione Onda - Post-normalizzazione"))

# Controllo Segmentazione
print(genera_plot_segmento(df_signal, id_target, samples_target, "Verifica Segmentazione - Pre correzione picchi"))

# Correzione Picchi Locali
cat("\nEsecuzione correzione picchi locali...\n")
df_signal <- correggi_picchi_locali(df_signal, raggio = 18)
cat("Correzione completata.\n")
print(genera_plot_segmento(df_signal, id_target, samples_target, "Verifica Segmentazione - Post correzione"))


#Correzione canali
df_signal <- esegui_correzioni_canali(df_signal)



#salvataggio dei pazienti con battiti normali
W_PRE  <- 100; W_POST <- 100;
cat(paste("\nEstrazione con finestra:", (W_PRE + W_POST)/360, "ms (Circa", W_PRE + W_POST,"Samples)"))
df_N_waves <- estrai_segmenti_battiti(df_signal, target_labels = "N", win_pre = W_PRE, win_post = W_POST)
print(genera_plot_griglia(df_N_waves, n_battiti = 6, titolo = "Dettaglio Primi Battiti Estratti Normali"))


# Verifica duplicati
conteggio_pvc_duplicati <- df_signal[Paziente %in% c(201, 202) & Picco == "V", .N, by = Paziente]
cat("\n--- VERIFICA BATTITI PVC PER PAZIENTE DUPLICATO ---\n")
print(conteggio_pvc_duplicati)
remove(conteggio_pvc_duplicati)


# Pulizia
df_signal <- df_signal[Paziente != 202]

print(genera_plot_segmento(df_signal, id_target, samples_target, "Visualizzazione Onda - Pre-normalizzazione"))



# Estrazione Segmenti PVC (Parametri specifici)
cat(paste("\nEstrazione con finestra:", (W_PRE + W_POST)/360, "ms (Circa", W_PRE + W_POST,"Samples)"))
df_V_waves <- estrai_segmenti_battiti(df_signal, target_labels = "V", win_pre = W_PRE, win_post = W_POST)

gc()

# Plot riassuntivi
if (!is.null(df_V_waves) && nrow(df_V_waves) > 0) {
  print(genera_plot_sovrapposto(df_V_waves, n_max = 7124, titolo = "Analisi Morfologica: Sovrapposizione 'V'"))
  print(genera_plot_griglia(df_V_waves, n_battiti = 6, titolo = "Dettaglio Primi Battiti Estratti"))
} else { cat("Nessun segmento trovato.\n") }

# Salvataggio Output
#file_output <- "../../pvc_segmentati_dataset.csv"

#if (!is.null(df_V_waves) && nrow(df_V_waves) > 0) {
#  cat(paste0("\nSalvataggio del dataset segmentato in: ", file_output, " ... "))
# fwrite(df_V_waves, file_output)
# cat("Completato!\n")
#  cat(paste("Totale righe salvate:", nrow(df_V_waves), "\n"))
#  cat(paste("Totale battiti unici:", length(unique(df_V_waves$Beat_ID)), "\n"))
#} else {
#  cat("\nATTENZIONE: Nessun dato da salvare (il dataframe è vuoto).\n")
#}

remove(df_signal)

