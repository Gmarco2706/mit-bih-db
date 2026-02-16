library(data.table)
library(ggplot2)
library(e1071) 
library(patchwork)
library(reshape2)

# --- CONFIGURAZIONE ---
RAGGIO_RICERCA <- 30      
CANALE_REF     <- "MLII"  
path_dataset <- "../../mit_bih_dataset_puro.csv" 

# --- PARAMETRI DI SEGMENTAZIONE ---
CAMPIONI_PRIMA <- 100  
CAMPIONI_DOPO  <- 100  
LUNGHEZZA_BATTITO <- CAMPIONI_PRIMA + CAMPIONI_DOPO + 1

# --- FUNZIONI DI SUPPORTO ---

theme_ecg <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_line(color = "#ff9999", linewidth = 0.5),
    panel.grid.minor = element_line(color = "#ffcccc", linewidth = 0.25),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    strip.background = element_rect(fill = "#f0f0f0", color = "black"),
    strip.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 10)
  )

conta_occorrenze_leads <- function(dt) {
  target_cols <- c("MLII", "V1", "V2", "V4", "V5")
  cols_presenti <- intersect(target_cols, names(dt))
  cols_mancanti <- setdiff(target_cols, names(dt))
  report <- dt[, lapply(.SD, function(x) sum(!is.na(x))), by = Paziente, .SDcols = cols_presenti]
  if(length(cols_mancanti) > 0) { report[, (cols_mancanti) := 0] }
  setcolorder(report, c("Paziente", target_cols))
  setorder(report, Paziente) 
  return(report)
}

rimuovi_baseline <- function(df, finestra_secondi = 0.8, fs = 360) {
  k <- round(finestra_secondi * fs)
  if (k %% 2 == 0) k <- k + 1 
  cat(paste(">>> Rimozione baseline in corso su MLII e V1 (finestra:", k, "campioni)...\n"))
  df[, MLII := MLII - data.table::frollmedian(MLII, n = k, align = "center", fill = NA), by = Paziente]
  df[, V1   := V1   - data.table::frollmedian(V1,   n = k, align = "center", fill = NA), by = Paziente]
  df <- df[!is.na(MLII) & !is.na(V1)]
  cat(">>> Baseline rimossa con successo.\n")
  return(df)
}

trova_picco_reale <- function(segnale_completo, indici_grezzi, raggio) {
  n <- length(indici_grezzi)
  indici_corretti <- numeric(n)
  max_len <- length(segnale_completo)
  cat("    Ricalcolo posizione precisa in corso... ")
  for(i in 1:n) {
    centro_grezzo <- indici_grezzi[i]
    inizio_finestra <- max(1, centro_grezzo - raggio)
    fine_finestra   <- min(max_len, centro_grezzo + raggio)
    chunk <- segnale_completo[inizio_finestra:fine_finestra]
    pos_max <- which.max(abs(chunk))
    if (length(pos_max) == 0) { indici_corretti[i] <- centro_grezzo
    } else { indici_corretti[i] <- (inizio_finestra - 1) + pos_max }
  }
  cat("Fatto.\n")
  return(indici_corretti)
}

estrai_matrice_battiti <- function(df, indici_picchi, prima, dopo) {
  n_battiti <- length(indici_picchi)
  lunghezza <- prima + dopo + 1
  matrice_mlii <- matrix(NA, nrow = n_battiti, ncol = lunghezza)
  matrice_v1   <- matrix(NA, nrow = n_battiti, ncol = lunghezza)
  cat(paste("   Estrazione di", n_battiti, "segmenti in corso... "))
  for(i in 1:n_battiti) {
    idx <- indici_picchi[i]
    start <- idx - prima
    end   <- idx + dopo
    if(start >= 1 && end <= nrow(df)) {
      matrice_mlii[i, ] <- df$MLII[start:end]
      matrice_v1[i, ]   <- df$V1[start:end]
    }
  }
  cat("Fatto.\n")
  return(list(MLII = matrice_mlii, V1 = matrice_v1))
}

# --- FUNZIONE PER PREPARARE UN BATTITO AL PLOT ---
prepara_battito_plot <- function(matrice_mlii, matrice_v1, riga, label) {
  df <- data.table(
    t_ms = (1:ncol(matrice_mlii) - (CAMPIONI_PRIMA + 1)) * (1000/360),
    MLII = matrice_mlii[riga, ],
    V1   = matrice_v1[riga, ]
  )
  # Usiamo setDT per garantire che l'oggetto sia un data.table dopo il melt
  df_long <- melt(df, id.vars = "t_ms", variable.name = "Lead", value.name = "mV")
  setDT(df_long) 
  
  df_long[, Tipo := label]
  return(df_long)
}

estrai_features_statistiche <- function(matrice, label) {
  dt_features <- data.table(
    Media    = rowMeans(matrice),
    DevStd   = apply(matrice, 1, sd),
    Skewness = apply(matrice, 1, skewness), 
    Kurtosis = apply(matrice, 1, kurtosis), 
    Max_Amp  = apply(matrice, 1, function(x) max(x) - min(x)), 
    Classe   = label
  )
  return(dt_features)
}

plot_istogramma_con_media <- function(df, x_var, titolo, colore_barre) {
  media_val <- mean(df[[x_var]], na.rm = TRUE)
  ggplot(df, aes_string(x = x_var)) + 
    geom_histogram(fill = colore_barre, alpha = 0.6, bins = 30) +
    geom_vline(xintercept = media_val, color = "blue", linetype = "dashed", linewidth = 1) +
    annotate("text", x = media_val, y = 0, label = paste("Media:", round(media_val, 2)), 
             color = "blue", angle = 90, vjust = -0.5, hjust = -0.1, size = 3) +
    theme_ecg + labs(title = titolo, x = x_var)
}

crea_boxplot_ob1 <- function(df, y_var, titolo, colore) {
  ggplot(df, aes_string(x = "Canale", y = y_var, fill = "Canale")) +
    geom_boxplot(alpha = 0.7, outlier.color = "black", outlier.shape = 16) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "blue") +
    scale_fill_manual(values = c("MLII" = colore, "V1" = "grey90")) +
    theme_ecg + labs(title = titolo, y = y_var, x = "Derivazione")
}

plot_ispezione_outlier <- function(id_battito, motivo) {
  sig_mlii <- matrice_V_MLII[id_battito, ]
  sig_v1   <- matrice_V_V1[id_battito, ]
  v_max_v1 <- max(sig_v1); v_min_v1 <- min(sig_v1)
  t_ms <- (1:length(sig_mlii) - (CAMPIONI_PRIMA + 1)) * (1000/360)
  df_p <- rbind(
    data.table(t = t_ms, mV = sig_mlii, Canale = "MLII"),
    data.table(t = t_ms, mV = sig_v1,   Canale = "V1")
  )
  ggplot(df_p, aes(x = t, y = mV, color = Canale)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~Canale, scales = "free_y", ncol = 1) +
    theme_minimal() + 
    scale_color_manual(values = c("MLII" = "blue", "V1" = "darkred")) +
    labs(title = paste("ISPEZIONE OB1:", motivo),
         subtitle = paste0("ID: ", id_battito, " | Ampiezza Totale V1: ", round(df_V_V1$Max_Amp[id_battito], 2), " mV",
                           "\n(V1 -> Max: ", round(v_max_v1, 2), " | Min: ", round(v_min_v1, 2), " | Kurt: ", round(df_V_V1$Kurtosis[id_battito], 2), ")"),
         x = "Tempo (ms)", y = "mV")
}

# --------------------------------------------------------------------------------------------------------------
# --- 1. CARICAMENTO E PRE-PROCESSING ---

if(!exists("df_ecg")){
  df_ecg = fread(path_dataset)
  setDT(df_ecg); setorder(df_ecg, Paziente, sample_index)
  print(conta_occorrenze_leads(df_ecg))
  df_ecg <- df_ecg[Paziente %in% df_ecg[, .(ok=any(!is.na(MLII))&any(!is.na(V1))), by=Paziente][(ok), Paziente]]
  cat("Conteggio occorrenze dopo filtro lead...\n")
  print(conta_occorrenze_leads(df_ecg))
  
  paziente_test <- unique(df_ecg$Paziente)[1]
  df_esempio <- df_ecg[Paziente == paziente_test][1:1000]
  df_plot <- melt(df_esempio, id.vars = "sample_index", measure.vars = c("MLII", "V1"), variable.name = "Lead", value.name = "mV")
  print(ggplot(df_plot, aes(x = sample_index, y = mV, color = Lead)) +
          geom_line() + facet_wrap(~Lead, ncol = 1, scales = "free_y") + theme_bw() +
          labs(title = paste("Segnale ECG Grezzo - Paziente:", paziente_test), x = "Sample Index", y = "Ampiezza (mV)"))
  
  df_ecg <- rimuovi_baseline(df_ecg)
}

df_esempio_clean <- df_ecg[Paziente == paziente_test][1:1000]
df_plot_clean <- melt(df_esempio_clean, id.vars = "sample_index", measure.vars = c("MLII", "V1"), variable.name = "Lead", value.name = "mV")
print(ggplot(df_plot_clean, aes(x = sample_index, y = mV, color = Lead)) +
        geom_line() + facet_wrap(~Lead, ncol = 1, scales = "free_y") + theme_ecg +
        labs(title = paste0("Segnale ECG centrato - Paziente: ", paziente_test), subtitle = "Dopo rimozione Baseline Wander", x = "Sample Index", y = "Ampiezza (mV)"))

# --------------------------------------------------------------------------------------------------------------
# --- 2. CENTRATURA E SEGMENTAZIONE ---

cat(">>> 1. Recupero indici annotazioni originali...\n")
idx_N_raw <- which(df_ecg$Picco == "N")
idx_V_raw <- which(df_ecg$Picco == "V")
cat(sprintf("   Trovati: %d Normali (N) e %d PVC (V) - Posizioni grezze.\n", length(idx_N_raw), length(idx_V_raw)))

# Grafico Prima della Correzione
raggio_v <- 100
idx_test_pre <- idx_V_raw[1]
df_pre <- df_ecg[(idx_test_pre - raggio_v):(idx_test_pre + raggio_v), ]
df_pre[, t_ms := (1:.N - (raggio_v + 1)) * (1000/360)]
df_pre_long <- melt(df_pre, id.vars = "t_ms", measure.vars = c("MLII", "V1"), variable.name = "Lead", value.name = "mV")

print(ggplot(df_pre_long, aes(x = t_ms, y = mV)) + geom_line(color = "black") + geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
        facet_wrap(~Lead, ncol = 1, scales = "free_y") + theme_ecg + labs(title = "Picco PRIMA della Correzione"))

cat(paste(">>> 2. Centratura basata sul canale", CANALE_REF, "...\n"))
vettore_segnale <- df_ecg[[CANALE_REF]]
idx_N_clean <- trova_picco_reale(vettore_segnale, idx_N_raw, RAGGIO_RICERCA)
idx_V_clean <- trova_picco_reale(vettore_segnale, idx_V_raw, RAGGIO_RICERCA)

# Grafico Dopo la Correzione
idx_test_post <- idx_V_clean[1]
df_post <- df_ecg[(idx_test_post - raggio_v):(idx_test_post + raggio_v), ]
df_post[, t_ms := (1:.N - (raggio_v + 1)) * (1000/360)]
df_post_long <- melt(df_post, id.vars = "t_ms", measure.vars = c("MLII", "V1"), variable.name = "Lead", value.name = "mV")
print(ggplot(df_post_long, aes(x = t_ms, y = mV)) + geom_line(color = "black") + geom_vline(xintercept = 0, color = "blue", linetype = "dashed", linewidth = 1) +
        facet_wrap(~Lead, ncol = 1, scales = "free_y") + theme_ecg + labs(title = "Picco DOPO la Correzione"))

# Estrazione matrici
cat(">>> Estrazione battiti...\n")
liste_N <- estrai_matrice_battiti(df_ecg, idx_N_clean, CAMPIONI_PRIMA, CAMPIONI_DOPO)
liste_V <- estrai_matrice_battiti(df_ecg, idx_V_clean, CAMPIONI_PRIMA, CAMPIONI_DOPO)
print(head(liste_V))

# --- FILTRAGGIO SINCRONIZZATO (Mantiene l'allineamento tra MLII e V1) ---

# 1. Per i battiti NORMALI (N)
# Il battito è valido solo se NON ha NA su MLII E NON ha NA su V1
idx_validi_N <- complete.cases(liste_N$MLII) & complete.cases(liste_N$V1)

matrice_N_MLII <- liste_N$MLII[idx_validi_N, ]
matrice_N_V1   <- liste_N$V1[idx_validi_N, ]

# 2. Per le PVC (V)
# Stessa logica: scartiamo il battito se manca un pezzo su uno qualsiasi dei canali
idx_validi_V <- complete.cases(liste_V$MLII) & complete.cases(liste_V$V1)

matrice_V_MLII <- liste_V$MLII[idx_validi_V, ]
matrice_V_V1   <- liste_V$V1[idx_validi_V, ]

# --- VERIFICA ---
cat(sprintf("Battiti N validi e allineati: %d\n", nrow(matrice_N_MLII)))
cat(sprintf("Battiti V validi e allineati: %d\n", nrow(matrice_V_MLII)))

# Confronto Morfologico
esempio_N <- prepara_battito_plot(matrice_N_MLII, matrice_N_V1, 10, "Normale (N)")
esempio_V <- prepara_battito_plot(matrice_V_MLII, matrice_V_V1, 10, "Extrasistole (V)")
print(ggplot(rbind(esempio_N, esempio_V), aes(x = t_ms, y = mV, color = Tipo)) + geom_line(linewidth = 0.8) + facet_grid(Lead ~ Tipo, scales = "free_y") +
        scale_color_manual(values = c("Normale (N)" = "black", "Extrasistole (V)" = "red")) + theme_ecg + labs(title = "Confronto Morfologico Battiti Estratti"))

# --------------------------------------------------------------------------------------------------------------
# --- 3. ANALISI STATISTICA E DISTRIBUZIONI ---

cat(">>> Estrazione feature statistiche...\n")
df_N_MLII <- estrai_features_statistiche(matrice_N_MLII, "Normale (N)")
df_V_MLII <- estrai_features_statistiche(matrice_V_MLII, "PVC (V)")
df_N_V1   <- estrai_features_statistiche(matrice_N_V1, "Normale (N)")
df_V_V1   <- estrai_features_statistiche(matrice_V_V1, "PVC (V)")

# Istogrammi PVC
p1_v_m <- plot_istogramma_con_media(df_V_MLII, "Max_Amp", "Ampiezza MLII (V)", "red")
p2_v_m <- plot_istogramma_con_media(df_V_MLII, "Skewness", "Asimmetria MLII (V)", "darkred")
p3_v_m <- plot_istogramma_con_media(df_V_MLII, "Kurtosis", "Kurtosis MLII (V)", "orange")
print((p1_v_m | p2_v_m | p3_v_m) + plot_annotation(title = "MLII: Struttura interna dei battiti PVC (V)"))

p1_v_v1 <- plot_istogramma_con_media(df_V_V1, "Max_Amp", "Ampiezza V1 (V)", "red")
p2_v_v1 <- plot_istogramma_con_media(df_V_V1, "Skewness", "Asimmetria V1 (V)", "darkred")
p3_v_v1 <- plot_istogramma_con_media(df_V_V1, "Kurtosis", "Kurtosis V1 (V)", "orange")
print((p1_v_v1 | p2_v_v1 | p3_v_v1) + plot_annotation(title = "V1: Struttura interna dei battiti PVC (V)"))

# Istogrammi Normali
p1_n_m <- plot_istogramma_con_media(df_N_MLII, "Max_Amp", "Ampiezza MLII (N)", "forestgreen")
p2_n_m <- plot_istogramma_con_media(df_N_MLII, "Skewness", "Asimmetria MLII (N)", "darkgreen")
p3_n_m <- plot_istogramma_con_media(df_N_MLII, "Kurtosis", "Kurtosis MLII (N)", "green")
print((p1_n_m | p2_n_m | p3_n_m) + plot_annotation(title = "MLII: Struttura interna dei battiti NORMALI (N)"))

p1_n_v1 <- plot_istogramma_con_media(df_N_V1, "Max_Amp", "Ampiezza V1 (N)", "forestgreen")
p2_n_v1 <- plot_istogramma_con_media(df_N_V1, "Skewness", "Asimmetria V1 (N)", "darkgreen")
p3_n_v1 <- plot_istogramma_con_media(df_N_V1, "Kurtosis", "Kurtosis V1 (N)", "green")
print((p1_n_v1 | p2_n_v1 | p3_n_v1) + plot_annotation(title = "V1: Struttura interna dei battiti NORMALI (N)"))

# Boxplot
df_boxplot_V <- rbind(df_V_MLII[, Canale := "MLII"], df_V_V1[, Canale := "V1"])
b1_v <- crea_boxplot_ob1(df_boxplot_V, "Max_Amp", "Ampiezza (V)", "red")
b2_v <- crea_boxplot_ob1(df_boxplot_V, "Skewness", "Asimmetria (V)", "darkred")
b3_v <- crea_boxplot_ob1(df_boxplot_V, "Kurtosis", "Kurtosis (V)", "orange")
print((b1_v | b2_v | b3_v) + plot_annotation(title = "BOXPLOT PVC: Analisi Outliers e Differenze Canali"))

# --------------------------------------------------------------------------------------------------------------
# --- 4. ISPEZIONE OUTLIER E ANOMALIE ---

id_sospetti_unici <- unique(c(which((df_V_MLII$Max_Amp > 6 | df_V_MLII$Max_Amp<1) | df_V_MLII$Kurtosis > 7.5), which(df_V_V1$Max_Amp > 6 | df_V_V1$Kurtosis > 15 | df_V_V1$Skewness > 2.5 | df_V_V1$Skewness < -5.0)))
if(length(id_sospetti_unici) > 0) {
  df_comparativo <- data.table()
  for(i in id_sospetti_unici[1:min(6, length(id_sospetti_unici))]) {
    t_ms <- (1:ncol(matrice_V_MLII) - (CAMPIONI_PRIMA + 1)) * (1000/360)
    temp <- rbind(data.table(t = t_ms, mV = matrice_V_MLII[i, ], Canale = "MLII", ID = paste("Battito ID:", i)),
                  data.table(t = t_ms, mV = matrice_V_V1[i, ],   Canale = "V1",   ID = paste("Battito ID:", i)))
    df_comparativo <- rbind(df_comparativo, temp)
  }
  print(ggplot(df_comparativo, aes(x = t, y = mV, color = Canale)) + geom_line() + facet_grid(Canale ~ ID, scales = "free_y") +
          scale_color_manual(values = c("MLII" = "blue", "V1" = "darkred")) + theme_ecg + labs(title = "Ispezione Sincrona Outlier: MLII vs V1 (V)"))
}

# Casi Estremi e Range 20-30
id_max_amp  <- which.max(df_V_V1$Max_Amp)
id_max_kurt <- which.max(df_V_V1$Kurtosis)
id_kurt_20_50 <- which(df_V_V1$Kurtosis > 20 & df_V_V1$Kurtosis < 50)

p_amp  <- plot_ispezione_outlier(id_max_amp, "Massima Ampiezza su V1 (8 mV)")
p_kurt <- plot_ispezione_outlier(id_max_kurt, "Massima Kurtosis su V1 (50)")
print(p_amp / p_kurt)

if(length(id_kurt_20_50) > 0) {
  lista_grafici_mid <- list()
  id_esempi <- id_kurt_20_50[1:min(4, length(id_kurt_20_50))]
  for(i in id_esempi) { lista_grafici_mid[[as.character(i)]] <- plot_ispezione_outlier(i, "Kurtosis tra 20 e 50") }
  print(wrap_plots(lista_grafici_mid, ncol = 2))
}

# --------------------------------------------------------------------------------------------------------------
# --- 5. ELIMINAZIONE INCROCIATA E RESOCONTO ---

outlier_MLII <- which((df_V_MLII$Max_Amp > 6  | df_V_MLII$Max_Amp<1) | df_V_MLII$Kurtosis > 7.5) # Valori presi dal boxplot
outlier_V1 <- which(df_V_V1$Max_Amp > 6 | df_V_V1$Kurtosis > 15 | df_V_V1$Skewness > 2.5 | df_V_V1$Skewness < -5.0) # Valori presi dal boxplot
indici_da_rimuovere <- unique(c(outlier_MLII, outlier_V1))

cat(sprintf(">>> Eliminazione incrociata: rimozione di %d battiti su %d totali.\n", length(indici_da_rimuovere), nrow(df_V_MLII)))

df_V_MLII_clean <- df_V_MLII[-indici_da_rimuovere, ]
df_V_V1_clean   <- df_V_V1[-indici_da_rimuovere, ]
matrice_V_MLII_clean <- matrice_V_MLII[-indici_da_rimuovere, ]
matrice_V_V1_clean   <- matrice_V_V1[-indici_da_rimuovere, ]

# Scrittura su file
fwrite(as.data.table(matrice_V_MLII_clean), file = "../matrice_MLII_clean.csv", col.names = TRUE) # salvataggio per generazione dati sintetici
fwrite(as.data.table(matrice_V_V1_clean), file = "../matrice_V1_clean.csv", col.names = TRUE) # salvataggio per generazione dati sintetici

cat("\n--- RESOCONTO ELIMINAZIONE INCROCIATA OUTLIER ---\n")
totali <- nrow(df_V_MLII); rimossi <- length(indici_da_rimuovere); rimasti <- totali - rimossi
cat(sprintf("Battiti PVC Iniziali: %d\nBattiti Rimossi: %d\nBattiti Puliti: %d\nPercentuale Rimossa: %.2f%%\n", totali, rimossi, rimasti, (rimossi/totali)*100))

resoconto_medie <- data.table(
  Feature = c("Max_Amp_MLII", "Deviazione_Standard_AMP_MLII","Skewness_MLII","Deviazione_Standard_Skewness_MLII","Kurtosis_MLII", "Deviazione_Standard_Kurtosis_MLII", "Max_Amp_V1", "Deviazione_Standard_AMP_V1","Skewness_V1", "Deviazione_Standard_Skewness_V1", "Kurtosis_V1", "Deviazione_Standard_Kurtosis_V1"),
  Prima = c(mean(df_V_MLII$Max_Amp), sd(df_V_MLII$Max_Amp),mean(df_V_MLII$Skewness), sd(df_V_MLII$Skewness), mean(df_V_MLII$Kurtosis), sd(df_V_MLII$Kurtosis), mean(df_V_V1$Max_Amp), sd(df_V_V1$Max_Amp), mean(df_V_V1$Skewness), sd(df_V_V1$Skewness), mean(df_V_V1$Kurtosis), sd(df_V_V1$Kurtosis)),
  Dopo  = c(mean(df_V_MLII_clean$Max_Amp), sd(df_V_MLII_clean$Max_Amp), mean(df_V_MLII_clean$Skewness), sd(df_V_MLII_clean$Skewness), mean(df_V_MLII_clean$Kurtosis), sd(df_V_MLII_clean$Kurtosis), mean(df_V_V1_clean$Max_Amp), sd(df_V_V1_clean$Max_Amp), mean(df_V_V1_clean$Skewness), sd(df_V_V1_clean$Skewness), mean(df_V_V1_clean$Kurtosis), sd(df_V_V1_clean$Kurtosis))
)
resoconto_medie[, Variazione_Perc := ((Dopo - Prima) / Prima) * 100]
print(resoconto_medie)


# --- RIGENERAZIONE ISTOGRAMMI DOPO PULIZIA (PVC) ---
cat(">>> Generazione istogrammi sui dati puliti (eliminazione incrociata)...\n")

# 1. Istogrammi PVC su MLII (PULITI)
p1_v_m_clean <- plot_istogramma_con_media(df_V_MLII_clean, "Max_Amp", "Ampiezza MLII (V) - PULITO", "red")
p2_v_m_clean <- plot_istogramma_con_media(df_V_MLII_clean, "Skewness", "Asimmetria MLII (V) - PULITO", "darkred")
p3_v_m_clean <- plot_istogramma_con_media(df_V_MLII_clean, "Kurtosis", "Kurtosis MLII (V) - PULITO", "orange")

print((p1_v_m_clean | p2_v_m_clean | p3_v_m_clean) + 
        plot_annotation(title = "MLII (Clean): Distribuzioni PVC dopo rimozione outlier"))

# 2. Istogrammi PVC su V1 (PULITI)
p1_v_v1_clean <- plot_istogramma_con_media(df_V_V1_clean, "Max_Amp", "Ampiezza V1 (V) - PULITO", "red")
p2_v_v1_clean <- plot_istogramma_con_media(df_V_V1_clean, "Skewness", "Asimmetria V1 (V) - PULITO", "darkred")
p3_v_v1_clean <- plot_istogramma_con_media(df_V_V1_clean, "Kurtosis", "Kurtosis V1 (V) - PULITO", "orange")

print((p1_v_v1_clean | p2_v_v1_clean | p3_v_v1_clean) + 
        plot_annotation(title = "V1 (Clean): Distribuzioni PVC dopo rimozione outlier"))

# --- MATRICE DI CORRELAZIONE INCROCIATA (Solo V) ---
cat(">>> Analisi delle dipendenze tra i canali per la classe V...\n")

# Dataset unico per i battiti V con i dati di entrambi i canali
df_V_full <- data.table(
  Amp_MLII  = df_V_MLII_clean$Max_Amp,
  Skew_MLII = df_V_MLII_clean$Skewness,
  Kurt_MLII = df_V_MLII_clean$Kurtosis,
  Amp_V1    = df_V_V1_clean$Max_Amp,
  Kurt_V1   = df_V_V1_clean$Kurtosis,
  Skew_V1   = df_V_V1_clean$Skewness
)

# Calcolo correlazione
cor_V <- cor(df_V_full, use = "complete.obs")

# Visualizzazione Heatmap
print(ggplot(melt(cor_V), aes(Var1, Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", high="red", mid="white", limit=c(-1,1)) +
  geom_text(aes(label = round(value, 2)), size = 4) +
  theme_minimal() +
  labs(title = "OB1: Struttura dipendenze Classe V",
       subtitle = "Correlazione incrociata tra le feature di MLII e V1",
       x="", y=""))

# --- SCATTER PLOT INCROCIATO ---
print(ggplot(df_V_full, aes(x = Amp_MLII, y = Amp_V1)) +
  geom_point(alpha = 0.3, color = "red") +
  theme_ecg +
  labs(title = "OB1: Relazione Ampiezza MLII vs V1 (Solo V)",
       x = "Ampiezza MLII (mV)", y = "Ampiezza V1 (mV)"))

# --- SCATTER PLOT INCROCIATO ---
print(ggplot(df_V_full, aes(x = Skew_MLII, y = Skew_V1)) +
        geom_point(alpha = 0.3, color = "orange") +
        theme_ecg +
        labs(title = "OB1: Relazione Skewness MLII vs V1 (Solo V)",
             x = "Skewness MLII", y = "Skewness V1"))

# --- SCATTER PLOT INCROCIATO ---
print(ggplot(df_V_full, aes(x = Kurt_MLII, y = Kurt_V1)) +
        geom_point(alpha = 0.3, color = "green") +
        theme_ecg +
        labs(title = "OB1: Relazione Curtosi MLII vs V1 (Solo V)",
             subtitle = "Identificazione di bias di ampiezza tra le derivazioni",
             x = "Curtosi MLII", y = "Curtosi V1"))


# --- PULIZIA FINALE ---
remove(df_N_MLII, df_V_MLII, df_N_V1, df_V_V1)