library(data.table)
library(ggplot2)
library(e1071) 
library(patchwork)
library(reshape2)

# ==============================================================================
# 1. CONFIGURAZIONE PATH
# ==============================================================================

# Dataset 1: Context Aware
path_context_MLII <- "../Dati_Sintetici_GPT-5_context_aware/matrice_MLII_clean_sintetici.csv"
path_context_V1   <- "../Dati_Sintetici_GPT-5_context_aware/matrice_V1_clean_sintetici.csv"

# Dataset 2: No Context
path_no_context_MLII <- "../Dati_sintetici_GPT-5_no_context/matrice_MLII_clean_sintetici.csv"
path_no_context_V1   <- "../Dati_sintetici_GPT-5_no_context/matrice_V1_clean_sintetici.csv"

# ==============================================================================
# 2. FUNZIONI DI SUPPORTO E GRAFICA
# ==============================================================================

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

estrai_features_statistiche <- function(matrice, label) {
  # Calcolo statistiche riga per riga (per ogni battito)
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

analizza_dataset <- function(path_mlii, path_v1, nome_dataset, col_hist, col_box) {
  
  cat(paste0("\n======================================================\n"))
  cat(paste0("   ANALISI DATASET: ", nome_dataset, "\n"))
  cat(paste0("======================================================\n"))
  
  # 1. Caricamento
  m_MLII <- as.matrix(fread(path_mlii, header = TRUE))
  m_V1   <- as.matrix(fread(path_v1, header = TRUE))
  
  cat(sprintf("Dimensioni: %d battiti caricati.\n", nrow(m_MLII)))
  
  # 2. Calcolo Feature
  df_MLII <- estrai_features_statistiche(m_MLII, nome_dataset)
  df_V1   <- estrai_features_statistiche(m_V1,   nome_dataset)
  
  # 3. Istogrammi MLII (Combinati per risparmiare spazio, se vuoi separare anche questi dimmelo)
  p1 <- plot_istogramma_con_media(df_MLII, "Max_Amp", paste(nome_dataset, "- Ampiezza MLII"), col_hist)
  p2 <- plot_istogramma_con_media(df_MLII, "Skewness", paste(nome_dataset, "- Skewness MLII"), col_hist)
  p3 <- plot_istogramma_con_media(df_MLII, "Kurtosis", paste(nome_dataset, "- Kurtosis MLII"), col_hist)
  print((p1 | p2 | p3) + plot_annotation(title = paste("MLII:", nome_dataset)))
  
  # 4. Istogrammi V1
  p4 <- plot_istogramma_con_media(df_V1, "Max_Amp", paste(nome_dataset, "- Ampiezza V1"), col_hist)
  p5 <- plot_istogramma_con_media(df_V1, "Skewness", paste(nome_dataset, "- Skewness V1"), col_hist)
  p6 <- plot_istogramma_con_media(df_V1, "Kurtosis", paste(nome_dataset, "- Kurtosis V1"), col_hist)
  print((p4 | p5 | p6) + plot_annotation(title = paste("V1:", nome_dataset)))
  
  # 5. Boxplot Comparativi
  df_box <- rbind(df_MLII[, Canale := "MLII"], df_V1[, Canale := "V1"])
  b1 <- crea_boxplot_ob1(df_box, "Max_Amp", "Ampiezza", col_box)
  b2 <- crea_boxplot_ob1(df_box, "Skewness", "Asimmetria", col_box)
  b3 <- crea_boxplot_ob1(df_box, "Kurtosis", "Kurtosis", col_box)
  print((b1 | b2 | b3) + plot_annotation(title = paste("BOXPLOT:", nome_dataset)))
  
  # 6. Preparazione Dati Correlazione e Scatter
  df_full <- data.table(
    Amp_MLII  = df_MLII$Max_Amp,
    Kurt_MLII = df_MLII$Kurtosis,
    Skew_MLII = df_MLII$Skewness,
    Amp_V1    = df_V1$Max_Amp,
    Kurt_V1   = df_V1$Kurtosis,
    Skew_V1   = df_V1$Skewness
  )
  
  # Heatmap Correlazione Incrociata
  cor_mat <- cor(df_full, use = "complete.obs")
  print(ggplot(melt(cor_mat), aes(Var1, Var2, fill=value)) +
          geom_tile() +
          scale_fill_gradient2(low="blue", high="red", mid="white", limit=c(-1,1)) +
          geom_text(aes(label = round(value, 2)), size = 3) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = paste("Correlazione Incrociata:", nome_dataset),
               subtitle = "MLII vs V1 Feature Dependencies", x="", y=""))
  
  # 7. Scatter Plots (STAMPATI SEPARATAMENTE)
  
  # --- Scatter 1: Ampiezza ---
  print(ggplot(df_full, aes(x = Amp_MLII, y = Amp_V1)) +
          geom_point(alpha = 0.3, color = col_box) +
          theme_ecg +
          labs(title = paste("Scatter Ampiezza:", nome_dataset),
               subtitle = "Correlazione Ampiezza MLII vs V1",
               x = "Ampiezza MLII", y = "Ampiezza V1"))
  
  # --- Scatter 2: Skewness ---
  print(ggplot(df_full, aes(x = Skew_MLII, y = Skew_V1)) +
          geom_point(alpha = 0.3, color = col_box) +
          theme_ecg +
          labs(title = paste("Scatter Skewness:", nome_dataset),
               subtitle = "Correlazione Skewness MLII vs V1",
               x = "Skewness MLII", y = "Skewness V1"))
  
  # --- Scatter 3: Kurtosis ---
  print(ggplot(df_full, aes(x = Kurt_MLII, y = Kurt_V1)) +
          geom_point(alpha = 0.3, color = col_box) +
          theme_ecg +
          labs(title = paste("Scatter Kurtosis:", nome_dataset),
               subtitle = "Correlazione Kurtosis MLII vs V1",
               x = "Kurtosis MLII", y = "Kurtosis V1"))
  
  # 8. Ritorna statistiche medie per tabella finale
  return(data.table(
    Dataset = nome_dataset,
    Amp_MLII = mean(df_MLII$Max_Amp), Kurt_MLII = mean(df_MLII$Kurtosis), Skew_MLII = mean(df_MLII$Skewness),
    Amp_V1 = mean(df_V1$Max_Amp),     Kurt_V1 = mean(df_V1$Kurtosis),     Skew_V1 = mean(df_V1$Skewness)
  ))
}

# ==============================================================================
# 3. ESECUZIONE ANALISI
# ==============================================================================

# --- A. ANALISI CONTEXT AWARE ---
res_context <- analizza_dataset(
  path_context_MLII, 
  path_context_V1, 
  "CONTEXT AWARE (GPT-5)", 
  "forestgreen", 
  "darkgreen"
)

# --- B. ANALISI NO CONTEXT ---
res_no_context <- analizza_dataset(
  path_no_context_MLII, 
  path_no_context_V1, 
  "NO CONTEXT (GPT-5)", 
  "purple",       
  "darkorchid"
)

# ==============================================================================
# 4. TABELLA COMPARATIVA FINALE
# ==============================================================================
cat("\n======================================================\n")
cat("   CONFRONTO FINALE: VALORI MEDI\n")
cat("======================================================\n")

final_comparison <- rbind(res_context, res_no_context)
print(final_comparison)

cat("\nAnalisi completata.\n")