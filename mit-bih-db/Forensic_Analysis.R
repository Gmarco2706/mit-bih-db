library(data.table)
library(ggplot2)
library(reshape2)
library(e1071)

# ==============================================================================
# CONFIGURAZIONE SOGLIE
# ==============================================================================
# 1. FORMA E DIMENSIONE
SOGLIA_SOSPETTO <- 0.85   # Forma: Correlazione > 85%
SOGLIA_AMP_MIN  <- 0.70   # Ampiezza: 70% - 130%
SOGLIA_AMP_MAX  <- 1.30   

# 2. STATISTICHE (Kurtosis e Skewness)
SOGLIA_DIFF_KURT <- 3.0   
SOGLIA_DIFF_SKEW <- 1.0   

FS <- 360

# ==============================================================================
# STEP 0: CONTROLLO DATI
# ==============================================================================
cat(">>> [STEP 0] Verifica dati...\n")
if(!exists("centroidi")) stop("Mancano i 'centroidi'. Esegui script Clustering.")
if(!exists("matrice_N_MLII")) stop("Mancano le matrici N. Esegui script studio_dati.R")

# ==============================================================================
# STEP 1: PREPARAZIONE E PRE-CALCOLO METRICHE SU 'N' (DOPPIO CANALE)
# ==============================================================================
cat(">>> [STEP 1] Calcolo Metriche Avanzate (MLII + V1) su Battiti N...\n")

# A. Centratura
mat_N_M <- sweep(matrice_N_MLII, 1, rowMeans(matrice_N_MLII), "-")
mat_N_V <- sweep(matrice_N_V1,   1, rowMeans(matrice_N_V1),   "-")
ids_N   <- 1:nrow(mat_N_M)

# B. Calcolo Ampiezze N 
amps_N  <- apply(mat_N_M, 1, function(x) max(x) - min(x))

# C. Calcolo Kurtosis e Skewness su ENTRAMBI i canali
cat("    - Calcolo Statistiche MLII e V1...\n")
K_N_M <- apply(mat_N_M, 1, kurtosis, type=2); S_N_M <- apply(mat_N_M, 1, skewness, type=2)
K_N_V <- apply(mat_N_V, 1, kurtosis, type=2); S_N_V <- apply(mat_N_V, 1, skewness, type=2)

# ==============================================================================
# STEP 2: SCANSIONE FORENSE
# ==============================================================================
cat(">>> [STEP 2] Scansione Forense...\n")

classifica <- data.frame()
nomi_cluster <- names(centroidi)

for(k in nomi_cluster) {
  cent <- centroidi[[k]]
  len_sig <- min(ncol(mat_N_M), length(cent$MLII))
  
  # 1. Dati Segmentati
  seg_N_M <- mat_N_M[, 1:len_sig]; tpl_M <- cent$MLII[1:len_sig]
  seg_N_V <- mat_N_V[, 1:len_sig]; tpl_V <- cent$V1[1:len_sig]
  
  # 2. Correlazione
  avg_corr <- pmin(as.vector(cor(t(seg_N_M), tpl_M)), as.vector(cor(t(seg_N_V), tpl_V)))
  
  # 3. Rapporto Ampiezza 
  ratio_amp <- amps_N / (cent$Amp + 1e-6)
  
  # 4. Differenza Statistica (Media MLII + V1)
  K_T_M <- kurtosis(tpl_M, type=2); S_T_M <- skewness(tpl_M, type=2)
  K_T_V <- kurtosis(tpl_V, type=2); S_T_V <- skewness(tpl_V, type=2)
  
  diff_k_avg <- (abs(K_N_M - K_T_M) + abs(K_N_V - K_T_V)) / 2
  diff_s_avg <- (abs(S_N_M - S_T_M) + abs(S_N_V - S_T_V)) / 2
  
  classifica <- rbind(classifica, data.frame(
    Beat_ID = ids_N,
    Cluster_PVC = as.numeric(k),
    Correlazione = avg_corr,
    Rapporto_Amp = as.vector(ratio_amp),
    Diff_Kurt = as.vector(diff_k_avg),
    Diff_Skew = as.vector(diff_s_avg)
  ))
}

dt_class <- as.data.table(classifica)

# ==============================================================================
# STEP 3: CALCOLO "BEST MATCH" PER CLUSTER
# ==============================================================================
# Ricerca del battito N che ha la correlazione più alta per ogni Cluster PVC,
# indipendentemente dal fatto che sia un "falso negativo" o no.
cat(">>> Identificazione Miglior Candidato per ogni Cluster...\n")
dt_best_per_cluster <- dt_class[order(Cluster_PVC, -Correlazione), .SD[1], by=Cluster_PVC]


# ==============================================================================
# STEP 4: VISUALIZZAZIONE "MIGLIOR MATCH" PER OGNI CLUSTER
# ==============================================================================
if(nrow(dt_best_per_cluster) > 0) {
  cat("\n>>> Generazione Grafico: IL MIGLIOR 'N' PER OGNI CLUSTER PVC...\n")
  
  lista_plot_best <- list()
  
  for(i in 1:nrow(dt_best_per_cluster)) {
    bid   <- dt_best_per_cluster$Beat_ID[i]
    clust <- dt_best_per_cluster$Cluster_PVC[i]
    cent  <- centroidi[[as.character(clust)]]
    len_p <- min(ncol(mat_N_M), length(cent$MLII))
    
    lbl <- paste0("CLUSTER ", clust, "\nBest N: ", bid, "\n",
                  "Corr: ", round(dt_best_per_cluster$Correlazione[i]*100,0), "% | Amp: ", round(dt_best_per_cluster$Rapporto_Amp[i], 2), "x\n",
                  "dK: ", round(dt_best_per_cluster$Diff_Kurt[i], 1), " | dS: ", round(dt_best_per_cluster$Diff_Skew[i], 1))
    
    df_temp <- rbind(
      data.frame(Time=1:len_p, mV=mat_N_M[bid, 1:len_p], Canale="MLII", Tipo="Recuperato (N)", Panel=lbl),
      data.frame(Time=1:len_p, mV=mat_N_V[bid, 1:len_p], Canale="V1",   Tipo="Recuperato (N)", Panel=lbl),
      data.frame(Time=1:len_p, mV=cent$MLII[1:len_p],    Canale="MLII", Tipo="PVC Template",   Panel=lbl),
      data.frame(Time=1:len_p, mV=cent$V1[1:len_p],      Canale="V1",   Tipo="PVC Template",   Panel=lbl)
    )
    lista_plot_best[[i]] <- df_temp
  }
  
  df_tot_best <- do.call(rbind, lista_plot_best)
  df_tot_best$Tipo <- factor(df_tot_best$Tipo, levels = c("Recuperato (N)", "PVC Template"))
  df_tot_best$Panel <- factor(df_tot_best$Panel, levels = unique(df_tot_best$Panel))
  
  p_best <- ggplot(df_tot_best, aes(x=Time, y=mV)) +
    theme_bw() +
    geom_line(aes(color=Tipo, linetype=Tipo, size=Tipo)) +
    scale_color_manual(values = c("Recuperato (N)"="blue", "PVC Template"="red")) +
    scale_linetype_manual(values = c("Recuperato (N)"="solid", "PVC Template"="dashed")) +
    scale_size_manual(values = c("Recuperato (N)"=0.8, "PVC Template"=1.0)) +
    
    # Griglia: Righe=Cluster, Colonne=Canali
    facet_grid(Panel ~ Canale, scales="free_y", switch="y") +
    
    labs(title = "PANORAMICA: IL MIGLIOR CANDIDATO 'N' PER OGNI CLUSTER PVC", 
         subtitle="Questi sono i battiti N che più assomigliano ai cluster (anche se non sono errori certi).", 
         x="Campioni", y="mV") +
    theme(legend.position="top", strip.text.y.left = element_text(angle=0, size=7, face="bold"),
          panel.spacing.y = unit(0.5, "lines"))
  
  print(p_best)
}


# ==============================================================================
# STEP 5: FILTRO COLPEVOLI 
# ==============================================================================
# Ricerca errori (Falsi Negativi)
dt_best_match <- dt_class[order(Beat_ID, -Correlazione), .SD[1], by=Beat_ID]

dt_colpevoli <- dt_best_match[
  Correlazione > SOGLIA_SOSPETTO & 
    Rapporto_Amp >= SOGLIA_AMP_MIN & Rapporto_Amp <= SOGLIA_AMP_MAX &
    Diff_Kurt <= SOGLIA_DIFF_KURT &
    Diff_Skew <= SOGLIA_DIFF_SKEW
]

n_tot <- nrow(dt_best_match)
n_colp <- nrow(dt_colpevoli)

cat("\n============================================================\n")
cat(" REPORT FORENSE: ANALISI FALSI NEGATIVI\n")
cat("============================================================\n")
cat(sprintf("Totale Battiti 'N' Scansionati: %d\n", n_tot))
cat("CRITERI DI ACCUSA:\n")
cat(sprintf("  1. Correlazione > %.0f%%\n", SOGLIA_SOSPETTO*100))
cat(sprintf("  2. Ampiezza %.0f%% - %.0f%%\n", SOGLIA_AMP_MIN*100, SOGLIA_AMP_MAX*100))
cat(sprintf("  3. dKurt < %.1f | dSkew < %.1f\n", SOGLIA_DIFF_KURT, SOGLIA_DIFF_SKEW))
cat("------------------------------------------------------------\n")
cat(sprintf("BATTITI CONFERMATI COME 'V': %d (%.2f%%)\n", n_colp, (n_colp/n_tot)*100))
cat("============================================================\n")


# ==============================================================================
# STEP 6: VISUALIZZAZIONE "COLPEVOLI"
# ==============================================================================
if(n_colp > 0) {
  cat("\n>>> Generazione Grafico Casi Confermati...\n")
  
  n_show <- min(n_colp, 8) 
  lista_plot_data <- list()
  
  for(i in 1:n_show) {
    bid   <- dt_colpevoli$Beat_ID[i]
    clust <- dt_colpevoli$Cluster_PVC[i]
    cent  <- centroidi[[as.character(clust)]]
    len_p <- min(ncol(mat_N_M), length(cent$MLII))
    
    lbl <- paste0("CASO ", i, "\nBeat ", bid, " (Cl ", clust, ")\n",
                  "Corr: ", round(dt_colpevoli$Correlazione[i]*100,0), "% | Amp: ", round(dt_colpevoli$Rapporto_Amp[i], 2), "x\n",
                  "dK: ", round(dt_colpevoli$Diff_Kurt[i], 1), " | dS: ", round(dt_colpevoli$Diff_Skew[i], 1))
    
    df_temp <- rbind(
      data.frame(Time=1:len_p, mV=mat_N_M[bid, 1:len_p], Canale="MLII", Tipo="Recuperato (N)", Panel=lbl),
      data.frame(Time=1:len_p, mV=mat_N_V[bid, 1:len_p], Canale="V1",   Tipo="Recuperato (N)", Panel=lbl),
      data.frame(Time=1:len_p, mV=cent$MLII[1:len_p],    Canale="MLII", Tipo="PVC Template",   Panel=lbl),
      data.frame(Time=1:len_p, mV=cent$V1[1:len_p],      Canale="V1",   Tipo="PVC Template",   Panel=lbl)
    )
    lista_plot_data[[i]] <- df_temp
  }
  
  df_tot <- do.call(rbind, lista_plot_data)
  df_tot$Tipo <- factor(df_tot$Tipo, levels = c("Recuperato (N)", "PVC Template"))
  df_tot$Panel <- factor(df_tot$Panel, levels = unique(df_tot$Panel))
  
  p <- ggplot(df_tot, aes(x=Time, y=mV)) +
    theme_bw() +
    geom_line(aes(color=Tipo, linetype=Tipo, size=Tipo)) +
    scale_color_manual(values = c("Recuperato (N)"="blue", "PVC Template"="red")) +
    scale_linetype_manual(values = c("Recuperato (N)"="solid", "PVC Template"="dashed")) +
    scale_size_manual(values = c("Recuperato (N)"=0.8, "PVC Template"=1.0)) +
    facet_grid(Panel ~ Canale, scales="free_y", switch="y") +
    labs(title = "FALSI NEGATIVI CONFERMATI (ERRORI GRAVI)", 
         subtitle="Questi battiti superano TUTTE le soglie di somiglianza.", 
         x="Campioni", y="mV") +
    theme(legend.position="top", strip.text.y.left = element_text(angle=0, size=8, face="bold"), 
          panel.spacing.y = unit(0.5, "lines"))
  
  print(p)
  
} else {
  cat("Nessun falso negativo confermato.\n")
}

cat("Analisi Completata.\n")