library(data.table)
library(ggplot2)
library(reshape2)
library(e1071)

# ==============================================================================
# CONFIGURAZIONE SOGLIE (APPROCCIO QUANTILI)
# ==============================================================================
# 1. FORMA
SOGLIA_SOSPETTO <- 0.80   # Correlazione > 80% su ENTRAMBI i canali

# 2. QUANTILI
Q_LOW  <- 0.0015
Q_HIGH <- 0.9985 

# CALCOLO DINAMICO DELLE PERCENTUALI PER LE STAMPE E I GRAFICI
PERC_LOW <- Q_LOW * 100
PERC_HIGH <- Q_HIGH * 100
PERC_INTERVAL <- (Q_HIGH - Q_LOW) * 100

FS <- 360

# ==============================================================================
# STEP 0: CONTROLLO DATI
# ==============================================================================
cat(">>> [STEP 0] Verifica dati...\n")
if(!exists("centroidi")) stop("Mancano i 'centroidi'.")
if(!exists("labs")) stop("Manca il vettore 'labs'.")
if(!exists("matrice_V_MLII_clean") | !exists("matrice_V_V1_clean")) stop("Mancano le matrici originali.")

# ==============================================================================
# STEP 1: PREPARAZIONE METRICHE SU 'N' (DOPPIO CANALE)
# ==============================================================================
cat(">>> [STEP 1] Calcolo Metriche su Battiti N...\n")

ids_N   <- 1:nrow(matrice_N_MLII)

# B. Calcolo Ampiezze N
cat("    - Calcolo Ampiezze MLII e V1 su N...\n")
amps_N_M <- apply(matrice_N_MLII, 1, function(x) max(x) - min(x))
amps_N_V <- apply(matrice_N_V1, 1, function(x) max(x) - min(x))

# C. Calcolo Kurtosis e Skewness N
cat("    - Calcolo Statistiche MLII e V1 su N...\n")
K_N_M <- apply(matrice_N_MLII, 1, kurtosis, type=2) 
S_N_M <- apply(matrice_N_MLII, 1, skewness, type=2)
K_N_V <- apply(matrice_N_V1, 1, kurtosis, type=2) 
S_N_V <- apply(matrice_N_V1, 1, skewness, type=2)

cat(">>> Profilazione Statistica Completa (Ampiezza, Skew, Kurt)...\n")
cat("    - Calcolo metriche su tutte le PVC originali per costruire i riferimenti...\n")

# 1. Calcoliamo TUTTO per le PVC originali (V)
# Ampiezza
A_V_all_M <- apply(matrice_V_MLII_clean, 1, function(x) max(x)-min(x))
A_V_all_V <- apply(matrice_V_V1_clean, 1, function(x) max(x)-min(x))
# Kurtosis
K_V_all_M <- apply(matrice_V_MLII_clean, 1, kurtosis, type=2)
K_V_all_V <- apply(matrice_V_V1_clean, 1, kurtosis, type=2)
# Skewness
S_V_all_M <- apply(matrice_V_MLII_clean, 1, skewness, type=2)
S_V_all_V <- apply(matrice_V_V1_clean, 1, skewness, type=2)

cluster_stats <- list()
nomi_cluster <- names(centroidi)

for(k in nomi_cluster) {
  idx_k <- which(labs == as.numeric(k))
  
  # Salviamo i quantili Inferiore e Superiore per TUTTE le 6 variabili
  cluster_stats[[k]] <- list(
    # --- MLII ---
    AmpM_qL = quantile(A_V_all_M[idx_k], probs=Q_LOW), 
    AmpM_qH = quantile(A_V_all_M[idx_k], probs=Q_HIGH),
    #---
    KM_qL   = quantile(K_V_all_M[idx_k], probs=Q_LOW), 
    KM_qH   = quantile(K_V_all_M[idx_k], probs=Q_HIGH),
    #---
    SM_qL   = quantile(S_V_all_M[idx_k], probs=Q_LOW), 
    SM_qH   = quantile(S_V_all_M[idx_k], probs=Q_HIGH),
    
    # --- V1 ---
    AmpV_qL = quantile(A_V_all_V[idx_k], probs=Q_LOW), 
    AmpV_qH = quantile(A_V_all_V[idx_k], probs=Q_HIGH),
    #---
    KV_qL   = quantile(K_V_all_V[idx_k], probs=Q_LOW), 
    KV_qH   = quantile(K_V_all_V[idx_k], probs=Q_HIGH),
    #---
    SV_qL   = quantile(S_V_all_V[idx_k], probs=Q_LOW), 
    SV_qH   = quantile(S_V_all_V[idx_k], probs=Q_HIGH)
  )
}


# --- BLOCCO GRAFICO: VISUALIZZAZIONE DISTRIBUZIONI (Quantili) ---
cat(">>> Generazione Istogrammi Statistici per Cluster...\n")

# 1. Prepariamo i dati delle distribuzioni
df_dist <- data.frame(
  Cluster = labs,
  # MLII
  Amp_M = A_V_all_M, 
  Kur_M = K_V_all_M, 
  Skw_M = S_V_all_M,
  # V1
  Amp_V = A_V_all_V, 
  Kur_V = K_V_all_V, 
  Skw_V = S_V_all_V
)

# 2. Ciclo per ogni Cluster
for(k in nomi_cluster) {
  
  # A. Dati del Cluster corrente
  df_k <- subset(df_dist, Cluster == as.numeric(k))
  stats <- cluster_stats[[k]]
  
  # B. Creiamo gli Istogrammi delle Statistiche (6 pannelli)
  df_k_long <- melt(df_k[, -1]) # Rimuovi colonna Cluster dal melt
  
  # Creazione dataframe per le linee di riferimento
  df_lines <- data.frame(
    variable = c("Amp_M", "Kur_M", "Skw_M", "Amp_V", "Kur_V", "Skw_V"),
    q_low  = c(stats$AmpM_qL, stats$KM_qL, stats$SM_qL, stats$AmpV_qL, stats$KV_qL, stats$SV_qL),
    q_high = c(stats$AmpM_qH, stats$KM_qH, stats$SM_qH, stats$AmpV_qH, stats$KV_qH, stats$SV_qH)
  )
  
  p_hist <- ggplot(df_k_long, aes(x=value)) +
    geom_histogram(aes(y=..density..), bins=30, fill="skyblue", color="black", alpha=0.6) +
    geom_density(color="darkblue", size=1) +
    geom_vline(data=df_lines, aes(xintercept=q_low), color="red", linetype="dashed", size=1) +
    geom_vline(data=df_lines, aes(xintercept=q_high), color="red", linetype="dashed", size=1) +
    facet_wrap(~variable, scales="free", ncol=3) +
    theme_bw() +
    # STRINGA DINAMICA NEL SOTTOTITOLO
    labs(title=paste("Distribuzione Statistica Cluster", k), 
         subtitle=paste0("Linee Rosse: Intervallo di Tolleranza Empirico (", PERC_INTERVAL, "% dei dati)"),
         x="Valore Metrica", y="Densità")
  
  print(p_hist)
}
# ----------------------------------------------------------------------

# ==============================================================================
# STEP 2: SCANSIONE FORENSE
# ==============================================================================
cat(">>> [STEP 2] Scansione Forense...\n")

classifica <- data.frame()

for(k in nomi_cluster) {
  cent  <- centroidi[[k]]
  stats <- cluster_stats[[k]] 
  
  len_sig <- min(ncol(matrice_N_MLII), length(cent$MLII))
  
  # 1. Dati Segmentati
  seg_N_M <- matrice_N_MLII[, 1:len_sig]; 
  tpl_M <- cent$MLII[1:len_sig]
  seg_N_V <- matrice_N_V1[, 1:len_sig]; 
  tpl_V <- cent$V1[1:len_sig]
  
  # 2. Correlazione (Minimo dei due canali)
  avg_corr <- pmin(as.vector(cor(t(seg_N_M), tpl_M)), as.vector(cor(t(seg_N_V), tpl_V)))
  
  # 3. TEST DEI QUANTILI
  pass_AmpM <- (amps_N_M >= stats$AmpM_qL) & (amps_N_M <= stats$AmpM_qH)
  pass_AmpV <- (amps_N_V >= stats$AmpV_qL) & (amps_N_V <= stats$AmpV_qH)
  pass_KM <- (K_N_M >= stats$KM_qL) & (K_N_M <= stats$KM_qH)
  pass_KV <- (K_N_V >= stats$KV_qL) & (K_N_V <= stats$KV_qH)
  pass_SM <- (S_N_M >= stats$SM_qL) & (S_N_M <= stats$SM_qH)
  pass_SV <- (S_N_V >= stats$SV_qL) & (S_N_V <= stats$SV_qH)
  
  # 4. AGGREGAZIONE LOGICA
  punteggio_pass <- pass_AmpM + pass_AmpV + pass_KM + pass_KV + pass_SM + pass_SV
  ha_passato_tutto <- (punteggio_pass == 6)
  
  classifica <- rbind(classifica, data.frame(
    Beat_ID = ids_N,
    Cluster_PVC = as.numeric(k),
    Correlazione = avg_corr,
    Passa_Statistica = ha_passato_tutto
  ))
}

dt_class <- as.data.table(classifica)

# ==============================================================================
# STEP 5: FILTRO COLPEVOLI (CRITERIO UNICO)
# ==============================================================================
dt_best_match <- dt_class[order(Beat_ID, -Correlazione), .SD[1], by=Beat_ID]

dt_colpevoli <- dt_best_match[
  Correlazione > SOGLIA_SOSPETTO & 
    Passa_Statistica == TRUE
]

n_tot <- nrow(dt_best_match)
n_colp <- nrow(dt_colpevoli)

cat("\n============================================================\n")
cat(" REPORT FORENSE\n")
cat("============================================================\n")
cat(sprintf("Totale Battiti 'N' Scansionati: %d\n", n_tot))
cat("CRITERI DI ACCUSA:\n")
cat(sprintf("  1. Correlazione > %.0f%%\n", SOGLIA_SOSPETTO*100))
# STRINGHE DINAMICHE NEL REPORT IN CONSOLE
cat(sprintf("  2. COERENZA STATISTICA TOTALE: Intervallo Quantilico (%.2f%% - %.2f%%)\n", PERC_LOW, PERC_HIGH))
cat(sprintf("      (Il battito deve rientrare nel %g%% della distribuzione empirica per:\n", PERC_INTERVAL))
cat("       Ampiezza, Skewness e Curtosi sui due i canali)\n")
cat("------------------------------------------------------------\n")
cat(sprintf("CANDIDATI SELEZIONATI: %d (%.3f%%)\n", n_colp, (n_colp/n_tot)*100))
cat("============================================================\n")


# ==============================================================================
# STEP 6: VISUALIZZAZIONE "COLPEVOLI"
# ==============================================================================
if(n_colp > 0) {
  cat("\n>>> Generazione Grafico Casi Confermati...\n")
  
  n_show <- min(n_colp, 12) 
  lista_plot_data <- list()
  
  for(i in 1:n_show) {
    bid   <- dt_colpevoli$Beat_ID[i]
    clust <- dt_colpevoli$Cluster_PVC[i]
    cent  <- centroidi[[as.character(clust)]]
    len_p <- min(ncol(matrice_N_MLII), length(cent$MLII))
    
    # ETICHETTA DEL PANNELLO DINAMICA (es: PASS (99.7%))
    lbl <- paste0("CASO ", i, "\nBeat ", bid, " (Cl ", clust, ")\n",
                  "Corr: ", round(dt_colpevoli$Correlazione[i]*100,0), "%\n",
                  "Statistica: PASS (", PERC_INTERVAL, "%)")
    
    df_temp <- rbind(
      data.frame(Time=1:len_p, mV=matrice_N_MLII[bid, 1:len_p], Canale="MLII", Tipo="Recuperato (N)", Panel=lbl),
      data.frame(Time=1:len_p, mV=matrice_N_V1[bid, 1:len_p], Canale="V1",   Tipo="Recuperato (N)", Panel=lbl),
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
    # TITOLI E SOTTOTITOLI DINAMICI
    labs(title = "CANDIDATI FALSI NEGATIVI (QUANTILI EMPIRICI)", 
         subtitle=paste0("Battiti simili al cluster (Ampiezza e Forma entro il ", PERC_INTERVAL, "% empirico)."), 
         x="Campioni", y="mV") +
    theme(legend.position="top", strip.text.y.left = element_text(angle=0, size=7, face="bold"), 
          panel.spacing.y = unit(0.5, "lines"))
  
  print(p)
  
} else {
  cat("Nessun candidato trovato.\n")
}
cat("Analisi Completata.\n")