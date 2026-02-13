library(data.table)
library(dbscan)
library(plotly)
library(RColorBrewer)
library(e1071)
library(ggplot2)

# --- CONFIGURAZIONE ---
EPS_VAL   <- 8.2
PCA_COMPS <- 21
FS        <- 360

if(!exists("beat_ids")) beat_ids <- 1:nrow(matrice_V_MLII_clean)

# --- FUNZIONI---
calcola_wss <- function(X, labels) {
  wss <- 0
  for(k in unique(labels[labels!=0])) {
    p <- X[labels == k, , drop=FALSE]
    if(nrow(p) > 1) wss <- wss + sum(rowSums(sweep(p, 2, colMeans(p))^2))
  }
  return(wss)
}

genera_mappa_colori <- function(labels) {
  u <- sort(unique(labels[labels!=0]))
  cols <- if(length(u)<=8) brewer.pal(9,"Set1")[c(1:5,7:9)] else hcl.colors(length(u), "Dark 3")
  names(cols) <- as.character(u)
  return(cols[1:length(u)])
}

processa <- function(m) {
  amp <- apply(m, 1, function(x) max(x)-min(x))
  list(shape = m/(amp+1e-6), amp = amp)
}

# ==============================================================================
# 1. FEATURE & GLOBAL PCA (SUI DATI ESISTENTI)
# ==============================================================================
cat(">>> [1] Feature Extraction su matrici esistenti...\n")

stats <- function(m) data.table(K=apply(m,1,kurtosis), S=apply(m,1,skewness))

# Uso matrici in memoria
f_MLII <- processa(matrice_V_MLII_clean)
f_V1   <- processa(matrice_V_V1_clean)
s_MLII <- stats(matrice_V_MLII_clean)
s_V1   <- stats(matrice_V_V1_clean)

# Creazione matrice totale
input_total <- cbind(f_MLII$shape, f_V1$shape, 
                     f_MLII$amp, s_MLII, f_V1$amp, s_V1)

cat(">>> PCA Global (scale=TRUE)...\n")
pca <- prcomp(input_total, center=TRUE, scale.=TRUE)
print(summary(pca)$importance[,1:30])
X1 <- pca$x[, 1:PCA_COMPS]
K1 <- ncol(X1) * 2

# ==============================================================================
# 2. DBSCAN
# ==============================================================================
cat(sprintf(">>> [2] DBSCAN (EPS=%.2f, MinPts=%d)...\n", EPS_VAL, K1))
set.seed(123)
res <- dbscan(X1, eps=EPS_VAL, minPts=K1)
labs <- res$cluster

# ==============================================================================
# 3. OUTPUT 
# ==============================================================================

# A. STATISTICHE
n_noise <- sum(labs==0)
cat("\n------------------------------------------------\n")
cat(sprintf("CLUSTER TROVATI: %d (Rumore: %d | %.1f%%)\n", max(labs), n_noise, 100*n_noise/length(labs)))

if(max(labs) > 0) {
  # 1. WSS
  cat(sprintf("WSS (Compattezza): %.2f\n", calcola_wss(X1, labs)))
  
  # 2. CONTEGGIO
  cat("\nBATTITI PER CLUSTER:\n")
  counts <- data.table(Cluster=labs)[Cluster!=0, .N, by=Cluster][order(-N)]
  print(counts)
  cat("------------------------------------------------\n")
  
  # B. GRAFICI
  cols <- genera_mappa_colori(labs)
  
  # PLOT 3D
  print(plot_ly(data.table(P1=X1[,1], P2=X1[,2], P3=X1[,3], C=factor(labs))[C!=0], 
                x=~P1, y=~P2, z=~P3, color=~C, colors=cols, 
                type='scatter3d', mode='markers', marker=list(size=3)) %>% 
          layout(title="Clustering Global PCA"))
  
  # CENTROIDI
  cat(">>> Calcolo Centroidi...\n")
  
  centroidi <- list()
  df_plot_list <- list()
  
  # Ciclo sui cluster per calcolare la media direttamente dalla matrice
  u_cl <- sort(unique(labs[labs!=0]))
  
  # Creiamo asse temporale fittizio (o centrato)
  len_sig <- ncol(matrice_V_MLII_clean)
  t_axis <- (1:len_sig - (len_sig/2)) / FS 
  
  for(k in u_cl) {
    idx <- which(labs == k)
    # Calcolo media colonne (gestisce anche caso 1 solo elemento)
    if(length(idx)>1) {
      avg_MLII <- colMeans(matrice_V_MLII_clean[idx,])
      avg_V1   <- colMeans(matrice_V_V1_clean[idx,])
    } else {
      avg_MLII <- matrice_V_MLII_clean[idx,]
      avg_V1   <- matrice_V_V1_clean[idx,]
    }
    
    # --- PARTE AGGIUNTA FONDAMENTALE ---
    # Calcoliamo l'ampiezza di riferimento e salviamo nella lista 'centroidi'
    amp_ref <- max(avg_MLII) - min(avg_MLII)
    centroidi[[as.character(k)]] <- list(MLII = avg_MLII, V1 = avg_V1, Amp = amp_ref)
    # -----------------------------------
    
    df_plot_list[[paste0(k,"_M")]] <- data.table(Cluster=k, Canale="MLII", t=t_axis, mV=avg_MLII)
    df_plot_list[[paste0(k,"_V")]] <- data.table(Cluster=k, Canale="V1",   t=t_axis, mV=avg_V1)
  }
  
  df_melt <- rbindlist(df_plot_list)
  
  # PLOT FACET
  p <- ggplot(df_melt, aes(x=t, y=mV, color=factor(Cluster))) +
    geom_line(linewidth=1.2) +
    scale_color_manual(values=cols) +
    facet_wrap(Cluster ~ Canale, ncol=6, scales="free") + 
    theme_bw() +
    labs(title="Centroidi Cluster (Global PCA)", x="Tempo (s)", y="mV")
  
  print(p)
  
} else {
  cat("!!! NESSUN CLUSTER TROVATO.\n")
}