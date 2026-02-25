library(data.table)
library(dbscan)
library(plotly)
library(RColorBrewer)
library(e1071)
library(ggplot2)
library(cluster)
library(patchwork) # Fondamentale per unire i grafici Gomito e Silhouette

# --- CONFIGURAZIONE ---
EPS_VAL   <- 8.2
PCA_COMPS <- 21
FS        <- 360
K_OPT     <- 8 # Aggiunto per l'esecuzione del K-Means ottimale

if(!exists("beat_ids")) beat_ids <- 1:nrow(matrice_V_MLII_clean)

# --- FUNZIONI OTTIMIZZATE (PROGRAMMAZIONE FUNZIONALE) ---

calcola_wss <- function(X, labels) {
  cluster_validi <- unique(labels[labels != 0])
  if(length(cluster_validi) == 0) return(0)
  
  wss_list <- sapply(cluster_validi, function(k) {
    p <- X[labels == k, , drop=FALSE]
    if(nrow(p) > 1) return(sum(rowSums(sweep(p, 2, colMeans(p))^2)))
    return(0)
  })
  return(sum(wss_list))
}

genera_mappa_colori <- function(labels) {
  u <- sort(unique(labels[labels!=0]))
  if(length(u) == 0) return(c())
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
# 2. VALUTAZIONE K-MEANS (Elbow Method & Silhouette)
# ==============================================================================
cat("\n>>> [2] Valutazione preventiva K-Means (WCSS e Silhouette)...\n")

k_seq <- 2:15 # Range di cluster da testare
dist_X1 <- dist(X1)

# Esecuzione K-means iterativa tramite lapply (nstart=25 assicura robustezza)
kmeans_models <- lapply(k_seq, function(k) kmeans(X1, centers = k, nstart = 25))

# Estrazione WCSS e Silhouette con sapply
wcss_vals <- sapply(kmeans_models, function(km) km$tot.withinss)
sil_vals  <- sapply(kmeans_models, function(km) mean(silhouette(km$cluster, dist_X1)[, 3]))

df_kmeans <- data.table(K = k_seq, WCSS = wcss_vals, Silhouette = sil_vals)

# Grafico Gomito (WCSS)
p_elbow <- ggplot(df_kmeans, aes(x=K, y=WCSS)) + 
  geom_line(color="blue", linewidth=1) + geom_point(color="red", size=3) +
  scale_x_continuous(breaks=k_seq) +
  theme_bw() + labs(title="Metodo del Gomito (WCSS)", x="Numero di Cluster (k)", y="WCSS")

# Grafico Silhouette
p_sil <- ggplot(df_kmeans, aes(x=K, y=Silhouette)) + 
  geom_line(color="darkgreen", linewidth=1) + geom_point(color="orange", size=3) +
  scale_x_continuous(breaks=k_seq) +
  theme_bw() + labs(title="Analisi Silhouette Media", x="Numero di Cluster (k)", y="Silhouette Media")

print((p_elbow | p_sil) + plot_annotation(title = "Analisi Pre-Clustering: Ricerca K Ottimale"))

# ==============================================================================
# 3. ESECUZIONE K-MEANS (K OTTENUTO DALLA VALUTAZIONE)
# ==============================================================================
cat(sprintf("\n>>> [3] Esecuzione K-Means Standard con K = %d...\n", K_OPT))
set.seed(123)
res_kmeans <- kmeans(X1, centers = K_OPT, nstart = 25)
labs_km <- res_kmeans$cluster

cat("\n--- RISULTATI K-MEANS ---\n")
cat(sprintf("WSS (Compattezza): %.2f\n", res_kmeans$tot.withinss))
sil_km_obj <- silhouette(labs_km, dist_X1)
cat(sprintf("Silhouette Score Medio: %.4f\n", mean(sil_km_obj[, 3])))

cat("\nBATTITI PER CLUSTER (K-MEANS):\n")
counts_km <- data.table(Cluster=labs_km)[, .N, by=Cluster][order(-N)]
print(counts_km)

# PLOT 3D K-MEANS
cols_km <- genera_mappa_colori(labs_km)
print(plot_ly(data.table(P1=X1[,1], P2=X1[,2], P3=X1[,3], C=factor(labs_km)), 
              x=~P1, y=~P2, z=~P3, color=~C, colors=cols_km, 
              type='scatter3d', mode='markers', marker=list(size=3)) %>% 
        layout(title=sprintf("Clustering K-Means (K=%d)", K_OPT)))

# CENTROIDI K-MEANS (Isolati per non sovrascrivere quelli del DBSCAN)
cat(">>> Calcolo Centroidi K-Means...\n")
u_cl_km <- sort(unique(labs_km))

# Calcoliamo l'asse temporale qui, così vale per tutto il resto dello script
len_sig <- ncol(matrice_V_MLII_clean)
t_axis <- (1:len_sig - (len_sig/2)) / FS 

plot_data_km <- lapply(u_cl_km, function(k) {
  idx <- which(labs_km == k)
  if(length(idx) > 1) {
    avg_MLII <- colMeans(matrice_V_MLII_clean[idx, , drop=FALSE])
    avg_V1   <- colMeans(matrice_V_V1_clean[idx, , drop=FALSE])
  } else {
    avg_MLII <- matrice_V_MLII_clean[idx, ]
    avg_V1   <- matrice_V_V1_clean[idx, ]
  }
  
  rbind(
    data.table(Cluster=k, Canale="MLII", t=t_axis, mV=avg_MLII),
    data.table(Cluster=k, Canale="V1",   t=t_axis, mV=avg_V1)
  )
})

df_melt_km <- rbindlist(plot_data_km)

p_km <- ggplot(df_melt_km, aes(x=t, y=mV, color=factor(Cluster))) +
  geom_line(linewidth=1.2) +
  scale_color_manual(values=cols_km) +
  facet_wrap(Cluster ~ Canale, ncol=6, scales="free") + 
  theme_bw() +
  labs(title=sprintf("Centroidi Cluster K-Means (K=%d)", K_OPT), x="Tempo (s)", y="mV")
print(p_km)

# ==============================================================================
# 4. DBSCAN
# ==============================================================================
cat(sprintf("\n>>> [4] DBSCAN (EPS=%.2f, MinPts=%d)...\n", EPS_VAL, K1))
set.seed(123)
res <- dbscan(X1, eps=EPS_VAL, minPts=K1)
labs <- res$cluster

# ==============================================================================
# 5. OUTPUT E STATISTICHE DBSCAN
# ==============================================================================
n_noise <- sum(labs==0)
cat("\n------------------------------------------------\n")
cat(sprintf("CLUSTER TROVATI: %d (Rumore: %d | %.1f%%)\n", max(labs), n_noise, 100*n_noise/length(labs)))

if(max(labs) > 0) {
  # 1. WSS
  wss_val <- calcola_wss(X1, labs)
  cat(sprintf("WSS (Compattezza): %.2f\n", wss_val))
  
  # Calcoliamo solo sui punti assegnati (escludendo il rumore 0)
  idx_clustered <- which(labs != 0)
  
  if(length(unique(labs[idx_clustered])) > 1) {
    cat(">>> Calcolo Silhouette Score...\n")
    
    # Calcolo distanza euclidea solo sui punti clusterizzati
    dist_matrix_db <- dist(X1[idx_clustered, ])
    sil_obj <- silhouette(labs[idx_clustered], dist_matrix_db)
    avg_sil <- mean(sil_obj[, 3])
    cat(sprintf("Silhouette Score Medio: %.4f\n", avg_sil))
  } else {
    cat("Silhouette Score: Non calcolabile (Meno di 2 cluster validi trovati)\n")
  }
  # -------------------------------------
  
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
          layout(title="Clustering DBSCAN Global PCA"))
  
  # CENTROIDI
  cat(">>> Calcolo Centroidi DBSCAN...\n")
  
  u_cl <- sort(unique(labs[labs!=0]))
  
  risultati_centroidi <- lapply(u_cl, function(k) {
    idx <- which(labs == k)
    
    # drop=FALSE garantisce che se c'è un solo elemento, resti matrice ed eviti crash su colMeans
    if(length(idx) > 1) {
      avg_MLII <- colMeans(matrice_V_MLII_clean[idx, , drop=FALSE])
      avg_V1   <- colMeans(matrice_V_V1_clean[idx, , drop=FALSE])
    } else {
      avg_MLII <- matrice_V_MLII_clean[idx, ]
      avg_V1   <- matrice_V_V1_clean[idx, ]
    }
    
    amp_ref <- max(avg_MLII) - min(avg_MLII)
    
    dt_plot <- rbind(
      data.table(Cluster=k, Canale="MLII", t=t_axis, mV=avg_MLII),
      data.table(Cluster=k, Canale="V1",   t=t_axis, mV=avg_V1)
    )
    
    return(list(
      centroid_data = list(MLII = avg_MLII, V1 = avg_V1, Amp = amp_ref),
      plot_data = dt_plot
    ))
  })
  
  # Assegnazione alla lista 'centroidi' e nomi dinamici
  centroidi <- lapply(risultati_centroidi, function(x) x$centroid_data)
  names(centroidi) <- as.character(u_cl)
  
  df_melt <- rbindlist(lapply(risultati_centroidi, function(x) x$plot_data))
  
  # PLOT FACET
  p <- ggplot(df_melt, aes(x=t, y=mV, color=factor(Cluster))) +
    geom_line(linewidth=1.2) +
    scale_color_manual(values=cols) +
    facet_wrap(Cluster ~ Canale, ncol=6, scales="free") + 
    theme_bw() +
    labs(title="Centroidi Cluster (DBSCAN)", x="Tempo (s)", y="mV")
  
  print(p)
  
} else {
  cat("!!! NESSUN CLUSTER TROVATO CON DBSCAN.\n")
}