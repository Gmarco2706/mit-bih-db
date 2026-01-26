library(data.table)
library(ggplot2)
library(dbscan)   # Algoritmo per il clustering basato su densità
library(cluster)  # Calcolo delle metriche di validazione (Silhouette Score)
library(plotly)

# --- 0. IMPOSTAZIONI GRAFICHE ---
theme_ecg <- theme_bw() + 
  theme(panel.grid.major = element_line(color = "#ff9999", linewidth = 0.5),
        panel.grid.minor = element_line(color = "#ffcccc", linewidth = 0.25),
        strip.background = element_rect(fill="#f0f0f0"),
        plot.title = element_text(face="bold"))

# ==============================================================================
# 1. CARICAMENTO DATI
# ==============================================================================
file_input <- "../pvc_segmentati_dataset.csv"

if (!file.exists(file_input)) stop("ERRORE: File non trovato. Esegui prima lo Script 1.")

cat("--- CARICAMENTO DATASET ---\n")
df_waves <- fread(file_input)
cat(paste("Caricati", length(unique(df_waves$Beat_ID)), "battiti unici.\n"))

if (!"V1" %in% names(df_waves)) {
  stop("ERRORE CRITICO: Il dataset non contiene la colonna 'V1'. \nDevi rieseguire lo Script 1 filtrando i pazienti che hanno MLII + V1.")
}

# ==============================================================================
# 2. PREPARAZIONE MATRICI (VERSIONE MULTICANALE)
# ==============================================================================
cat("Creazione matrici morfologiche (Doppio Canale)...\n")

matrice_MLII_dt <- dcast(df_waves, Beat_ID ~ Tempo_Locale, value.var = "MLII")
matrice_V1_dt   <- dcast(df_waves, Beat_ID ~ Tempo_Locale, value.var = "V1")

beat_ids <- matrice_MLII_dt$Beat_ID

raw_MLII <- as.matrix(matrice_MLII_dt[, -1])
raw_V1   <- as.matrix(matrice_V1_dt[, -1])

# ==============================================================================
# 3. ESTRAZIONE FEATURE (ANALISI SPAZIALE MLII + V1)
# ==============================================================================
cat("Estrazione Feature (Analisi Spaziale MLII + V1)...\n")

processa_canale <- function(matrice_raw) {
  # Baseline Correction (Mediana primi 20 campioni)
  baseline <- apply(matrice_raw[, 1:20], 1, median)
  centered <- matrice_raw - baseline
  
  # Estrazione Ampiezza (Peak-to-Peak)
  amp <- apply(centered, 1, function(x) max(x) - min(x))
  
  # Normalizzazione Forma (Shape pura)
  norm_shape <- centered / (amp + 1e-6)
  
  return(list(shape = norm_shape, amp = amp))
}

feat_MLII <- processa_canale(raw_MLII)
feat_V1   <- processa_canale(raw_V1)

# Concatenazione delle forme per PCA: [Forma_MLII | Forma_V1]
matrice_combinata <- cbind(feat_MLII$shape, feat_V1$shape)

# PCA (Riduzione Dimensionale)
pca_res <- prcomp(matrice_combinata, center = TRUE, scale. = FALSE)

# STAMPA SUMMARY PCA (Analisi della varianza spiegata)
print(summary(pca_res))

# Selezione delle prime componenti per la morfologia globale
X_pca <- pca_res$x[, 1:7]

# Dataset finale per clustering: [Componenti Forma] + [Ampiezze Canali]
X_final <- cbind(X_pca, Amp_MLII = feat_MLII$amp, Amp_V1 = feat_V1$amp)

# Standardizzazione per DBSCAN
X_cluster_input <- scale(X_final)

cat("Dataset Multicanale pronto per il clustering.\n")

# ==============================================================================
# 4. ESECUZIONE CLUSTERING (DBSCAN) E VALIDAZIONE
# ==============================================================================
cat("\n--- ESECUZIONE CLUSTERING (DBSCAN) ---\n")

set.seed(123)
res_dbscan <- dbscan(X_cluster_input, eps = 1.5, minPts = 15)
labels <- res_dbscan$cluster

# Calcolo Silhouette Score (Escludendo il rumore, label 0)
mask_validi <- labels != 0

if(sum(mask_validi) > 0) {
  sil <- silhouette(labels[mask_validi], dist(X_cluster_input[mask_validi,]))
  avg_sil <- mean(sil[, 3])
  
  cat("\n>>> RISULTATO VALIDAZIONE (RQ2) <<<\n")
  cat(paste("Silhouette Score:", round(avg_sil, 3), "\n"))
  cat("INTERPRETAZIONE:\n > 0.5: Struttura Pattern Forte\n < 0.2: Struttura Debole\n")
} else {
  cat("ATTENZIONE: Trovato solo rumore. Aumenta 'eps'.\n")
}

n_clusters <- length(unique(labels[labels != 0]))
cat(paste("\nPattern trovati:", n_clusters, "\n"))
cat(paste("Battiti scartati come Rumore:", sum(labels == 0), "\n"))

# ==============================================================================
# 5. CARATTERIZZAZIONE PATTERN (GRAFICI E STATISTICHE)
# ==============================================================================

map_cluster <- data.table(Beat_ID = beat_ids, Cluster = labels)
df_clustered <- merge(df_waves, map_cluster, by = "Beat_ID")
df_clean <- df_clustered[Cluster != 0]

# Tabella frequenze
stats <- unique(df_clean[, .(Beat_ID, Cluster)])[, .N, by = Cluster]
stats[, Pct := round(N / sum(N) * 100, 1)]
setorder(stats, -N)

cat("\n>>> RISULTATO FREQUENZA PATTERN (RQ1) <<<\n")
print(stats)

# --- PLOT 1: CENTROIDI MULTICANALE ---
df_centroids <- df_clean[, .(MLII = mean(MLII), V1 = mean(V1)), 
                         by = .(Cluster, Tempo_Locale)]

df_centroids_long <- melt(df_centroids, 
                          id.vars = c("Cluster", "Tempo_Locale"), 
                          measure.vars = c("MLII", "V1"),
                          variable.name = "Canale", value.name = "Ampiezza")

p1 <- ggplot(df_centroids_long, aes(x = (Tempo_Locale+120)/360, y = Ampiezza)) +
  theme_ecg +
  geom_line(aes(color = factor(Cluster)), linewidth = 1.2) +
  facet_grid(Cluster ~ Canale, scales = "free_y", labeller = label_both) +
  labs(title = "RQ1: Centroidi dei Pattern Identificati",
       subtitle = "Morfologia media su MLII e V1 per ciascun cluster",
       x = "Tempo (s)", y = "Ampiezza Media (Z-Score)", 
       color = "Pattern ID")

print(p1)

df_scatter_3d <- data.table(
  PC1 = X_pca[, 1],
  PC2 = X_pca[, 2],
  PC3 = X_pca[, 3],
  Cluster = factor(labels)
)

# Rimuoviamo il rumore (Cluster 0) per una visualizzazione pulita dei pattern
df_scatter_3d_clean <- df_scatter_3d[Cluster != "0"]

cat("\nGenerazione del grafico interattivo 3D in corso...\n")

# Creazione del plot 3D interattivo
p3 <- plot_ly(df_scatter_3d_clean, 
              x = ~PC1, 
              y = ~PC2, 
              z = ~PC3, 
              color = ~Cluster, 
              colors = "Set1", # Palette di colori distinta per i cluster
              type = 'scatter3d', 
              mode = 'markers',
              marker = list(size = 3, opacity = 0.6)) %>%
  layout(title = "RQ1: Mappa 3D dei Pattern (PCA Space)",
         scene = list(
           xaxis = list(title = 'PC1 (Forma)'),
           yaxis = list(title = 'PC2 (Forma)'),
           zaxis = list(title = 'PC3 (Forma)')
         ),
         margin = list(l = 0, r = 0, b = 0, t = 40))

# Visualizzazione del grafico
print(p3)