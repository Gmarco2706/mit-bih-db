# ECG Synthesis and Forensic Analysis (MIT-BIH)

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![LaTeX](https://img.shields.io/badge/latex-%23008080.svg?style=for-the-badge&logo=latex&logoColor=white)
![LLM](https://img.shields.io/badge/GPT--5-Context--Aware-green?style=for-the-badge)

Questo repository contiene il workflow analitico per la caratterizzazione, il clustering e la generazione sintetica di segnali ECG derivanti dal MIT-BIH Arrhythmia Database. Il progetto valida l'efficacia dei modelli linguistici (LLM) nella generazione di dati medici coerenti tramite l'approccio In-Context Learning.

---

## Obiettivi del Progetto

* **Estrazione Feature**: Analisi morfologica dei canali MLII e V1 tramite il calcolo di Ampiezza, Skewness e Kurtosis.
* **Clustering Densità-Basato**: Utilizzo dell'algoritmo DBSCAN per raggruppare i battiti patologici (PVC) in cluster densi e isolare automaticamente il rumore clinico (outliers).
* **Scansione Forense**: Sistema di Anomaly Detection basato sulla correlazione di Pearson (logica pmin) e intervalli quantilici (0.15% - 99.85%) per la verifica della ground truth dei battiti sani.
* **Sintesi Statistica**: Generazione di dati sintetici senza addestramento (Zero-Shot), confrontando le performance di un approccio Context-Aware rispetto a una baseline priva di contesto.

---

## Struttura dei File

* **studio_dati.R**: Script dedicato al pre-processing, alla pulizia dei dati e all'analisi descrittiva dei segnali.
* **Cluster.R**: Implementazione del clustering DBSCAN, identificazione dei template fisiologici e suddivisione dei battiti PVC in raggruppamenti morfologici coerenti.
* **Forensic_Analysis.R**: Algoritmo per il rilevamento di eventuali falsi negativi all'interno della classe dei battiti normali.
* **Sintetici.R**: Analisi statistica comparativa tra i segnali reali e quelli generati dall'agente intelligente.
* **matrice_MLII_clean.csv / matrice_V1_clean.csv**: Dataset originali processati utilizzati come riferimento.

---

## Risultati Principali

1. **Suddivisione in Cluster**: L'analisi tramite DBSCAN ha permesso di mappare la variabilità dei battiti patologici, organizzandoli in cluster distinti che riflettono diverse morfologie d'onda, separando efficacemente i segnali core dal rumore.
2. **Validazione Ground Truth**: La scansione forense non ha rilevato PVC nascoste tra i battiti etichettati come normali, confermando l'affidabilità della classificazione originale e del filtraggio applicato.
3. **Qualità della Sintesi**: L'approccio Context-Aware ha prodotto segnali con una Kurtosis media di 1.07, in linea con il valore reale di 1.26, a differenza della baseline No Context che ha mostrato deviazioni significative (5.15).
4. **In-Context Learning**: I risultati dimostrano che la precisione del prompt e la fornitura di vincoli statistici permettono una generazione sintetica fedele senza necessità di effettuare il training del modello.

---

**Tecnologie utilizzate:** R, ggplot2, DBSCAN, LaTeX, GPT-5 (Agente Intelligente).