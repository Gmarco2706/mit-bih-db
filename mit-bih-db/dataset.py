import wfdb
import pandas as pd
import os
import csv
import re

# --- CONFIGURAZIONE ---
DATA_PATH = 'mitdb' 
OUTPUT_FILE = 'mit_bih_dataset_puro.csv'

records = [
    '100', '101', '102', '103', '104', '105', '106', '107', '108', '109',
    '111', '112', '113', '114', '115', '116', '117', '118', '119', '121',
    '122', '123', '124', '200', '201', '202', '203', '205', '207', '208',
    '209', '210', '212', '213', '214', '215', '217', '219', '220', '221',
    '222', '223', '228', '230', '231', '232', '233', '234'
]

target_leads = ['MLII', 'V1', 'V2', 'V4', 'V5']
beat_symbols = ['N', 'L', 'R', 'B', 'A', 'a', 'J', 'S', 'V', 'r', 'F', 'e', 'j', 'n', 'E', '/', 'f', 'Q', '?']

def get_clean_meds(comments):
    """Estrae ESCLUSIVAMENTE i farmaci dai commenti"""
    age, sex, meds = '', '', ''
    if not comments:
        return age, sex, meds

    # 1. Estrazione Età e Sesso dalla prima riga
    first_line = comments[0]
    match = re.search(r'(\d+)\s+([MF])', first_line)
    if match:
        age = match.group(1)
        sex = match.group(2)
    
    # 2. Estrazione Medicine: nel MIT-BIH sono tipicamente nella riga 2
    # Se la riga contiene 'None', la consideriamo vuota.
    if len(comments) > 1:
        potential_meds = comments[1].strip()
        if potential_meds.lower() != 'none':
            meds = potential_meds
            
    return age, sex, meds

def build_dataset():
    if os.path.exists(OUTPUT_FILE):
        os.remove(OUTPUT_FILE)
        
    is_first = True
    print(f"Generazione dataset in corso...")

    for rec in records:
        path = os.path.join(DATA_PATH, rec)
        if not os.path.exists(path + '.dat'): continue

        try:
            signals, fields = wfdb.rdsamp(path)
            ann = wfdb.rdann(path, 'atr')
            
            age, sex, meds = get_clean_meds(fields['comments'])

            # Creazione DataFrame
            df = pd.DataFrame(index=range(len(signals)))
            df['Paziente'] = rec
            df['sample_index'] = range(len(df))
            df['età'] = age
            df['sesso'] = sex
            df['medicine'] = meds
            
            # Inizializza colonne lead richieste
            for lead in target_leads:
                df[lead] = ''
            
            # Assegna segnali se presenti
            for i, sig_name in enumerate(fields['sig_name']):
                if sig_name in target_leads:
                    df[sig_name] = signals[:, i]

            # Inserimento Picco (Solo battiti)
            df['Picco'] = ''
            # Usiamo un approccio veloce per mappare i simboli
            ann_map = {sample: symbol for sample, symbol in zip(ann.sample, ann.symbol) if symbol in beat_symbols}
            
            # Creiamo una serie temporanea per le label e la uniamo
            # Questo è molto più veloce del ciclo .at[] su 30 milioni di righe
            df.loc[df['sample_index'].isin(ann_map.keys()), 'Picco'] = df['sample_index'].map(ann_map)

            # Ordinamento colonne richiesto
            cols = ['MLII', 'V1', 'V2', 'V4', 'V5', 'Paziente', 'sample_index', 'sesso', 'età', 'medicine', 'Picco']
            df = df[cols]

            # Scrittura CSV
            df.to_csv(OUTPUT_FILE, index=False, mode='a', header=is_first, quoting=csv.QUOTE_ALL, escapechar='\\')
            is_first = False
            
            print(f"Record {rec} completato. Medicine trovate: {meds if meds else 'Nessuna'}")

        except Exception as e:
            print(f"Errore record {rec}: {e}")

    print(f"\nFINITO! Il file {OUTPUT_FILE} è pronto.")

if __name__ == "__main__":
    build_dataset()