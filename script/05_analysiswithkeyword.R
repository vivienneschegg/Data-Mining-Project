# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 05: Theoriegeleitete Wörterbuchanalyse (Strategic Decoupling)
# ==============================================================================

library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)

# 1. Daten aus den vorherigen Schritten laden
# Wir benötigen das bereinigte tidy_corpus aus Skript 04
if (!exists("tidy_corpus")) {
  stop("FEHLER: 'tidy_corpus' wurde nicht im Workspace gefunden. Bitte Skript 04 zuerst ausführen!")
}

# 2. Erweitertes, bilinguales (DE/EN) theoretisches Wörterbuch
# Schlägt die Brücke zu Meyer & Rowan (1977) sowie Bromley & Powell (2012)
sustainability_dict <- data.frame(
  word = c(
    # --- INSTITUTIONELLE FASSADE (Abstract, value-based language) ---
    # Englische Begriffe
    "sustainability", "responsibility", "commitment", "values", "future", 
    "impact", "global", "mission", "vision", "transformation", "reimagine",
    "society", "social", "stewardship", "governance", "esg", "sdg", "purpose", 
    "leadership", "integrity", "strive", "generations",
    # Deutsche Entsprechungen (wichtig für den Schweizer Markt)
    "nachhaltigkeit", "verantwortung", "engagement", "werte", "zukunft",
    "wirkung", "global", "auftrag", "vision", "transformation", "gesellschaft",
    "sozial", "führung", "unternehmensführung", "wertegemeinschaft", "generationen",
    
    # --- TECHNISCHE SUBSTANZ (Operational, solution-oriented language) ---
    # Englische Begriffe
    "recycling", "technology", "solution", "carbon", "co2", "emissions", 
    "renewable", "energy", "efficiency", "circular", "material", "waste",
    "science", "innovation", "engineering", "robotics", "biological", "software",
    "hardware", "infrastructure", "production", "process", "biodegradable",
    # Deutsche Entsprechungen (wichtig für den Schweizer Markt)
    "recycling", "technologie", "lösung", "kohlenstoff", "emissionen",
    "erneuerbar", "energie", "effizienz", "zirkulär", "kreislaufwirtschaft",
    "material", "abfall", "wissenschaft", "innovation", "ingenieurwesen", "robotik",
    "biologisch", "produktion", "prozess", "verfahren", "umwelttechnik"
  ),
  category = c(
    rep("Institutional_Façade", 22 + 16), # Anzahl EN + DE Fassade
    rep("Technical_Substance", 23 + 21)  # Anzahl EN + DE Substanz
  ),
  stringsAsFactors = FALSE
)

# 3. Analyse der strategischen Entkopplung (Strategic Decoupling)
# Korrektur: Verwendung von 'type' statt 'group' passend zur CSV
decoupling_analysis <- tidy_corpus %>%
  inner_join(sustainability_dict, by = "word") %>%
  group_by(type, category) %>%
  tally() %>%
  group_by(type) %>%
  mutate(share = n / sum(n) * 100)

print("--- ERGEBNIS DER WÖRTERBUCHANALYSE (PROZENTUALE ANTEILE) ---")
print(decoupling_analysis)


# 4. Detaillierte Analyse auf Unternehmensebene
# Berechnet das Verhältnis von Fassade vs. Substanz für jedes der 45 Unternehmen
firm_level_decoupling <- tidy_corpus %>%
  inner_join(sustainability_dict, by = "word") %>%
  group_by(name, type, category) %>%
  tally() %>%
  group_by(name) %>%
  mutate(share = n / sum(n) * 100) %>%
  filter(category == "Technical_Substance") %>% # Wir isolieren den Substanz-Anteil
  select(name, type, technical_substance_share = share) %>%
  arrange(desc(technical_substance_share))

print("--- TOP 10 UNTERNEHMEN NACH TECHNISCHER SUBSTANZ (IN %) ---")
print(head(firm_level_decoupling, 10))


# 5. Laden und Bereitstellen des Isomorphie-Index aus Skript 04
# Hier nutzen wir die saubere Kosinus-Ähnlichkeit zum aggregierten Benchmark-Profil
if (file.exists("isomorphism_index_results.csv")) {
  final_similarity <- read_csv("isomorphism_index_results.csv")
  
  print("--- ISOMORPHIE-RANKING DER STARTUPS (DISTANZ ZU DEN GREEN GIANTS) ---")
  print(final_similarity)
} else {
  warning("Die Datei 'isomorphism_index_results.csv' wurde nicht gefunden. Das Ranking basiert auf dem Live-Workspace.")
}

# 6. Ergebnisse für das finale Visualisierungs-Skript exportieren
write_csv(decoupling_analysis, "decoupling_aggregate_results.csv")
write_csv(firm_level_decoupling, "decoupling_firm_results.csv")

print("Wörterbuch- und Entkopplungsdaten erfolgreich für Skript 06 exportiert!")