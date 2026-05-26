# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 04: TF-IDF & Kosinus-Ähnlichkeit (Isomorphie-Index)
# ==============================================================================

library(jsonlite)
library(tidytext)
library(dplyr)
library(stopwords)
library(widyr)
library(ggplot2)
library(forcats)
library(tidyr)

# 1. Eingelesene JSON-Textdaten laden
if (file.exists("full_research_data.json")) {
  research_data <- fromJSON(readLines("full_research_data.json", warn = FALSE))
} else {
  stop("FEHLER: Die Datei 'full_research_data.json' wurde nicht gefunden! Bitte Skript 03 zuerst ausführen.")
}

# 2. Text-Korpus bereinigen und tokenisieren (Tidy-Format)
# Korrektur: Wir nutzen 'type' anstelle von 'group', passend zu deiner CSV-Struktur.
tidy_corpus <- research_data %>%
  unnest_tokens(word, content) %>%
  # Bilingualer Stopwort-Filter (Deutsch & Englisch)
  filter(!word %in% stopwords("de"), 
         !word %in% stopwords("en"),
         # Entfernt funktionale Web-Standardwörter, die das Ergebnis verzerren
         !word %in% c("cookie", "cookies", "privacy", "datenschutz", "impressum", "contact", "kontakt"),
         nchar(word) > 3,           # Nur Wörter mit mehr als 3 Buchstaben
         !grepl("[0-9]", word))     # Reine Zahlen entfernen

# 3. TF-IDF Analyse für beide Organisationsformen (Startups vs. Incumbents)
# Berechnet, welche Wörter statistisch charakteristisch für eine Gruppe sind
group_tf_idf <- tidy_corpus %>%
  count(type, word, sort = TRUE) %>%
  bind_tf_idf(word, type, n) %>%
  arrange(desc(tf_idf))

print("--- TOP 15 CHARAKTERISTISCHE WÖRTER FÜR STARTUPS (TF-IDF) ---")
print(head(group_tf_idf %>% filter(type == "Startup"), 15))

print("--- TOP 15 CHARAKTERISTISCHE WÖRTER FÜR ETABLIERTE UNTERNEHMEN (TF-IDF) ---")
print(head(group_tf_idf %>% filter(type == "Incumbent"), 15))


# 4. Mathematische Modellierung des Isomorphie-Index (Kosinus-Ähnlichkeit)
# Ziel: Linguistische Distanz jedes einzelnen Startups zum aggregierten Incumbent-Profil messen

# Schritt A: Erstellung des aggregierten "Benchmark-Profils" der etablierten Unternehmen
incumbent_profile <- tidy_corpus %>%
  filter(type == "Incumbent") %>%
  count(word) %>%
  mutate(name = "AGGREGATED_INCUMBENT") %>%
  select(name, word, n)

# Schritt B: Worthäufigkeiten für jedes einzelne Startup berechnen
startup_counts <- tidy_corpus %>%
  filter(type == "Startup") %>%
  count(name, word) %>%
  select(name, word, n)

# Schritt C: Zusammenführung für die Vektorraum-Matrix
matrix_data <- bind_rows(startup_counts, incumbent_profile)

# Schritt D: Berechnung der paarweisen Kosinus-Ähnlichkeit über die Wortvektoren
full_similarity <- matrix_data %>%
  pairwise_similarity(name, word, n)

# Schritt E: Filterung auf die für deine Forschungsfrage relevante Relation
# Wir wollen NUR die Ähnlichkeit JEDES Startups zum AGGREGATED_INCUMBENT isolieren
isomorphism_index <- full_similarity %>%
  filter(item2 == "AGGREGATED_INCUMBENT") %>%
  select(startup = item1, cosine_similarity = similarity) %>%
  arrange(desc(cosine_similarity))

# Organisationstyp für das spätere Plotten wieder hinzufügen (Sicherheits-Check)
isomorphism_index <- isomorphism_index %>%
  left_join(distinct(research_data %>% select(name, type)), by = c("startup" = "name"))

print("--- DER MIMETISCHE ISOMORPHIE-INDEX (RANKING DER STARTUPS) ---")
print(isomorphism_index)


# 5. Datensätze für die nächsten Pipeline-Schritte sichern
write_csv(group_tf_idf, "group_tf_idf_results.csv")
write_csv(isomorphism_index, "isomorphism_index_results.csv")

print("Analysedaten erfolgreich exportiert! Bereit für die Skripte 05 (Dictionary) und 06 (Plots).")