# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 04c: Isomorphie-Index ALTERNATIV mit TF-IDF-Gewichtung
# (statt roher Worthäufigkeiten), um zu prüfen, ob generisches, gruppenübergreifend
# geteiltes Vokabular den ursprünglichen Cosine-Wert künstlich aufbläht
#
# HINWEIS zur Abgrenzung von Skript 04a Teil 3: Dort wird eine global
# TF-IDF-gewichtete Firma-Wort-Matrix gegen eine Permutationsbaseline getestet
# (Frage: unterscheidet sich der TF-IDF-Isomorphie-Wert von Zufall?). Hier
# wird stattdessen pro Startup die TF-IDF-Kosinus-Ähnlichkeit zum aggregierten
# Incumbent-Profil berechnet und direkt mit dem rohen Wert aus Skript 04
# verglichen (Frage: verändert TF-IDF-Gewichtung Ranking/Werte pro Startup?).
#
# Voraussetzung: tidy_corpus, isomorphism_index, research_data
# (aus Skript 04) müssen im Workspace vorhanden sein.
# ==============================================================================

library(dplyr)
library(tidytext)
library(widyr)
library(readr)

if (!exists("tidy_corpus") | !exists("isomorphism_index") | !exists("research_data")) {
  stop("FEHLER: 'tidy_corpus', 'isomorphism_index' oder 'research_data' nicht im Workspace gefunden. Bitte Skript 04 zuerst ausführen!")
}

# ------------------------------------------------------------------------------
# 1. TF-IDF-Gewichtung pro Firma berechnen
# ------------------------------------------------------------------------------
tfidf_weighted <- tidy_corpus %>%
  count(name, word) %>%
  bind_tf_idf(word, name, n)

incumbent_profile_tfidf <- tfidf_weighted %>%
  filter(name %in% (research_data %>% filter(type == "Incumbent") %>% pull(name) %>% unique())) %>%
  group_by(word) %>%
  summarise(tf_idf = sum(tf_idf), .groups = "drop") %>%
  mutate(name = "AGGREGATED_INCUMBENT") %>%
  select(name, word, tf_idf)

startup_tfidf <- tfidf_weighted %>%
  filter(name %in% (research_data %>% filter(type == "Startup") %>% pull(name) %>% unique())) %>%
  select(name, word, tf_idf)

matrix_data_tfidf <- bind_rows(startup_tfidf, incumbent_profile_tfidf)

# ------------------------------------------------------------------------------
# 2. Paarweise Kosinus-Ähnlichkeit (TF-IDF-gewichtet)
# ------------------------------------------------------------------------------
full_similarity_tfidf <- matrix_data_tfidf %>%
  pairwise_similarity(name, word, tf_idf)

isomorphism_index_tfidf <- full_similarity_tfidf %>%
  filter(item2 == "AGGREGATED_INCUMBENT") %>%
  select(startup = item1, cosine_similarity_tfidf = similarity) %>%
  arrange(desc(cosine_similarity_tfidf))

isomorphism_index_tfidf <- isomorphism_index_tfidf %>%
  left_join(distinct(research_data %>% select(name, type)), by = c("startup" = "name"))

print("--- ISOMORPHIE-INDEX TF-IDF-GEWICHTET (zum Vergleich) ---")
print(isomorphism_index_tfidf, n = Inf)

# ------------------------------------------------------------------------------
# 3. Direkter Vergleich: hat sich das Ranking/die Werte stark verändert?
# ------------------------------------------------------------------------------
comparison_roh_vs_tfidf <- isomorphism_index %>%
  select(startup, cosine_similarity) %>%
  left_join(isomorphism_index_tfidf %>% select(startup, cosine_similarity_tfidf), by = "startup")

print("--- VERGLEICH ROH vs. TF-IDF-GEWICHTET (pro Startup) ---")
print(comparison_roh_vs_tfidf, n = Inf)

# ------------------------------------------------------------------------------
# 4. Export (comparison_roh_vs_tfidf neu ergänzt, damit Skript 06 darauf
#    zugreifen kann, ohne beide Vorgängerskripte erneut laufen lassen zu müssen)
# ------------------------------------------------------------------------------
write_csv(isomorphism_index_tfidf, "isomorphism_index_tfidf_results.csv")
write_csv(comparison_roh_vs_tfidf, "isomorphism_index_roh_vs_tfidf_comparison.csv")

print("-> Skript 04c abgeschlossen. Ergebnisse exportiert für Skript 06.")