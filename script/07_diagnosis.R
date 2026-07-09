# ==============================================================================
# DIAGNOSESKRIPT: Warum erscheinen nur 8 von 30 Start-ups im Isomorphie-Index?
# Voraussetzung: firms_data3_final (Skript 02), research_data (Skript 04),
# tidy_corpus (Skript 04) und isomorphism_index (Skript 04) müssen im
# Workspace vorhanden sein.
# ==============================================================================

library(dplyr)

# ------------------------------------------------------------------------------
# 0. STUFE A: Wie viele der 30 Start-ups haben nach dem CRAWLER (Skript 02)
#    überhaupt mindestens eine Unterseite gefunden?
#    Prüft: Fällt die Firma schon VOR dem Scraping raus (find_company_subpages
#    gab NA zurück)?
# ------------------------------------------------------------------------------
if (exists("firms_data3_final")) {
  startup_coverage_crawler <- firms_data3_final %>%
    filter(type == "Startup") %>%
    distinct(name) %>%
    nrow()
  
  cat("STUFE A - Start-ups mit >=1 gecrawlter Unterseite (Skript 02):",
      startup_coverage_crawler, "von 30\n\n")
} else {
  cat("STUFE A: 'firms_data3_final' nicht im Workspace - Skript 02 zuerst laufen lassen.\n\n")
}

# ------------------------------------------------------------------------------
# 1. STUFE B: Wie viele Start-ups sind im Rohdatensatz NACH dem SCRAPING
#    (Skript 03) vorhanden (vor Tokenisierung)?
#    Prüft: Ist das Scraping selbst (GET-Request, Status-Code) unvollständig?
# ------------------------------------------------------------------------------
startup_coverage_raw <- research_data %>%
  filter(type == "Startup") %>%
  distinct(name) %>%
  nrow()

cat("Start-ups im Rohdatensatz (research_data):", startup_coverage_raw, "von 30\n\n")

# ------------------------------------------------------------------------------
# 2. Inhaltslänge pro Start-up (nchar-basiert), VOR der Tokenisierung
#    Prüft: Gibt es Start-ups mit auffällig wenig gescraptem Text?
# ------------------------------------------------------------------------------
content_length_startups <- research_data %>%
  filter(type == "Startup") %>%
  mutate(content_nchar = nchar(content)) %>%
  select(name, content_nchar) %>%
  arrange(content_nchar)

cat("--- Inhaltslänge (nchar) pro Start-up, aufsteigend sortiert ---\n")
print(content_length_startups)
cat("\n")

# ------------------------------------------------------------------------------
# 3. Wie viele Start-ups sind NACH Tokenisierung/Bereinigung noch im tidy_corpus?
#    Prüft: Verschwinden Start-ups durch den Bereinigungsfilter
#    (Stopwords, nchar > 3, Zahlen raus)?
# ------------------------------------------------------------------------------
startup_coverage_tidy <- tidy_corpus %>%
  filter(type == "Startup") %>%
  distinct(name) %>%
  nrow()

cat("Start-ups im tidy_corpus (nach Tokenisierung/Bereinigung):",
    startup_coverage_tidy, "von 30\n\n")

# ------------------------------------------------------------------------------
# 4. Wortanzahl pro Start-up NACH Tokenisierung/Bereinigung
#    Prüft: Wie "dünn" ist das bereinigte Vokabular pro Start-up wirklich?
# ------------------------------------------------------------------------------
word_count_per_startup <- tidy_corpus %>%
  filter(type == "Startup") %>%
  count(name, sort = FALSE, name = "n_words_cleaned") %>%
  arrange(n_words_cleaned)

cat("--- Anzahl bereinigter Wörter pro Start-up, aufsteigend sortiert ---\n")
print(word_count_per_startup)
cat("\n")

# ------------------------------------------------------------------------------
# 5. Überlappung mit dem Incumbent-Vokabular
#    Kernprüfung: pairwise_similarity() liefert nur Paare mit mind. 1
#    gemeinsamem Wort. Start-ups ohne jegliche Überlappung fallen daher
#    automatisch aus dem Isomorphie-Index heraus.
# ------------------------------------------------------------------------------
incumbent_vocab <- tidy_corpus %>%
  filter(type == "Incumbent") %>%
  distinct(word) %>%
  pull(word)

overlap_check <- tidy_corpus %>%
  filter(type == "Startup") %>%
  distinct(name, word) %>%
  mutate(in_incumbent_vocab = word %in% incumbent_vocab) %>%
  group_by(name) %>%
  summarise(
    total_unique_words = n(),
    overlapping_words = sum(in_incumbent_vocab),
    overlap_share_pct = round(100 * overlapping_words / total_unique_words, 1),
    .groups = "drop"
  ) %>%
  arrange(overlapping_words)

cat("--- Überlappung jedes Start-ups mit dem Incumbent-Vokabular ---\n")
cat("(0 überlappende Wörter = kann NICHT im Isomorphie-Index erscheinen)\n")
print(overlap_check)
cat("\n")

# ------------------------------------------------------------------------------
# 6. Direkter Abgleich: Welche Start-ups fehlen tatsächlich im Isomorphie-Index?
# ------------------------------------------------------------------------------
if (exists("isomorphism_index")) {
  missing_startups <- overlap_check %>%
    filter(!name %in% isomorphism_index$startup)
  
  cat("--- Start-ups OHNE Eintrag im Isomorphie-Index ---\n")
  print(missing_startups)
  
  cat("\nZusammenfassung:\n")
  cat(nrow(missing_startups), "von", nrow(overlap_check),
      "Start-ups fehlen im Isomorphie-Index.\n")
  cat("Davon mit 0 überlappenden Wörtern (technisch unmöglich, aufzutauchen):",
      sum(missing_startups$overlapping_words == 0), "\n")
} else {
  cat("Hinweis: 'isomorphism_index' nicht im Workspace gefunden.",
      "Bitte Skript 04 zuerst vollständig ausführen.\n")
}

# ==============================================================================
# ANALOGE PRÜFUNG FÜR INCUMBENTS (n=15 laut Skript 01)
# Hintergrund: desc_stats aus Skript 05 zeigt N=9 für Incumbents,
# obwohl die Stichprobe 15 Incumbents umfasst. Prüft, an welcher Stufe
# die fehlenden 6 Incumbents verschwinden.
# ==============================================================================

cat("\n\n================ ANALOGE PRÜFUNG: INCUMBENTS ================\n\n")

# ------------------------------------------------------------------------------
# 7. STUFE A: Wie viele der 15 Incumbents haben nach dem Crawler (Skript 02)
#    mindestens eine Unterseite gefunden?
# ------------------------------------------------------------------------------
if (exists("firms_data3_final")) {
  incumbent_coverage_crawler <- firms_data3_final %>%
    filter(type == "Incumbent") %>%
    distinct(name) %>%
    nrow()
  
  cat("STUFE A - Incumbents mit >=1 gecrawlter Unterseite (Skript 02):",
      incumbent_coverage_crawler, "von 15\n\n")
} else {
  cat("STUFE A: 'firms_data3_final' nicht im Workspace.\n\n")
}

# ------------------------------------------------------------------------------
# 8. STUFE B: Wie viele Incumbents sind im Rohdatensatz nach dem Scraping
#    (research_data) vorhanden?
# ------------------------------------------------------------------------------
incumbent_coverage_raw <- research_data %>%
  filter(type == "Incumbent") %>%
  distinct(name) %>%
  nrow()

cat("STUFE B - Incumbents in research_data (nach Scraping):",
    incumbent_coverage_raw, "von 15\n\n")

# ------------------------------------------------------------------------------
# 9. Inhaltslänge pro Incumbent (nchar-basiert), VOR der Tokenisierung
# ------------------------------------------------------------------------------
content_length_incumbents <- research_data %>%
  filter(type == "Incumbent") %>%
  mutate(content_nchar = nchar(content)) %>%
  select(name, content_nchar) %>%
  arrange(content_nchar)

cat("--- Inhaltslänge (nchar) pro Incumbent, aufsteigend sortiert ---\n")
print(content_length_incumbents)
cat("\n")

# ------------------------------------------------------------------------------
# 10. STUFE C: Wie viele Incumbents sind nach Tokenisierung/Bereinigung
#     noch im tidy_corpus?
# ------------------------------------------------------------------------------
incumbent_coverage_tidy <- tidy_corpus %>%
  filter(type == "Incumbent") %>%
  distinct(name) %>%
  nrow()

cat("STUFE C - Incumbents im tidy_corpus (nach Tokenisierung):",
    incumbent_coverage_tidy, "von 15\n\n")

# ------------------------------------------------------------------------------
# 11. STUFE D: Wie viele Incumbents erscheinen in firm_level_decoupling
#     (Skript 05, Wörterbuchtreffer)? Hier könnte eine vierte Fehlerquelle
#     liegen: Incumbent hat Text, aber KEIN Wort trifft auf sustainability_dict.
# ------------------------------------------------------------------------------
if (exists("firm_level_decoupling")) {
  incumbent_coverage_dict <- firm_level_decoupling %>%
    filter(type == "Incumbent") %>%
    distinct(name) %>%
    nrow()
  
  cat("STUFE D - Incumbents in firm_level_decoupling (Skript 05, Wörterbuchtreffer):",
      incumbent_coverage_dict, "von 15\n\n")
  
  # Welche Incumbents fehlen konkret in firm_level_decoupling,
  # obwohl sie im tidy_corpus vorhanden sind?
  incumbents_in_tidy <- tidy_corpus %>%
    filter(type == "Incumbent") %>%
    distinct(name) %>%
    pull(name)
  
  incumbents_in_dict <- firm_level_decoupling %>%
    filter(type == "Incumbent") %>%
    distinct(name) %>%
    pull(name)
  
  missing_in_dict <- setdiff(incumbents_in_tidy, incumbents_in_dict)
  
  cat("--- Incumbents mit Text im tidy_corpus, aber OHNE Treffer im Wörterbuch (Skript 05) ---\n")
  print(missing_in_dict)
} else {
  cat("STUFE D: 'firm_level_decoupling' nicht im Workspace - Skript 05 zuerst ausführen.\n")
}

# ==============================================================================
# DIAGNOSESKRIPT: Warum erscheinen nur 8 von 30 Start-ups im Isomorphie-Index?
# Voraussetzung: firms_data3_final (Skript 02), research_data (Skript 04),
# tidy_corpus (Skript 04) und isomorphism_index (Skript 04) müssen im
# Workspace vorhanden sein.
# ==============================================================================

library(dplyr)

# ------------------------------------------------------------------------------
# 0. STUFE A: Wie viele der 30 Start-ups haben nach dem CRAWLER (Skript 02)
#    überhaupt mindestens eine Unterseite gefunden?
#    Prüft: Fällt die Firma schon VOR dem Scraping raus (find_company_subpages
#    gab NA zurück)?
# ------------------------------------------------------------------------------
if (exists("firms_data3_final")) {
  startup_coverage_crawler <- firms_data3_final %>%
    filter(type == "Startup") %>%
    distinct(name) %>%
    nrow()
  
  cat("STUFE A - Start-ups mit >=1 gecrawlter Unterseite (Skript 02):",
      startup_coverage_crawler, "von 30\n\n")
} else {
  cat("STUFE A: 'firms_data3_final' nicht im Workspace - Skript 02 zuerst laufen lassen.\n\n")
}

# ------------------------------------------------------------------------------
# 1. STUFE B: Wie viele Start-ups sind im Rohdatensatz NACH dem SCRAPING
#    (Skript 03) vorhanden (vor Tokenisierung)?
#    Prüft: Ist das Scraping selbst (GET-Request, Status-Code) unvollständig?
# ------------------------------------------------------------------------------
startup_coverage_raw <- research_data %>%
  filter(type == "Startup") %>%
  distinct(name) %>%
  nrow()

cat("Start-ups im Rohdatensatz (research_data):", startup_coverage_raw, "von 30\n\n")

# ------------------------------------------------------------------------------
# 2. Inhaltslänge pro Start-up (nchar-basiert), VOR der Tokenisierung
#    Prüft: Gibt es Start-ups mit auffällig wenig gescraptem Text?
# ------------------------------------------------------------------------------
content_length_startups <- research_data %>%
  filter(type == "Startup") %>%
  mutate(content_nchar = nchar(content)) %>%
  select(name, content_nchar) %>%
  arrange(content_nchar)

cat("--- Inhaltslänge (nchar) pro Start-up, aufsteigend sortiert ---\n")
print(content_length_startups)
cat("\n")

# ------------------------------------------------------------------------------
# 3. Wie viele Start-ups sind NACH Tokenisierung/Bereinigung noch im tidy_corpus?
#    Prüft: Verschwinden Start-ups durch den Bereinigungsfilter
#    (Stopwords, nchar > 3, Zahlen raus)?
# ------------------------------------------------------------------------------
startup_coverage_tidy <- tidy_corpus %>%
  filter(type == "Startup") %>%
  distinct(name) %>%
  nrow()

cat("Start-ups im tidy_corpus (nach Tokenisierung/Bereinigung):",
    startup_coverage_tidy, "von 30\n\n")

# ------------------------------------------------------------------------------
# 4. Wortanzahl pro Start-up NACH Tokenisierung/Bereinigung
#    Prüft: Wie "dünn" ist das bereinigte Vokabular pro Start-up wirklich?
# ------------------------------------------------------------------------------
word_count_per_startup <- tidy_corpus %>%
  filter(type == "Startup") %>%
  count(name, sort = FALSE, name = "n_words_cleaned") %>%
  arrange(n_words_cleaned)

cat("--- Anzahl bereinigter Wörter pro Start-up, aufsteigend sortiert ---\n")
print(word_count_per_startup)
cat("\n")

# ------------------------------------------------------------------------------
# 5. Überlappung mit dem Incumbent-Vokabular
#    Kernprüfung: pairwise_similarity() liefert nur Paare mit mind. 1
#    gemeinsamem Wort. Start-ups ohne jegliche Überlappung fallen daher
#    automatisch aus dem Isomorphie-Index heraus.
# ------------------------------------------------------------------------------
incumbent_vocab <- tidy_corpus %>%
  filter(type == "Incumbent") %>%
  distinct(word) %>%
  pull(word)

overlap_check <- tidy_corpus %>%
  filter(type == "Startup") %>%
  distinct(name, word) %>%
  mutate(in_incumbent_vocab = word %in% incumbent_vocab) %>%
  group_by(name) %>%
  summarise(
    total_unique_words = n(),
    overlapping_words = sum(in_incumbent_vocab),
    overlap_share_pct = round(100 * overlapping_words / total_unique_words, 1),
    .groups = "drop"
  ) %>%
  arrange(overlapping_words)

cat("--- Überlappung jedes Start-ups mit dem Incumbent-Vokabular ---\n")
cat("(0 überlappende Wörter = kann NICHT im Isomorphie-Index erscheinen)\n")
print(overlap_check)
cat("\n")

# ------------------------------------------------------------------------------
# 6. Direkter Abgleich: Welche Start-ups fehlen tatsächlich im Isomorphie-Index?
# ------------------------------------------------------------------------------
if (exists("isomorphism_index")) {
  missing_startups <- overlap_check %>%
    filter(!name %in% isomorphism_index$startup)
  
  cat("--- Start-ups OHNE Eintrag im Isomorphie-Index ---\n")
  print(missing_startups)
  
  cat("\nZusammenfassung:\n")
  cat(nrow(missing_startups), "von", nrow(overlap_check),
      "Start-ups fehlen im Isomorphie-Index.\n")
  cat("Davon mit 0 überlappenden Wörtern (technisch unmöglich, aufzutauchen):",
      sum(missing_startups$overlapping_words == 0), "\n")
} else {
  cat("Hinweis: 'isomorphism_index' nicht im Workspace gefunden.",
      "Bitte Skript 04 zuerst vollständig ausführen.\n")
}

# ==============================================================================
# ANALOGE PRÜFUNG FÜR INCUMBENTS (n=15 laut Skript 01)
# Hintergrund: desc_stats aus Skript 05 zeigt N=9 für Incumbents,
# obwohl die Stichprobe 15 Incumbents umfasst. Prüft, an welcher Stufe
# die fehlenden 6 Incumbents verschwinden.
# ==============================================================================

cat("\n\n================ ANALOGE PRÜFUNG: INCUMBENTS ================\n\n")

# ------------------------------------------------------------------------------
# 7. STUFE A: Wie viele der 15 Incumbents haben nach dem Crawler (Skript 02)
#    mindestens eine Unterseite gefunden?
# ------------------------------------------------------------------------------
if (exists("firms_data3_final")) {
  incumbent_coverage_crawler <- firms_data3_final %>%
    filter(type == "Incumbent") %>%
    distinct(name) %>%
    nrow()
  
  cat("STUFE A - Incumbents mit >=1 gecrawlter Unterseite (Skript 02):",
      incumbent_coverage_crawler, "von 15\n\n")
} else {
  cat("STUFE A: 'firms_data3_final' nicht im Workspace.\n\n")
}

# ------------------------------------------------------------------------------
# 8. STUFE B: Wie viele Incumbents sind im Rohdatensatz nach dem Scraping
#    (research_data) vorhanden?
# ------------------------------------------------------------------------------
incumbent_coverage_raw <- research_data %>%
  filter(type == "Incumbent") %>%
  distinct(name) %>%
  nrow()

cat("STUFE B - Incumbents in research_data (nach Scraping):",
    incumbent_coverage_raw, "von 15\n\n")

# ------------------------------------------------------------------------------
# 9. Inhaltslänge pro Incumbent (nchar-basiert), VOR der Tokenisierung
# ------------------------------------------------------------------------------
content_length_incumbents <- research_data %>%
  filter(type == "Incumbent") %>%
  mutate(content_nchar = nchar(content)) %>%
  select(name, content_nchar) %>%
  arrange(content_nchar)

cat("--- Inhaltslänge (nchar) pro Incumbent, aufsteigend sortiert ---\n")
print(content_length_incumbents)
cat("\n")

# ------------------------------------------------------------------------------
# 10. STUFE C: Wie viele Incumbents sind nach Tokenisierung/Bereinigung
#     noch im tidy_corpus?
# ------------------------------------------------------------------------------
incumbent_coverage_tidy <- tidy_corpus %>%
  filter(type == "Incumbent") %>%
  distinct(name) %>%
  nrow()

cat("STUFE C - Incumbents im tidy_corpus (nach Tokenisierung):",
    incumbent_coverage_tidy, "von 15\n\n")

# ------------------------------------------------------------------------------
# 11. STUFE D: Wie viele Incumbents erscheinen in firm_level_decoupling
#     (Skript 05, Wörterbuchtreffer)? Hier könnte eine vierte Fehlerquelle
#     liegen: Incumbent hat Text, aber KEIN Wort trifft auf sustainability_dict.
# ------------------------------------------------------------------------------
if (exists("firm_level_decoupling")) {
  incumbent_coverage_dict <- firm_level_decoupling %>%
    filter(type == "Incumbent") %>%
    distinct(name) %>%
    nrow()
  
  cat("STUFE D - Incumbents in firm_level_decoupling (Skript 05, Wörterbuchtreffer):",
      incumbent_coverage_dict, "von 15\n\n")
  
  # Welche Incumbents fehlen konkret in firm_level_decoupling,
  # obwohl sie im tidy_corpus vorhanden sind?
  incumbents_in_tidy <- tidy_corpus %>%
    filter(type == "Incumbent") %>%
    distinct(name) %>%
    pull(name)
  
  incumbents_in_dict <- firm_level_decoupling %>%
    filter(type == "Incumbent") %>%
    distinct(name) %>%
    pull(name)
  
  missing_in_dict <- setdiff(incumbents_in_tidy, incumbents_in_dict)
  
  cat("--- Incumbents mit Text im tidy_corpus, aber OHNE Treffer im Wörterbuch (Skript 05) ---\n")
  print(missing_in_dict)
} else {
  cat("STUFE D: 'firm_level_decoupling' nicht im Workspace - Skript 05 zuerst ausführen.\n")
}