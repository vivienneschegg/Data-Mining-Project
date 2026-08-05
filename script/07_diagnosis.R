# ==============================================================================
# DIAGNOSESKRIPT: Abdeckung der Pipeline pro Organisationstyp
# Voraussetzung: firms_data3_final, research_data, tidy_corpus,
# isomorphism_index, firm_level_decoupling müssen im Workspace vorhanden sein
# (d.h. Skript 01 -> 02 -> 03 -> 04 -> 05 müssen vorher in derselben
# R-Sitzung durchgelaufen sein)
# ==============================================================================

library(dplyr)

# ------------------------------------------------------------------------------
# Gemeinsame Diagnosefunktion für Start-ups und Incumbents
# ------------------------------------------------------------------------------
diagnose_coverage <- function(org_type, total_n) {
  cat("\n================ DIAGNOSE:", toupper(org_type), "================\n\n")
  
  # Stufe A: Crawler (Skript 02)
  if (exists("firms_data3_final")) {
    n_crawler <- firms_data3_final %>% filter(type == org_type) %>% distinct(name) %>% nrow()
    cat("Stufe A - mit >=1 gecrawlter Unterseite:", n_crawler, "von", total_n, "\n")
  }
  
  # Stufe B: Scraping (Skript 03/04)
  n_raw <- research_data %>% filter(type == org_type) %>% distinct(name) %>% nrow()
  cat("Stufe B - im Rohdatensatz nach Scraping:", n_raw, "von", total_n, "\n")
  
  content_length <- research_data %>%
    filter(type == org_type) %>%
    mutate(content_nchar = nchar(content)) %>%
    select(name, content_nchar) %>%
    arrange(content_nchar)
  cat("\n--- Inhaltslänge (nchar), aufsteigend ---\n")
  print(content_length)
  
  # Stufe C: Tokenisierung (Skript 04)
  n_tidy <- tidy_corpus %>% filter(type == org_type) %>% distinct(name) %>% nrow()
  cat("\nStufe C - im tidy_corpus nach Tokenisierung:", n_tidy, "von", total_n, "\n")
  
  word_count <- tidy_corpus %>%
    filter(type == org_type) %>%
    count(name, name = "n_words_cleaned") %>%
    arrange(n_words_cleaned)
  cat("\n--- Bereinigte Wortanzahl pro Firma, aufsteigend ---\n")
  print(word_count)
  
  # Stufe D: Wörterbuchtreffer (Skript 05)
  if (exists("firm_level_decoupling")) {
    n_dict <- firm_level_decoupling %>% filter(type == org_type) %>% distinct(name) %>% nrow()
    cat("\nStufe D - in firm_level_decoupling (Wörterbuchtreffer):", n_dict, "von", total_n, "\n")
    
    missing_in_dict <- setdiff(
      tidy_corpus %>% filter(type == org_type) %>% distinct(name) %>% pull(name),
      firm_level_decoupling %>% filter(type == org_type) %>% distinct(name) %>% pull(name)
    )
    cat("--- Text vorhanden, aber kein Wörterbuchtreffer ---\n")
    print(missing_in_dict)
  }
  
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# Ausführen für beide Gruppen
# WICHTIG: total_n anpassen an die aktuelle Zielstichprobe (35 Start-ups, 19 Incumbents)
# ------------------------------------------------------------------------------
diagnose_coverage("Startup", 35)
diagnose_coverage("Incumbent", 19)

# ------------------------------------------------------------------------------
# Überlappung mit Incumbent-Vokabular (nur für Start-ups relevant, da
# pairwise_similarity() nur Paare mit >=1 gemeinsamem Wort liefert)
# ------------------------------------------------------------------------------
incumbent_vocab <- tidy_corpus %>% filter(type == "Incumbent") %>% distinct(word) %>% pull(word)

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

cat("\n================ ÜBERLAPPUNG MIT INCUMBENT-VOKABULAR ================\n\n")
cat("(0 überlappende Wörter = kann NICHT im Isomorphie-Index erscheinen)\n")
print(overlap_check, n = Inf)

if (exists("isomorphism_index")) {
  missing_startups <- overlap_check %>% filter(!name %in% isomorphism_index$startup)
  cat("\n--- Start-ups OHNE Eintrag im Isomorphie-Index ---\n")
  print(missing_startups)
  cat("\n", nrow(missing_startups), "von", nrow(overlap_check), "Start-ups fehlen.\n")
  cat("Davon mit 0 Überlappung (technisch unmöglich):",
      sum(missing_startups$overlapping_words == 0), "\n")
}

