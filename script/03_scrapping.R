# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 03: Web-Scraping und Textextraktion
# ==============================================================================
library(httr)
library(rvest)
library(dplyr)
library(tidytext)
library(stopwords)
library(jsonlite)
library(robotstxt)
library(stringr)

academic_user_agent <- "VivienneSchegg-MasterThesis-UniLuzern/1.0 (+mailto:vivienne.schegg@stud.unilu.ch; wissenschaftliche Datenerhebung fuer Masterseminararbeit, Uni Luzern)"

# ------------------------------------------------------------------------------
# robots.txt-Cache pro Domain (statt pro Unterseite neu abzufragen)
# ------------------------------------------------------------------------------
robots_cache <- new.env()

is_scraping_allowed <- function(url) {
  domain <- str_extract(url, "(?<=://)([^/]+)")
  
  if (!is.null(robots_cache[[domain]])) {
    return(robots_cache[[domain]])
  }
  
  result <- tryCatch(
    isTRUE(paths_allowed(url, user_agent = academic_user_agent)),
    error = function(e) {
      warning(paste("robots.txt konnte nicht geprüft werden für:", url, "-> übersprungen"))
      FALSE
    }
  )
  
  robots_cache[[domain]] <- result
  result
}

# ------------------------------------------------------------------------------
# Scraping-Funktion mit Timeout (verhindert lange Hänger bei langsamen Seiten)
# ------------------------------------------------------------------------------
get_site_text_httr <- function(url) {
  if (!is_scraping_allowed(url)) {
    message(paste("  -> robots.txt verbietet Scraping für:", url))
    return(NA)
  }
  tryCatch({
    Sys.sleep(0.5)  # Politeness-Pause, bewusst beibehalten
    res <- GET(url, user_agent(academic_user_agent), timeout(10))
    if (status_code(res) == 200) {
      text <- read_html(res) %>% html_nodes("p, h1, h2, h3") %>% html_text(trim = TRUE)
      paste(text, collapse = " ")
    } else {
      NA
    }
  }, error = function(e) NA)
}

# ------------------------------------------------------------------------------
# Ein Durchlauf statt zwei redundanter Blöcke für Startup/Incumbent
# ------------------------------------------------------------------------------
full_research_data <- firms_data3_final %>%
  mutate(content = sapply(subpage_url, get_site_text_httr)) %>%
  filter(!is.na(content))

write_json(full_research_data, "full_research_data.json", pretty = TRUE)
print(paste("full_research_data.json erfolgreich geschrieben mit",
            nrow(full_research_data), "Zeilen."))

# ------------------------------------------------------------------------------
# Isomorphie-Index ALTERNATIV mit TF-IDF-Gewichtung berechnen
# (statt roher Worthäufigkeiten), um zu prüfen, ob generisches, gruppenübergreifend
# geteiltes Vokabular den ursprünglichen Cosine-Wert künstlich aufbläht
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

full_similarity_tfidf <- matrix_data_tfidf %>%
  pairwise_similarity(name, word, tf_idf)

isomorphism_index_tfidf <- full_similarity_tfidf %>%
  filter(item2 == "AGGREGATED_INCUMBENT") %>%
  select(startup = item1, cosine_similarity_tfidf = similarity) %>%
  arrange(desc(cosine_similarity_tfidf))

isomorphism_index_tfidf <- isomorphism_index_tfidf %>%
  left_join(distinct(research_data %>% select(name, type)), by = c("startup" = "name"))

print("--- ISOMORPHIE-INDEX TF-IDF-GEWICHTET (zum Vergleich) ---")
print(isomorphism_index_tfidf, n = 35)

# Direkter Vergleich: hat sich das Ranking/die Werte stark verändert?
comparison <- isomorphism_index %>%
  select(startup, cosine_similarity) %>%
  left_join(isomorphism_index_tfidf %>% select(startup, cosine_similarity_tfidf), by = "startup")

print("--- VERGLEICH ROH vs. TF-IDF-GEWICHTET ---")
print(comparison, n = 35)

write_csv(isomorphism_index_tfidf, "isomorphism_index_tfidf_results.csv")