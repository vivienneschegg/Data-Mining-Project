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

get_site_text_httr <- function(url) {
  if (!is_scraping_allowed(url)) {
    message(paste("  -> robots.txt verbietet Scraping für:", url))
    return(NA)
  }
  tryCatch({
    Sys.sleep(0.5)
    res <- GET(url, user_agent(academic_user_agent), timeout(10))
    if (status_code(res) == 200) {
      text <- read_html(res) %>% html_nodes("p, h1, h2, h3") %>% html_text(trim = TRUE)
      paste(text, collapse = " ")
    } else {
      NA
    }
  }, error = function(e) NA)
}

full_research_data <- firms_data3_final %>%
  mutate(content = sapply(subpage_url, get_site_text_httr)) %>%
  filter(!is.na(content))

write_json(full_research_data, "full_research_data.json", pretty = TRUE)
print(paste("full_research_data.json erfolgreich geschrieben mit",
            nrow(full_research_data), "Zeilen."))

# HINWEIS: Der TF-IDF-gewichtete Isomorphie-Index (vormals hier am Ende)
# wurde nach Skript 04c ausgegliedert, da er 'tidy_corpus' und
# 'isomorphism_index' aus Skript 04 benötigt.