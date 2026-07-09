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


# 1. Filter the Incumbent Firms
incumbent_analysis <- firms_data3_final %>%
  filter(type == "Incumbent")

# 2. Scrapping with httr
get_site_text_httr <- function(url) {
  tryCatch({
    res <- GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWeb64"))
    
    if (status_code(res) == 200) {
      page <- read_html(res)
      text <- page %>% html_nodes("p, h1, h2, h3") %>% html_text(trim = TRUE)
      return(paste(text, collapse = " "))
    } else {
      return(NA)
    }
  }, error = function(e) return(NA))
}

incumbent_analysis <- incumbent_analysis %>%
  mutate(content = sapply(subpage_url, get_site_text_httr)) %>%
  filter(!is.na(content))

# The same for Startups
startup_analysis <- firms_data3_final %>%
  filter(type == "Startup")

startup_analysis <- startup_analysis %>%
  mutate(content = sapply(subpage_url, get_site_text_httr)) %>%
  filter(!is.na(content))

# Creating the whole data list (nutzt bestehende 'type'-Spalte, keine
# zusätzliche 'group'-Spalte nötig)
full_research_data <- bind_rows(startup_analysis, incumbent_analysis)

write_json(full_research_data, "full_research_data.json", pretty = TRUE)

print(paste("full_research_data.json erfolgreich geschrieben mit",
            nrow(full_research_data), "Zeilen."))
