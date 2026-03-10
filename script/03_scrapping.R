library(httr)
library(rvest)
library(dplyr)
library(tidytext)
install.packages("stopwords")
library(stopwords)
library(jsonlite)

# 1. Filter the Incumbent Firms
incumbent_analysis <- firms_data3_final %>%
  filter(type == "Incumbent")

# 2. Scrapping with httr
get_site_text_httr <- function(url) {
  tryCatch({
    # Anfrage mit User-Agent (simuliert einen Browser)
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

#Counting Keywords
incumbent_keywords <- incumbent_analysis %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% stopwords("en"),
         !word %in% stopwords("de"),
         nchar(word) > 3,
         !grepl("[0-9]", word)) %>%
  count(word, sort = TRUE)

# Top 20 in incumbent firms
print(head(incumbent_keywords, 20))

#The same for Startups+
startup_analysis <- firms_data3_final %>%
  filter(type == "Startup")

startup_analysis <- startup_analysis %>%
  mutate(content = sapply(subpage_url, get_site_text_httr)) %>%
  filter(!is.na(content))

startup_keywords <- startup_analysis %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% stopwords("en"),
         !word %in% stopwords("de"),
         nchar(word) > 3,
         !grepl("[0-9]", word)) %>%
  count(word, sort = TRUE)

#Creating the whole data list
full_research_data <- bind_rows(
  startup_analysis %>% mutate(group = "Startup"),
  incumbent_analysis %>% mutate(group = "Incumbent")
)

write_json(full_research_data, "full_research_data.json", pretty = TRUE)

#Compare the lists

comparison <- full_join(
  startup_keywords %>% rename(n_startup = n),
  incumbent_keywords %>% rename(n_incumbent = n),
  by = "word"
) %>%
  replace_na(list(n_startup = 0, n_incumbent = 0))

#Unique words in Startup
unique_startup_trends <- comparison %>%
  filter(n_incumbent == 0) %>%
  filter(n_startup > 2) %>% 
  arrange(desc(n_startup))

top_startup_diff <- comparison %>%
  mutate(diff = n_startup - n_incumbent) %>%
  arrange(desc(diff))

# the results
print("Keywords EXCLUSIVE to Startups")
head(unique_startup_trends, 20)

print("Keywords with strongest STARTUP focus vs. Incumbents")
head(top_startup_diff, 20)

#Doesn't make much sense to analysis it this way...i will define key word according to literature.