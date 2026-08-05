# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 02: Automatisierter Web-Crawler für Unterseiten
# ==============================================================================

install.packages("chromote")
install.packages("robotstxt")

library(rvest)
library(xml2)
library(tidyr)
library(dplyr)
library(httr)
library(stringr)
library(chromote)
library(robotstxt)

# Ethik-Layer: identifizierender User-Agent + robots.txt-Check
academic_user_agent <- "VivienneSchegg-MasterThesis-UniLuzern/1.0 (+mailto:vivienne.schegg@stud.unilu.ch; wissenschaftliche Datenerhebung fuer Masterseminararbeit, Uni Luzern)"

is_scraping_allowed <- function(url) {
  tryCatch(
    isTRUE(paths_allowed(url, user_agent = academic_user_agent)),
    error = function(e) {
      warning(paste("robots.txt konnte nicht geprüft werden für:", url, "-> übersprungen"))
      FALSE
    }
  )
}

# 1. Daten laden
if (!file.exists("firms_data_starting.csv")) stop("FEHLER: 'firms_data_starting.csv' nicht gefunden!")
firms_data3 <- read.csv("firms_data_starting.csv", stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# Links aus HTML extrahieren und nach Keywords filtern
# (Keyword-Liste erweitert um "über uns", "a propos", "story" für
# nicht-englische/-standardisierte Linktexte, "team" bewusst ausgeschlossen)
# ------------------------------------------------------------------------------
extract_relevant_links <- function(page, main_url, domain_name) {
  nodes <- html_nodes(page, "a")
  if (length(nodes) == 0) return(NA)
  
  temp_links <- data.frame(
    text = html_text(nodes, trim = TRUE),
    url  = html_attr(nodes, "href"),
    stringsAsFactors = FALSE
  ) %>% filter(!is.na(url) & url != "")
  
  keywords <- "about|mission|sustainability|nachhaltigkeit|company|impact|uber-uns|über uns|values|werte|responsibility|verantwortung|esg|csr|a propos|story"
  
  relevant <- temp_links %>%
    filter(str_detect(text, regex(keywords, ignore_case = TRUE)) |
             str_detect(url, regex(keywords, ignore_case = TRUE)))
  
  if (nrow(relevant) == 0) return(NA)
  
  final_urls <- c()
  for (l in relevant$url) {
    abs_url <- url_absolute(l, main_url)
    if (!is.na(domain_name) && str_detect(abs_url, domain_name)) {
      final_urls <- c(final_urls, str_split(abs_url, "#")[[1]][1])
    }
  }
  
  if (length(final_urls) == 0) return(NA)
  unique(c(main_url, final_urls))
}

# ------------------------------------------------------------------------------
# Crawl-Funktionen: httr (schnell) mit chromote-Fallback (JS-Rendering)
# robots.txt wird zentral in find_company_subpages() geprüft, nicht doppelt
# ------------------------------------------------------------------------------
find_company_subpages_httr <- function(main_url, domain_name) {
  tryCatch({
    response <- GET(main_url, user_agent(academic_user_agent), timeout(10))
    if (status_code(response) >= 400) return(NA)
    extract_relevant_links(read_html(response), main_url, domain_name)
  }, error = function(e) {
    warning(paste("httr-Fehler bei:", main_url, "-", e$message))
    NA
  })
}

find_company_subpages_chromote <- function(main_url, domain_name, wait_seconds = 6) {
  tryCatch({
    session <- ChromoteSession$new()
    on.exit(session$close(), add = TRUE)
    session$Page$navigate(main_url)
    session$Page$loadEventFired()
    Sys.sleep(wait_seconds)
    html_content <- session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    extract_relevant_links(read_html(html_content), main_url, domain_name)
  }, error = function(e) {
    warning(paste("chromote-Fehler bei:", main_url, "-", e$message))
    NA
  })
}

find_company_subpages <- function(main_url) {
  main_url <- str_trim(main_url)
  domain_name <- main_url %>%
    str_remove_all("https?://") %>%
    str_remove_all("www\\.") %>%
    str_split("/") %>%
    purrr::pluck(1, 1)
  
  if (!is_scraping_allowed(main_url)) {
    message(paste("  -> robots.txt verbietet Crawling für:", main_url))
    return(NA)
  }
  
  result <- find_company_subpages_httr(main_url, domain_name)
  if (length(result) == 1 && is.na(result)) {
    message(paste("  -> httr fand keine Links, versuche chromote für:", main_url))
    result <- find_company_subpages_chromote(main_url, domain_name)
  }
  result
}

# 2. Crawling-Schleife (mit Politeness-Pause)
firms_subpages <- list()
print("Starte den Crawling-Prozess...")

for (i in 1:nrow(firms_data3)) {
  comp_name <- str_trim(firms_data3$name[i])
  comp_url  <- str_trim(firms_data3$url[i])
  message(paste0("[", i, "/", nrow(firms_data3), "] Crawle: ", comp_name))
  firms_subpages[[comp_name]] <- find_company_subpages(comp_url)
  Sys.sleep(0.5)
}

# 3. Ergebnis aufbereiten und exportieren
firms_data3_final <- data.frame(
  name = names(firms_subpages),
  subpage_url = I(firms_subpages)
) %>% 
  unnest(subpage_url) %>%
  filter(!is.na(subpage_url) & subpage_url != "") %>%
  left_join(firms_data3 %>% select(name, type), by = "name") %>%
  distinct()

print("--- CRAWLING BEENDET ---")
print(paste("Anzahl gefundener relevanter Unterseiten:", nrow(firms_data3_final)))
head(firms_data3_final, n = 20)

write_csv(firms_data3_final, "firms_subpages_crawled.csv")