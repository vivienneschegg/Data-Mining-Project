# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 02: Automatisierter Web-Crawler für Unterseiten
# ==============================================================================

install.packages("chromote")

library(rvest)
library(xml2)
library(tidyr)
library(dplyr)
library(httr)
library(stringr)
library(chromote)

# 1. Daten direkt aus CSV laden
if (file.exists("firms_data_starting.csv")) {
  firms_data3 <- read.csv("firms_data_starting.csv", stringsAsFactors = FALSE)
} else {
  stop("FEHLER: Die Datei 'firms_data_starting.csv' wurde nicht gefunden!")
}

# ------------------------------------------------------------------------------
# Hilfsfunktion: Links aus einem HTML-String extrahieren und filtern
# ------------------------------------------------------------------------------
extract_relevant_links <- function(page, main_url, domain_name) {
  nodes <- html_nodes(page, "a")
  if (length(nodes) == 0) return(NA)
  
  temp_links <- data.frame(
    text = html_text(nodes, trim = TRUE),
    url  = html_attr(nodes, "href"),
    stringsAsFactors = FALSE
  ) %>% 
    filter(!is.na(url) & url != "")
  
  # Keyword-Liste erweitert, jede Ergänzung belegt durch echte Linktexte
  # aus dem Crawling-Lauf (siehe Chat-Verlauf):
  # - "über uns": Exnaton nutzt "Über uns" (Umlaut + Leerzeichen statt
  #   Bindestrich), das ursprüngliche "uber-uns" matcht das nicht
  # - "a propos": Enerdrape (französischsprachig) nutzt "A propos"
  # - "story": Neology nutzt "Our Story" statt "About"
  # - "team": Neology nutzt zusätzlich "Our Team"
  keywords <- "about|mission|sustainability|nachhaltigkeit|company|impact|uber-uns|über uns|values|werte|responsibility|verantwortung|esg|csr|a propos|story|team"
  
  relevant <- temp_links %>%
    filter(
      str_detect(text, regex(keywords, ignore_case = TRUE)) |
        str_detect(url, regex(keywords, ignore_case = TRUE))
    )
  
  if (nrow(relevant) == 0) return(NA)
  
  final_urls <- c()
  for (l in relevant$url) {
    abs_url <- url_absolute(l, main_url)
    if (!is.na(domain_name) && str_detect(abs_url, domain_name)) {
      abs_url <- str_split(abs_url, "#")[[1]][1]
      final_urls <- c(final_urls, abs_url)
    }
  }
  
  if (length(final_urls) == 0) return(NA)
  return(unique(c(main_url, final_urls)))
}

# ------------------------------------------------------------------------------
# Variante 1: Schneller Crawl via httr (wie bisher)
# ------------------------------------------------------------------------------
find_company_subpages_httr <- function(main_url, domain_name) {
  tryCatch({
    response <- GET(
      main_url, 
      user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"),
      timeout(10)
    )
    if (status_code(response) >= 400) return(NA)
    page <- read_html(response)
    return(extract_relevant_links(page, main_url, domain_name))
  }, error = function(e) {
    warning(paste("httr-Fehler beim Crawlen von:", main_url, "-", e$message))
    return(NA)
  })
}

# ------------------------------------------------------------------------------
# Variante 2: Fallback mit JavaScript-Rendering via chromote
# ------------------------------------------------------------------------------
find_company_subpages_chromote <- function(main_url, domain_name, wait_seconds = 6) {
  tryCatch({
    session <- ChromoteSession$new()
    on.exit(session$close(), add = TRUE)
    
    session$Page$navigate(main_url)
    session$Page$loadEventFired()
    Sys.sleep(wait_seconds)
    
    html_content <- session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    
    message(paste("     [DEBUG] chromote HTML-Länge für", main_url, ":", nchar(html_content), "Zeichen"))
    
    page <- read_html(html_content)
    all_link_nodes <- html_nodes(page, "a")
    message(paste("     [DEBUG] Gefundene <a>-Tags insgesamt:", length(all_link_nodes)))
    
    if (length(all_link_nodes) > 0) {
      link_texts <- html_text(all_link_nodes, trim = TRUE)
      link_texts_nonempty <- link_texts[link_texts != ""]
      message(paste("     [DEBUG] Linktexte (erste 15):",
                    paste(head(link_texts_nonempty, 15), collapse = " | ")))
    }
    
    result <- extract_relevant_links(page, main_url, domain_name)
    if (length(result) == 1 && is.na(result)) {
      message("     [DEBUG] -> Keine Keyword-Treffer unter den gefundenen Links (NA)")
    }
    return(result)
    
  }, error = function(e) {
    warning(paste("chromote-Fehler beim Crawlen von:", main_url, "-", e$message))
    return(NA)
  })
}

# ------------------------------------------------------------------------------
# Kombinierte Funktion: httr zuerst, chromote nur bei Bedarf (NA-Ergebnis)
# ------------------------------------------------------------------------------
find_company_subpages <- function(main_url) {
  main_url <- str_trim(main_url)
  domain_name <- main_url %>%
    str_remove_all("https?://") %>%
    str_remove_all("www\\.") %>%
    str_split("/") %>%
    purrr::pluck(1, 1)
  
  result <- find_company_subpages_httr(main_url, domain_name)
  
  if (length(result) == 1 && is.na(result)) {
    message(paste("  -> httr fand keine Links, versuche chromote für:", main_url))
    result <- find_company_subpages_chromote(main_url, domain_name)
  }
  
  return(result)
}

# 3. Schleife über alle 45 Firmen mit einer kurzen Pause (Politeness-Rule)
firms_subpages <- list()

print("Starte den Crawling-Prozess für 45 Unternehmen...")

for (i in 1:nrow(firms_data3)) {
  comp_name <- str_trim(firms_data3$name[i])
  comp_url  <- str_trim(firms_data3$url[i])
  
  message(paste0("[", i, "/", nrow(firms_data3), "] Crawle: ", comp_name))
  
  found_pages <- find_company_subpages(comp_url)
  firms_subpages[[comp_name]] <- found_pages
  
  Sys.sleep(0.5)
}

# 4. Dataframe für die nächste Phase (Scraping) aufbereiten
firms_data3_final <- data.frame(
  name = names(firms_subpages),
  subpage_url = I(firms_subpages)
) %>% 
  unnest(subpage_url) %>%
  filter(!is.na(subpage_url) & subpage_url != "") %>%
  left_join(firms_data3 %>% select(name, type), by = "name")

firms_data3_final <- distinct(firms_data3_final)

print("--- CRAWLING ERFOLGREICH BEENDET ---")
print(paste("Anzahl gefundener relevanter Unterseiten gesamt:", nrow(firms_data3_final)))
head(firms_data3_final, n = 20)

write_csv(firms_data3_final, "firms_subpages_crawled.csv")