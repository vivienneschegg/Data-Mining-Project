# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 02: Automatisierter Web-Crawler für Unterseiten (Erweiterte Version)
# ==============================================================================

library(rvest)
library(xml2)
library(tidyr)
library(dplyr)
library(httr)
library(stringr) 

# 1. Daten direkt aus CSV laden
if (file.exists("firms_data_starting.csv")) {
  firms_data3 <- read.csv("firms_data_starting.csv", stringsAsFactors = FALSE)
} else {
  stop("FEHLER: Die Datei 'firms_data_starting.csv' wurde nicht gefunden!")
}

# 2. Optimierte Suchfunktion mit User-Agent und Fehlerbehandlung
find_company_subpages <- function(main_url) {
  tryCatch({
    # Bereinigung der URL von Whitespaces
    main_url <- str_trim(main_url)
    domain_name <- main_url %>%
      str_remove_all("https?://") %>%
      str_remove_all("www\\.") %>%
      str_split("/") %>%
      purrr::pluck(1, 1)

    # Zeitlimit (Timeout) auf 10 Sekunden setzen, falls ein Server nicht antwortet
    response <- GET(
      main_url, 
      user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"),
      timeout(10)
    )
  
    # Falls die Seite nicht erreichbar ist (z.B. HTTP 404 oder 500)
    if (status_code(response) >= 400) return(NA)
    # HTML Inhalt einlesen
    page <- read_html(response)
    # Alle Link-Nodes extrahieren
    nodes <- html_nodes(page, "a")
    if (length(nodes) == 0) return(NA)
    # Links in ein Dataframe konvertieren
    temp_links <- data.frame(
      text = html_text(nodes, trim = TRUE),
      url  = html_attr(nodes, "href"),
      stringsAsFactors = FALSE
    ) %>% 
      filter(!is.na(url) & url != "")
    
    # Erweiterte Keywords für organisationssoziologischen Fokus
    keywords <- "about|mission|sustainability|nachhaltigkeit|company|impact|uber-uns|values|werte|responsibility|verantwortung|esg|csr"
    
    # Filterung nach relevanten Texten ODER URLs
    relevant <- temp_links %>%
      filter(
        str_detect(text, regex(keywords, ignore_case = TRUE)) |
          str_detect(url, regex(keywords, ignore_case = TRUE))
      )
    
    if (nrow(relevant) == 0) return(NA)
    
    final_urls <- c()
    for (l in relevant$url) {
      # Relative Links in absolute URLs umwandeln
      abs_url <- url_absolute(l, main_url)
      
      # Strict Domain-Validation: Verhindert "Domain-Leaking" zu LinkedIn, Twitter, etc.
      if (!is.na(domain_name) && str_detect(abs_url, domain_name)) {
        # Fragmente entfernen (z.B. #about-section), um Duplikate zu vermeiden
        abs_url <- str_split(abs_url, "#")[[1]][1]
        final_urls <- c(final_urls, abs_url)
      }
    }
    
    # Haupt-URL immer mit einschließen
    final_urls <- c(main_url, final_urls)
    
    return(unique(final_urls))
    
  }, error = function(e) {
    # Gibt eine Warnung in der Konsole aus, bricht aber die Schleife nicht ab
    warning(paste("Fehler beim Crawlen von:", main_url, "-", e$message))
    return(NA)
  })
}

# 3. Schleife über alle 45 Firmen mit einer kurzen Pause (Politeness-Rule)
firms_subpages <- list()

print("Starte den Crawling-Prozess für 45 Unternehmen...")

for (i in 1:nrow(firms_data3)) {
  comp_name <- str_trim(firms_data3$name[i])
  comp_url  <- str_trim(firms_data3$url[i])
  
  message(paste0("[", i, "/", nrow(firms_data3), "] Crawle: ", comp_name))
  
  # Funktion ausführen
  found_pages <- find_company_subpages(comp_url)
  firms_subpages[[comp_name]] <- found_pages
  
  # Höflichkeitspause (0.5 Sekunden), damit Schweizer Server nicht blockieren
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

# Duplikate auf Zeilenebene entfernen
firms_data3_final <- distinct(firms_data3_final)

# Ergebnis anzeigen und speichern
print("--- CRAWLING ERFOLGREICH BEENDET ---")
print(paste("Anzahl gefundener relevanter Unterseiten gesamt:", nrow(firms_data3_final)))
head(firms_data3_final, n = 20)

# Zwischenergebnis als CSV speichern (wichtig für die Persistenz der Pipeline!)
write_csv(firms_data3_final, "firms_subpages_crawled.csv")