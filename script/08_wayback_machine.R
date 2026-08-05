# ==============================================================================
# DIAGNOSESKRIPT: Wayback-Machine-Abdeckungstest FÜR ALLE START-UPS
# Prüft pro Start-up, wie viele Jahres-Snapshots relevanter, echter Textseiten
# (About/Mission/Team etc., ohne Bild-/PDF-/Video-Assets) im Internet Archive
# verfügbar sind, als Grundlage für die Entscheidung Panel-Design ja/nein
# ==============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(readr)

# ------------------------------------------------------------------------------
# 1. Start-up-Domains laden (nur type == "Startup")
# ------------------------------------------------------------------------------
firms_data3 <- read.csv("firms_data_starting.csv", stringsAsFactors = FALSE)

startup_domains <- firms_data3 %>%
  filter(type == "Startup") %>%
  mutate(domain = str_remove(url, "^https?://(www\\.)?") %>% str_remove("/$"))

cat("Anzahl zu testender Start-ups:", nrow(startup_domains), "\n\n")

# ------------------------------------------------------------------------------
# 2. Funktion: relevante archivierte Unterseiten einer Domain abrufen
#    (dieselbe Keyword-Logik wie im Crawler, Skript 02)
# ------------------------------------------------------------------------------
list_archived_subpages <- function(domain, from_year = 2015, to_year = 2026) {
  keywords <- "about|mission|sustainability|nachhaltigkeit|company|impact|uber-uns|ueber-uns|values|werte|responsibility|verantwortung|esg|csr"
  
  cdx_url <- paste0(
    "http://web.archive.org/cdx/search/cdx",
    "?url=", URLencode(domain, reserved = TRUE),
    "&matchType=domain",
    "&output=json",
    "&from=", from_year,
    "&to=", to_year,
    "&filter=statuscode:200",
    "&collapse=urlkey",
    "&limit=2000"
  )
  
  result <- tryCatch({
    res <- GET(cdx_url, timeout(20))
    if (status_code(res) != 200) return(NULL)
    content(res, as = "text", encoding = "UTF-8")
  }, error = function(e) NULL)
  
  if (is.null(result) || result == "") return(NULL)
  parsed <- tryCatch(fromJSON(result), error = function(e) NULL)
  if (is.null(parsed) || is.null(nrow(parsed)) || nrow(parsed) <= 1) return(NULL)
  
  df <- as.data.frame(parsed[-1, , drop = FALSE], stringsAsFactors = FALSE)
  colnames(df) <- parsed[1, ]
  df$year <- substr(df$timestamp, 1, 4)
  
  df <- df %>% filter(str_detect(original, regex(keywords, ignore_case = TRUE)))
  
  # Reine Medien-Assets (Bilder/PDFs/Videos) rausfiltern, nur echte Textseiten behalten
  df %>% filter(!str_detect(original, "\\.(jpg|jpeg|png|webp|pdf|mp4|svg|vtt|gif|ico|css|js)$"))
}

# ------------------------------------------------------------------------------
# 3. Für jedes Start-up abrufen und zusammenfassen (mit Politeness-Pause)
# ------------------------------------------------------------------------------
coverage_results <- list()

for (i in 1:nrow(startup_domains)) {
  name_i <- startup_domains$name[i]
  domain_i <- startup_domains$domain[i]
  
  cat("[", i, "/", nrow(startup_domains), "] Prüfe:", name_i, "-", domain_i, "\n")
  
  pages <- list_archived_subpages(domain_i)
  
  if (is.null(pages) || nrow(pages) == 0) {
    cat("  -> Keine relevanten Textseiten im Archive gefunden\n\n")
    coverage_results[[name_i]] <- data.frame(
      name = name_i, n_years = 0, n_pages = 0, years = NA, unique_paths = NA
    )
  } else {
    n_years <- length(unique(pages$year))
    n_pages <- nrow(pages)
    cat("  ->", n_years, "Jahre,", n_pages, "relevante Seiten-Treffer:",
        paste(sort(unique(pages$year)), collapse = ", "), "\n\n")
    
    coverage_results[[name_i]] <- data.frame(
      name = name_i,
      n_years = n_years,
      n_pages = n_pages,
      years = paste(sort(unique(pages$year)), collapse = ", "),
      unique_paths = paste(unique(pages$original), collapse = " | ")
    )
  }
  
  Sys.sleep(1)  # Politeness-Pause gegenüber dem Internet Archive
}

# ------------------------------------------------------------------------------
# 4. Gesamtübersicht
# ------------------------------------------------------------------------------
coverage_summary <- bind_rows(coverage_results)

cat("\n================ GESAMTÜBERSICHT ================\n\n")
print(coverage_summary %>% select(name, n_years, n_pages) %>% arrange(desc(n_years)),
      row.names = FALSE)

cat("\n--- FAZIT ---\n")
cat("Durchschnittliche Anzahl Jahre mit relevanten Seiten:",
    round(mean(coverage_summary$n_years), 1), "\n")
cat("Start-ups mit >= 3 Jahren (Minimum für sinnvolles Panel):",
    sum(coverage_summary$n_years >= 3), "von", nrow(coverage_summary), "\n")
cat("Start-ups mit 0 Jahren (keine Abdeckung):",
    sum(coverage_summary$n_years == 0), "von", nrow(coverage_summary), "\n")

# ------------------------------------------------------------------------------
# 5. Ergebnis speichern (inkl. der einzelnen gefundenen URL-Pfade zur
#    späteren Detailprüfung, welche Unterseiten konkret genutzt werden könnten)
# ------------------------------------------------------------------------------
write_csv(coverage_summary %>% select(-unique_paths), "wayback_coverage_startups_summary.csv")
write_csv(coverage_summary, "wayback_coverage_startups_full.csv")

cat("\n-> Ergebnisse gespeichert in 'wayback_coverage_startups_summary.csv'\n")
cat("   und 'wayback_coverage_startups_full.csv' (inkl. URL-Details).\n")