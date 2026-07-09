# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 05: Theoriegeleitete Wörterbuchanalyse & Statistische Inferenz 
# ==============================================================================

library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(readr)

# 1. Daten aus den vorherigen Schritten laden
if (!exists("tidy_corpus")) {
  stop("FEHLER: 'tidy_corpus' wurde nicht im Workspace gefunden. Bitte Skript 04 zuerst ausführen!")
}

# 2. Erweitertes, bilinguales (DE/EN) theoretisches Wörterbuch
sustainability_dict <- data.frame(
  word = c(
    # --- INSTITUTIONELLE FASSADE (Abstract, value-based language) ---
    "sustainability", "responsibility", "commitment", "values", "future", 
    "impact", "global", "mission", "vision", "transformation", "reimagine",
    "society", "social", "stewardship", "governance", "esg", "sdg", "purpose", 
    "leadership", "integrity", "strive", "generations",
    "nachhaltigkeit", "verantwortung", "engagement", "werte", "zukunft",
    "wirkung", "auftrag", "gesellschaft",
    "sozial", "führung", "unternehmensführung", "wertegemeinschaft", "generationen",
    
    # --- TECHNISCHE SUBSTANZ (Operational, solution-oriented language) ---
    "recycling", "technology", "solution", "carbon", "co2", "emissions", 
    "renewable", "energy", "efficiency", "circular", "material", "waste",
    "science", "innovation", "engineering", "robotics", "biological", "software",
    "hardware", "infrastructure", "production", "process", "biodegradable",
    "technologie", "lösung", "kohlenstoff", "emissionen",
    "erneuerbar", "energie", "effizienz", "zirkulär", "kreislaufwirtschaft",
    "abfall", "wissenschaft", "ingenieurwesen", "robotik",
    "biologisch", "produktion", "prozess", "verfahren", "umwelttechnik"
  ),
  category = c(
    rep("Institutional_Façade", 22 + 13),
    rep("Technical_Substance", 23 + 18)
  ),
  stringsAsFactors = FALSE
)

# 3. Aggregierte Analyse der strategischen Entkopplung
decoupling_analysis <- tidy_corpus %>%
  inner_join(sustainability_dict, by = "word") %>%
  group_by(type, category) %>%
  tally() %>%
  group_by(type) %>%
  mutate(share = n / sum(n) * 100)

# 4. Detaillierte Analyse auf Unternehmensebene
firm_level_decoupling <- tidy_corpus %>%
  inner_join(sustainability_dict, by = "word") %>%
  group_by(name, type, category) %>%
  tally() %>%
  group_by(name) %>%
  mutate(share = n / sum(n) * 100) %>%
  filter(category == "Technical_Substance") %>% 
  select(name, type, technical_substance_share = share) %>%
  arrange(desc(technical_substance_share))


# ==============================================================================
# BERECHNUNG DER INFERENZSTATISTIK
# ==============================================================================

# --- STATISTIK 1: Gruppenvergleich Fassade vs. Substance (t-Test) ---
desc_stats <- firm_level_decoupling %>%
  group_by(type) %>%
  summarise(
    Mean_Substance = mean(technical_substance_share, na.rm = TRUE),
    SD_Substance   = sd(technical_substance_share, na.rm = TRUE),
    N              = n(),
    .groups = "drop"
  )

ttest_result <- t.test(technical_substance_share ~ type, data = firm_level_decoupling, alternative = "two.sided")

pool_sd <- sqrt(((desc_stats$SD_Substance[1]^2) + (desc_stats$SD_Substance[2]^2)) / 2)
cohens_d <- (desc_stats$Mean_Substance[2] - desc_stats$Mean_Substance[1]) / pool_sd

# --- STATISTIK 2: Korrelationstest ---
if (file.exists("isomorphism_index_results.csv")) {
  iso_index_data <- read_csv("isomorphism_index_results.csv")
  statistical_merger <- iso_index_data %>%
    left_join(firm_level_decoupling, by = c("startup" = "name"))
  
  correlation_result <- cor.test(statistical_merger$cosine_similarity, 
                                 statistical_merger$technical_substance_share, 
                                 method = "pearson")
} else {
  correlation_result <- NULL
}


# 1. Textdatei vorbereiten (Überschreiben falls vorhanden)
report_file <- "statistical_results_report.txt"
cat("======================================================================\n", file = report_file)
cat("STATISTISCHER BERICHT: INSTITUTIONELLER ISOMORPHISMUS\n", file = report_file, append = TRUE)
cat("======================================================================\n\n", file = report_file, append = TRUE)

# Funktion um Text gleichzeitig an die Konsole und die Datei zu senden
log_text <- function(text_string) {
  cat(text_string) # Ausgabe in R-Konsole
  cat(text_string, file = report_file, append = TRUE) # Ausgabe in Datei
}

# --- Block 1: Deskriptive Statistik ---
log_text("1. DESKRIPTIVE STATISTIK (Anteil technischer Substanz pro Gruppe):\n")
print(desc_stats) # Konsole
# Für die Datei müssen wir die Tabelle konvertieren:
write.table(desc_stats, file = report_file, append = TRUE, row.names = FALSE, sep = "\t")

log_text("\n----------------------------------------------------------------------\n")

# --- Block 2: t-Test ---
log_text("2. INFERENZSTATISTIK (Hypothesentest zum Strategic Decoupling):\n")
log_text("H0: Es gibt keinen Unterschied im Substanz-Vokabular zwischen Incumbents und Startups.\n")
log_text("H1: Startups nutzen signifikant mehr technische Substanzsprache als Incumbents.\n\n")
log_text(paste0("t-Wert:       ", round(ttest_result$statistic, 4), "\n"))
log_text(paste0("Freiheitsgrad:", round(ttest_result$parameter, 2), "\n"))
log_text(paste0("p-Wert:       ", format.pval(ttest_result$p.value, digits = 4), "\n"))
log_text(paste0("95% Konfidenzintervall des Unterschieds: [", round(ttest_result$conf.int[1], 2), ", ", round(ttest_result$conf.int[2], 2), "]\n"))
log_text(paste0("Effektstärke (Cohen's d):               ", round(cohens_d, 4), "\n"))

if(ttest_result$p.value < 0.05) {
  log_text("\nINTERPRETATION: H0 wird verworfen! Der Unterschied ist statistisch signifikant.\n")
} else {
  log_text("\nINTERPRETATION: H0 kann nicht verworfen werden. Kein signifikanter Unterschied.\n")
}

log_text("\n----------------------------------------------------------------------\n")

# --- Block 3: Korrelation ---
log_text("3. KORRELATIONSANALYSE (Das Mimetische Spannungsfeld):\n")
log_text("H0: Linguistische Anpassung (Isomorphie) korreliert nicht mit dem technischen Inhalt.\n")
log_text("H1: Höhere mimetische Isomorphie führt zu einem Sinken des technischen Substanz-Anteils.\n\n")

if (!is.null(correlation_result)) {
  log_text(paste0("Pearson Korrelationskoeffizient (r): ", round(correlation_result$estimate, 4), "\n"))
  log_text(paste0("p-Wert des Korrelationstests:        ", format.pval(correlation_result$p.value, digits = 4), "\n"))
  
  if(correlation_result$p.value < 0.05) {
    log_text("\nINTERPRETATION: H0 wird verworfen! Es liegt eine signifikante Korrelation vor.\n")
    if(correlation_result$estimate < 0) {
      log_text("Richtungsanalyse: Die Korrelation ist NEGATIV. Dies verifiziert empirisch das\n")
      log_text("organisationssoziologische Spannungsfeld: Je mehr sich ein Startup sprachlich an\n")
      log_text("Großkonzerne anpasst, desto weniger technische Substanz vermittelt es.\n")
    }
  } else {
    log_text("\nINTERPRETATION: H0 bleibt bestehen. Kein statistisch nachweisbarer linearer Zusammenhang.\n")
  }
} else {
  log_text("Keine Daten für Korrelationstest verfügbar (isomorphism_index_results.csv fehlt).\n")
}

log_text("======================================================================\n")


# 6. Ergebnisse für das finale Visualisierungs-Skript exportieren
write_csv(decoupling_analysis, "decoupling_aggregate_results.csv")
write_csv(firm_level_decoupling, "decoupling_firm_results.csv")

print("")
print("-> ERFOLGREICH: Daten exportiert & 'statistical_results_report.txt' geschrieben!")