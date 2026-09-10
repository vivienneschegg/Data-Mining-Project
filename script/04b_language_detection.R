# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 04b: Spracherkennung pro Unterseite & Kreuztabelle nach Gruppe
# ==============================================================================

install.packages("cld2")
library(cld2)
library(dplyr)
library(readr)

# ------------------------------------------------------------------------------
# 1. Sprache pro Unterseite erkennen (auf dem Rohtext, VOR Tokenisierung/
#    Stopword-Entfernung, da erkannte Sprache auf natürlichem Text
#    zuverlässiger ist als auf bereinigten Einzelwörtern)
# ------------------------------------------------------------------------------
research_data_lang <- research_data %>%
  mutate(detected_language = detect_language(content))

cat("--- Erkannte Sprachen (Rohverteilung, alle Unterseiten) ---\n")
print(table(research_data_lang$detected_language, useNA = "ifany"))

# ------------------------------------------------------------------------------
# 2. Auf Haupt-Sprachen reduzieren (de, en, fr), alles andere als "other"
#    (NA = Spracherkennung fehlgeschlagen, meist bei sehr kurzem/leerem Text)
# ------------------------------------------------------------------------------
research_data_lang <- research_data_lang %>%
  mutate(language_grouped = case_when(
    detected_language == "de" ~ "de",
    detected_language == "en" ~ "en",
    detected_language == "fr" ~ "fr",
    is.na(detected_language) ~ "unknown",
    TRUE ~ "other"
  ))

# ------------------------------------------------------------------------------
# 3. Kreuztabelle auf UNTERSEITEN-Ebene: Gruppe x Sprache
# ------------------------------------------------------------------------------
cat("\n--- KREUZTABELLE (Unterseiten-Ebene): Organisationstyp x Sprache ---\n")
crosstab_pages <- table(research_data_lang$type, research_data_lang$language_grouped)
print(crosstab_pages)

cat("\n--- Als Anteile (Zeilenprozent) ---\n")
print(round(prop.table(crosstab_pages, margin = 1) * 100, 1))

# Chi-Quadrat-Test: ist die Sprachverteilung zwischen den Gruppen signifikant
# unterschiedlich? (bestätigt/verneint Andreas Vermutung statistisch)
chisq_result <- chisq.test(crosstab_pages)
cat("\n--- Chi-Quadrat-Test: Sprachverteilung Start-up vs. Incumbent ---\n")
print(chisq_result)

if (chisq_result$p.value < 0.05) {
  cat("\nHINWEIS: Sprachverteilung unterscheidet sich signifikant zwischen den\n")
  cat("Gruppen. Andreas Vermutung bestätigt sich, robustheitscheck auf\n")
  cat("einsprachiger Teilstichprobe ist notwendig (siehe Skript 04d/05c).\n")
} else {
  cat("\nHINWEIS: Keine signifikante Sprachungleichverteilung zwischen den\n")
  cat("Gruppen gefunden. Sollte trotzdem transparent berichtet werden.\n")
}

# ------------------------------------------------------------------------------
# 4. Kreuztabelle auf FIRMEN-Ebene: dominante Sprache pro Firma
#    (eine Firma kann mehrere Unterseiten in unterschiedlichen Sprachen haben,
#    hier wird die häufigste Sprache pro Firma als "dominant" gewertet)
# ------------------------------------------------------------------------------
firm_dominant_language <- research_data_lang %>%
  filter(language_grouped %in% c("de", "en", "fr")) %>%
  count(name, type, language_grouped) %>%
  group_by(name, type) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(name, type, dominant_language = language_grouped)

cat("\n--- KREUZTABELLE (Firmen-Ebene): Organisationstyp x dominante Sprache ---\n")
crosstab_firms <- table(firm_dominant_language$type, firm_dominant_language$dominant_language)
print(crosstab_firms)

cat("\n--- Als Anteile (Zeilenprozent) ---\n")
print(round(prop.table(crosstab_firms, margin = 1) * 100, 1))

# ------------------------------------------------------------------------------
# 5. Ergebnisse sichern
# ------------------------------------------------------------------------------
write_csv(research_data_lang %>% select(name, type, detected_language, language_grouped),
          "language_detection_pages.csv")
write_csv(firm_dominant_language, "language_detection_firms.csv")

saveRDS(
  list(crosstab_pages = crosstab_pages, crosstab_firms = crosstab_firms,
       chisq = chisq_result, firm_dominant_language = firm_dominant_language),
  "language_detection_results.rds"
)

# ------------------------------------------------------------------------------
# Bereinigter Test: NUR de/en vergleichen (fr/other/unknown raus, da zu
# kleine Zellen und "unknown" vermutlich Längen- statt Sprachartefakt ist)
# ------------------------------------------------------------------------------
crosstab_de_en <- research_data_lang %>%
  filter(language_grouped %in% c("de", "en")) %>%
  count(type, language_grouped) %>%
  tidyr::pivot_wider(names_from = language_grouped, values_from = n, values_fill = 0)

print(crosstab_de_en)

crosstab_de_en_matrix <- as.matrix(crosstab_de_en[, c("de", "en")])
rownames(crosstab_de_en_matrix) <- crosstab_de_en$type

fisher_result <- fisher.test(crosstab_de_en_matrix)
cat("\n--- Fisher-Test NUR de vs. en (bereinigt) ---\n")
print(fisher_result)