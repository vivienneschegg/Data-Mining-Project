# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 05a: Seeded Topic Model (keyATM) als Robustheitscheck
# zum bestehenden 41-Begriffe-Wörterbuch (Skript 05)
# ==============================================================================

install.packages("keyATM")
install.packages("quanteda")

library(quanteda)
library(keyATM)
library(dplyr)

# ------------------------------------------------------------------------------
# 1. Text pro Firma aggregieren (research_data hat eine Zeile pro Unterseite)
# ------------------------------------------------------------------------------
firm_corpus_df <- research_data %>%
  group_by(name, type) %>%
  summarise(text = paste(content, collapse = " "), .groups = "drop")

cat("Anzahl Firmen im Rohkorpus:", nrow(firm_corpus_df), "\n")

# ------------------------------------------------------------------------------
# 2. Quanteda-Korpus, Tokenisierung, bilinguale Bereinigung
#    (analog zum Filter in Skript 04)
# ------------------------------------------------------------------------------
corpus_obj <- corpus(firm_corpus_df, text_field = "text", docid_field = "name")

toks <- tokens(corpus_obj, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(c(stopwords("de"), stopwords("en"),
                  "cookie", "cookies", "privacy", "datenschutz", "impressum", "contact", "kontakt")) %>%
  tokens_select(min_nchar = 4)

dfm_obj <- dfm(toks)
dfm_obj <- dfm_trim(dfm_obj, min_termfreq = 2)

# ------------------------------------------------------------------------------
# 3. Leere Dokumente entfernen (Emissium, Oxyle: content_nchar = 0) UND
#    firm_corpus_df sofort synchron halten
# ------------------------------------------------------------------------------
dfm_obj <- dfm_subset(dfm_obj, ntoken(dfm_obj) > 0)

firm_corpus_df <- firm_corpus_df %>% filter(name %in% docnames(dfm_obj))
firm_corpus_df <- firm_corpus_df[match(docnames(dfm_obj), firm_corpus_df$name), ]

cat("Anzahl Firmen nach Bereinigung:", nrow(firm_corpus_df), "\n")
stopifnot(nrow(firm_corpus_df) == ndoc(dfm_obj))

keyATM_docs <- keyATM_read(texts = dfm_obj)

# ------------------------------------------------------------------------------
# 4. Seed-Wortliste (12 statt 41 Begriffe, verifiziert per Häufigkeitscheck:
#    "verantwortung", "werte", "technologie" wegen zu geringer Vorkommenszahl
#    durch stärkere Begriffe aus dem bestehenden Wörterbuch ersetzt)
# ------------------------------------------------------------------------------
keywords <- list(
  facade    = c("nachhaltigkeit", "sustainability", "responsibility",
                "values", "impact", "commitment"),
  substance = c("recycling", "technology", "engineering",
                "innovation", "material", "energy")
)

png("keyword_check.png", width = 1000, height = 700)
visualize_keywords(keyATM_docs, keywords)
dev.off()

# ------------------------------------------------------------------------------
# 5. Modell schätzen (2 Seed-Themen + 2 freie, datengetriebene Themen)
# ------------------------------------------------------------------------------
set.seed(123)
fit <- keyATM(
  docs = keyATM_docs,
  no_keyword_topics = 2,
  keywords = keywords,
  model = "base",
  options = list(seed = 123, iterations = 3000)
)

png("model_fit.png", width = 1000, height = 700)
plot_modelfit(fit)
dev.off()

# ------------------------------------------------------------------------------
# 6. Themenanteile pro Firma extrahieren
# ------------------------------------------------------------------------------
theta <- as.data.frame(fit$theta)
theta$name <- firm_corpus_df$name
theta$type <- firm_corpus_df$type

cat("\n--- Spaltennamen der Themenanteile ---\n")
print(colnames(theta))
print(theta)

# ------------------------------------------------------------------------------
# 7. t-Test: keyATM-geschätzte Substance-Quote, unbereinigte Stichprobe (n=36)
# ------------------------------------------------------------------------------
substance_col <- grep("substance", colnames(theta), value = TRUE)
cat("\nGefundene Substance-Spalte:", substance_col, "\n")

ttest_keyatm <- t.test(theta[[substance_col]] ~ theta$type)
print(ttest_keyatm)

# ------------------------------------------------------------------------------
# 8. Ausreisser-Diagnose: welche Firmen zeigen atypisch extreme Facade-Werte?
#    Identifiziert: Swiss Prime Site (identischer Content über alle 13
#    Unterseiten, Scraping-Artefakt) und Planted Foods (Top-Wörter fast nur
#    Cookie-Consent-Banner-Text, kein echter Seiteninhalt erfasst)
# ------------------------------------------------------------------------------
cat("\n--- Auffällige Firmen mit sehr hohem Facade-Anteil (>0.6) ---\n")
print(theta %>% filter(`1_facade` > 0.6) %>%
        select(name, type, `1_facade`, all_of(substance_col), Other_1, Other_2))

# Zur Nachvollziehbarkeit: Swiss Prime Site auf identischen Content prüfen
sps_content <- research_data %>% filter(name == "Swiss Prime Site") %>% pull(content)
cat("\nSwiss Prime Site - identische Unterseiten:",
    sum(duplicated(sps_content)) + 1, "von", length(sps_content), "\n")

# ------------------------------------------------------------------------------
# 9. Bereinigter t-Test ohne die zwei datenqualitätsbedingten Ausreisser (n=34)
# ------------------------------------------------------------------------------
theta_clean <- theta %>%
  filter(!name %in% c("Swiss Prime Site", "Planted Foods"))

cat("\nFirmen nach Bereinigung:", nrow(theta_clean),
    "(", sum(theta_clean$type == "Startup"), "Start-ups,",
    sum(theta_clean$type == "Incumbent"), "Incumbents)\n")

ttest_keyatm_clean <- t.test(theta_clean[[substance_col]] ~ theta_clean$type)
print(ttest_keyatm_clean)

# ------------------------------------------------------------------------------
# 10. Gesamtvergleich aller drei Varianten
# ------------------------------------------------------------------------------
cat("\n--- VERGLEICH ALLER DREI VARIANTEN ---\n")
cat("Wörterbuch (Skript 05):        t =", round(ttest_result$statistic, 4),
    " p =", format.pval(ttest_result$p.value, digits = 4), "\n")
cat("keyATM (n=36, unbereinigt):    t =", round(ttest_keyatm$statistic, 4),
    " p =", format.pval(ttest_keyatm$p.value, digits = 4), "\n")
cat("keyATM (n=34, bereinigt):      t =", round(ttest_keyatm_clean$statistic, 4),
    " p =", format.pval(ttest_keyatm_clean$p.value, digits = 4), "\n")

# ------------------------------------------------------------------------------
# 11. Ergebnisse sichern für spätere Verwendung (z.B. Skript 06 / Textkapitel)
# ------------------------------------------------------------------------------
saveRDS(
  list(dict = ttest_result, keyatm_full = ttest_keyatm, keyatm_clean = ttest_keyatm_clean),
  "robustness_check_results.rds"
)

cat("\n-> Robustheitscheck abgeschlossen. Ergebnisse in 'robustness_check_results.rds' gesichert.\n")

# ==============================================================================
# ANHANG: Determinismus-Check 
# ==============================================================================

# Dokumentreihenfolge protokollieren, BEVOR Fit B läuft
cat("--- Dokumentreihenfolge vor Fit B (erste 10) ---\n")
print(head(dfm_obj@docvars$docname_, 10))
cat("Anzahl Dokumente in dfm_obj:", ndoc(dfm_obj), "\n")
cat("Anzahl Dokumente in firm_corpus_df:", nrow(firm_corpus_df), "\n\n")

set.seed(123)
fit_B <- keyATM(
  docs = keyATM_docs,
  no_keyword_topics = 2,
  keywords = keywords,
  model = "base",
  options = list(seed = 123, iterations = 3000)
)

theta_B <- as.data.frame(fit_B$theta)
theta_B$name <- firm_corpus_df$name
theta_B$type <- firm_corpus_df$type
substance_col_B <- grep("substance", colnames(theta_B), value = TRUE)

ttest_B <- t.test(theta_B[[substance_col_B]] ~ theta_B$type)

cat("--- VERGLEICH: Fit A (oben, 'theta') vs. Fit B (hier, 'theta_B') ---\n")
cat("Fit A: t =", round(as.numeric(ttest_keyatm$statistic), 4),
    " p =", format.pval(ttest_keyatm$p.value, digits = 4), "\n")
cat("Fit B: t =", round(as.numeric(ttest_B$statistic), 4),
    " p =", format.pval(ttest_B$p.value, digits = 4), "\n")

cat("\nSind die theta-Werte für die Substance-Spalte identisch?",
    isTRUE(all.equal(theta[[substance_col]], theta_B[[substance_col_B]])), "\n")
cat("Maximale absolute Differenz:",
    max(abs(theta[[substance_col]] - theta_B[[substance_col_B]])), "\n")

# ==============================================================================
# Seed-Stabilitätscheck mit 40 Seeds (Andreas Vorgabe: 30-50)
# ==============================================================================

library(keyATM)
library(dplyr)
library(readr)

set.seed(1)  # nur für die Seed-Ziehung selbst, nicht für keyATM
seeds_to_test <- sample(1:100000, 40)

seed_stability_results_40 <- data.frame()

for (s in seeds_to_test) {
  set.seed(s)
  fit_seed <- keyATM(
    docs = keyATM_docs,
    no_keyword_topics = 2,
    keywords = keywords,
    model = "base",
    options = list(seed = s, iterations = 3000, verbose = FALSE)
  )
  
  theta_seed <- as.data.frame(fit_seed$theta)
  theta_seed$name <- firm_corpus_df$name
  theta_seed$type <- firm_corpus_df$type
  substance_col_seed <- grep("substance", colnames(theta_seed), value = TRUE)
  
  ttest_seed <- t.test(theta_seed[[substance_col_seed]] ~ theta_seed$type)
  
  seed_stability_results_40 <- bind_rows(seed_stability_results_40, data.frame(
    seed = s,
    t_value = round(as.numeric(ttest_seed$statistic), 4),
    p_value = ttest_seed$p.value,
    mean_incumbent = round(as.numeric(ttest_seed$estimate[1]), 4),
    mean_startup = round(as.numeric(ttest_seed$estimate[2]), 4)
  ))
  
  cat("Seed", s, "-> t =", round(as.numeric(ttest_seed$statistic), 3),
      " p =", format.pval(ttest_seed$p.value, digits = 3), "\n")
}

n_sig <- sum(seed_stability_results_40$p_value < 0.05)
n_right_dir <- sum(seed_stability_results_40$mean_startup > seed_stability_results_40$mean_incumbent)

cat("\n--- ZUSAMMENFASSUNG ---\n")
cat("Signifikant in", n_sig, "von", nrow(seed_stability_results_40), "Seeds\n")
cat("Richtig gerichtet in", n_right_dir, "von", nrow(seed_stability_results_40), "Seeds\n")
cat("t-Wert Spanne:", round(min(seed_stability_results_40$t_value), 3), "bis",
    round(max(seed_stability_results_40$t_value), 3), "\n")
cat("Median t:", round(median(seed_stability_results_40$t_value), 3), "\n")

write_csv(seed_stability_results_40, "keyatm_seed_stability_40seeds.csv")
