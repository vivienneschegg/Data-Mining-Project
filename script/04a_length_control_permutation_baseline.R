# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 04a: Dokumentlänge als Kontrollvariable & Permutationsbaseline
#             für den Isomorphie-Index (roh UND TF-IDF-gewichtet)
# Voraussetzung: tidy_corpus, isomorphism_index und research_data
# (aus Skript 04) müssen im Workspace vorhanden sein.
# ==============================================================================

library(dplyr)
library(tidytext)
library(Matrix)
library(readr)

# ==============================================================================
# TEIL 1: Dokumentlänge als Kontrollvariable
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Bereinigte Wortanzahl pro Firma berechnen und an den Isomorphie-Index anfügen
# ------------------------------------------------------------------------------
firm_length <- tidy_corpus %>%
  count(name, name = "n_words_cleaned")

isomorphism_index_with_length <- isomorphism_index %>%
  left_join(firm_length, by = c("startup" = "name"))

print(isomorphism_index_with_length, n = Inf)

# ------------------------------------------------------------------------------
# 2. Korrelation zwischen Textlänge und Isomorphie-Index prüfen
# ------------------------------------------------------------------------------
length_cor <- cor.test(isomorphism_index_with_length$cosine_similarity,
                       isomorphism_index_with_length$n_words_cleaned,
                       method = "pearson")

cat("\n--- Korrelation Textlänge x Isomorphie-Index ---\n")
print(length_cor)

if (length_cor$p.value < 0.05) {
  cat("\nHINWEIS: Signifikante Korrelation, Dokumentlänge beeinflusst den\n")
  cat("Isomorphie-Index systematisch. Sollte als Kontrollvariable in H2\n")
  cat("aufgenommen oder explizit als Limitation benannt werden.\n")
} else {
  cat("\nKeine signifikante Korrelation zwischen Textlänge und Isomorphie-Index.\n")
}

write_csv(isomorphism_index_with_length, "isomorphism_index_with_length.csv")


# ==============================================================================
# TEIL 2: Permutationsbaseline, ROHE Worthäufigkeiten (ursprüngliche Version)
# ==============================================================================

# ------------------------------------------------------------------------------
# 3. Firma-Wort-Matrix (roh) für alle Firmen aufbauen
# ------------------------------------------------------------------------------
firm_word_counts <- tidy_corpus %>% count(name, word)
dtm_raw <- firm_word_counts %>% cast_sparse(name, word, n)

firm_types <- tidy_corpus %>% distinct(name, type)
firm_types <- firm_types[match(rownames(dtm_raw), firm_types$name), ]

# ------------------------------------------------------------------------------
# 4. Hilfsfunktionen: Kosinus-Ähnlichkeit und mittlere Gruppenähnlichkeit
#    (mat wird als Parameter übergeben, damit dieselbe Funktion für roh UND
#    TF-IDF-gewichtet in Teil 3 wiederverwendet werden kann)
# ------------------------------------------------------------------------------
cosine_to_vector <- function(mat, vec) {
  num <- as.numeric(mat %*% vec)
  denom <- sqrt(rowSums(mat^2)) * sqrt(sum(vec^2))
  num / denom
}

compute_mean_similarity <- function(types_vector, mat) {
  incumbent_idx <- which(types_vector == "Incumbent")
  startup_idx   <- which(types_vector == "Startup")
  incumbent_profile <- colSums(mat[incumbent_idx, , drop = FALSE])
  sims <- cosine_to_vector(mat[startup_idx, , drop = FALSE], incumbent_profile)
  mean(sims, na.rm = TRUE)
}

# ------------------------------------------------------------------------------
# 5. Beobachteter Wert (rohe Version, echte Gruppenzuordnung)
# ------------------------------------------------------------------------------
observed_mean_sim_raw <- compute_mean_similarity(firm_types$type, dtm_raw)
cat("\n--- PERMUTATIONSBASELINE (ROH) ---\n")
cat("Beobachteter Mittelwert Isomorphie-Index (roh):", round(observed_mean_sim_raw, 4), "\n")

# ------------------------------------------------------------------------------
# 6. Permutationstest (roh): 1000x Gruppenlabels zufällig neu mischen
# ------------------------------------------------------------------------------
set.seed(123)
n_perm <- 1000
perm_means_raw <- numeric(n_perm)

for (i in 1:n_perm) {
  shuffled_types <- sample(firm_types$type)
  perm_means_raw[i] <- compute_mean_similarity(shuffled_types, dtm_raw)
}

cat("Null-Verteilung (roh): M =", round(mean(perm_means_raw), 4),
    " SD =", round(sd(perm_means_raw), 4), "\n")

p_value_perm_raw <- mean(abs(perm_means_raw - mean(perm_means_raw)) >= abs(observed_mean_sim_raw - mean(perm_means_raw)))
cat("Permutations-p-Wert (roh):", round(p_value_perm_raw, 4), "\n")

png("permutation_baseline_raw.png", width = 900, height = 600)
hist(perm_means_raw, breaks = 40, col = "#bdc3c7", border = "white",
     main = "Permutationsbaseline (rohe Worthäufigkeiten)",
     xlab = "Mittlere Kosinus-Ähnlichkeit", ylab = "Häufigkeit")
abline(v = observed_mean_sim_raw, col = "#27ae60", lwd = 3)
legend("topright", legend = c("Beobachteter Wert"), col = "#27ae60", lwd = 3, bty = "n")
dev.off()


# ==============================================================================
# TEIL 3: Permutationsbaseline, TF-IDF-GEWICHTET (korrigierte Version)
# Begründung: rohe Worthäufigkeiten lassen generisches, gruppenübergreifend
# geteiltes Vokabular den Cosine-Wert dominieren. TF-IDF wertet Wörter ab,
# die firmenübergreifend häufig vorkommen, und hebt distinktive Begriffe hervor.
# ==============================================================================

# ------------------------------------------------------------------------------
# 7. Firma-Wort-Matrix TF-IDF-gewichtet aufbauen
# ------------------------------------------------------------------------------
firm_word_tfidf <- tidy_corpus %>%
  count(name, word) %>%
  bind_tf_idf(word, name, n)

dtm_tfidf <- firm_word_tfidf %>% cast_sparse(name, word, tf_idf)

firm_types_tfidf <- tidy_corpus %>% distinct(name, type)
firm_types_tfidf <- firm_types_tfidf[match(rownames(dtm_tfidf), firm_types_tfidf$name), ]

# ------------------------------------------------------------------------------
# 8. Beobachteter Wert (TF-IDF-gewichtet, echte Gruppenzuordnung)
# ------------------------------------------------------------------------------
observed_mean_sim_tfidf <- compute_mean_similarity(firm_types_tfidf$type, dtm_tfidf)
cat("\n--- PERMUTATIONSBASELINE (TF-IDF-GEWICHTET) ---\n")
cat("Beobachteter Mittelwert Isomorphie-Index (TF-IDF):", round(observed_mean_sim_tfidf, 4), "\n")

# ------------------------------------------------------------------------------
# 9. Permutationstest (TF-IDF): 1000x Gruppenlabels zufällig neu mischen
# ------------------------------------------------------------------------------
set.seed(123)
perm_means_tfidf <- numeric(n_perm)

for (i in 1:n_perm) {
  shuffled_types <- sample(firm_types_tfidf$type)
  perm_means_tfidf[i] <- compute_mean_similarity(shuffled_types, dtm_tfidf)
}

cat("Null-Verteilung (TF-IDF): M =", round(mean(perm_means_tfidf), 4),
    " SD =", round(sd(perm_means_tfidf), 4), "\n")

p_value_perm_tfidf <- mean(abs(perm_means_tfidf - mean(perm_means_tfidf)) >= abs(observed_mean_sim_tfidf - mean(perm_means_tfidf)))
cat("Permutations-p-Wert (TF-IDF):", round(p_value_perm_tfidf, 4), "\n")

if (p_value_perm_tfidf < 0.05) {
  cat("\nHINWEIS: Der TF-IDF-gewichtete Isomorphie-Index ist signifikant von der\n")
  cat("Zufallsbaseline verschieden, absolute Werte sind damit interpretierbar.\n")
} else {
  cat("\nHINWEIS: Auch der TF-IDF-gewichtete Isomorphie-Index unterscheidet sich\n")
  cat("NICHT signifikant von der Zufallsbaseline. Muss transparent so berichtet werden.\n")
}

png("permutation_baseline_tfidf.png", width = 900, height = 600)
hist(perm_means_tfidf, breaks = 40, col = "#bdc3c7", border = "white",
     main = "Permutationsbaseline (TF-IDF-gewichtet)",
     xlab = "Mittlere Kosinus-Ähnlichkeit", ylab = "Häufigkeit")
abline(v = observed_mean_sim_tfidf, col = "#27ae60", lwd = 3)
legend("topright", legend = c("Beobachteter Wert"), col = "#27ae60", lwd = 3, bty = "n")
dev.off()


# ==============================================================================
# TEIL 4: Gesamtvergleich und Sicherung aller Ergebnisse
# ==============================================================================
cat("\n--- GESAMTVERGLEICH: ROH vs. TF-IDF-GEWICHTET ---\n")
cat("Roh:      beobachtet =", round(observed_mean_sim_raw, 4),
    " | p =", round(p_value_perm_raw, 4), "\n")
cat("TF-IDF:   beobachtet =", round(observed_mean_sim_tfidf, 4),
    " | p =", round(p_value_perm_tfidf, 4), "\n")

saveRDS(
  list(
    length_correlation = length_cor,
    raw = list(observed = observed_mean_sim_raw, perm_means = perm_means_raw, p_value = p_value_perm_raw),
    tfidf = list(observed = observed_mean_sim_tfidf, perm_means = perm_means_tfidf, p_value = p_value_perm_tfidf)
  ),
  "permutation_baseline_results.rds"
)
