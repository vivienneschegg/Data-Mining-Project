# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 05c: Seed-Stabilitätscheck für keyATM
# ==============================================================================

library(keyATM)
library(dplyr)
library(ggplot2)
library(readr)

if (!exists("keyATM_docs") | !exists("keywords") | !exists("firm_corpus_df")) {
  stop("FEHLER: 'keyATM_docs', 'keywords' oder 'firm_corpus_df' nicht im Workspace gefunden. Bitte Skript 05b zuerst ausführen!")
}

seeds_to_test <- c(123, 42, 1, 2024, 777)
n_iterations  <- 3000

seed_stability_results <- data.frame()
theta_by_seed <- list()

for (s in seeds_to_test) {
  
  cat("\n============================================================\n")
  cat("Seed:", s, "\n")
  cat("============================================================\n")
  
  set.seed(s)
  fit_seed <- keyATM(
    docs = keyATM_docs,
    no_keyword_topics = 2,
    keywords = keywords,
    model = "base",
    options = list(seed = s, iterations = n_iterations, verbose = FALSE)
  )
  
  theta_seed <- as.data.frame(fit_seed$theta)
  theta_seed$name <- firm_corpus_df$name
  theta_seed$type <- firm_corpus_df$type
  
  substance_col_seed <- grep("substance", colnames(theta_seed), value = TRUE)
  
  ttest_seed <- t.test(theta_seed[[substance_col_seed]] ~ theta_seed$type)
  
  seed_stability_results <- bind_rows(seed_stability_results, data.frame(
    seed            = s,
    t_value         = round(as.numeric(ttest_seed$statistic), 4),
    p_value         = ttest_seed$p.value,
    mean_incumbent  = round(as.numeric(ttest_seed$estimate[1]), 4),
    mean_startup    = round(as.numeric(ttest_seed$estimate[2]), 4)
  ))
  
  theta_by_seed[[as.character(s)]] <- theta_seed
  
  cat("t =", round(as.numeric(ttest_seed$statistic), 4),
      " p =", format.pval(ttest_seed$p.value, digits = 4),
      " Mean Incumbent =", round(as.numeric(ttest_seed$estimate[1]), 4),
      " Mean Startup =", round(as.numeric(ttest_seed$estimate[2]), 4), "\n")
}

cat("\n\n--- GESAMTÜBERSICHT ÜBER ALLE SEEDS ---\n")
print(seed_stability_results)

n_significant <- sum(seed_stability_results$p_value < 0.05)
n_direction_startup_higher <- sum(seed_stability_results$mean_startup > seed_stability_results$mean_incumbent)

cat("\nAnzahl Seeds mit p < .05:", n_significant, "von", length(seeds_to_test), "\n")
cat("Anzahl Seeds mit Richtung 'Startup > Incumbent':", n_direction_startup_higher, "von", length(seeds_to_test), "\n")
cat("Streuung t-Werte: Min =", round(min(seed_stability_results$t_value), 3),
    " Max =", round(max(seed_stability_results$t_value), 3),
    " SD =", round(sd(seed_stability_results$t_value), 3), "\n")

if (n_significant == length(seeds_to_test) & n_direction_startup_higher == length(seeds_to_test)) {
  cat("\nHINWEIS: Ergebnis ist über alle getesteten Seeds stabil (Richtung und Signifikanz konsistent).\n")
} else if (n_significant == 0) {
  cat("\nHINWEIS: Über keinen der getesteten Seeds signifikant.\n")
} else {
  cat("\nHINWEIS: INSTABILES ERGEBNIS. Richtung und/oder Signifikanz wechseln je nach Seed.\n")
}

write_csv(seed_stability_results, "keyatm_seed_stability_results.csv")

saveRDS(
  list(summary = seed_stability_results, theta_by_seed = theta_by_seed),
  "keyatm_seed_stability_full.rds"
)

cat("\n-> Seed-Stabilitätscheck abgeschlossen. Ergebnisse exportiert.\n")