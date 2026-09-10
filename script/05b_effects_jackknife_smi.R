# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 05b: Effektstärken mit Konfidenzintervallen, Leave-one-out-Jackknife
#             und H1-Vergleich mit/ohne SMI-Firmen
# ==============================================================================

install.packages("effsize")
library(effsize)
library(dplyr)
library(readr)

# ==============================================================================
# TEIL 1: H1 mit und ohne SMI-Firmen
# ==============================================================================

smi_firms <- c("Roche", "Richemont")

firm_level_decoupling_no_smi <- firm_level_decoupling %>%
  filter(!name %in% smi_firms)

cat("--- H1 OHNE SMI-Firmen ---\n")
cat("Stichprobe ohne SMI:", nrow(firm_level_decoupling_no_smi),
    "(", sum(firm_level_decoupling_no_smi$type == "Startup"), "Start-ups,",
    sum(firm_level_decoupling_no_smi$type == "Incumbent"), "Incumbents)\n")

ttest_no_smi <- t.test(technical_substance_share ~ type, data = firm_level_decoupling_no_smi)
print(ttest_no_smi)

cat("\n--- VERGLEICH: H1 MIT vs. OHNE SMI-Firmen ---\n")
cat("Mit SMI (n=13 Incumbents):  t =", round(ttest_result$statistic, 4),
    " p =", format.pval(ttest_result$p.value, digits = 4), "\n")
cat("Ohne SMI (n=11 Incumbents): t =", round(ttest_no_smi$statistic, 4),
    " p =", format.pval(ttest_no_smi$p.value, digits = 4), "\n")


# ==============================================================================
# TEIL 2: Effektstärken (Cohen's d) mit Konfidenzintervallen
# ==============================================================================

cat("\n--- Wörterbuch (Skript 05) ---\n")
d_dict <- cohen.d(technical_substance_share ~ type, data = firm_level_decoupling)
print(d_dict)

cat("\n--- Wörterbuch OHNE SMI-Firmen ---\n")
d_dict_no_smi <- cohen.d(technical_substance_share ~ type, data = firm_level_decoupling_no_smi)
print(d_dict_no_smi)

cat("\n--- keyATM (unbereinigt) ---\n")
d_keyatm <- cohen.d(theta[[substance_col]] ~ theta$type)
print(d_keyatm)

cat("\n--- keyATM (bereinigt, ohne Swiss Prime Site/Planted Foods) ---\n")
d_keyatm_clean <- cohen.d(theta_clean[[substance_col]] ~ theta_clean$type)
print(d_keyatm_clean)


# ==============================================================================
# TEIL 3: Leave-one-out-Jackknife über die 12 keyATM-Seed-Wörter
# ==============================================================================

all_seed_words <- unlist(keywords)
jackknife_results <- data.frame()

for (word_to_drop in all_seed_words) {
  cat_name <- names(keywords)[sapply(keywords, function(x) word_to_drop %in% x)]
  
  reduced_keywords <- keywords
  reduced_keywords[[cat_name]] <- setdiff(reduced_keywords[[cat_name]], word_to_drop)
  
  fit_jk <- keyATM(
    docs = keyATM_docs,
    no_keyword_topics = 2,
    keywords = reduced_keywords,
    model = "base",
    options = list(seed = 123, iterations = 1000, verbose = FALSE)
  )
  
  theta_jk <- as.data.frame(fit_jk$theta)
  theta_jk$name <- firm_corpus_df$name
  theta_jk$type <- firm_corpus_df$type
  sub_col_jk <- grep("substance", colnames(theta_jk), value = TRUE)
  
  ttest_jk <- t.test(theta_jk[[sub_col_jk]] ~ theta_jk$type)
  
  jackknife_results <- bind_rows(jackknife_results, data.frame(
    dropped_word = word_to_drop,
    category = cat_name,
    t_value = round(as.numeric(ttest_jk$statistic), 4),
    p_value = ttest_jk$p.value,
    mean_incumbent = round(as.numeric(ttest_jk$estimate[1]), 4),
    mean_startup = round(as.numeric(ttest_jk$estimate[2]), 4)
  ))
  
  cat("Entfernt:", word_to_drop, "-> t =", round(as.numeric(ttest_jk$statistic), 2),
      " p =", format.pval(ttest_jk$p.value, digits = 3), "\n")
}

cat("\n--- JACKKNIFE-GESAMTERGEBNIS ---\n")
print(jackknife_results)

cat("\nAlle 12 Durchläufe weiterhin signifikant (p<.05)?",
    all(jackknife_results$p_value < 0.05), "\n")
cat("Schwächster t-Wert (kleinster Betrag):", min(abs(jackknife_results$t_value)), "\n")
cat("Höchster p-Wert (am wenigsten signifikant):", max(jackknife_results$p_value), "\n")

write_csv(jackknife_results, "keyatm_jackknife_results.csv")


# ==============================================================================
# Alle Ergebnisse aus diesem Skript sichern
# ==============================================================================
saveRDS(
  list(
    h1_no_smi = ttest_no_smi,
    cohens_d_dict = d_dict,
    cohens_d_dict_no_smi = d_dict_no_smi,
    cohens_d_keyatm = d_keyatm,
    cohens_d_keyatm_clean = d_keyatm_clean,
    jackknife = jackknife_results
  ),
  "robustness_supplementary_results.rds"
)

