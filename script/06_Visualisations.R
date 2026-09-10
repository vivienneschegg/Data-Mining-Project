# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 06: Datenvisualisierung — aufgeteilt in Haupttext (5) und Anhang (8)
# ==============================================================================

library(dplyr)
library(ggplot2)
library(forcats)
library(readr)
library(tidytext)
library(tidyr)
library(wordcloud)
library(RColorBrewer)
library(ggrepel)

# ------------------------------------------------------------------------------
# Verzeichnisse anlegen
# ------------------------------------------------------------------------------
dir.create("Visualisations/MainText", showWarnings = FALSE, recursive = TRUE)
dir.create("Visualisations/Appendix", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# 1. Daten laden
# ------------------------------------------------------------------------------
if (file.exists("decoupling_aggregate_results.csv") &
    file.exists("isomorphism_index_results.csv") &
    file.exists("decoupling_firm_results.csv")) {
  
  gap_data          <- read_csv("decoupling_aggregate_results.csv")
  firm_decoupling   <- read_csv("decoupling_firm_results.csv")
  
  if (file.exists("isomorphism_index_with_length.csv")) {
    iso_index_data <- read_csv("isomorphism_index_with_length.csv")
  } else {
    iso_index_data <- read_csv("isomorphism_index_results.csv")
  }
  
} else {
  stop("FEHLER: Die Analysedaten aus Skript 04/05 wurden nicht gefunden!")
}

n_startups_iso <- nrow(iso_index_data)
n_incumbents   <- length(unique(firm_decoupling$name[firm_decoupling$type == "Incumbent"]))


# ==============================================================================
# ================================ HAUPTTEXT ==================================
# ==============================================================================

# ------------------------------------------------------------------------------
# FIGURE 1 (Section 4.2): Most Distinctive TF-IDF Terms
# ------------------------------------------------------------------------------
if (exists("group_tf_idf")) {
  top_tf_idf <- group_tf_idf %>%
    group_by(type) %>%
    slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
    ungroup()
  
  fig1 <- ggplot(top_tf_idf, aes(x = reorder_within(word, tf_idf, type), y = tf_idf, fill = type)) +
    geom_col(width = 0.6) +
    scale_x_reordered() +
    coord_flip() +
    facet_wrap(~type, scales = "free_y") +
    scale_fill_manual(values = c("Incumbent" = "#2c3e50", "Startup" = "#27ae60")) +
    labs(
      title = "Figure 1: Most Distinctive Terms by Organization Type (TF-IDF)",
      subtitle = "Top 10 exclusive words per group, weighted by statistical distinctiveness",
      x = "Term",
      y = "TF-IDF weight (higher = more characteristic of this group)"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14),
          strip.text = element_text(face = "bold", size = 12))
  
  print(fig1)
  ggsave("Visualisations/MainText/Figure_1_Distinctive_TFIDF.png", plot = fig1, width = 10, height = 6, dpi = 300)
} else {
  warning("Objekt 'group_tf_idf' nicht gefunden. Figure 1 nicht erstellt.")
}


# ------------------------------------------------------------------------------
# FIGURE 2 (Section 4.3): Mimetic Isomorphism Index (Ranking)
# ------------------------------------------------------------------------------
fig2 <- ggplot(iso_index_data, aes(x = reorder(startup, cosine_similarity), y = cosine_similarity)) +
  geom_col(width = 0.7, fill = "#27ae60") +
  coord_flip() +
  labs(
    title = "Figure 2: Mimetic Isomorphism Index",
    subtitle = paste0("Linguistic convergence of ", n_startups_iso, " Swiss start-ups toward the collective incumbent profile"),
    y = "Isomorphism score (cosine similarity: 0 = divergent, 1 = identical)",
    x = "Sustainability start-up"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 9))

print(fig2)
ggsave("Visualisations/MainText/Figure_2_Isomorphism_Ranking.png", plot = fig2, width = 9, height = 8, dpi = 300)


# ------------------------------------------------------------------------------
# FIGURE 3 (Section 4.4): Institutional Façade vs. Technical Substance
# ------------------------------------------------------------------------------
fig3 <- ggplot(gap_data, aes(x = type, y = share, fill = category)) +
  geom_col(width = 0.5, color = "white", lwd = 0.7) +
  geom_text(aes(label = paste0(round(share, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 4.5) +
  scale_fill_manual(
    values = c("Institutional_Fa\u00e7ade" = "#2c3e50", "Technical_Substance" = "#27ae60"),
    labels = c("Institutional Fa\u00e7ade", "Technical Substance")
  ) +
  labs(
    title = "Figure 3: Institutional Fa\u00e7ade vs. Technical Substance",
    subtitle = "Aggregated vocabulary share by organization type",
    y = "Vocabulary share (%)", x = "Organization type", fill = "Theoretical construct"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 14),
        panel.grid.major.x = element_blank())

print(fig3)
ggsave("Visualisations/MainText/Figure_3_Strategic_Gap.png", plot = fig3, width = 8, height = 6, dpi = 300)


# ------------------------------------------------------------------------------
# FIGURE 4 (Section 4.5): Boxplot — Technical Substance by Organization Type
# ------------------------------------------------------------------------------
if (exists("firm_decoupling")) {
  fig4 <- ggplot(firm_decoupling, aes(x = type, y = technical_substance_share, fill = type)) +
    geom_boxplot(width = 0.5, alpha = 0.8, outlier.shape = NA) +
    geom_jitter(width = 0.1, size = 2, alpha = 0.6, color = "black") +
    scale_fill_manual(values = c("Incumbent" = "#2c3e50", "Startup" = "#27ae60")) +
    labs(
      title = "Figure 4: Technical Substance by Organization Type",
      subtitle = "Distribution of firm-level substance shares underlying the H1 t-test",
      x = "Organization type", y = "Share of technical substance (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14))
  
  print(fig4)
  ggsave("Visualisations/MainText/Figure_4_Boxplot_Substance.png", plot = fig4, width = 7, height = 6, dpi = 300)
} else {
  warning("Objekt 'firm_decoupling' nicht gefunden. Figure 4 nicht erstellt.")
}


# ------------------------------------------------------------------------------
# FIGURE 5 (Section 4.6): Scatterplot — Isomorphism x Technical Substance
# ------------------------------------------------------------------------------
correlation_data <- iso_index_data %>%
  left_join(firm_decoupling, by = c("startup" = "name"))

fig5 <- ggplot(correlation_data, aes(x = cosine_similarity, y = technical_substance_share)) +
  geom_point(color = "#27ae60", size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", color = "#2c3e50", linetype = "solid", se = TRUE, fill = "gray90", lwd = 0.8) +
  geom_text_repel(aes(label = startup), size = 3, max.overlaps = 20) +
  labs(
    title = "Figure 5: The Tension Between Fa\u00e7ade and Substance at the Firm Level",
    subtitle = paste0("Correlation across ", n_startups_iso, " start-ups: field-level adaptation vs. technical language"),
    x = "Isomorphism index (similarity to incumbents)",
    y = "Share of technical terms (dictionary filter, %)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

print(fig5)
ggsave("Visualisations/MainText/Figure_5_Decoupling_Micro_Scatter.png", plot = fig5, width = 9, height = 7, dpi = 300)


# ==============================================================================
# ================================= ANHANG ====================================
# ==============================================================================

# ------------------------------------------------------------------------------
# FIGURE A1: Bilingual Wordclouds
# ------------------------------------------------------------------------------
if (exists("tidy_corpus")) {
  png("Visualisations/Appendix/Figure_A1_Wordclouds.png", width = 2400, height = 1400, res = 300)
  par(mfrow=c(1,2), mar=c(1,1,3,1))
  set.seed(123)
  
  startup_colors <- colorRampPalette(c("#a8ddc0", "#27ae60"))(8)
  startup_words <- tidy_corpus %>% filter(type == "Startup") %>% count(word)
  wordcloud(words = startup_words$word, freq = startup_words$n, max.words = 40,
            scale = c(2.5, 0.4), colors = startup_colors)
  title("Start-ups: Technical Substance", col.main = "#27ae60", font.main = 2, cex.main = 1.1)
  
  incumbent_colors <- colorRampPalette(c("#8395a7", "#2c3e50"))(8)
  incumbent_words <- tidy_corpus %>% filter(type == "Incumbent") %>% count(word)
  wordcloud(words = incumbent_words$word, freq = incumbent_words$n, max.words = 40,
            scale = c(2.5, 0.4), colors = incumbent_colors)
  title("Incumbents: Institutional Fa\u00e7ade", col.main = "#2c3e50", font.main = 2, cex.main = 1.1)
  
  dev.off()
  par(mfrow=c(1,1))
  print("Figure A1: Wordclouds gespeichert.")
}


# ------------------------------------------------------------------------------
# FIGURE A2: Density Distribution of the Isomorphism Index
# ------------------------------------------------------------------------------
figA2 <- ggplot(iso_index_data, aes(x = cosine_similarity)) +
  geom_density(fill = "#27ae60", alpha = 0.4, color = "#27ae60", lwd = 1) +
  geom_vline(aes(xintercept = mean(cosine_similarity)), color = "#2c3e50", linetype = "dashed", lwd = 1) +
  annotate("text", x = mean(iso_index_data$cosine_similarity) + 0.015, y = 1.0,
           label = paste("Field mean:", round(mean(iso_index_data$cosine_similarity), 3)),
           color = "#2c3e50", fontface = "bold", hjust = 0) +
  labs(
    title = "Figure A2: Density Distribution of the Isomorphism Index",
    subtitle = "A continuous gradient of institutional adaptation rather than a sharp field split",
    x = "Isomorphism score (cosine similarity to the established benchmark)", y = "Density"
  ) +
  theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold", size = 14))

print(figA2)
ggsave("Visualisations/Appendix/Figure_A2_Isomorphism_Density.png", plot = figA2, width = 8, height = 5, dpi = 300)


# ------------------------------------------------------------------------------
# FIGURE A3: Permutation Baseline
# ------------------------------------------------------------------------------
if (file.exists("permutation_baseline_results.rds")) {
  permutation_baseline_results <- readRDS("permutation_baseline_results.rds")
  
  perm_df <- bind_rows(
    data.frame(variant = "Raw", cosine_similarity = permutation_baseline_results$raw$perm_means),
    data.frame(variant = "TF-IDF-weighted", cosine_similarity = permutation_baseline_results$tfidf$perm_means)
  )
  observed_df <- data.frame(
    variant = c("Raw", "TF-IDF-weighted"),
    observed = c(permutation_baseline_results$raw$observed, permutation_baseline_results$tfidf$observed),
    p_value  = c(permutation_baseline_results$raw$p_value, permutation_baseline_results$tfidf$p_value)
  ) %>% mutate(label = paste0("observed = ", round(observed, 3), "\np = ", format.pval(p_value, digits = 3)))
  
  figA3 <- ggplot(perm_df, aes(x = cosine_similarity)) +
    geom_histogram(fill = "#bdc3c7", color = "white", bins = 40) +
    geom_vline(data = observed_df, aes(xintercept = observed), color = "#27ae60", lwd = 1.1) +
    geom_text(data = observed_df, aes(x = observed, y = Inf, label = label),
              color = "#2c3e50", fontface = "bold", size = 3.2, hjust = -0.05, vjust = 1.3) +
    facet_wrap(~variant, scales = "free_x") +
    labs(
      title = "Figure A3: Permutation Baseline for the Isomorphism Index",
      subtitle = "Observed mean vs. 1,000 random group permutations, raw and TF-IDF-weighted",
      x = "Mean cosine similarity under random group assignment", y = "Frequency"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14), strip.text = element_text(face = "bold", size = 12))
  
  print(figA3)
  ggsave("Visualisations/Appendix/Figure_A3_Permutation_Baseline.png", plot = figA3, width = 10, height = 5.5, dpi = 300)
} else {
  warning("Datei 'permutation_baseline_results.rds' nicht gefunden. Figure A3 nicht erstellt.")
}


# ------------------------------------------------------------------------------
# FIGURE A4: Isomorphism Index, Raw vs. TF-IDF-Weighted
# ------------------------------------------------------------------------------
if (file.exists("isomorphism_index_roh_vs_tfidf_comparison.csv")) {
  comparison_roh_vs_tfidf <- read_csv("isomorphism_index_roh_vs_tfidf_comparison.csv")
  
  figA4 <- ggplot(comparison_roh_vs_tfidf, aes(x = cosine_similarity, y = cosine_similarity_tfidf)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
    geom_point(color = "#27ae60", size = 3.5, alpha = 0.8) +
    geom_text_repel(aes(label = startup), size = 3, max.overlaps = 20) +
    labs(
      title = "Figure A4: Isomorphism Index, Raw vs. TF-IDF-Weighted",
      subtitle = "Weighting shifts individual rankings without changing the overall pattern",
      x = "Cosine similarity (raw, Script 04)", y = "Cosine similarity (TF-IDF-weighted, Script 04c)"
    ) +
    theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold", size = 14))
  
  print(figA4)
  ggsave("Visualisations/Appendix/Figure_A4_Raw_vs_TFIDF.png", plot = figA4, width = 8, height = 7, dpi = 300)
} else {
  warning("Datei 'isomorphism_index_roh_vs_tfidf_comparison.csv' nicht gefunden. Figure A4 nicht erstellt.")
}


# ------------------------------------------------------------------------------
# FIGURE A5: Use of Institutional Façade Keywords
# ------------------------------------------------------------------------------
if (exists("tidy_corpus") & exists("sustainability_dict")) {
  top_shared_words <- tidy_corpus %>%
    filter(word %in% sustainability_dict$word[sustainability_dict$category == "Institutional_Fa\u00e7ade"]) %>%
    count(type, word) %>%
    group_by(type) %>%
    slice_max(n, n = 10, with_ties = FALSE) %>%
    ungroup()
  
  figA5 <- ggplot(top_shared_words, aes(x = reorder_within(word, n, type), y = n, color = type)) +
    geom_point(size = 3.5) +
    geom_segment(aes(x = reorder_within(word, n, type), xend = reorder_within(word, n, type), y = 0, yend = n), lwd = 1) +
    scale_x_reordered() + coord_flip() +
    facet_wrap(~type, scales = "free_y") +
    scale_color_manual(values = c("Incumbent" = "#2c3e50", "Startup" = "#27ae60")) +
    labs(
      title = "Figure A5: Use of Institutional Fa\u00e7ade Keywords",
      subtitle = "Frequency of the ten most common fa\u00e7ade terms by organization type",
      y = "Absolute word count (n)", x = "Institutional term"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14), legend.position = "none",
          strip.text = element_text(face = "bold", size = 12))
  
  print(figA5)
  ggsave("Visualisations/Appendix/Figure_A5_Top_Keywords.png", plot = figA5, width = 10, height = 6, dpi = 300)
}


# ------------------------------------------------------------------------------
# FIGURE A6: keyATM Comparison (Dictionary vs. keyATM)
# ------------------------------------------------------------------------------
if (file.exists("robustness_check_results.rds")) {
  robustness_check_results <- readRDS("robustness_check_results.rds")
  
  keyatm_comparison_df <- data.frame(
    variant = c("Dictionary\n(Script 05)", "keyATM\nunadjusted", "keyATM\nadjusted"),
    t_value = c(as.numeric(robustness_check_results$dict$statistic),
                as.numeric(robustness_check_results$keyatm_full$statistic),
                as.numeric(robustness_check_results$keyatm_clean$statistic)),
    p_value = c(robustness_check_results$dict$p.value,
                robustness_check_results$keyatm_full$p.value,
                robustness_check_results$keyatm_clean$p.value)
  ) %>% mutate(variant = factor(variant, levels = variant), p_label = paste0("p ", format.pval(p_value, digits = 3)))
  
  figA6 <- ggplot(keyatm_comparison_df, aes(x = variant, y = t_value, fill = variant)) +
    geom_col(width = 0.55) +
    geom_text(aes(label = p_label), vjust = -0.6, fontface = "bold", size = 3.8, color = "#2c3e50") +
    scale_fill_manual(values = c("#2c3e50", "#5d9c7a", "#27ae60")) +
    labs(
      title = "Figure A6: Robustness of H1 Across Three Operationalizations",
      subtitle = "Dictionary-based vs. keyATM-based classification",
      x = NULL, y = "t-value"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", plot.title = element_text(face = "bold", size = 14))
  
  print(figA6)
  ggsave("Visualisations/Appendix/Figure_A6_KeyATM_Comparison.png", plot = figA6, width = 8, height = 6, dpi = 300)
} else {
  warning("Datei 'robustness_check_results.rds' nicht gefunden. Figure A6 nicht erstellt.")
}


# ------------------------------------------------------------------------------
# FIGURE A7: Effect Sizes (Cohen's d) — Dictionary only (mit/ohne SMI)
# ------------------------------------------------------------------------------
if (file.exists("robustness_supplementary_results.rds")) {
  robustness_supplementary_results <- readRDS("robustness_supplementary_results.rds")
  
  extract_d <- function(d_obj, label) {
    data.frame(variant = label, d = as.numeric(d_obj$estimate),
               ci_lower = as.numeric(d_obj$conf.int[1]), ci_upper = as.numeric(d_obj$conf.int[2]))
  }
  
  cohend_df <- bind_rows(
    extract_d(robustness_supplementary_results$cohens_d_dict, "Dictionary (full sample)"),
    extract_d(robustness_supplementary_results$cohens_d_dict_no_smi, "Dictionary (excl. SMI firms)")
  ) %>% mutate(variant = factor(variant, levels = rev(variant)))
  
  figA7 <- ggplot(cohend_df, aes(x = d, y = variant)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    geom_pointrange(aes(xmin = ci_lower, xmax = ci_upper), color = "#27ae60", size = 0.8, lwd = 1) +
    labs(
      title = "Figure A7: Effect Sizes (Cohen's d) with Confidence Intervals",
      subtitle = "Dictionary-based classification, full sample vs. excluding branch-unrelated SMI firms",
      x = "Cohen's d (start-up \u2212 incumbent)", y = NULL
    ) +
    theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold", size = 14))
  
  print(figA7)
  ggsave("Visualisations/Appendix/Figure_A7_Cohens_D_Forest.png", plot = figA7, width = 8, height = 5, dpi = 300)
} else {
  warning("Datei 'robustness_supplementary_results.rds' nicht gefunden. Figure A7 nicht erstellt.")
}


# ------------------------------------------------------------------------------
# FIGURE A8: Dominant Language by Firm and Organization Type
# ------------------------------------------------------------------------------
if (exists("firm_dominant_language")) {
  lang_share_df <- firm_dominant_language %>%
    count(type, dominant_language) %>%
    group_by(type) %>%
    mutate(share = n / sum(n) * 100)
  
  figA8 <- ggplot(lang_share_df, aes(x = type, y = share, fill = dominant_language)) +
    geom_col(width = 0.5, color = "white", lwd = 0.7) +
    geom_text(aes(label = paste0(round(share, 1), "%")),
              position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 4) +
    scale_fill_manual(values = c("de" = "#2c3e50", "en" = "#27ae60", "fr" = "#5d9c7a",
                                 "other" = "#bdc3c7", "unknown" = "#7f8c8d")) +
    labs(
      title = "Figure A8: Dominant Language by Firm and Organization Type",
      subtitle = "Significant imbalance confirmed even when restricted to German vs. English",
      x = "Organization type", y = "Share of firms (%)", fill = "Dominant language"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 14),
          panel.grid.major.x = element_blank())
  
  print(figA8)
  ggsave("Visualisations/Appendix/Figure_A8_Language_Distribution.png", plot = figA8, width = 8, height = 6, dpi = 300)
} else {
  warning("Objekt 'firm_dominant_language' nicht im Workspace. Figure A8 nicht erstellt.")
}

