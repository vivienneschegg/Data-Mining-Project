# ==============================================================================
# PROJEKT: Masterseminararbeit - Institutioneller Isomorphismus
# SKRIPT 06: Datenvisualisierung und wissenschaftliche Grafiken (Figure 1-7)
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

if (!dir.exists("Visualisations")) {
  dir.create("Visualisations")
}

# 1. Daten aus den vorherigen Analyseschritten laden
if (file.exists("decoupling_aggregate_results.csv") & 
    file.exists("isomorphism_index_results.csv") &
    file.exists("decoupling_firm_results.csv")) {
  
  gap_data          <- read_csv("decoupling_aggregate_results.csv")
  iso_index_data    <- read_csv("isomorphism_index_results.csv")
  firm_decoupling   <- read_csv("decoupling_firm_results.csv")
  
} else {
  stop("FEHLER: Die Analysedaten aus Skript 04/05 wurden nicht gefunden! Bitte führe diese zuerst aus.")
}

# ==============================================================================
# NACHHALTIGKEITS-WÖRTERBUCH & RE-IMPORT FÜR ROHDATEN-PLOTS
# ==============================================================================
if (!exists("tidy_corpus") | !exists("sustainability_dict")) {
  warning("Hinweis: 'tidy_corpus' oder 'sustainability_dict' nicht im Workspace gefunden. Plots basieren auf den geladenen CSVs.")
}


# ==============================================================================
# FIGURE 1: STRATEGIC GAP (Aggregierte Entkopplung nach Meyer & Rowan)
# ==============================================================================
plot1 <- ggplot(gap_data, aes(x = type, y = share, fill = category)) +
  geom_col(width = 0.5, color = "white", lwd = 0.7) +
  geom_text(aes(label = paste0(round(share, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold", size = 4.5) +
  scale_fill_manual(
    values = c("Institutional_Façade" = "#2c3e50", "Technical_Substance" = "#27ae60"),
    labels = c("Institutional Façade (Institutionelle Fassade)", "Technical Substance (Technische Substance)")
  ) +
  labs(
    title = "Figure 1: Strategic Decoupling vs. Technical Substance",
    subtitle = "Vergleich des linguistischen Fokus von etablierten Unternehmen (n=15) vs. Start-ups (n=30)",
    y = "Vokabular-Anteil (%)", 
    x = "Organisationstyp",
    fill = "Theoretisches Konstrukt"
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  )

print(plot1)
ggsave("Visualisations/Figure_1_Strategic_Gap.png", plot = plot1, width = 8, height = 6, dpi = 300)


# ==============================================================================
# FIGURE 2: ISOMORPHISM GRADIENT (Das Ranking aller 30 Start-ups)
# ==============================================================================
plot2 <- ggplot(iso_index_data, aes(x = reorder(startup, cosine_similarity), y = cosine_similarity, fill = cosine_similarity)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_gradient(low = "#a1d99b", high = "#00441b") +
  labs(
    title = "Figure 2: Mimetic Isomorphism Index",
    subtitle = "Linguistische Konvergenz der einzelnen Schweizer Start-ups zum kollektiven Corporate-Profil",
    y = "Isomorphie Score (Kosinus-Ähnlichkeit: 0 = Divergent, 1 = Identisch)", 
    x = "Nachhaltigkeits-Start-up"
  ) +
  theme_minimal(base_size = 11) + 
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 9)
  )

print(plot2)
ggsave("Visualisations/Figure_2_Isomorphism_Gradient.png", plot = plot2, width = 9, height = 8, dpi = 300)


# ==============================================================================
# FIGURE 3: TOP INSTITUTIONAL KEYWORDS (Lollipop Chart für das Feld)
# ==============================================================================
if (exists("tidy_corpus") & exists("sustainability_dict")) {
  top_shared_words <- tidy_corpus %>%
    filter(word %in% sustainability_dict$word[sustainability_dict$category == "Institutional_Façade"]) %>%
    count(type, word) %>%
    group_by(type) %>%
    slice_max(n, n = 10, with_ties = FALSE) %>%
    ungroup()
  
  plot3 <- ggplot(top_shared_words, aes(x = reorder_within(word, n, type), y = n, color = type)) +
    geom_point(size = 3.5) +
    geom_segment(aes(x = reorder_within(word, n, type), xend = reorder_within(word, n, type), y = 0, yend = n), lwd = 1) +
    scale_x_reordered() + 
    coord_flip() +
    facet_wrap(~type, scales = "free_y") +
    scale_color_manual(values = c("Incumbent" = "#2c3e50", "Startup" = "#27ae60")) +
    labs(
      title = "Figure 3: Use of Institutional Keywords",
      subtitle = "Häufigkeit der Top 10 'Fassaden'-Begriffe zur Generierung sozialer Legitimität",
      y = "Absoluter Word Count (n)", 
      x = "Institutioneller Begriff"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 12)
    )
  
  print(plot3)
  ggsave("Visualisations/Figure_3_Top_Keywords.png", plot = plot3, width = 10, height = 6, dpi = 300)
}


# ==============================================================================
# FIGURE 4: VERTEILUNGSDICHTE (Strukturelle Fragmentierung des Feldes)
# ==============================================================================
plot4 <- ggplot(iso_index_data, aes(x = cosine_similarity)) +
  geom_density(fill = "#27ae60", alpha = 0.4, color = "#27ae60", lwd = 1) +
  geom_vline(aes(xintercept = mean(cosine_similarity)), color = "#2c3e50", linetype = "dashed", lwd = 1) +
  annotate("text", x = mean(iso_index_data$cosine_similarity) + 0.015, y = 1.0, 
           label = paste("Ø Feld-Mittelwert:", round(mean(iso_index_data$cosine_similarity), 2)), 
           color = "#2c3e50", fontface = "bold", hjust = 0) +
  labs(
    title = "Figure 4: Verteilungsdichte der mimetischen Isomorphie",
    subtitle = "Häufigkeitskonzentration: Drängen Start-ups kollektiv zur Anpassung oder spaltet sich das Feld?",
    x = "Isomorphie Score (Kosinus-Ähnlichkeit zum etablierten Standard)",
    y = "Dichte (Dichte-Konzentration)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

print(plot4)
ggsave("Visualisations/Figure_4_Isomorphism_Density.png", plot = plot4, width = 8, height = 5, dpi = 300)


# ==============================================================================
# FIGURE 5: SCATTERPLOT (Das organisationssoziologische Spannungsfeld)
# ==============================================================================
correlation_data <- iso_index_data %>%
  left_join(firm_decoupling, by = c("startup" = "name"))

plot5 <- ggplot(correlation_data, aes(x = cosine_similarity, y = technical_substance_share)) +
  geom_point(color = "#27ae60", size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", color = "#2c3e50", linetype = "solid", se = TRUE, fill = "gray90", lwd = 0.8) +
  geom_text_repel(aes(label = startup), size = 3, max.overlaps = 20) + 
  labs(
    title = "Figure 5: Das Spannungsfeld zwischen Fassade und Substanz auf Mikro-Ebene",
    subtitle = "Korrelation zwischen allgemeiner Feld-Anpassung und dem spezifischen Anteil technischer Sprache",
    x = "Isomorphie Index (Allgemeine Ähnlichkeit zu den Incumbents)",
    y = "Anteil technischer Begriffe im Wörterbuch-Filter (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

print(plot5)
ggsave("Visualisations/Figure_5_Decoupling_Micro_Scatter.png", plot = plot5, width = 9, height = 7, dpi = 300)

# ==============================================================================
# FIGURE 6: HÖCHSTE TF-IDF WERTE (Distinktive Kernbegriffe im Kontrast)
# ==============================================================================
if (exists("group_tf_idf")) {
  
  # KORREKTUR: Die Top 10 Begriffe explizit hier berechnen, 
  # damit das Objekt 'top_tf_idf' garantiert existiert:
  top_tf_idf <- group_tf_idf %>%
    group_by(type) %>%
    slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
    ungroup()
  
  plot6 <- ggplot(top_tf_idf, aes(x = reorder_within(word, tf_idf, type), y = tf_idf, fill = type)) +
    geom_col(width = 0.6) +
    scale_x_reordered() +
    coord_flip() +
    facet_wrap(~type, scales = "free_y") +
    scale_fill_manual(values = c("Incumbent" = "#2c3e50", "Startup" = "#27ae60")) +
    labs(
      title = "Figure 6: Die distinktivsten Kernbegriffe der beiden Welten (TF-IDF)",
      subtitle = "Top 10 exklusive Wörter pro Gruppe, gewichtet nach ihrer statistischen Trennschärfe",
      x = "Spezifischer Begriff",
      y = "TF-IDF Gewichtung (Höher = Charakteristischer für diese Gruppe)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none", 
      plot.title = element_text(face = "bold", size = 14),
      strip.text = element_text(face = "bold", size = 12)
    )
  
  print(plot6)
  ggsave("Visualisations/Figure_6_Distinctive_TFIDF.png", plot = plot6, width = 10, height = 6, dpi = 300)
  
} else {
  warning("Objekt 'group_tf_idf' wurde nicht im Workspace gefunden. Figure 6 konnte nicht erstellt werden.")
}


# ==============================================================================
# FIGURE 7: BILINGUALE WORD CLOUDS (Der explorative Abschluss)
# ==============================================================================
if (exists("tidy_corpus")) {
  # Hochauflösenden PNG-Export für die Wordcloud starten
  png("Visualisations/Figure_7_Wordclouds.png", width = 2400, height = 1400, res = 300)
  
  par(mfrow=c(1,2), mar=c(1,1,3,1)) 
  set.seed(123)
  
  # A. Wordcloud für Start-ups (Fokus auf Technik & Operationen)
  startup_words <- tidy_corpus %>% filter(type == "Startup") %>% count(word)
  wordcloud(
    words = startup_words$word, 
    freq = startup_words$n, 
    max.words = 40, 
    scale = c(2.5, 0.4),
    colors = brewer.pal(8, "Dark2")
  )
  title("Startups: Technical Substance", col.main = "#27ae60", font.main = 2, cex.main = 1.1)
  
  # B. Wordcloud für Incumbents (Fokus auf institutionalisierte Legitimität)
  incumbent_words <- tidy_corpus %>% filter(type == "Incumbent") %>% count(word)
  wordcloud(
    words = incumbent_words$word, 
    freq = incumbent_words$n, 
    max.words = 40, 
    scale = c(2.5, 0.4),
    colors = brewer.pal(8, "Blues")[5:8]
  )
  title("Incumbents: Institutional Façade", col.main = "#2c3e50", font.main = 2, cex.main = 1.1)
  
  dev.off() # Grafikgerät schließen und Datei schreiben
  par(mfrow=c(1,1)) # R-Standardkonfiguration wiederherstellen
  print("Figure 7: Wordclouds erfolgreich unter 'Figure_7_Wordclouds.png' gespeichert.")
}
