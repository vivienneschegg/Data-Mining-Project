#   Visualisation 1: strategic gap
gap_data <- tidy_corpus %>%
  inner_join(sustainability_dict, by = "word") %>%
  group_by(group, category) %>%
  tally() %>%
  group_by(group) %>%
  mutate(share = n / sum(n) * 100)

plot1 <- ggplot(gap_data, aes(x = group, y = share, fill = category)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(share, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Institutional_Façade" = "#2c3e50", "Technical_Substance" = "#27ae60")) +
  labs(title = "Figure 1: Strategic Decoupling vs. Technical Substance",
       subtitle = "Comparing the linguistic focus of Incumbents vs. Startups",
       y = "Vocabulary Share (%)", x = "") +
  theme_minimal() + theme(legend.position = "bottom")
print(plot1)

#   Visualisation 2: isomorphism gradient
incumbent_profile <- tidy_corpus %>%
  filter(group == "Incumbent") %>%
  count(word) %>%
  mutate(name = "AVERAGE_INCUMBENT")

comparison_matrix <- tidy_corpus %>%
  filter(group == "Startup") %>%
  count(name, word) %>%
  bind_rows(incumbent_profile)

iso_index_data <- comparison_matrix %>%
  pairwise_similarity(name, word, n) %>%
  filter(item2 == "AVERAGE_INCUMBENT") %>%
  rename(name = item1, index = similarity)

plot2 <- ggplot(iso_index_data, aes(x = reorder(name, index), y = index, fill = index)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#a1d99b", high = "#00441b") +
  labs(title = "Figure 2: Mimetic Isomorphism Index",
       subtitle = "Cosine similarity to the aggregate Incumbent profile",
       y = "Isomorphism Score (0 = Different, 1 = Identical)", x = "Startup Name") +
  theme_minimal() + theme(legend.position = "none")
print(plot2)

#   Visualisation 3: Top Keywords
top_shared_words <- tidy_corpus %>%
  filter(word %in% sustainability_dict$word[sustainability_dict$category == "Institutional_Façade"]) %>%
  count(group, word) %>%
  group_by(group) %>%
  slice_max(n, n = 10)

plot3 <- ggplot(top_shared_words, aes(x = reorder(word, n), y = n, color = group)) +
  geom_point(size = 3) +
  geom_segment(aes(x = word, xend = word, y = 0, yend = n)) +
  coord_flip() +
  facet_wrap(~group, scales = "free_y") +
  labs(title = "Figure 3: Use of Institutional Keywords",
       subtitle = "Frequency of 'Façade' terms per group",
       y = "Count (n)", x = "Institutional Term") +
  theme_minimal() + scale_color_manual(values = c("Incumbent" = "#2c3e50", "Startup" = "#27ae60"))

print(plot3)

# Visualisation 4: Word Cloud
install.packages("wordcloud")
library(wordcloud)

set.seed(123)
par(mfrow=c(1,2)) # Two plots side-by-side

startup_words <- tidy_corpus %>% filter(group == "Startup") %>% count(word)
wordcloud(words = startup_words$word, freq = startup_words$n, max.words=50, colors=brewer.pal(8, "Dark2"), main="Startups: Technical Focus")

incumbent_words <- tidy_corpus %>% filter(group == "Incumbent") %>% count(word)
wordcloud(words = incumbent_words$word, freq = incumbent_words$n, max.words=50, colors=brewer.pal(8, "Blues"), main="Incumbents: Institutional Façade")

#   Visualisation 5: Network Analysis
install.packages("ggraph")
install.packages("igraph")
install.packages("tidygraph")
library(ggraph)
library(igraph)
library(widyr)
library(tidygraph)

word_cors <- tidy_corpus %>%
  group_by(word) %>%
  filter(n() >= 10) %>% # Only common words to avoid noise
  pairwise_cor(word, name, sort = TRUE) # Correlation based on appearing in the same company profiles

threshold <- 0.35
word_network <- word_cors %>%
  filter(correlation > threshold) %>%
  as_tbl_graph(directed = FALSE)

plot4 <- ggraph(word_network, layout = "fr") + # Fruchterman-Reingold layout for organic clusters
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "grey70") +
  geom_node_point(color = "#2c3e50", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, size = 4, fontface = "bold") +
  theme_graph() +
  labs(title = "Figure 5: The Isomorphic Word Network",
       subtitle = "Connections represent words frequently appearing together across Swiss companies",
       caption = paste("Correlation threshold >", threshold))

print(plot4)