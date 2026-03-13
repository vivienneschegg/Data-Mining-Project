library(jsonlite)
library(tidytext)
library(dplyr)
library(stopwords)
install.packages("widyr")
library(widyr)
library(ggplot2)
library(forcats)
#Analysis with the keyword found in script 03
research_data <- fromJSON(readLines("full_research_data.json", warn = FALSE))

# Clean the dataset
tidy_corpus <- research_data %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% stopwords("de"), 
         !word %in% stopwords("en"),
         nchar(word) > 3,
         !grepl("[0-9]", word))

# TS-IDF Analysis for both groups
group_tf_idf <- tidy_corpus %>%
  count(group, word, sort = TRUE) %>%
  bind_tf_idf(word, group, n) %>%
  arrange(desc(tf_idf))

head(group_tf_idf %>% filter(group == "Startup"), 15)
head(group_tf_idf %>% filter(group == "Incumbent"), 15)

#Similarity
similarity_matrix <- tidy_corpus %>%
  count(name, word) %>%
  pairwise_similarity(name, word, n) %>%
  arrange(desc(similarity))

print(similarity_matrix)

#Visualisation
isomorphism_ranking <- similarity_matrix %>%
  filter(item1 %in% research_data$name[research_data$group == "Startup"],
         item2 %in% research_data$name[research_data$group == "Incumbent"]) %>%
  group_by(item1) %>%
  summarise(mean_similarity = mean(similarity)) %>%
  arrange(desc(mean_similarity))

ggplot(isomorphism_ranking, aes(x = mean_similarity, y = fct_reorder(item1, mean_similarity))) +
  geom_segment(aes(x = 0, xend = mean_similarity, yend = item1), color = "grey") +
  geom_point(size = 4, color = "#27ae60") +
  labs(
    title = "Mimetic Isomorphism Index",
    subtitle = "How similar is each Startup's language to the Incumbent 'Green Giants'?",
    x = "Cosine Similarity Score",
    y = "Startup Name"
  ) +
  theme_minimal()

# Top 15 Words
top_tfidf <- group_tf_idf %>%
  group_by(group) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup()

ggplot(top_tfidf, aes(x = tf_idf, y = fct_reorder(word, tf_idf), fill = group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~group, scales = "free") +
  scale_fill_manual(values = c("Startup" = "#27ae60", "Incumbent" = "#2c3e50")) +
  theme_minimal() +
  labs(
    title = "Distinctive Terminology (TF-IDF)",
    subtitle = "Characteristic keywords for each organizational group",
    x = "Uniqueness Score (TF-IDF)",
    y = NULL
  )


#I won't use those visualisation, doesn't seem that interesting
