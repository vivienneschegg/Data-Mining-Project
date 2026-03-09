library(jsonlite)
library(tidytext)
library(dplyr)
library(stopwords)
install.packages("widyr")
library(widyr)

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