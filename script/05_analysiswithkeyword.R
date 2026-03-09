#Analysis with pre-chosen keywords
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)

sustainability_dict <- data.frame(
  word = c(
    # Institutional/Abstract (The "Façade")
    "sustainability", "responsibility", "commitment", "values", "future", 
    "impact", "global", "mission", "vision", "transformation", "reimagine",
    "society", "social", "stewardship", "governance", "esg", "sdg",
    
    # Technical/Substance (The "Action")
    "recycling", "technology", "solution", "carbon", "co2", "emissions", 
    "renewable", "energy", "efficiency", "circular", "material", "waste",
    "science", "innovation", "engineering", "robotics", "biological"
  ),
  category = c(rep("Institutional_Façade", 17), rep("Technical_Substance", 17))
)

# Analysis
isomorphism_analysis <- tidy_corpus %>%
  inner_join(sustainability_dict, by = "word") %>%
  group_by(group, category) %>%
  tally() %>%
  group_by(group) %>%
  mutate(share = n / sum(n) * 100)

# Print results
print(isomorphism_analysis)