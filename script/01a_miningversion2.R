install.packages("xml2")
library(rvest)
library(xml2)
library(tidyverse)

# 1. Load  base data from the CSV
firms_data3 <- read.csv("firms_data_starting.csv", stringsAsFactors = FALSE)

# 2. Function to find relevant subpages for one specific URL
find_company_subpages <- function(main_url) {
  tryCatch({
    # Read the homepage
    page <- read_html(main_url)
    
    # Identify the domain (e.g., "depoly.co") to keep links internal
    domain_name <- url_parse(main_url)$server
    
    # Get all links (<a> tags)
    nodes <- html_nodes(page, "a")
    link_texts <- html_text(nodes, trim = TRUE)
    link_urls <- html_attr(nodes, "href")
    
    # Combine into a temporary table
    temp_links <- data.frame(text = link_texts, url = link_urls, stringsAsFactors = FALSE)
    temp_links <- na.omit(temp_links) # Remove empty links
    
    # Define keywords to search for
    keywords <- "about|mission|sustainability|nachhaltigkeit|company|impact|uber-uns"
    
    # Filter: Link text or URL must contain a keyword
    relevant <- temp_links[grepl(keywords, temp_links$text, ignore.case = TRUE) | 
                             grepl(keywords, temp_links$url, ignore.case = TRUE), ]
    
    # Clean URLs: Convert relative (/about) to absolute (https://site.com/about)
    final_urls <- c()
    for (l in relevant$url) {
      abs_url <- url_absolute(l, main_url)
      if (!is.na(domain_name) && grepl(domain_name, abs_url)) {
        final_urls <- c(final_urls, abs_url)
      }
    }
    
    return(unique(final_urls))
    
  }, error = function(e) return(NA))
}

# 3. Loop through your CSV and find subpages for every firm
all_results_list <- list()

for (i in 1:nrow(firms_data3)) {

  all_results_list[[firms_data3$name[i]]] <- find_company_subpages(firms_data3$url[i])
}

firms_data3_final <- data.frame(
  name = names(all_results_list),
  subpage_url = I(all_results_list)
) %>% 
  unnest(subpage_url) %>%
  filter(!is.na(subpage_url)) %>%
  left_join(firms_data3 %>% select(name, type), by = "name")

# View your result
print("New table created: firms_data3_final")
head(firms_data3_final)