library(rvest)
library(xml2)
library(tidyr)
library(dplyr)
library(httr)

# 1. Load CSV directly into 'firms_data3' to make the script reproducible
firms_data3 <- read.csv("firms_data_starting.csv", stringsAsFactors = FALSE)

# 2. Define the search function
find_company_subpages <- function(main_url) {
  tryCatch({
    page <- read_html(main_url)
    domain_name <- url_parse(main_url)$server
    
    nodes <- html_nodes(page, "a")
    temp_links <- data.frame(
      text = html_text(nodes, trim = TRUE),
      url  = html_attr(nodes, "href"),
      stringsAsFactors = FALSE
    ) %>% na.omit()
    
    # Keywords for the subpages
    keywords <- "about|mission|sustainability|nachhaltigkeit|company|impact|uber-uns"
    
    relevant <- temp_links[grepl(keywords, temp_links$text, ignore.case = TRUE) | 
                             grepl(keywords, temp_links$url, ignore.case = TRUE), ]
    
    final_urls <- c()
    for (l in relevant$url) {
      abs_url <- url_absolute(l, main_url)
      # Check if it's the same domain to avoid LinkedIn etc.(happend in previous versions)
      if (!is.na(domain_name) && grepl(domain_name, abs_url)) {
        final_urls <- c(final_urls, abs_url)
      }
    }
    return(unique(final_urls))
  }, error = function(e) return(NA))
}

# 3. Run the loop using 'firms_data3'
firms_subpages <- list()

for (i in 1:nrow(firms_data3)) {
  firms_subpages[[firms_data3$name[i]]] <- find_company_subpages(firms_data3$url[i])
}

# 4. Update 'firms_data3' with the found sub pages
firms_data3_final <- data.frame(
  name = names(firms_subpages),
  subpage_url = I(firms_subpages)
) %>% 
  unnest(subpage_url) %>%
  filter(!is.na(subpage_url)) %>%
  left_join(firms_data3 %>% select(name, type), by = "name")

# View results
print("New table created: firms_data3_final")
head(firms_data3_final)