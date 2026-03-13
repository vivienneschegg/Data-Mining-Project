Author: Vivienne Schegg
Course: Data Mining in Social Sciences using R (Spring 2026)
University: University of Lucerne

# Research Question: To what extent do swiss sustainability startups adopt the institutionalized corporate vocabulary of incumbents versus maintaning a distinct technical focus?

# Project Goal
The goal of this project is to analysze whether Swiss green-tech startups adopt the institutionalized language of established corporations to gain legitimacy. This project should test if the liablitiy of newness forces swiss diruptors to adopt the corporate vocabulary of incumbents to gain legitimacy among local investors and regulators.


# The project should demonstrate
- Automated data collection: Creating a custom crawler to identify and scrape the corporate pages which are relevant for sustainability values of the company.
- Data Mining: TF-IDF analysis and Cosine Similarity measures
- Reproducibility: Creating a structured R pipeline from raw URL discovery to final visualisation

# Data Source
- Startup Sample (n = 15): Top-tier Swiss cleantech and sustainability firms selected from the "TOP 100 Swiss Startup" list
- Incumbent Sample (n = 5): Leading Swiss-listed corporations (SMI/SPI) with mature ESG profiles

# Methodology and Repository Structure

01_miming: Data Initialization
- Defining the sampling frame
- Generates firms_data_startung.csv with base URLs
02_searchingsubpages: Automated Crawler
- Crawler uses keyword matching (e.g., about us, sustainability) to automatically discover relevant subpages for scraping
- Ensures reproducability by automating the discovery of content rather than manual link collection
03_scrapping: Text Extraction
  - Scrapes the identified subpages using httr and rvest
  - Cleans test and saves unified dataset to full_research_data.json
04_tf_idf_analysis: Baseline Mining
- Calculates TF-IDF scores to find group-specific keywords
- Uses Cosine Similarity to create mathematical "isomorphism index"
05_analysiswithkeywords: Theory-Driven
- Categorizes corpus into to theoretical constructs: institutional facade (symbolic, value-based language) and technical substance (operational, action-oriented language)
06_visualisations: Data communication
- Generating final figures for interpretation

# Key Findings
- Analysis shows that incumbents rely more heavily on facade terminology (symbolic legitimacy) while startups emphasizes substance (technical legitimacy). In incumbents nearly 2/3 of the sustainability communication is in the facade category. Where as in Startup 3/4 of the vocabulary is in the technical part. Their legitimacy needs to be functional, they are relevant because of there doing and not of the symbolic/meaning.
- With the final_similarity-ouput we can rank the startups according to their gradient of communicational isomorphism. The absolut cosine-similarity-values are in a rather low range. But in relative comparison a big differences can be found between the startups. Isomorphism isn't a binary state in the sustainability sector but a gradient.
- The TF-IDF-Analysis shows clearly two worlds. In startups the top-words are composite, robotics, cellulose and underwater, which describes physical processes. In incumbents words like reimagine, lives and social are often found. The organisations focus more on their impact on society and their visionary goals.


# Reflection:
While the sample size is small, this project serves as a purposive sample of the most influential actors in the swiss sustainability field. It should demostrates how data mining can be used to empirically test the "strategic decoupling" hypothesis moving beyond qualitative case studies to a quanitative, reproducible measure of organizational behavior. The analysis shows that in the sustainability sector is a clear duality. Incumbents tend to use social legitimacy where as startups use the technical substance for their main legitimacy. 
