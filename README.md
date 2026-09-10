Author: Vivienne Schegg
Course: Data Mining in Social Sciences using R (Spring 2026)
University: University of Lucerne

# Research Question: To what extent do Swiss sustainability start-ups adopt the institutionalised corporate vocabulary of established companies or do they retain an independent technical focus?

# Project Goal
The goal of this project is to analyze whether Swiss green-tech start-ups adopt the institutionalized language of established corporations to gain legitimacy. This project tests whether the "liability of newness" forces Swiss disruptors to adopt the corporate vocabulary of incumbents to gain legitimacy among local investors and regulators.

# The project should demonstrate
- Automated data collection: Creating a custom crawler to identify and scrape the corporate pages which are relevant for sustainability values of the company.
- Data Mining: TF-IDF analysis and Cosine Similarity measures
- Reproducibility: Creating a structured R pipeline from raw URL discovery to final visualisation

# Data Source
- Start-up sample: 35 Swiss cleantech and sustainability firms initially targeted, drawn from the "TOP 100 Swiss Startup" list; 26 yielded a complete, evaluable text corpus.
- Incumbent sample: 19 leading Swiss-listed corporations (SMI/SPI) with mature ESG profiles initially targeted; 12 yielded a complete, evaluable text corpus.
- Final analyzed sample: n = 38 (26 start-ups, 12 incumbents). See the paper, Appendix Table A1, for the full firm-by-firm exclusion breakdown.

# Methodology and Repository Structure

01_mining: Data Initialization
- Defines two firm dataframes: firms_data (first version with specific subpages) and firms2_data (cleaned, main domains only, 35 startups + 19 incumbents)
- Exports firms_data_starting.csv as the pipeline's starting point

02_searchingsubpages: Automated Crawler
- Function find_company_subpages(): keyword matching (about, mission, sustainability, nachhaltigkeit, impact, esg, csr, etc.) across all links on a homepage
- Domain validation prevents following external links (e.g. LinkedIn/Instagram); user-agent simulation and timeout handling
- Exports firms_subpages_crawled.csv
- 
03_scrapping: Text Extraction
- Scrapes the identified subpages via httr/rvest, targeting semantic tags (p, h1, h2, h3)
- Cleans text and saves the unified dataset as full_research_data.json

03a_manuel_scrapping: Manual Supplement
- Manual re-collection for 5 incumbents blocked by bot detection (Nestlé, Holcim, SBB, Swisscom, ABB Schweiz), using browser devtools with the same DOM selector as the automated crawler
- Appends these firms to research_data and overwrites full_research_data.json

04_tf_idf_analysis: Baseline Mining
- Tokenizes the corpus (tidy_corpus), bilingual stopword filter (DE/EN)
- Calculates TF-IDF scores for group-characteristic words (startup vs. incumbent) and the isomorphism index via cosine similarity

04a_length_control_permutation_baseline: Robustness Check I
- Tests correlation between document length and the isomorphism index (control variable)
- Permutation test (1'000 iterations) for both the raw and the TF-IDF-weighted firm-word matrix, testing whether the isomorphism index differs significantly from a random baseline

04b_language_detection: Robustness Check II
- Detects language per subpage (cld2), cross-tabs organization type × language at both subpage and firm level
- Chi-square / Fisher test on whether the language distribution differs significantly between startups and incumbents

04c_isomophism_index: Robustness Check III
- Recalculates the isomorphism index alternatively with TF-IDF weighting (instead of raw word counts) per startup
- Compares ranking/values directly against the raw version from Script 04

05_analysiswithkeyword: Theory-Driven Dictionary Analysis
- Bilingual 41-term dictionary categorizing the corpus into "Institutional Facade" (value-based language) and "Technical Substance" (operational language)
- Runs t-test, Cohen's d and correlation test (isomorphism index × substance share), writes statistical_results_report.txt

05a_seeded_topic_model: Robustness Check IV (keyATM)
- Seeded topic model (keyATM) with 12 seed terms as a robustness check against the 41-term dictionary
- Identifies and cleans data-quality outliers (Swiss Prime Site, Planted Foods); determinism check (Fit A vs. Fit B) and a 40-seed stability test

05b_effects_jackknife_smi: Robustness Check V
- H1 test with/without SMI firms (Roche, Richemont)
- Effect sizes (Cohen's d) with confidence intervals for both the dictionary and keyATM approach; leave-one-out jackknife across the 12 keyATM seed words

5c_seed-stability: Robustness Check VI
- keyATM seed-stability 
- Tests whether significance and direction of the effect stay consistent across different random seeds

06_Visualisations: Data Communication
- Generates 5 main-text figures (Figure 1–5: TF-IDF terms, isomorphism ranking, facade/substance gap, boxplot, decoupling scatter) and 8 appendix figures (A1–A8: wordclouds, density distribution, permutation baseline, raw-vs-TF-IDF, keywords, keyATM comparison, Cohen's d forest plot, language distribution)
- All figures exported as PNG

07_diagnosis: Pipeline Coverage Diagnostics
- Function diagnose_coverage(): checks, per organization type, how many firms make it through each pipeline stage (crawler → scraping → tokenization → dictionary hits)
- Also includes an overlap check of startup vocabulary against incumbent vocabulary (a precondition for appearing in the isomorphism index)

08_wayback_machine: Panel-Design Feasibility Test
- Tests, for all startups, how many yearly snapshots of relevant text pages are available in the Internet Archive (Wayback CDX API)
- Served as a decision basis against a panel design --> not used, for testing purposes only

# Key Findings
- Incumbents rely more heavily on facade terminology (symbolic legitimacy), while start-ups emphasize substance (technical legitimacy). Among incumbents, over two-thirds (68.1%) of sustainability communication falls into the facade category; among start-ups, roughly three-fifths (61.2%) of the vocabulary is technical substance. Start-ups' legitimacy needs to be functional, they are relevant because of what they do, not primarily because of symbolic meaning.
- The isomorphism index ranks start-ups by their gradient of communicational alignment with incumbent vocabulary. Absolute cosine-similarity values are in a fairly low range, but relative differences between start-ups are substantial. Isomorphism is not a binary state in the sustainability sector but a gradient.
- The TF-IDF analysis shows two distinct worlds. Among start-ups, top-ranked terms include "clearspace", "orbit", "synhelion", "composite", "robotics" and "drone", describing concrete physical processes and products. Among incumbents, terms like "reimagine", "caregivers", "recruiting" and "committee" dominate, reflecting a focus on communication, governance and broader societal impact rather than technical operations.


# Reflection:
The results present a more nuanced picture than a simple duality. H1, that start-ups use significantly more technical-substance language than incumbents, is strongly and robustly confirmed: the effect holds across the dictionary-based classification, a method-independent keyATM check, a sample excluding non-industry SMI incumbents, and a language-restricted subsample, ruling out translation artifacts as an explanation. This lends empirical, quantifiable support to Meyer and Rowan's (1977) classical argument that the evolution of organizational language is itself a core aspect of institutional isomorphism.
H2, however, was not supported: a start-up's degree of linguistic alignment with incumbent vocabulary does not correlate significantly with its share of technical substance language. This is not simply a null result to explain away. It suggests that isomorphism and substance orientation are two largely independent dimensions of communication rather than a linear trade-off, a start-up can professionalize its vocabulary without hollowing out its technical core, consistent with the category-spanning "hybrid vigor" described by Wry, Lounsbury and Jennings (2014). The limited sample size (n = 26 start-ups) also constrains the statistical power to detect a moderate effect, so this finding should be read as inconclusive rather than as firm evidence against H2.
Several methodological limitations qualify these findings and are worth carrying forward into any follow-up work. Data coverage was uneven: only 38 of the 54 originally targeted firms could be fully processed, for reasons ranging from bot-detection to JavaScript-rendered pages without server-side HTML, a pattern that itself reflects real differences in web-presence maturity between start-ups and incumbents. The dictionary and keyATM methods classify individual words without capturing the syntactic or symbolic context in which they are used, so neither can distinguish a genuinely technical use of a term like "sustainability" from a purely symbolic one. The isomorphism index is also confounded to some extent by raw text length, and start-ups and incumbents differ significantly in their language distribution (German vs. English), which required an additional robustness check to rule out as a confound for H1. Finally, the cross-sectional design captures a single point in time and cannot speak to whether individual start-ups' language actually shifts toward institutionalized vocabulary as they mature, a natural direction for longitudinal follow-up research.
Methodologically, the project shows that abstract institutional-theory constructs, mimetic isomorphism, decoupling, vocabularies of practice, can be operationalized and tested quantitatively at scale using automated text mining, rather than relying solely on qualitative case studies. Substantively, it also cautions against over-interpretation: a high share of institutional facade language among incumbents should be read as a linguistic pattern rather than proof of greenwashing or deficient sustainability performance, since this study measures communication, not actual environmental impact.
