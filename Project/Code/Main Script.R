library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='tiran')

install.packages(c("stringdist", "fuzzyjoin"))

library(tidyverse)
library(readxl)
library(RPostgres) 
library(dbplyr) 
library(fuzzyjoin)
library(stringdist)


-------------------------------
# 1ST STEP -The intention of the below step is to clean both compustat extracts and layoff database company names before initiating fuzzy matching.
  
# Load required packages
library(dplyr)
library(stringr)

# Define common company suffixes and noise words to remove
remove_words <- c(
  # Legal entity suffixes
  "inc", "incorporated", "corp", "corporation", "llc", "ltd", "limited",
  "co", "company", "lp", "llp", "plc", "ag", "sa", "nv", "bv",
  
  # Common noise words
  "the", "and", "group", "holdings", "holding", "international",
  "global", "services", "solutions", "systems", "technologies",
  
  # Geographical indicators
  "usa", "america", "american", "us", "united states"
)

# Create comprehensive cleaning function
clean_company_names <- function(x) {
  x %>%
    # Step 1: Convert to lowercase
    str_to_lower() %>%
    
    # Step 2: Remove special characters and punctuation (keep spaces and letters/numbers)
    str_replace_all("[^a-zA-Z0-9\\s]", " ") %>%
    
    # Step 3: Remove common suffixes and noise words
    str_replace_all(paste0("\\b(", paste(remove_words, collapse = "|"), ")\\b"), " ") %>%
    
    # Step 4: Clean up multiple spaces and trim
    str_squish() %>%
    
    # Step 5: Remove any remaining single characters (except meaningful ones)
    str_replace_all("\\b[a-z]\\b", " ") %>%
    
    # Step 6: Final cleanup
    str_squish()
}

# Apply preprocessing to both datasets
# Clean layoff data
layoff_data <- layoff_data %>%
  mutate(Company_clean = clean_company_names(Company))

# Clean compustat data
compustat_companies <- compustat_companies %>%
mutate(conm_clean = clean_company_names(conm))

--------------------------------------------------------
#2ND STEP
# This step will attempt to match the company names of compustat with layoff databse extracted to identify gvkeys for data extraction.
  
  # Replace your exact matching with fuzzy matching
  matched_companies <- stringdist_left_join(
    layoff_data, compustat_companies,
    by = c("Company" = "conm_clean"),
    method = "jw",           # Jaro-Winkler distance
    max_dist = 0.3,         # Threshold (tune this)
    distance_col = "dist"
  ) %>%
  group_by(Company) %>%
  slice_min(order_by = dist, n = 1) %>%  # Keep best match
  ungroup() %>%
  filter(!is.na(gvkey))

# Check results
cat("Match rate:", round(nrow(matched_companies)/nrow(layoff_data)*100, 1), "%\n")









# Replace your exact matching with fuzzy matching
matched_companies2 <- stringdist_left_join(
  layoff_data, compustat_companies,
  by = c("Company" = "conm_clean"),
  method = "jw",           # Jaro-Winkler distance
  max_dist = 0.3,         # Threshold (tune this)
  distance_col = "dist"
) %>%
  group_by(Company) %>%
  slice_min(order_by = dist, n = 1) %>%  # Keep best match
  ungroup() %>%
  filter(!is.na(gvkey))

# Check results
cat("Match rate:", round(nrow(matched_companies)/nrow(layoff_data)*100, 1), "%\n")












# Check match rate
cat("Total layoff announcements:", nrow(layoff_data), "\n")
cat("Successfully fuzzy matched:", nrow(matched_companies), "\n") 
cat("Match rate:", round(nrow(matched_companies)/nrow(layoff_data)*100, 1), "%\n")

# Examine the quality of matches
matched_companies %>%
  filter(dist > 0) %>%  # Look at imperfect matches
  select(Company, conm, dist) %>%
  arrange(desc(dist)) %>%
  head(20)

# Check Boeing matches specifically
matched_companies%>%
  filter(str_detect(Company, regex("boeing", ignore_case = TRUE))) %>%
  select(Company, conm, dist) %>%
  distinct()