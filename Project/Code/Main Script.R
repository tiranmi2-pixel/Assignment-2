library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='tiran')

library(tidyverse)
library(readxl)
library(RPostgres) 
library(dbplyr)    

# Import  layoff data
layoff_data <- read_csv(here("Project", "Processed Data", "Processed-warn.csv")) %>%
  mutate(
    # Trim only the first three columns 
    State = str_trim(State),
    Company = str_trim(Company), 
    City = str_trim(City)
  )


# Get Compustat company names for matching
compustat_companies <- tbl(wrds, sql("SELECT DISTINCT gvkey, conm, tic
                                     FROM comp.funda 
                                     WHERE fyear >= 2015 AND fyear <= 2024
                                     AND datafmt = 'STD' AND consol = 'C'")) %>%
  collect() %>%
  mutate(conm_clean = str_trim(str_to_upper(conm)))



------
  # First attempt: exact matching
  matched_companies <- layoff_data %>%
  left_join(compustat_companies, 
            by = c("Company" = "conm_clean")) %>%
  filter(!is.na(gvkey))

# Check match rate
cat("Total layoff announcements:", nrow(layoff_data), "\n")
cat("Successfully matched to Compustat:", nrow(matched_companies), "\n")
cat("Match rate:", round(nrow(matched_companies)/nrow(layoff_data)*100, 1), "%\n")

# Examine unmatched companies
unmatched <- layoff_data %>%
  anti_join(compustat_companies, 
            by = c("Company" = "conm_clean")) %>%
  count(Company, sort = TRUE)

head(unmatched, 20)  # Look at most common unmatched names

--------------------------
  