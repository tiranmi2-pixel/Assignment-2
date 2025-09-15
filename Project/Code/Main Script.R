install.packages(c("stringdist", "fuzzyjoin"))
library(RPostgres)
library(tidyverse)
library(readxl)
library(RPostgres) 
library(dbplyr) 
library(fuzzyjoin)
library(stringdist)
library(dplyr)
library(stringr)
library(here)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='tiran')


# Pull and apply FINAL, most robust cleaning to Compustat data
compustat_companies <- tbl(wrds, sql("SELECT DISTINCT gvkey, conm, tic
                                      FROM comp.funda
                                      WHERE fyear >= 2015 AND fyear <= 2024
                                      AND datafmt = 'STD' AND consol = 'C'")) %>%
  collect() %>%
  mutate(
    clean_conm = str_to_upper(conm),
    # IMPROVEMENT 1: Explicitly handle ampersands before punctuation removal
    clean_conm = str_replace_all(clean_conm, "&", " AND "),
    # IMPROVEMENT 2: Expanded the list of generic suffixes to remove
    clean_conm = str_remove_all(clean_conm, "\\b(LLC|INC|LP|CORP|LTD|LLP|CORPORATION|COMPANY|CO|GROUP|HOLDINGS)\\b"),
    clean_conm = str_replace_all(clean_conm, "[[:punct:]]", " "),
    clean_conm = str_squish(clean_conm),
    # IMPROVEMENT 3: Remove all remaining whitespace to handle cases like "WAL MART" vs "WALMART"
    clean_conm = str_remove_all(clean_conm, "\\s")
  )

# --- 3. PREPARE & CLEAN LOCAL LAYOFF DATA ---

# Define the path to your processed data folder
processed_data_path <- here("Project", "Processed Data")
processed_file <- file.path(processed_data_path, "Processed-warn.csv")

# Read the CSV 
df <- read.csv(processed_file)

# Apply the identical, most robust cleaning logic to the local data
df_cleaned <- df %>%
  mutate(
    clean_conm = str_to_upper(Company),
    clean_conm = str_replace(clean_conm, ".*\\bDBA\\b", ""),
    clean_conm = str_remove_all(clean_conm, ",?\\s*\\d{3,}.*"),
    # IMPROVEMENT 1: Explicitly handle ampersands
    clean_conm = str_replace_all(clean_conm, "&", " AND "),
    # IMPROVEMENT 2: Expanded suffix list
    clean_conm = str_remove_all(clean_conm, "\\b(LLC|INC|LP|CORP|LTD|LLP|CORPORATION|COMPANY|CO|GROUP|HOLDINGS)\\b"),
    clean_conm = str_replace_all(clean_conm, "[[:punct:]]", " "),
    clean_conm = str_squish(clean_conm),
    # IMPROVEMENT 3: Remove all remaining whitespace
    clean_conm = str_remove_all(clean_conm, "\\s")
  )

# --- 4. PERFORM FASTER "BEST MATCH FIRST" FUZZY JOIN ---

matched_companies <- stringdist_left_join(
  df_cleaned,
  compustat_companies,
  by = c("clean_conm" = "clean_conm"),
  method = "jw",
  max_dist = 0.2, # Key for speed
  distance_col = "dist"
) %>%
  group_by(Company) %>%
  slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # IMPROVEMENT 4: Slightly stricter filter to improve quality by removing more ambiguous matches
  filter(dist < 0.14 & !is.na(gvkey))

# --- 5. CALCULATE MORE ACCURATE MATCH RATE ---

# This select() command now chooses ONLY the columns you want and puts them in your specified order.
final_output <- matched_companies %>%
  select(gvkey, tic, Company, conm, City, Number.of.Workers, WARN.Received.Date, Effective.Date)


matched_original_companies <- matched_companies %>%
  distinct(Company) %>%
  nrow()

total_original_companies <- df_cleaned %>%
  distinct(Company) %>%
  nrow()

cat("Match rate:", round(matched_original_companies / total_original_companies * 100, 1), "%\n")

# --- 6. SAVE MATCHED DATA ---
dir.create(processed_data_path, showWarnings = FALSE, recursive = TRUE)
output_file <- file.path(processed_data_path, "Matched-WARN-Compustat-Final.csv")
write.csv(final_output, output_file, row.names = FALSE)
cat("Final matched data successfully saved to:", output_file, "\n")