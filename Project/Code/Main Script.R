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

wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='tiran')



----------------------------------
  
  # Corrected Code
  compustat_companies <- tbl(wrds, sql("SELECT DISTINCT gvkey, conm, tic
                                     FROM comp.funda 
                                     WHERE fyear >= 2015 AND fyear <= 2024
                                     AND datafmt = 'STD' AND consol = 'C'")) %>%
  collect() %>%
  mutate(conm_clean = str_trim(str_to_upper(conm)))

# Import processed layoff database and get rid of unwanted colunmns
processed_file <- file.path(processed_data_path, "Processed-warn.csv")
df<- read.csv(processed_file)
df_new <- df %>% select(-c(9:13))

-------------------------------
  # This step will attempt to match the company names of compustat with layoff databse extracted to identify gvkeys for data extraction.
  matched_companies <- stringdist_left_join(
    df_new, compustat_companies,
    by = c("Company" = "conm"),
    method = "jw",
    max_dist = 0.3,
    distance_col = "dist"
  ) %>%
  group_by(Company) %>%
  slice_min(order_by = dist, n = 1) %>%
  ungroup() %>%
  filter(!is.na(gvkey))

cat("Match rate:", round(nrow(matched_companies)/nrow(df_new)*100, 1), "%\n")