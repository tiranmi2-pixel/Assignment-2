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
library(lubridate)
library(zoo)
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

----------------------------------------------------
# --- 7. LINK GVKEY TO PERMNO FOR CRSP DATA ---
  # Get the CRSP-Compustat linking table
  ccm_link <- tbl(wrds, sql("SELECT gvkey, lpermno as permno, lpermco as permco, 
                           linkdt, linkenddt, linktype, linkprim
                           FROM crsp.ccmxpf_lnkhist")) %>%
  collect() %>%
  mutate(gvkey = str_trim(as.character(gvkey))) %>%
  filter(linktype %in% c("LU", "LC") & linkprim %in% c("P", "C")) %>%
  mutate(
    linkdt = ymd(linkdt),
    linkenddt = if_else(is.na(linkenddt), today(), ymd(linkenddt))
  )

# Prepare the layoff data by cleaning keys and dates
layoff_data_prepared <- final_output %>%
  filter(!is.na(WARN.Received.Date) & WARN.Received.Date != "") %>%
  mutate(
    gvkey = str_trim(as.character(gvkey)),
    warn_date = mdy(WARN.Received.Date)
  )

# Use an inner_join to perform the match and filter in a single step
# This will ONLY keep rows where a valid link is found for the specific date
final_with_permno <- inner_join(
  layoff_data_prepared,
  ccm_link,
  # This 'join_by' performs the exact logic you described:
  by = join_by(
    gvkey, # Match the gvkey...
    warn_date >= linkdt, # ...and check that the warn_date is after the start date...
    warn_date <= linkenddt # ...and before the end date.
  )
) %>%
  # If an event has multiple valid stock links (rare), sort to get the best one
  arrange(gvkey, warn_date, linkprim) %>%
  # Keep only the single best link for each unique layoff event
  distinct(gvkey, Company, WARN.Received.Date, .keep_all = TRUE) %>%
  # Select the final columns
  select(gvkey, permno, permco, tic, Company, conm, City, 
         Number.of.Workers, WARN.Received.Date, Effective.Date, warn_date)

# --- 8. SUMMARY AND DIAGNOSTICS---

# Calculate total records before and after the link
total_attempts <- nrow(final_output)
successful_matches <- nrow(final_with_permno)

# Calculate percentage
match_percentage <- (successful_matches / total_attempts) * 100

# Print the overall summary
cat("\n--- Overall PERMNO Link Summary ---\n")
cat("Started with:", total_attempts, "records from fuzzy match\n")
cat("Successfully linked:", successful_matches, "records to PERMNO\n")
cat("Overall Match Rate:", round(match_percentage, 1), "%\n\n")

# Identify the dropped observations by comparing the two dataframes
# Ensure gvkey is character in both for a safe join
dropped_obs <- anti_join(
  final_output %>% mutate(gvkey = as.character(gvkey)), 
  final_with_permno %>% mutate(gvkey = as.character(gvkey)), 
  by = c("gvkey", "Company", "WARN.Received.Date")
)

# Print the summary of the most frequently dropped companies
cat("--- Top 10 Most Frequently Dropped Companies ---\n")
print(
  dropped_obs %>%
    # We count by 'conm' as it's the standardized name from Compustat
    count(conm, sort = TRUE) %>%
    head(10)
)


--------------------------------------------------------------------
# Step 1: Fetch Compustat annual fundamentals data 
  #Fetch Compustat annual fundamentals data
  compustat_funda <- tbl(wrds, sql("SELECT gvkey, fyear, datadate, xrd, sale, at
                                  FROM comp.funda 
                                  WHERE fyear >= 2010 AND fyear <= 2024
                                  AND datafmt = 'STD' AND consol = 'C'")) %>%
  collect() %>%
  mutate(
    datadate = ymd(datadate),
    gvkey = str_trim(as.character(gvkey))  # Ensure consistent format
  ) %>%
  filter(!is.na(xrd), !is.na(sale), sale > 0) %>%  # Keep only firms with valid R&D and sales data
  arrange(gvkey, datadate)

# Step 2: Calculate R&D intensity and multi-year averages
compustat_with_rd <- compustat_funda %>%
  group_by(gvkey) %>%
  arrange(datadate) %>%
  mutate(
    rd_intensity = ifelse(sale > 0, xrd / sale, NA),
    # CORRECTED: Use rollapply with align = "right" instead of rollapplyr
    rd_intensity_1yr = rd_intensity,
    rd_intensity_3yr = rollapply(rd_intensity, width = 3, FUN = mean, 
                                 na.rm = TRUE, fill = NA, align = "right"),
    rd_intensity_5yr = rollapply(rd_intensity, width = 5, FUN = mean, 
                                 na.rm = TRUE, fill = NA, align = "right")
  ) %>%
  ungroup() %>%
  filter(!is.na(rd_intensity))
# Step 3: Join with your layoff data

layoff_with_rd <- final_with_permno %>%
  mutate(
    gvkey = str_trim(as.character(gvkey)),
    warn_date = ymd(warn_date)
  ) %>%
  left_join(compustat_with_rd, by = "gvkey") %>%
  filter(datadate <= warn_date) %>%
  group_by(gvkey, Company, warn_date) %>%
  slice_max(datadate, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    gvkey, permno, permco, tic, Company, conm, City, 
    Number.of.Workers, WARN.Received.Date, Effective.Date, warn_date,
    fyear, datadate, xrd, sale, at,
    rd_intensity_1yr, rd_intensity_3yr, rd_intensity_5yr
  )

# Step 4: Create R&D intensity groups
layoff_final <- layoff_with_rd %>%
  filter(!is.na(rd_intensity_1yr)) %>%
  mutate(
    # Create median splits for different R&D measures
    rd_median_1yr = median(rd_intensity_1yr, na.rm = TRUE),
    rd_group_1yr = ifelse(rd_intensity_1yr >= rd_median_1yr, "High R&D", "Low R&D"),
    
    rd_median_3yr = median(rd_intensity_3yr, na.rm = TRUE),
    rd_group_3yr = ifelse(rd_intensity_3yr >= rd_median_3yr, "High R&D", "Low R&D"),
    
    rd_median_5yr = median(rd_intensity_5yr, na.rm = TRUE),
    rd_group_5yr = ifelse(rd_intensity_5yr >= rd_median_5yr, "High R&D", "Low R&D")
    # NOTE: No comma after the last line!
  )
-----------------------------------------------------------------
  # Extracting final input file for event study portal in wrds as api access was not allowed.
  
  # Create upload data for WRDS Event Study
  upload_data <- layoff_final %>%
  select(permno, WARN.Received.Date) %>%
  filter(!is.na(permno), !is.na(WARN.Received.Date)) %>%
  mutate(
    permno = as.integer(permno),
    event_date = mdy(WARN.Received.Date)  # assuming MM/DD/YYYY format
  ) %>%
  select(permno, event_date) %>%
  filter(!is.na(permno), !is.na(event_date))

# Define the file path using here package for "Static Data" subfolder
static_data_path <- here("Project", "Processed Data", "Static Data")
output_file <- file.path(static_data_path, "layoff_events_upload.txt")

write_delim(upload_data, 
            file = output_file, 
            delim = "\t", 
            col_names = FALSE)
---------------------------------------------------------------------------------------
  # Save layoff_final dataset to CSV
  write.csv(layoff_final, 
            file = file.path(processed_data_path, "layoff_final_with RD scores.csv"), 
            row.names = FALSE)
