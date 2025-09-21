#==============================================================================
# Main Script: Data Merging, Linking, and Variable Creation
#
# Purpose: This is the central script of the analysis process. It performs
#          three critical tasks:
#          1. Matches layoff announcements to public firms in Compustat.
#          2. Links matched firms to their stock market identifiers in CRSP.
#          3. Calculates R&D intensity metrics for each firm-event.
#          4. produce upload file to WRDS event study to obtain abnormal returns.  
#==============================================================================


# Load Libraries and Establish Database Connection ---
# A  set of libraries is loaded for database interaction,
# data manipulation, string matching, and date handling.
library(RPostgres)
library(tidyverse)
library(dbplyr)
library(fuzzyjoin)
library(stringdist)
library(stringr)
library(here)
library(lubridate)
library(zoo)

# A connection to the WRDS PostgreSQL database is established.
# This requires valid user credentials.
wrds <- dbConnect(Postgres(),
#                  host = 'wrds-pgdata.wharton.upenn.edu',
#                  port = 9737,
#                 dbname = 'wrds',
#                  sslmode = 'require',
#                  user = 'tiran') # Replace with your username


#==============================================================================
# Part I: Fuzzy Matching Layoff Data to Compustat
#==============================================================================

#--- 1.Prepare Compustat Company Data ---
# A list of unique public firms is retrieved from Compustat for the relevant
# sample period. Company names are then standardized for matching.
compustat_companies <- tbl(wrds, sql("SELECT DISTINCT gvkey, conm, tic
                                      FROM comp.funda
                                      WHERE fyear >= 2015 AND fyear <= 2024
                                      AND datafmt = 'STD' AND consol = 'C'")) %>%
  collect() %>%
  mutate(
    # A robust cleaning process is applied to company names.
    clean_conm = str_to_upper(conm),
    clean_conm = str_replace_all(clean_conm, "&", " AND "),
    clean_conm = str_remove_all(clean_conm, "\\b(LLC|INC|LP|CORP|LTD|LLP|CORPORATION|COMPANY|CO|GROUP|HOLDINGS)\\b"),
    clean_conm = str_replace_all(clean_conm, "[[:punct:]]", " "),
    clean_conm = str_squish(clean_conm),
    clean_conm = str_remove_all(clean_conm, "\\s")
  )


#---2. Prepare Local Layoff Data ---
# The cleaned layoff announcement data is loaded from the 'Processed Data' folder.
processed_data_path <- here("Project", "Processed Data")
processed_file <- file.path(processed_data_path, "Processed-warn.csv")
df <- read.csv(processed_file)

# ---3.To prepare for the fuzzy match, the company names in the layoff data must
# be standardized. This block applies the exact same cleaning rules used on the
# Compustat names, removing variations in punctuation, suffixes (like 'LLC'),
# and spacing to create a consistent format.# To prepare for the fuzzy match, the company names in the layoff data must
# be standardized. This block applies the exact same cleaning rules used on the
# Compustat names, removing variations in punctuation, suffixes (like 'LLC'),
# and spacing to create a consistent format.
df_cleaned <- df %>%
  mutate(
    clean_conm = str_to_upper(Company),
    clean_conm = str_replace(clean_conm, ".*\\bDBA\\b", ""),
    clean_conm = str_remove_all(clean_conm, ",?\\s*\\d{3,}.*"),
    clean_conm = str_replace_all(clean_conm, "&", " AND "),
    clean_conm = str_remove_all(clean_conm, "\\b(LLC|INC|LP|CORP|LTD|LLP|CORPORATION|COMPANY|CO|GROUP|HOLDINGS)\\b"),
    clean_conm = str_replace_all(clean_conm, "[[:punct:]]", " "),
    clean_conm = str_squish(clean_conm),
    clean_conm = str_remove_all(clean_conm, "\\s")
  )


# --- 4. Perform Fuzzy Join ---
# A string-distance join matches the layoff data to the Compustat data.
# The Jaro-Winkler method ('jw') is used, which is effective for short strings
# like company names. The process keeps only the best match for each company.
matched_companies <- stringdist_left_join(
  df_cleaned,
  compustat_companies,
  by = c("clean_conm" = "clean_conm"),
  method = "jw",
  max_dist = 0.2,
  distance_col = "dist"
) %>%
  group_by(Company) %>%
  slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(dist < 0.14 & !is.na(gvkey)) # Filter for high-quality matches


# --- 5. Save Matched Data ---
# The matched dataset, containing layoff information linked to Compustat
# identifiers, is saved to a new file.
final_output <- matched_companies %>%
  select(gvkey, tic, Company, conm, City, Number.of.Workers, WARN.Received.Date, Effective.Date)

output_file <- file.path(processed_data_path, "Matched-WARN-Compustat.csv")
write.csv(final_output, output_file, row.names = FALSE)



# --- 4a. Display Match Rate ---
# Calculate and print the success rate of the fuzzy join to the console.
total_companies <- n_distinct(df_cleaned$Company)
matched_count <- n_distinct(matched_companies$Company)
success_rate <- (matched_count / total_companies) * 100

cat(sprintf("Successfully matched %d out of %d unique companies.\n", matched_count, total_companies))
cat(sprintf("Success Rate: %.2f%%\n", success_rate))


#==============================================================================
# Part II: Linking Compustat GVKEY to CRSP PERMNO
#==============================================================================

# --- 1. Fetch CRSP-Compustat Linking Table ---
# The official linking history table is retrieved from CRSP to map firm
# identifiers (gvkey) to security identifiers (permno).
ccm_link <- tbl(wrds, sql("SELECT gvkey, lpermno as permno, linkdt, linkenddt
                           FROM crsp.ccmxpf_lnkhist
                           WHERE linktype IN ('LU', 'LC') AND linkprim IN ('P', 'C')")) %>%
  collect() %>%
  mutate(
    linkdt = ymd(linkdt),
    linkenddt = if_else(is.na(linkenddt), today(), ymd(linkenddt))
  )


# --- 2. Join Layoff Data with Linking Table ---
# An inner join is performed, matching each layoff event to the correct permno
# based on the event date falling within the link's valid date range.
layoff_data_prepared <- final_output %>%
  mutate(warn_date = mdy(WARN.Received.Date))

final_with_permno <- inner_join(
  layoff_data_prepared,
  ccm_link,
  by = join_by(gvkey, warn_date >= linkdt, warn_date <= linkenddt)
) %>%
  distinct(gvkey, Company, WARN.Received.Date, .keep_all = TRUE)


#==============================================================================
# Part III: Calculating R&D Intensity
#==============================================================================

# --- 1. Fetch Annual Financial Data ---
# Annual financial data is retrieved from Compustat for all firms in the sample.
# The period (2010-2024) is chosen to ensure sufficient historical data
# for calculating 5-year rolling averages.
compustat_funda <- tbl(wrds, sql("SELECT gvkey, fyear, datadate, xrd, sale
                                  FROM comp.funda
                                  WHERE fyear >= 2005 AND fyear <= 2024
                                  AND datafmt = 'STD' AND consol = 'C'")) %>%
  collect() %>%
  filter(!is.na(xrd), !is.na(sale), sale > 0)


# --- 2. Calculate R&D Intensity Metrics ---
# R&D intensity is calculated as R&D expenditure divided by sales.
# Rolling averages over 3- and 5-year windows are then computed to create
# stable, long-term measures of a firm's R&D strategy.
compustat_with_rd <- compustat_funda %>%
  group_by(gvkey) %>%
  arrange(datadate) %>%
  mutate(
    rd_intensity = xrd / sale,
    rd_intensity_1yr = rd_intensity,
    rd_intensity_3yr = rollapply(rd_intensity, width = 3, FUN = mean, na.rm = TRUE, fill = NA, align = "right"),
    rd_intensity_5yr = rollapply(rd_intensity, width = 5, FUN = mean, na.rm = TRUE, fill = NA, align = "right")
  ) %>%
  ungroup()


# --- 3. Merge R&D Data with Layoff Events ---
# The calculated R&D intensity metrics are merged with the layoff event data.
# The join logic ensures that each layoff is matched with the most recent
# financial data available *before* the announcement date.
layoff_with_rd <- final_with_permno %>%
  left_join(compustat_with_rd, by = "gvkey") %>%
  filter(datadate <= warn_date) %>%
  group_by(gvkey, Company, warn_date) %>%
  slice_max(datadate, n = 1, with_ties = FALSE) %>%
  ungroup()


# --- 4. Create High/Low R&D Groups ---
# The sample is split into two groups ("High R&D" and "Low R&D") based on
# the median R&D intensity. This binary classification is essential for the
# subsequent hypothesis testing.
layoff_final <- layoff_with_rd %>%
  # Ensure we have valid data for all three metrics before classifying
  filter(
    !is.na(rd_intensity_1yr),
    !is.na(rd_intensity_3yr),
    !is.na(rd_intensity_5yr)
  ) %>%
  mutate(
    # -- 1-Year R&D Group --
    rd_median_1yr = median(rd_intensity_1yr, na.rm = TRUE),
    rd_group_1yr  = ifelse(rd_intensity_1yr >= rd_median_1yr, "High R&D", "Low R&D"),
    
    # -- 3-Year R&D Group --
    rd_median_3yr = median(rd_intensity_3yr, na.rm = TRUE),
    rd_group_3yr  = ifelse(rd_intensity_3yr >= rd_median_3yr, "High R&D", "Low R&D"),
    
    # -- 5-Year R&D Group --
    rd_median_5yr = median(rd_intensity_5yr, na.rm = TRUE),
    rd_group_5yr  = ifelse(rd_intensity_5yr >= rd_median_5yr, "High R&D", "Low R&D")
  )

#==============================================================================
# Part IV: Prepare File for WRDS Event Study
#==============================================================================

# --- 1. Format and Save Event Study Input File ---
# A text file is created containing the permno and event date for each
# layoff and saved in the Processed Data/Static Data folder . This file is formatted specifically for upload to the
# WRDS Event Study web portal.
upload_data <- layoff_final %>%
  select(permno, WARN.Received.Date) %>%
  mutate(
    permno = as.integer(permno),
    event_date = mdy(WARN.Received.Date)
  ) %>%
  select(permno, event_date)

static_data_path <- here("Project", "Processed Data", "Static Data")
dir.create(static_data_path, showWarnings = FALSE, recursive = TRUE)
output_file_upload <- file.path(static_data_path, "layoff_events_upload.txt")

write_delim(upload_data,
            file = output_file_upload,
            delim = "\t",
            col_names = FALSE)
