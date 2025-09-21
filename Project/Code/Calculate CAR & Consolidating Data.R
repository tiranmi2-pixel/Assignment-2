#==============================================================================
# Calculate CAR & Consolidating Data
#
# Purpose: This script processes the output files from the WRDS Event Study.
#          It reads multiple CSV files, each corresponding to a different
#          event window, calculates the Cumulative Abnormal Return (CAR) for
#          each event, and then consolidates these values into a single,
#          wide-format master file.
#==============================================================================

#==============================================
# --- 1. Load Essential Libraries ---
#==============================================
# These packages provide tools for data manipulation, string operations,
# and  file path management.
library(tidyverse)
library(stringr)
library(here)

#==============================================
# --- 2. Define File Paths ---
#==============================================
# Relative paths are defined to ensure the script is portable and can be
# run on different machines without manual adjustments.
data_folder <- here("Project", "Processed Data", "Static Data")
output_folder <- here("Project", "Processed Data")
output_file_path <- file.path(output_folder, "Final_CAR_File.csv")

#==============================================
# --- 3. Identify Event Study Output Files ---
#==============================================
# A list of all CSV files located in the 'Static Data' directory is generated.
# These files are the direct output from the WRDS Event Study portal.
all_files <- list.files(path = data_folder, pattern = "*.csv", full.names = TRUE)

#==============================================
# --- 4. Process Each File to Calculate CAR ---
#==============================================
# The script iterates through each file in the list. For each one, it calculates
# the CAR by summing the abnormal returns ('abret') over the correct event window.
results_list <- map(all_files, function(file_path) {
  
  # Read the individual event study file.
  event_data <- read_csv(file_path, show_col_types = FALSE)
  
  # The event window size (e.g., 1, 3, 5) is extracted directly from the filename.
  window_num <- str_extract(basename(file_path), "\\d+") %>% as.numeric()
  
  # The CAR is calculated by summing the abnormal returns ('abret') within
  # the specific time window for each unique event ('uid').
  car_summary <- event_data %>%
    group_by(permno, uid) %>%
    summarise(
      CAR = sum(abret), # Sum all abnormal returns for the given event
      .groups = 'drop'
    ) %>%
    # The 'CAR' column is renamed to be descriptive of its window size.
    rename_with(~ paste0("CAR_", window_num, "_Day_Window"), .cols = "CAR")
  
  return(car_summary)
})

#==============================================
# --- 5. Consolidate All CAR Results ---
#==============================================
# All the individual CAR calculations, which are stored in a list,
# are now merged into a single, comprehensive dataframe. The join is
# performed using the unique event identifier ('uid').
final_results <- reduce(results_list, full_join, by = c("permno", "uid"))

#==============================================
# --- 6. Save the Final Consolidated File ---
#==============================================
# The final dataset, now in a wide format with a separate column for each
# CAR window, is saved to the 'Processed Data' folder. This file is ready
# to be merged with the firm-level financial data.
final_results_reordered <- final_results %>%
  select(uid, permno, everything())

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
write.csv(final_results_reordered, output_file_path, row.names = FALSE)