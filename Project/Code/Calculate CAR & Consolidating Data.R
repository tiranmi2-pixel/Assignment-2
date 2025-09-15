# --- 1. SET UP LIBRARIES ---
# If you don't have these, install them first
# install.packages(c("tidyverse", "stringr", "here"))
library(tidyverse)
library(stringr)
library(here) # For building relative file paths

# --- 2. DEFINE RELATIVE FOLDER PATHS USING here() ---

# Path to the folder containing your individual CAR files
# This assumes your R project is set up in the "Assignment-2" folder
data_folder <- here("Project", "Processed Data", "Static Data")

# Path for the final output file
output_folder <- here("Project", "Processed Data")
output_file_path <- file.path(output_folder, "Final CAR file.csv")

# --- 3. GET THE LIST OF CSV FILES ---
all_files <- list.files(path = data_folder, pattern = "*.csv", full.names = TRUE)

# --- 4. PROCESS EACH FILE INDIVIDUALLY AND STORE RESULTS IN MEMORY ---

# This list will hold the calculated CAR for each file
results_list <- map(all_files, function(file_path) {
  
  cat("Processing file:", basename(file_path), "\n")
  
  # Read the current file
  event_data <- read_csv(file_path, show_col_types = FALSE)
  
  # Extract the window number (e.g., 1, 3, 5) from the filename
  window_num <- str_extract(basename(file_path), "\\d+") %>% as.numeric()
  
  # Calculate CAR, keeping both permno and uid
  car_summary <- event_data %>%
    group_by(permno, uid) %>%
    summarise(
      CAR = case_when(
        window_num == 1  ~ sum(abret[evttime >= -1 & evttime <= 1]),
        window_num == 3  ~ sum(abret[evttime >= -3 & evttime <= 3]),
        window_num == 5  ~ sum(abret[evttime >= -5 & evttime <= 5]),
        window_num == 10 ~ sum(abret[evttime >= -10 & evttime <= 10]),
        TRUE ~ NA_real_
      ),
      .groups = 'drop'
    ) %>%
    # Rename the CAR column to be specific, e.g., "CAR_5_Day_Window"
    rename_with(~ paste0("CAR_", window_num, "_Day_Window"), .cols = "CAR")
  
  return(car_summary)
})

# --- 5. COMBINE ALL RESULTS FROM MEMORY ---
cat("\nAll files processed. Merging results...\n")

# Join all the dataframes in the list by their shared 'permno' and 'uid' columns
final_results <- reduce(results_list, full_join, by = c("permno", "uid"))

# --- 6. REORDER COLUMNS AND SAVE TO THE SPECIFIED FOLDER ---

# Reorder columns to place 'permno' first
final_results_reordered <- final_results %>%
  select(permno, uid, everything())

# Ensure the output directory exists before saving
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# Save the final, combined dataframe
write.csv(final_results_reordered, output_file_path, row.names = FALSE)

cat("\nMerge complete. Here is a sample of the final output:\n")
print(head(final_results_reordered))
cat("\nSuccessfully saved final results to:", output_file_path, "\n")