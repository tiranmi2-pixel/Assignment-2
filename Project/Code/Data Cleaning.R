
#PART 2 - Filtering the Raw data to get rid of temporary layoffs, permanent closures,small layoffs and blanks
# Output of this section ,after filtering, will be saved in the processed data folder.
# output file will only contain workforce reductions, large layoffs and small layoffs.

install.packages("dplyr")
install.packages("readr")
install.packages("stringr")
library(dplyr)
library(readr)
library(stringr)
library(here)

# Define the path for the raw data directory.
raw_data_path <- here("Project", "Raw Data")

# Define the full path for the combined CSV file created in the previous step.
combined_file <- file.path(raw_data_path, "warn_combined.csv")

# Read the combined CSV into a data frame, specifying column types as character.
warn_data_combined <- read_csv(combined_file, col_types = "c")


# This step will filter column G and H ,sequentially to get rid of temporary layoffs that would have distored the results.
processed_data <- warn_data_combined %>%
  # First, remove rows where 'Closure / Layoff' contains "Temporary" or "Other".
  # The `is.na()` check ensures that blank rows are kept.
  filter(is.na(`Closure / Layoff`) | !str_detect(`Closure / Layoff`, "Temporary|Other")) %>%
  
  # Second, filter the remaining data based on the 'Temporary/Permanent' column.
  filter(is.na(`Temporary/Permanent`) | !str_detect(`Temporary/Permanent`, "Temporary|Other"))



# Define the full path for the processed output file.
processed_file <- file.path(processed_data_path, "Processed-warn.csv")

# Save the filtered data frame to the new CSV file.
write_csv(processed_data, processed_file)

