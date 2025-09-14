#PART 1 - Extracting Data
#Below code will extract the CSV file from the so0urce for further processing. 
# The  here package is used solve file path problems the replicator will face during replication by creating relative paths to projects top directory.


# Load here package for dynamic path management
install.packages("here")
install.packages("googlesheets4")
library(googlesheets4)
library(here)
library(readr)


# Create Project/Raw Data directory structure dynamically
raw_data_path <- here("Project", "Raw Data")
if(!dir.exists(raw_data_path)) dir.create(raw_data_path, recursive = TRUE)

# Please note, the file download procedure is segregated to two steps as the layoff database contains 2025 lays off in a seperate file and prior data in a different file. 


# --- Download the first dataset --- 
# The URL of the public Google Sheet.
sheet_url <- "https://docs.google.com/spreadsheets/d/1ayO8dl7sXaIYBAwkBGRUjbDms6MAbZFvvxxRp8IyxvY/edit?gid=1731412249#gid=1731412249"

# De-authorize to handle public sheet access without needing a Google login.
gs4_deauth()

# Read the data directly from the Google Sheet.
warn_data <- read_sheet(sheet_url, col_types = "c")

# Write the downloaded data to a CSV file in the designated path.
write_csv(warn_data, here(raw_data_path, "warn.csv"))



# --- Download the second dataset ---

# The URL of the second public Google Sheet.
sheet_url_2 <- "https://docs.google.com/spreadsheets/d/1Qx6lv3zAL9YTsKJQNALa2GqBLXq0RER2lHvzyx32pRs/edit?gid=0#gid=0"

# Read the data directly from the second Google Sheet, also as character types.
additional_warn_data <- read_sheet(sheet_url_2, col_types = "c")

# Write the second downloaded data to a new CSV file.
write_csv(additional_warn_data, here(raw_data_path, "warn_additional.csv"))


# --- Combine and save the datasets ---

# Combine the two data frames by stacking them vertically.
# bind_rows() from dplyr is used as it safely handles columns that may not match.
combined_warn_data <- bind_rows(warn_data, additional_warn_data)

# Write the combined data to a new CSV file.
write_csv(combined_warn_data, here(raw_data_path, "warn_combined.csv"))



