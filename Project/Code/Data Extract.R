#PART 1 - Extracting Data
#Below code will extract the CSV file from the data.gov for further processing. 
# The  here package is used solve file path problems the replicator will face during replicaiton by creating relative paths to projects top directory.


# Load here package for dynamic path management
install.packages("here")
library(here)


# Create Project/Raw Data directory structure dynamically
raw_data_path <- here("Project", "Raw Data")
if(!dir.exists(raw_data_path)) dir.create(raw_data_path, recursive = TRUE)

# Download WARN dataset to dynamic path
download.file("https://data.oregon.gov/api/views/ijbz-jpx8/rows.csv?accessType=DOWNLOAD",
              destfile = file.path(raw_data_path, "warn.csv"),
              method = "auto")

#PART 2 - Filtering the Raw data to get rid of temporary layoffs, permanent closures,small layoffs and blanks
# Output of this section ,after filtering, will be saved in the processed data folder.
# output file will only contain workforce reductions, large layoffs and small layoffs.

install.packages("dplyr")
install.packages("readr")
library(dplyr)
library(readr)

# Define the path for the raw data directory
raw_data_path <- here("Project", "Raw Data")

# Define the full path for the raw CSV file
raw_file <- file.path(raw_data_path, "warn.csv")

#Read the downloaded CSV into a data frame
warn_data <- read_csv(raw_file, show_col_types = FALSE)

# Define the categories to filter out from the 'Layoff Type' column
categories_to_exclude <- c(
  "Temporary Layoff",
  "Permanent closure",
  "Small Layoff - 1 to 10 workers",
  "Other"
)

# Filter the data to remove specified categories and blank entries
processed_data <- warn_data %>%
  filter(
    !is.na(`Layoff Type`),
    !(`Layoff Type` %in% categories_to_exclude)
  )



# Define the path for the processed data directory and output file
processed_data_path <- here("Project", "Processed Data")
processed_file <- file.path(processed_data_path, "Processed-warn.csv")

# Save the filtered data frame to the new CSV file
write_csv(processed_data, processed_file)

