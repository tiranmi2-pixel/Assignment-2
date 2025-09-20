#==============================================================================
# Data Extraction: Layoff Announcements (2015-2024)
#
# Purpose: This script downloads the primary layoff dataset from a public
#          Google Sheet and saves it locally for further processing. It is
#          the first step in the data collection pipeline.
#==============================================================================

#==============================================
# --- 1. Load Essential Libraries ---
#==============================================
# These packages are required for path management, reading Google Sheets,
# and handling data frames.
library(here)
library(googlesheets4)
library(readr)
library(dplyr)

#==============================================
# --- 2. Set Up Project Directory ---
#==============================================
# Using the here() package ensures that file paths are relative to the
# project's top-level directory, making the script more portable and
# reproducible for other users.
raw_data_path <- here("Project", "Raw Data")
if (!dir.exists(raw_data_path)) {
  dir.create(raw_data_path, recursive = TRUE)
}

#==============================================
# --- 3. Download Primary Layoff Dataset (2015-2024) ---
#==============================================
# The target data is stored in a publicly accessible Google Sheet.
sheet_url <- "https://docs.google.com/spreadsheets/d/1ayO8dl7sXaIYBAwkBGRUjbDms6MAbZFvvxxRp8IyxvY/edit?gid=1731412249#gid=1731412249"

# De-authenticate to access a public Google Sheet without a login prompt.
gs4_deauth()

# Read the sheet directly into a data frame.
# All columns are imported as character type ('c') to prevent parsing
# errors and preserve leading zeros or special characters.
warn_data <- read_sheet(sheet_url, col_types = "c")

#==============================================
# --- 4. Save Raw Data Locally ---
#==============================================
# Save the raw data to the 'Project/Raw Data' folder. This creates a local
# copy, ensuring that the analysis can be replicated even if the original
# source becomes unavailable.
output_file_path <- here(raw_data_path, "warn_data_2015_2024.csv")
write_csv(warn_data, output_file_path)