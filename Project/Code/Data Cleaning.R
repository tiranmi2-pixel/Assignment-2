#==============================================================================
# Data Cleaning: Raw Layoff Announcements
#
# Purpose: This script takes the raw layoff data downloaded in the previous
#          step and cleans it to isolate permanent workforce reductions.
#          By filtering out temporary events and company closures, we ensure
#          the sample is appropriate for our research question.
#==============================================================================

#==============================================
# --- 1. Load Essential Libraries ---
#==============================================
# These packages provide tools for path management, data import/export,
# and data manipulation.
library(here)
library(readr)
library(dplyr)
library(stringr)

#==============================================
# --- 2. Define File Paths ---
#==============================================
# Define relative paths to ensure the script runs smoothly on any machine
# without needing to manually change directory paths.
raw_data_path <- here("Project", "Raw Data")
processed_data_path <- here("Project", "Processed Data")

# The input file is the output from the 'Data Extract.R' script.
input_file <- file.path(raw_data_path, "warn_data_2015_2024.csv")

# Create the output directory if it doesn't already exist.
if (!dir.exists(processed_data_path)) {
  dir.create(processed_data_path, recursive = TRUE)
}

#==============================================
# --- 3. Load Raw Data ---
#==============================================
# Read the raw CSV file into a data frame. All columns are specified as
# character type to prevent any data loss or parsing issues.
raw_warn_data <- read_csv(input_file, col_types = "c")

#==============================================
# --- 4. Filter for Relevant Layoff Events ---
#==============================================
# The goal is to focus on permanent layoffs. Hence,filter out events that are
# explicitly marked as "Temporary" or "Other" in the classification columns.

processed_data <- raw_warn_data %>%
  # First, filter the 'Closure / Layoff' column. We keep permanent layoffs
  # and any rows where this field is blank (NA).
  filter(is.na(`Closure / Layoff`) | !str_detect(`Closure / Layoff`, "Temporary|Other")) %>%
  
  # Next, apply the same logic to the 'Temporary/Permanent' column for an
  # extra layer of cleaning.
  filter(is.na(`Temporary/Permanent`) | !str_detect(`Temporary/Permanent`, "Temporary|Other"))

#==============================================
# --- 5. Save Processed Data ---
#==============================================
# The cleaned data is saved to a new CSV file in the 'Processed Data' folder.
# This file will serve as the input for the main analysis script.
output_file <- file.path(processed_data_path, "Processed-warn.csv")
write_csv(processed_data, output_file)