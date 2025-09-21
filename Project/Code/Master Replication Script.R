# ===================================================================
# MASTER REPLICATION SCRIPT (Corrected)
#
# Description:
# This script orchestrates the entire data processing and analysis
# pipeline by executing all component scripts in the correct order.
#
# Project: MAF900 Assignment 2
# Author: Tirandip
# Date: September 20, 2025
# ===================================================================

# --- 0. IMPORTANT: PRE-RUN CHECKLIST ---
# Before running, please ensure you have done the following:
#
# 1. FOLDER STRUCTURE:
#    All your .R script files (including this one) MUST be located
#    inside a subfolder named 'Code' within your main project folder.
#    Example: /Project/Code/master_replication.R
#
# 2. WRDS CREDENTIALS:
#    You MUST manually edit the 'Main Script.R' file. Find the line
#    that says 'user = 'your_wrds_username'' and replace the
#    placeholder with your actual WRDS username in quotes. The script
#    will fail if you do not do this.
#
# ===================================================================


# --- 1. SETUP: LOAD PACKAGES AND DEFINE PATHS ---
# Clear the environment to ensure a clean run
rm(list = ls())

# Consolidate all required packages from the project scripts
required_packages <- c(
  "tidyverse", "here", "googlesheets4", "RPostgres", "dbplyr",
  "fuzzyjoin", "stringdist", "lubridate", "zoo"
)

# Install any missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages) > 0) {
  cat("Installing missing packages:", paste(new_packages, collapse=", "), "\n")
  install.packages(new_packages)
}

# Load all packages
suppressPackageStartupMessages(lapply(required_packages, library, character.only = TRUE))

cat("Setup complete. Project root is at:", here::here(), "\n\n")


# --- 2. EXECUTE DATA PROCESSING PIPELINE ---
cat("Starting data processing pipeline...\n")
cat("----------------------------------------------------\n")

# A function to run a script from the 'Code' subfolder
run_script <- function(script_name) {
  script_path <- here::here("Code", script_name)
  cat(paste("Executing:", script_path, "\n"))
  tryCatch({
    source(script_path)
  }, error = function(e) {
    stop(paste("ERROR failed while running", script_name, ". Details:", e$message))
  })
}

# Execute scripts in logical order
run_script("Data Extract.R")
run_script("Data Cleaning.R")
run_script("Main Script.R")

cat("----------------------------------------------------\n\n")

# --- 3. MANUAL CHECKPOINT FOR WRDS EVENT STUDY ---
cat("======================= PAUSED =======================\n")
cat("ACTION REQUIRED:\n\n")
cat("1. The script 'Main Script.R' has created an upload file at:\n")
cat("   '", here::here("Project", "Processed Data", "Static Data", "layoff_events_upload.txt"), "'\n\n", sep="")
cat("2. Please upload this file to the WRDS Event Study web portal.\n\n")
cat("3. Download the event study results as CSV files and place them\n")
cat("   directly into the following folder:\n")
cat("   '", here::here("Project", "Processed Data", "Static Data"), "'\n\n", sep="")
cat("====================================================\n\n")

# Wait for user to confirm they have completed the manual step
readline(prompt = "Press [Enter] in the console AFTER you have placed the WRDS files in the 'Static Data' folder to continue...")

# --- 4. VERIFY WRDS FILES AND RESUME PIPELINE ---
static_data_folder <- here::here("Project", "Processed Data", "Static Data")
wrds_files <- list.files(path = static_data_folder, pattern = "\\.csv$", full.names = TRUE)

if (length(wrds_files) == 0) {
  stop("ERROR: No CSV files from WRDS were found in the 'Static Data' directory. Please add them and run the script again.")
}

cat("\nFound the following WRDS result file(s):\n")
print(basename(wrds_files))
cat("\nResuming script...\n")
cat("----------------------------------------------------\n")


# Continue with the rest of the pipeline
run_script("Calculate CAR & Consolidating Data.R")
run_script("Merge Dataframes (CAT & RD scores).R")


cat("----------------------------------------------------\n\n")

# --- 5. COMPLETION MESSAGE ---
cat("====================================================\n")
cat("DONE. All processing scripts executed successfully.\n")
cat("The final dataset is ready for analysis.\n")
cat("====================================================\n")

