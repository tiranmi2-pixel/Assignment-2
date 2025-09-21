# This script checks for all the packages needed to run the project.
# If any are missing, it will install them automatically.

# A list of all packages required for the entire project.
required_packages <- c(
  "tidyverse",      # For data manipulation and plotting (dplyr, ggplot2, etc.)
  "knitr",          # For creating tables
  "kableExtra",     # For styling tables
  "broom",          # For cleaning up statistical test outputs
  "here",           # For managing file paths
  "webshot2",       # For saving tables as images
  "RPostgres",      # For connecting to the WRDS database
  "dbplyr",         # For using dplyr with databases
  "fuzzyjoin",      # For fuzzy string matching
  "stringdist",     # For string distance calculations
  "lubridate",      # For handling dates
  "zoo",            # For rolling averages
  "googlesheets4"   # For reading from Google Sheets
)

# Checks which of the required packages are not yet installed.
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# If any packages are missing, this block will install them.
if(length(missing_packages) > 0) {
  cat("The following packages are missing and will be installed:\n")
  print(missing_packages)
  install.packages(missing_packages)
} else {
  cat("All required packages are already installed.\n")
}

# Loads all the required packages for the session.
cat("\nLoading all required packages...\n")
suppressPackageStartupMessages(
  lapply(required_packages, library, character.only = TRUE)
)
cat("Setup complete. You are ready to run the analysis.\n")