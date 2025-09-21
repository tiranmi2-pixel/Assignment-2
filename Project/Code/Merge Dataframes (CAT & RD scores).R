#==============================================================================
# Merge Dataframes: R&D Intensity and CAR
#
# Purpose: This script performs the final merge of the analysis. It combines
#          the dataset containing firm-level financial data and R&D intensity
#          scores with the dataset containing the calculated Cumulative
#          Abnormal Returns (CAR). The result is a single 
#          dataset ready for the final analysis and hypothesis testing.
#==============================================================================

#==============================================
# --- 1. Load Essential Libraries ---
#==============================================
# These packages provide core functions for data manipulation and joining.
library(dplyr)
library(here)

#==============================================
# --- 2. Prepare at Join Key ---
#==============================================
# A reliable join requires an identical unique identifier ('uid') in both
# datasets. The 'uid' in the main layoff file is reconstructed here to
# exactly match the format created by the WRDS Event Study portal
# (e.g., PERMNO-DDMONYYYY).
layoff_final_with_uid <- layoff_final %>%
  mutate(
    # The date is formatted and converted to uppercase to match the CAR file.
    uid = paste(
      permno,
      toupper(format(warn_date, "%d%b%Y")),
      sep = "-"
    )
  )

#==============================================
# --- 3. Merge the Two Datasets ---
#==============================================
# An inner join is used to combine the two dataframes. This ensures that
# only events for which both financial data and CAR results are available
# are included in the final sample.
final_analysis_dataset <- inner_join(
  layoff_final_with_uid,
  final_results_reordered, # This is the CAR data from the previous script
  by = "uid" # The join is performed on the unique ID
)

#==============================================
# --- 4. Select and Clean Final Columns ---
#==============================================
# To create a clean, analysis-ready dataset, the necessary columns are
# selected and renamed for clarity. Redundant or intermediate columns
# created during the merging process are removed.
final_dataset_cleaned <- final_analysis_dataset %>%
  select(
    # Firm and Event Identifiers
    uid,
    gvkey,
    permno = permno.x, # Keep the original permno column
    conm,
    
    # Layoff Event Details
    City,
    Number.of.Workers,
    WARN.Received.Date,

    
    # Key Financials and R&D Scores
   
    rd_intensity_1yr,
    rd_intensity_3yr,
    rd_intensity_5yr,
    rd_group_1yr,
    rd_group_3yr,
    rd_group_5yr,
    
    # CAR Results from Event Study
    CAR_1_Day_Window,
    CAR_3_Day_Window,
    CAR_5_Day_Window,
    CAR_10_Day_Window
  )

#==============================================
# --- 5. Save the Final Analysis-Ready Dataset ---
#==============================================
# The final, merged dataframe is saved to the 'Output' folder. This dataset
# serves as the primary input for all subsequent statistical analysis,
# tables, and plots.
output_folder <- here("Project", "Output")
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
output_file_path <- file.path(output_folder, "Final_Cleaned_Dataset.csv")
write.csv(final_dataset_cleaned, output_file_path, row.names = FALSE)

