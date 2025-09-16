# --- 1. PREPARE THE JOIN KEY (WITH CORRECTION) ---

# We need to create a 'uid' in the 'layoff_final' dataframe that
# perfectly matches the format in 'final_results_reordered'.

layoff_final_with_uid <- layoff_final %>%
  mutate(
    # THE FIX: We wrap the formatted date in toupper() to force the month to uppercase (e.g., "SEP")
    uid = paste(
      permno, 
      toupper(format(warn_date, "%d%b%Y")), 
      sep = "-"
    )
  )

# --- 2. MERGE THE TWO DATAFRAMES IN MEMORY ---

# Now that the 'uid' keys are in the identical format, the join will work.
final_analysis_dataset <- inner_join(
  layoff_final_with_uid,
  final_results_reordered,
  by = "uid" # Join by the corrected unique ID
)

# --- 3. Data Cleaning ---

final_dataset_cleaned <- final_analysis_dataset %>%
  select(
    # Firm and Event Identifiers
    uid,
    gvkey,
    permno = permno.x, # Rename permno.x to the cleaner 'permno'
    conm,
    
    # Layoff Event Details
    City,
    Number.of.Workers,
    WARN.Received.Date,
    Effective.Date,
    warn_date,
    
    # Financial Data
    xrd,
    sale,
    at,
    
    # R&D Intensity Scores
    rd_intensity_1yr,
    rd_intensity_3yr,
    rd_intensity_5yr,
    
    # R&D Median and Group Classifications
    rd_median_1yr,
    rd_median_3yr,
    rd_median_5yr,
    rd_group_1yr,
    rd_group_3yr,
    rd_group_5yr,
    
    # CAR Results
    CAR_1_Day_Window,
    CAR_3_Day_Window,
    CAR_5_Day_Window,
    CAR_10_Day_Window
  )

# Saving the final output file
output_folder <- here("Project", "Output")
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
output_file_path <- file.path(output_folder, "Final_Cleaned_Dataset.csv")
write.csv(final_dataset_cleaned, output_file_path, row.names = FALSE)

