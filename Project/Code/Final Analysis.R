#==============================================================================
# Summary Statistics
#==============================================================================


#==============================================================================
# Table 1- Creating a table with CAR against Rd intensity. 
#=============================================================================

library(dplyr)
library(tidyr)

# Summarize for RD intensity 1 year
summary_1yr <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_1yr)) %>%  # Add this line to filter out NAs
  group_by(rd_group_1yr) %>%
  summarise(
    CAR_1_Day_Window = mean(CAR_1_Day_Window, na.rm = TRUE),
    CAR_3_Day_Window = mean(CAR_3_Day_Window, na.rm = TRUE),
    CAR_5_Day_Window = mean(CAR_5_Day_Window, na.rm = TRUE),
    CAR_10_Day_Window = mean(CAR_10_Day_Window, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Period = "RD intensity 1 year") %>%
  rename(Intensity = rd_group_1yr)

# Summarize for RD intensity 3 year
summary_3yr <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_3yr)) %>%  # Add this line to filter out NAs
  group_by(rd_group_3yr) %>%
  summarise(
    CAR_1_Day_Window = mean(CAR_1_Day_Window, na.rm = TRUE),
    CAR_3_Day_Window = mean(CAR_3_Day_Window, na.rm = TRUE),
    CAR_5_Day_Window = mean(CAR_5_Day_Window, na.rm = TRUE),
    CAR_10_Day_Window = mean(CAR_10_Day_Window, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Period = "RD intensity 3 year") %>%
  rename(Intensity = rd_group_3yr)

# Summarize for RD intensity 5 year
summary_5yr <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_5yr)) %>%  # Add this line to filter out NAs
  group_by(rd_group_5yr) %>%
  summarise(
    CAR_1_Day_Window = mean(CAR_1_Day_Window, na.rm = TRUE),
    CAR_3_Day_Window = mean(CAR_3_Day_Window, na.rm = TRUE),
    CAR_5_Day_Window = mean(CAR_5_Day_Window, na.rm = TRUE),
    CAR_10_Day_Window = mean(CAR_10_Day_Window, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Period = "RD intensity 5 year") %>%
  rename(Intensity = rd_group_5yr)

# Combine summaries
summary_table <- bind_rows(summary_1yr, summary_3yr, summary_5yr) %>%
  select(Period, Intensity, CAR_1_Day_Window, CAR_3_Day_Window, CAR_5_Day_Window, CAR_10_Day_Window)

print(summary_table)

#==============================================================================
# Table 2- Create summary stat tables seperately for CAR and RD
#=============================================================================

# Load required libraries
library(dplyr)
library(tidyr)
# Define mode function
mode_func <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 1-Year R&D Intensity Group Breakdown
rd_1yr_breakdown <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_1yr), !is.na(rd_intensity_1yr)) %>%
  group_by(rd_group_1yr) %>%
  summarise(
    N = sum(!is.na(rd_intensity_1yr)),  # More explicit NA counting
    Mean = mean(rd_intensity_1yr, na.rm = TRUE),
    Mode = mode_func(rd_intensity_1yr, na.rm = TRUE),
    Std_Dev = sd(rd_intensity_1yr, na.rm = TRUE),
    Min = min(rd_intensity_1yr, na.rm = TRUE),
    Max = max(rd_intensity_1yr, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(RD_Measure = "1-Year R&D Intensity") %>%
  rename(RD_Group = rd_group_1yr)

# 3-Year R&D Intensity Group Breakdown
rd_3yr_breakdown <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_3yr), !is.na(rd_intensity_3yr)) %>%
  group_by(rd_group_3yr) %>%
  summarise(
    N = sum(!is.na(rd_intensity_3yr)),
    Mean = mean(rd_intensity_3yr, na.rm = TRUE),
    Mode = mode_func(rd_intensity_3yr, na.rm = TRUE),
    Std_Dev = sd(rd_intensity_3yr, na.rm = TRUE),
    Min = min(rd_intensity_3yr, na.rm = TRUE),
    Max = max(rd_intensity_3yr, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(RD_Measure = "3-Year R&D Intensity") %>%
  rename(RD_Group = rd_group_3yr)

# 5-Year R&D Intensity Group Breakdown
rd_5yr_breakdown <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_5yr), !is.na(rd_intensity_5yr)) %>%
  group_by(rd_group_5yr) %>%
  summarise(
    N = sum(!is.na(rd_intensity_5yr)),
    Mean = mean(rd_intensity_5yr, na.rm = TRUE),
    Mode = mode_func(rd_intensity_5yr, na.rm = TRUE),
    Std_Dev = sd(rd_intensity_5yr, na.rm = TRUE),
    Min = min(rd_intensity_5yr, na.rm = TRUE),
    Max = max(rd_intensity_5yr, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(RD_Measure = "5-Year R&D Intensity") %>%
  rename(RD_Group = rd_group_5yr)

# Combine all breakdowns with additional NA safeguards
rd_group_summary <- bind_rows(rd_1yr_breakdown, rd_3yr_breakdown, rd_5yr_breakdown) %>%
  select(RD_Measure, RD_Group, N, Mean, Mode, Std_Dev, Min, Max) %>%
  drop_na() %>%  # Remove any rows with NAs that might have slipped through
  mutate(across(where(is.numeric) & !N, ~round(.x, 4)))

print(rd_group_summary)


# Summary Stat table for CAR.
#====================================
# Load required libraries
library(dplyr)
library(tidyr)

# Define mode function
mode_func <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# CAR 1-Day Window Breakdown by R&D Groups
car_1day_breakdown <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_1yr), !is.na(CAR_1_Day_Window)) %>%
  group_by(rd_group_1yr) %>%
  summarise(
    N = sum(!is.na(CAR_1_Day_Window)),
    Mean = mean(CAR_1_Day_Window, na.rm = TRUE),
    Mode = mode_func(CAR_1_Day_Window, na.rm = TRUE),
    Std_Dev = sd(CAR_1_Day_Window, na.rm = TRUE),
    Min = min(CAR_1_Day_Window, na.rm = TRUE),
    Max = max(CAR_1_Day_Window, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(CAR_Window = "1-Day CAR") %>%
  rename(RD_Group = rd_group_1yr)

# CAR 3-Day Window Breakdown by R&D Groups
car_3day_breakdown <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_1yr), !is.na(CAR_3_Day_Window)) %>%
  group_by(rd_group_1yr) %>%
  summarise(
    N = sum(!is.na(CAR_3_Day_Window)),
    Mean = mean(CAR_3_Day_Window, na.rm = TRUE),
    Mode = mode_func(CAR_3_Day_Window, na.rm = TRUE),
    Std_Dev = sd(CAR_3_Day_Window, na.rm = TRUE),
    Min = min(CAR_3_Day_Window, na.rm = TRUE),
    Max = max(CAR_3_Day_Window, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(CAR_Window = "3-Day CAR") %>%
  rename(RD_Group = rd_group_1yr)

# CAR 5-Day Window Breakdown by R&D Groups
car_5day_breakdown <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_1yr), !is.na(CAR_5_Day_Window)) %>%
  group_by(rd_group_1yr) %>%
  summarise(
    N = sum(!is.na(CAR_5_Day_Window)),
    Mean = mean(CAR_5_Day_Window, na.rm = TRUE),
    Mode = mode_func(CAR_5_Day_Window, na.rm = TRUE),
    Std_Dev = sd(CAR_5_Day_Window, na.rm = TRUE),
    Min = min(CAR_5_Day_Window, na.rm = TRUE),
    Max = max(CAR_5_Day_Window, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(CAR_Window = "5-Day CAR") %>%
  rename(RD_Group = rd_group_1yr)

# CAR 10-Day Window Breakdown by R&D Groups
car_10day_breakdown <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_1yr), !is.na(CAR_10_Day_Window)) %>%
  group_by(rd_group_1yr) %>%
  summarise(
    N = sum(!is.na(CAR_10_Day_Window)),
    Mean = mean(CAR_10_Day_Window, na.rm = TRUE),
    Mode = mode_func(CAR_10_Day_Window, na.rm = TRUE),
    Std_Dev = sd(CAR_10_Day_Window, na.rm = TRUE),
    Min = min(CAR_10_Day_Window, na.rm = TRUE),
    Max = max(CAR_10_Day_Window, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(CAR_Window = "10-Day CAR") %>%
  rename(RD_Group = rd_group_1yr)

# Combine all CAR breakdowns
car_summary <- bind_rows(car_1day_breakdown, car_3day_breakdown, car_5day_breakdown, car_10day_breakdown) %>%
  select(CAR_Window, RD_Group, N, Mean, Mode, Std_Dev, Min, Max) %>%
  drop_na() %>%  # Remove any rows with NAs
  mutate(across(where(is.numeric) & !N, ~round(.x, 4)))

print(car_summary)

#==============================================================================
# Table 3- Create industry wise summary tables
#=============================================================================
library(dplyr)
library(RPostgres)  # if using WRDS connection
industry_by_gvkey <- tbl(wrds, sql("
  SELECT gvkey, sich, naicsh, datadate
  FROM comp.co_industry
  WHERE consol = 'C' AND popsrc = 'D'
    AND datadate >= '2015-01-01'
")) %>%
  collect() %>%
  group_by(gvkey) %>%
  arrange(desc(datadate)) %>%
  slice(1) %>%
  ungroup()

# Join industry classification to your main dataset
final_dataset_with_industry <- final_dataset_cleaned %>%
  left_join(industry_by_gvkey, by = "gvkey")
final_dataset_with_industry <- final_dataset_with_industry %>%
  mutate(
    sic_2digit = as.numeric(substr(as.character(sich), 1, 2)),
    industry_name = case_when(
      sic_2digit >= 1 & sic_2digit <= 9 ~ "Agriculture, Forestry, Fishing",
      sic_2digit >= 10 & sic_2digit <= 14 ~ "Mining",
      sic_2digit >= 15 & sic_2digit <= 17 ~ "Construction", 
      sic_2digit >= 20 & sic_2digit <= 39 ~ "Manufacturing",
      sic_2digit >= 40 & sic_2digit <= 49 ~ "Transportation & Utilities",
      sic_2digit >= 50 & sic_2digit <= 51 ~ "Wholesale Trade",
      sic_2digit >= 52 & sic_2digit <= 59 ~ "Retail Trade",
      sic_2digit >= 60 & sic_2digit <= 67 ~ "Finance & Insurance",
      sic_2digit >= 70 & sic_2digit <= 89 ~ "Services",
      sic_2digit >= 91 & sic_2digit <= 99 ~ "Public Administration",
      TRUE ~ "Other/Unknown"
    )
  )
# Count observations by industry
industry_counts <- final_dataset_with_industry %>%
  group_by(industry_name) %>%
  summarise(N = n()) %>%
  arrange(desc(N))

print(industry_counts)