#==============================================================================
# Summary Statistics
#==============================================================================


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

