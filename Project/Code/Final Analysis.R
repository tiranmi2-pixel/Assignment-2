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
# Table 3- Create industry wise RD Intensity
#=============================================================================
# Load required libraries
library(dplyr)
library(tidyr)

# Define mode function
mode_func <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate total firms for percentage calculation
total_firms <- final_dataset_with_industry %>%
  filter(!is.na(industry_name)) %>%
  nrow()

# Industry summary with percentage only for N column
industry_summary_final <- final_dataset_with_industry %>%
  filter(!is.na(industry_name)) %>%
  group_by(industry_name) %>%
  summarise(
    N = sum(!is.na(rd_intensity_1yr)),
    Mean = ifelse(sum(!is.na(rd_intensity_1yr)) > 0, 
                  mean(rd_intensity_1yr, na.rm = TRUE), NA),
    Mode = ifelse(sum(!is.na(rd_intensity_1yr)) > 0, 
                  mode_func(rd_intensity_1yr, na.rm = TRUE), NA),
    Std_Dev = ifelse(sum(!is.na(rd_intensity_1yr)) > 1, 
                     sd(rd_intensity_1yr, na.rm = TRUE), NA),
    Min = ifelse(sum(!is.na(rd_intensity_1yr)) > 0, 
                 min(rd_intensity_1yr, na.rm = TRUE), NA),
    Max = ifelse(sum(!is.na(rd_intensity_1yr)) > 0, 
                 max(rd_intensity_1yr, na.rm = TRUE), NA),
    .groups = 'drop'
  ) %>%
  # Add percentage column for N only and round statistics to 3 decimal places
  mutate(
    N_Pct = round((N / total_firms) * 100, 3),
    Mean = round(Mean, 3),
    Mode = round(Mode, 3),
    Std_Dev = round(Std_Dev, 3),
    Min = round(Min, 3),
    Max = round(Max, 3)
  ) %>%
  # Reorder columns: N, N_Pct, then statistics
  select(industry_name, N, N_Pct, Mean, Mode, Std_Dev, Min, Max) %>%
  arrange(desc(N))

print(industry_summary_final)



# Calculate total firms for percentage calculation
total_car_firms <- final_dataset_cleaned %>%
  filter(!is.na(CAR_1_Day_Window)) %>%
  nrow()

# 1-Day CAR Breakdown
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

# 3-Day CAR Breakdown
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

# 5-Day CAR Breakdown
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

# 10-Day CAR Breakdown
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

# Combine all CAR breakdowns with percentage and rounding
car_summary <- bind_rows(car_1day_breakdown, car_3day_breakdown, 
                         car_5day_breakdown, car_10day_breakdown) %>%
  select(CAR_Window, RD_Group, N, Mean, Mode, Std_Dev, Min, Max) %>%
  drop_na() %>%  # Remove any rows with NAs that might have slipped through
  mutate(
    N_Pct = round((N / total_car_firms) * 100, 3),
    across(where(is.numeric) & !any_of(c("N", "N_Pct")), ~round(.x, 3))
  ) %>%
  select(CAR_Window, RD_Group, N, N_Pct, Mean, Mode, Std_Dev, Min, Max) %>%
  arrange(CAR_Window, RD_Group)

print(car_summary)

#==============================================================================
# Table 4- 
#==============================================
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr) # Needed for pivot_longer()

# Assuming 'final_dataset_cleaned' is your final, merged dataframe

# --- 1. Data Preparation for Multiple CAR Windows ---
summary_by_decile_all_windows <- final_dataset_cleaned %>%
  filter(!is.na(rd_intensity_1yr)) %>%
  group_by(rd_decile = ntile(rd_intensity_1yr, 10)) %>%
  # Calculate the mean for EACH of the CAR windows
  summarise(
    `1 Day Window` = mean(CAR_1_Day_Window, na.rm = TRUE),
    `3 Day Window` = mean(CAR_3_Day_Window, na.rm = TRUE),
    `5 Day Window` = mean(CAR_5_Day_Window, na.rm = TRUE),
    `10 Day Window` = mean(CAR_10_Day_Window, na.rm = TRUE)
  ) %>%
  # Pivot the data from wide to long format for plotting
  pivot_longer(
    cols = ends_with("Window"),
    names_to = "CAR_Window",
    values_to = "mean_car"
  ) %>%
  # Ensure the order of the windows is correct in the legend
  mutate(
    CAR_Window = factor(CAR_Window, levels = c("1 Day Window", "3 Day Window", "5 Day Window", "10 Day Window"))
  )


# --- 2. Create the Multi-Line Plot ---
ggplot(summary_by_decile_all_windows, aes(x = rd_decile, y = mean_car, color = CAR_Window, group = CAR_Window)) +
  # Add the trend lines and points for each CAR window
  geom_line(size = 1.1) +
  geom_point(size = 2.5) +
  
  # Add a horizontal line at y=0 for reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  # Add titles and labels
  labs(
    title = "Mean CAR vs. 1-Year R&D Intensity by Event Window",
    subtitle = "Firms grouped by 1-Year R&D Intensity Deciles",
    x = "R&D Intensity Decile (1 = Lowest, 10 = Highest)",
    y = "Mean Cumulative Abnormal Return",
    color = "CAR Event Window" # This will be the title of the legend
  ) +
  
  # Use a clean theme and ensure all deciles are shown on the x-axis
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks = 1:10) +
  # Use a color-blind friendly palette for the lines
  scale_color_brewer(palette = "Dark2") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
