#==============================================================================
# Purpose: Generate all summary tables and plots for the final report.
# Please note that all outputs of this script gets saved in Output folder as png .
# If you choose to run the code step by step, then it will save the pngs while displaying the results.
#==============================================================================


#==============================================================================
# Part 1: Setup
#==============================================================================

# Loads all the packages needed for the script to run.
library(knitr)
library(kableExtra)
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(here)
library(webshot2)

# Defines a simple function to find the mode of a set of values.
mode_func <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#==============================================================================
# Part 2: Table 1 - R&D Classification Summary
#==============================================================================
# This section creates a table summarizing layoff announcements,
# unique firms, and total workers for each R&D group.

summary_1yr <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_1yr)) %>%
  group_by(Group = rd_group_1yr) %>%
  summarise(
    `Layoff Announcements` = n(),
    `Number of Unique Firms` = n_distinct(gvkey),
    `Total Workers Affected` = sum(Number.of.Workers, na.rm = TRUE)
  ) %>%
  mutate(Classification = "1-Year R&D")

summary_3yr <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_3yr)) %>%
  group_by(Group = rd_group_3yr) %>%
  summarise(
    `Layoff Announcements` = n(),
    `Number of Unique Firms` = n_distinct(gvkey),
    `Total Workers Affected` = sum(Number.of.Workers, na.rm = TRUE)
  ) %>%
  mutate(Classification = "3-Year R&D")

summary_5yr <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_5yr)) %>%
  group_by(Group = rd_group_5yr) %>%
  summarise(
    `Layoff Announcements` = n(),
    `Number of Unique Firms` = n_distinct(gvkey),
    `Total Workers Affected` = sum(Number.of.Workers, na.rm = TRUE)
  ) %>%
  mutate(Classification = "5-Year R&D")

final_summary_data <- bind_rows(summary_1yr, summary_3yr, summary_5yr) %>%
  select(Classification, everything())

professional_table_1 <- final_summary_data %>%
  kbl(
    caption = "Table 1: Summary Statistics by R&D Classification",
    booktabs = TRUE,
    col.names = c("Classification", "R&D Group", "Layoff Announcements", "Unique Firms", "Total Workers"),
    format.args = list(big.mark = ",")
  ) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  pack_rows(index = table(final_summary_data$Classification)) %>%
  footnote(general = "Data sourced from final_dataset_cleaned dataframe.")

output_folder <- here("Project", "Output")
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

print(professional_table_1)

save_kable(
  professional_table_1,
  file = here(output_folder, "Table_1_Summary_Statistics_by_RD_Classification.png")
)


#==============================================================================
# Part 3: Table 2 - CAR Summary Statistics
#==============================================================================
# This section creates a table with detailed statistics for CARs,
# broken down by R&D group and event window.

car_summary <- final_dataset_cleaned %>%
  select(rd_group_1yr, starts_with("CAR_")) %>%
  pivot_longer(
    cols = -rd_group_1yr,
    names_to = "CAR_Window",
    values_to = "CAR_Value"
  ) %>%
  filter(!is.na(rd_group_1yr) & !is.na(CAR_Value)) %>%
  group_by(CAR_Window, RD_Group = rd_group_1yr) %>%
  summarise(
    N = n(),
    Mean = mean(CAR_Value, na.rm = TRUE),
    Mode = mode_func(CAR_Value, na.rm = TRUE),
    Std_Dev = sd(CAR_Value, na.rm = TRUE),
    Min = min(CAR_Value, na.rm = TRUE),
    Max = max(CAR_Value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(CAR_Window = case_when(
    CAR_Window == "CAR_1_Day_Window"  ~ "1-Day CAR [-1, +1]",
    CAR_Window == "CAR_3_Day_Window"  ~ "3-Day CAR [-3, +3]",
    CAR_Window == "CAR_5_Day_Window"  ~ "5-Day CAR [-5, +5]",
    CAR_Window == "CAR_10_Day_Window" ~ "10-Day CAR [-10, +10]"
  )) %>%
  arrange(CAR_Window, RD_Group) %>%
  select(CAR_Window, RD_Group, N, Mean, Mode, Std_Dev, Min, Max)

professional_table_2 <- car_summary %>%
  mutate(across(where(is.numeric) & !N, ~round(.x, 4))) %>%
  kbl(
    caption = "Table 2: Summary Statistics for CARs by R&D Group",
    booktabs = TRUE,
    align = "llrrrrrr"
  ) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  pack_rows(index = table(car_summary$CAR_Window))

print(professional_table_2)

save_kable(
  professional_table_2,
  file = here(output_folder, "Table_2_Summary_Statistics_for_CARs_by_RD_Group.png")
)


#==============================================================================
# Part 4: Table 3 - Mean CARs by R&D Classification (Wide Format)
#==============================================================================
# This section creates the wide-format summary of mean CARs
# across all R&D classifications and event windows.

summary_1yr_wide <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_1yr)) %>%
  group_by(`R&D Group` = rd_group_1yr) %>%
  summarise(
    `Mean CAR [-1, +1]` = mean(CAR_1_Day_Window, na.rm = TRUE),
    `Mean CAR [-3, +3]` = mean(CAR_3_Day_Window, na.rm = TRUE),
    `Mean CAR [-5, +5]` = mean(CAR_5_Day_Window, na.rm = TRUE),
    `Mean CAR [-10, +10]` = mean(CAR_10_Day_Window, na.rm = TRUE)
  ) %>%
  mutate(Classification = "1-Year R&D")

summary_3yr_wide <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_3yr)) %>%
  group_by(`R&D Group` = rd_group_3yr) %>%
  summarise(
    `Mean CAR [-1, +1]` = mean(CAR_1_Day_Window, na.rm = TRUE),
    `Mean CAR [-3, +3]` = mean(CAR_3_Day_Window, na.rm = TRUE),
    `Mean CAR [-5, +5]` = mean(CAR_5_Day_Window, na.rm = TRUE),
    `Mean CAR [-10, +10]` = mean(CAR_10_Day_Window, na.rm = TRUE)
  ) %>%
  mutate(Classification = "3-Year R&D")

summary_5yr_wide <- final_dataset_cleaned %>%
  filter(!is.na(rd_group_5yr)) %>%
  group_by(`R&D Group` = rd_group_5yr) %>%
  summarise(
    `Mean CAR [-1, +1]` = mean(CAR_1_Day_Window, na.rm = TRUE),
    `Mean CAR [-3, +3]` = mean(CAR_3_Day_Window, na.rm = TRUE),
    `Mean CAR [-5, +5]` = mean(CAR_5_Day_Window, na.rm = TRUE),
    `Mean CAR [-10, +10]` = mean(CAR_10_Day_Window, na.rm = TRUE)
  ) %>%
  mutate(Classification = "5-Year R&D")

car_summary_combined <- bind_rows(summary_1yr_wide, summary_3yr_wide, summary_5yr_wide) %>%
  select(Classification, `R&D Group`, everything())

professional_table_3 <- car_summary_combined %>%
  kbl(
    caption = "Table 3: Mean CARs by R&D Classification and Event Window",
    booktabs = TRUE,
    align = "llcccc",
    digits = 4
  ) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  pack_rows(index = table(car_summary_combined$Classification))

print(professional_table_3)

save_kable(
  professional_table_3,
  file = here(output_folder, "Table_3_Mean_CARs_by_RD_Classification_and_Event_Window.png")
)


#==============================================================================
# Part 5: Table 4 - Hypothesis Test Results
#==============================================================================
# This section runs the t-tests and presents the results in a formatted table.

tidy_for_testing <- final_dataset_cleaned %>%
  pivot_longer(cols = starts_with("CAR_"), names_to = "Event_Window", values_to = "CAR_Value") %>%
  pivot_longer(cols = starts_with("rd_group_"), names_to = "Classification", values_to = "RD_Group") %>%
  mutate(
    Event_Window = case_when(
      Event_Window == "CAR_1_Day_Window"  ~ "[-1, +1]",
      Event_Window == "CAR_3_Day_Window"  ~ "[-3, +3]",
      Event_Window == "CAR_5_Day_Window"  ~ "[-5, +5]",
      Event_Window == "CAR_10_Day_Window" ~ "[-10, +10]"
    ),
    Classification = case_when(
      Classification == "rd_group_1yr" ~ "1-Year R&D",
      Classification == "rd_group_3yr" ~ "3-Year R&D",
      Classification == "rd_group_5yr" ~ "5-Year R&D"
    )
  )

ttest_results <- tidy_for_testing %>%
  filter(!is.na(CAR_Value) & !is.na(RD_Group)) %>%
  group_by(Classification, Event_Window) %>%
  summarise(test_output = list(broom::tidy(t.test(CAR_Value ~ RD_Group))), .groups = "drop") %>%
  unnest(test_output) %>%
  select(
    Classification, Event_Window,
    `Mean CAR (High R&D)` = estimate1, `Mean CAR (Low R&D)` = estimate2,
    Difference = estimate, `t-statistic` = statistic, `p-value` = p.value
  ) %>%
  arrange(Classification, factor(Event_Window, levels = c("[-1, +1]", "[-3, +3]", "[-5, +5]", "[-10, +10]")))

results_formatted_df <- ttest_results %>%
  mutate(
    across(c(`Mean CAR (High R&D)`:`t-statistic`), ~ sprintf("%.4f", .)),
    `p-value` = case_when(
      `p-value` < 0.01  ~ paste0(sprintf("%.4f", `p-value`), "**"),
      `p-value` < 0.05  ~ paste0(sprintf("%.4f", `p-value`), "*"),
      TRUE              ~ sprintf("%.4f", `p-value`)
    )
  )

professional_table_4 <- results_formatted_df %>%
  kbl(
    caption = "Table 4: T-Test for Difference in Mean CARs",
    booktabs = TRUE,
    align = "llrrrrr"
  ) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") %>%
  pack_rows(index = table(ttest_results$Classification)) %>%
  footnote(general = "Significance levels: * p < 0.05, ** p < 0.01")

print(professional_table_4)

save_kable(
  professional_table_4,
  file = here(output_folder, "Table_4_T-Test_for_Difference_in_Mean_CARs.png")
)


#==============================================================================
# Part 6: Figure 1 - Grouped Bar Chart
#==============================================================================
# This section creates a bar chart to visualize the mean CARs.

plot_data_bars <- car_summary_combined %>%
  filter(Classification == "1-Year R&D") %>%
  select(-Classification) %>%
  pivot_longer(
    cols = -`R&D Group`,
    names_to = "Event_Window",
    values_to = "Mean_CAR"
  )

figure_1_bar_chart <- ggplot(plot_data_bars, aes(x = Event_Window, y = Mean_CAR, fill = `R&D Group`)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, color = "black") +
  ggtitle("Figure 1: Mean CAR by R&D Group and Event Window") +
  labs(
    x = "Event Window",
    y = "Mean Cumulative Abnormal Return",
    fill = "R&D Group (1-Year)"
  ) +
  theme_bw() +
  scale_fill_manual(values = c("High R&D" = "#D55E00", "Low R&D" = "#0072B2"))

print(figure_1_bar_chart)

ggsave(
  filename = here(output_folder, "Figure_1_Mean_CAR_by_RD_Group_and_Event_Window.png"),
  plot = figure_1_bar_chart,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)