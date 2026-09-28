#### Preamble ####
# Purpose: Replicated tables from the paper on Apartment Evaluation study
# Author: Kexin Liu
# Date: 26 September 2026
# Contact: ws1nn2lj3@gmail.com
# License: MIT
# Pre-requisites: 
  # The `tidyverse` package must be installed
  # The 'tinytable' package must be installed
  # The 'analysis_data.csv' saved and ready to be read
  # The 'score_change_data.csv' is saved and ready to be read


#### Workspace setup ####

library(tidyverse)
library(tinytable)
apartment <- read_csv("data/02-analysis_data/analysis_data.csv")
scorechange <- read_csv("data/02-analysis_data/score_change_data.csv")

#### Tables in Data Section ####

size_breaks <- c(9, 19, 49, 99, Inf)
size_labels <- c("10–19", "20–49", "50–99", "100+")

units <- apartment[["CONFIRMED UNITS"]]

# Numeric characteristics of the latest-evaluation buildings.

unit_summary <- data.frame(
  Statistic = c("Total", "Missing", "Minimum", "Maximum", "Mean", "Median"), 
  "Confirmed Units" = c(
    formatC(sum(!is.na(units)), format = "f", digits = 0, big.mark = ","),
    formatC(sum(is.na(units)), format = "f", digits = 0, big.mark = ","),
    formatC(min(units, na.rm = TRUE), format = "f", digits = 0, big.mark = ","),
    formatC(max(units, na.rm = TRUE), format = "f", digits = 0, big.mark = ","),
    formatC(mean(units, na.rm = TRUE), format = "f", digits = 1, big.mark = ","),
    formatC(median(units, na.rm = TRUE), format = "f", digits = 0, big.mark = ",")),
  check.names = FALSE)

tt(unit_summary) |>
  style_tt(j = 2, align = "r")

# Keep all four recorded scores.

score_counts <- table(factor(apartment[["TENANT SERVICE REQUEST LOG"]],
                             levels = 0:3))
score_summary <- data.frame(Score = as.character(0:3),
                            `Published Meaning` = c("Not specified", 
                                                    "Not completed or properly 
                                                    updated",
                                                    "Not applicable", 
                                                    "Completed/full"),
                            Buildings = as.integer(score_counts),
                            `Share (%)` = 100 * as.integer(score_counts) 
                            / nrow(apartment),
                            check.names = FALSE)
tt(score_summary) |>
  format_tt(escape = TRUE) |>
  format_tt(j = 3, digits = 0, num_fmt = "decimal", num_mark_big = ",") |>
  format_tt(j = 4, digits = 1, num_fmt = "decimal") |>
  style_tt(j = c(3, 4), align = "r")


# The four size groups

size_summary <- apartment |>
  mutate(size_group = cut(.data[["CONFIRMED UNITS"]], breaks = size_breaks, 
                          labels = size_labels)) |>
  filter(!is.na(size_group)) |>
  count(size_group, name = "Building", .drop = FALSE) |>
  mutate(`Share (%)` = 100 * Building / sum(Building)) |>
  rename(`Building Size` = size_group)

tt(size_summary) |>
  format_tt(escape = TRUE) |>
  format_tt(j = 2, digits = 0, num_fmt = "decimal", num_mark_big = ",") |>
  format_tt(j = 3, digits = 1, num_fmt = "decimal") |>
  style_tt(j = 2:3, align = "r")


# Follow up information

scorechange$building_size <- cut(scorechange[["CONFIRMED UNITS"]],
                                 breaks = size_breaks, labels = size_labels)
score1_change <- scorechange |>
  filter(first_date < latest_date, first_score == 1, latest_score %in% c(1, 3)) |>
  mutate(interval_days = as.numeric(latest_date - first_date))

size_change_summary <- score1_change |>
  group_by(building_size) |>
  summarise(total = n(), stayed_at_1 = sum(latest_score == 1),
            percent_stayed_1 = stayed_at_1 / total,
            median_days = median(interval_days), .groups = "drop")

followup_table <- size_change_summary |>
transmute(`Size (Units)` = as.character(building_size),
          Eligible = total,
          `Scored 1 Again` = stayed_at_1,
          `Share (%)` = 100 * percent_stayed_1,
          `Median Gap (Days)` = median_days) |>
  bind_rows(tibble(
    `Size (Units)` = "Overall",
    Eligible = nrow(score1_change),
    `Scored 1 Again` = sum(score1_change$latest_score == 1),
    `Share (%)` = 100 * mean(score1_change$latest_score == 1),
    `Median Gap (Days)` = median(score1_change$interval_days)
  ))

tt(followup_table) |>
  format_tt(escape = TRUE) |>
  format_tt(j = c(2, 3, 5), digits = 0, num_fmt = "decimal") |>
  format_tt(j = 4, digits = 1, num_fmt = "decimal") |>
  style_tt(j = 2:5, align = "r")
