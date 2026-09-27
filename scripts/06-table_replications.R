#### Preamble ####
# Purpose: Replicated tables from the paper on Apartment Evaluation
# Author: Kexin Liu
# Date: 26 September 2026
# Contact: ws1nn2lj3@gmail.com
# License: 
# Pre-requisites: 
# The `tidyverse` package must be installed
# The 'tinytable' package must be installed
# Any other information needed? 

#### Workspace setup ####

library(tidyverse)
library(tinytable)
apartment <- read_csv("data/02-analysis_data/analysis_data.csv")


#### Tables in Data Section ####

units <- apartment[["CONFIRMED UNITS"]]
building_age <- 
  as.integer(format(as.Date(apartment[["EVALUATION COMPLETED ON"]]), "%Y")) - 
  apartment[["YEAR BUILT"]]

# Table 1: numeric characteristics of the latest-evaluation buildings.

numeric_summary <- data.frame(
  Measure = c("Confirmed Units", "Age (years)"),
  Buildings = c(sum(!is.na(units)), sum(!is.na(building_age))),
  Missing = c(sum(is.na(units)), sum(is.na(building_age))),
  Minimum = c(min(units, na.rm = TRUE), min(building_age, na.rm = TRUE)),
  Maximum = c(max(units, na.rm = TRUE), max(building_age, na.rm = TRUE)),
  Mean = c(mean(units, na.rm = TRUE), mean(building_age, na.rm = TRUE)),
  Median = c(median(units, na.rm = TRUE),
             median(building_age, na.rm = TRUE)))

table_1 <- tt(numeric_summary) |>
  format_tt(j = c(2, 3, 4, 5, 7), digits = 0,
            num_fmt = "decimal", num_mark_big = ",") |>
  format_tt(j = 6, digits = 1, num_fmt = "decimal") |>
  style_tt(j = 2:7, align = "r")

# Table 2: keep all four recorded scores.

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

table_2 <- tt(score_summary) |>
  format_tt(j = 3, digits = 0, num_fmt = "decimal", num_mark_big = ",") |>
  format_tt(j = 4, digits = 1, num_fmt = "decimal") |>
  style_tt(j = c(3, 4), align = "r")


#### Table in Appendix ####

# Appendix table 1: the four size groups used elsewhere in the paper.

size_summary <- apartment |>
  mutate(size_group = cut(.data[["CONFIRMED UNITS"]], breaks = size_breaks, 
                          labels = size_labels)) |>
  filter(!is.na(size_group)) |>
  count(size_group, name = "Buildings", .drop = FALSE) |>
  mutate(`Share (%)` = 100 * Buildings / sum(Buildings)) |>
  rename(`Building size` = size_group)

table_3 <- tt(size_summary) |>
  format_tt(j = 2, digits = 0, num_fmt = "decimal", num_mark_big = ",") |>
  format_tt(j = 3, digits = 1, num_fmt = "decimal") |>
  style_tt(j = 2:3, align = "r")