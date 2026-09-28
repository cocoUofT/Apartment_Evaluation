#### Preamble ####
# Purpose: Simulates apartment evaluation records for the building-size,
# follow-up-rating, and ward comparisons used in this study.
# Author: Kexin Liu
# Date: 24 September 2026
# Updated: 28 September 2026
# Contact: ws1nn2lj3@gmail.com
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed.


#### Workspace setup ####

library(tidyverse)
set.seed(924)

#### Simulate building characteristics ####

n_building <- 3500
building_id <- seq_len(n_building)

# These bands give an approximate right-skewed distribution of unit counts.

unit_ranges <- list(10:19, 20:49, 50:99, 100:199, 200:399, 400:800)

unit_band <- sample(seq_along(unit_ranges), n_building, replace = TRUE,
                    prob = c(0.208, 0.286, 0.192, 0.177, 0.122, 0.015))

confirmed_units <- vapply(unit_band, function(i) sample(unit_ranges[[i]], 1L),
                          integer(1))

# Use 25 fictional ward labels. 

ward_name <- sample(sprintf("Simulated Ward %02d", 1:25), n_building,
                    replace = TRUE)


#### Simulate evaluation history ####

# Give 1800 buildings one evaluation, 1600 two, and 100 three, total of 5300 records
# These counts are chosen for testing, not to reproduce the real sample exactly.

evaluation_counts <- sample(c(rep(1, 1800), rep(2, 1600), rep(3, 100)))
record_building <- rep(building_id, times = evaluation_counts)
n_evaluation <- length(record_building)

# Dates within a building are distinct. 

possible_dates <- seq(as.Date("2023-06-05"), as.Date("2026-09-24"), by = "day")
evaluation_time <- do.call(c, lapply(evaluation_counts, function(n) {
  sort(sample(possible_dates, size = n, replace = FALSE))
}))

# Retain all four recorded scores: 0 has no published meaning, 1 means the log
# was incomplete or outdated, 2 means not applicable, and 3 means completed/full.
# Scores are drawn independently with illustrative probabilities that sum to 1.
# No size effect, ward effect, or improvement over time is imposed here.
score_probs <- c(0.01, 0.10, 0.02, 0.87)

simulated <- data.frame(
  "_id" = seq_len(n_evaluation),
  "RSN" = record_building,
  "EVALUATION COMPLETED ON" = as.character(evaluation_time),
  "CONFIRMED UNITS" = confirmed_units[record_building],
  "TENANT SERVICE REQUEST LOG" = sample(0:3, n_evaluation, replace = TRUE,
                                        prob = score_probs),
  "WARDNAME" = ward_name[record_building],
  check.names = FALSE
)

# Shuffle rows so that selecting the earliest/latest record requires sorting.
simulated <- simulated[sample(nrow(simulated)), ]
row.names(simulated) <- NULL

#### Derive the analysis datasets ####

# Match the date conversion and latest-record selection in 03-clean_data.R.
evaluation_history <- simulated |>
  mutate(completion_date = as.Date(`EVALUATION COMPLETED ON`))

sorted <- evaluation_history |>
  arrange(RSN, desc(completion_date), desc(`_id`))
latest <- sorted[!duplicated(sorted$RSN), ]
row.names(latest) <- NULL

# Match the fixed size categories used in the paper; do not use quartiles.
size_breaks <- c(9, 19, 49, 99, Inf)
size_labels <- c("10–19", "20–49", "50–99", "100+")
latest$building_size <- cut(latest[["CONFIRMED UNITS"]],
                            breaks = size_breaks, labels = size_labels)

# Keep one earliest/latest pair per building, as in the real cleaning script.
# A building with only one evaluation has equal first_date and latest_date.
scorechange <- evaluation_history |>
  arrange(RSN, completion_date) |>
  group_by(RSN) |>
  summarise(first_date = first(completion_date),
            latest_date = last(completion_date),
            first_score = first(`TENANT SERVICE REQUEST LOG`),
            latest_score = last(`TENANT SERVICE REQUEST LOG`),
            `CONFIRMED UNITS` = last(`CONFIRMED UNITS`), .groups = "drop")

# The analysis subsequently keeps only buildings with a later evaluation,
# first_score == 1, and latest_score in c(1, 3).

#### Save data ####

dir.create("data/00-simulated_data", recursive = TRUE, showWarnings = FALSE)
write_csv(simulated, "data/00-simulated_data/simulated_evaluations.csv")
write_csv(latest, "data/00-simulated_data/simulated_data.csv")
write_csv(scorechange, "data/00-simulated_data/simulated_score_change_data.csv")
