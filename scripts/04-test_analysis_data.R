#### Preamble ####
# Purpose: Check the saved apartment evaluation data and derived analysis datasets
# Author: Kexin Liu
# Date: 24 September 2026
# Contact: ws1nn2lj3@gmail.com
# License: MIT
# Pre-requisites: 
  #`tidyverse` and `testthat` must be installed
  # Run scripts/03-clean_data.R first, from the project repo
# Addtional Notes:
  # Data below is downloaded on September 24, 2026
  # Changes are expected to be made if the source data is downloaded on another day

#### Workspace setup ####

library(tidyverse)
library(testthat)

# Read only the raw columns used by the current study

evaluations <- read_csv("../data/01-raw_data/raw_data.csv",
                        col_select = c("_id", "RSN", "EVALUATION COMPLETED ON", 
                                       "CONFIRMED UNITS",
                                       "TENANT SERVICE REQUEST LOG", "WARDNAME"),
                        show_col_types = FALSE) |>
  mutate(completion_date = as.Date(`EVALUATION COMPLETED ON`))

apartment <- read_csv("../data/02-analysis_data/analysis_data.csv",
                       show_col_types = FALSE)
scorechange <- read_csv("../data/02-analysis_data/score_change_data.csv",
                         show_col_types = FALSE)

ward_names <- c(
  "Beaches-East York", "Davenport", "Don Valley East", "Don Valley North",
  "Don Valley West", "Eglinton-Lawrence", "Etobicoke Centre", "Etobicoke North",
  "Etobicoke-Lakeshore", "Humber River-Black Creek", "Parkdale-High Park",
  "Scarborough Centre", "Scarborough North", "Scarborough Southwest",
  "Scarborough-Agincourt", "Scarborough-Guildwood", "Scarborough-Rouge Park",
  "Spadina-Fort York", "Toronto Centre", "Toronto-Danforth", "Toronto-St. Paul's",
  "University-Rosedale", "Willowdale", "York Centre", "York South-Weston")


#### Test data ####

test_that("records contain valid building characteristics and scores", {
  expect_false(anyNA(evaluations))
  expect_equal(anyDuplicated(evaluations[["_id"]]), 0)
  expect_equal(range(evaluations[["CONFIRMED UNITS"]]), c(10, 793))
  expect_true(all(evaluations[["CONFIRMED UNITS"]] %% 1 == 0))
  expect_true(all(evaluations[["TENANT SERVICE REQUEST LOG"]] %in% 0:3))
  expect_true(all(evaluations$WARDNAME %in% ward_names))
  expect_false(any(c("YEAR BUILT", "YEAR EVALUATED", "BUILDING AGE",
                     "PROPERTY TYPE") %in% names(apartment)))
})

## Only applies to the current downloaded data!!

test_that("evaluation histories include valid, distinct dates", {
  expect_s3_class(evaluations$completion_date, "Date")
  expect_true(all(evaluations$completion_date >= as.Date("2023-06-05") &
                    evaluations$completion_date <= as.Date("2026-09-24")))
  expect_equal(anyDuplicated(evaluations[c("RSN", "completion_date")]), 0)
  counts <- evaluations |> count(RSN)
  expect_equal(nrow(evaluations), 6731)
  expect_equal(nrow(counts), 3587)
  expect_equal(sum(counts$n == 1), 444)
  expect_equal(sum(counts$n == 2), 3142)
  expect_equal(sum(counts$n == 3), 1)
})

test_that("the main sample contains the actual latest row for each building", {
  expect_false(anyNA(apartment))
  expect_equal(anyDuplicated(apartment$RSN), 0)
  expect_setequal(apartment$RSN, evaluations$RSN)
  # Independently identify rows with no later evaluation in the same building.
  expected <- evaluations |>
    group_by(RSN) |>
    filter(completion_date == max(completion_date)) |>
    ungroup() |>
    arrange(RSN)
  actual <- apartment |> arrange(RSN)
  expect_equal(as.data.frame(actual[names(expected)]), as.data.frame(expected))
})

test_that("follow-up pairs use the true endpoints and their recorded scores", {
  expect_false(anyNA(scorechange))
  expect_equal(anyDuplicated(scorechange$RSN), 0)
  expect_setequal(scorechange$RSN, evaluations$RSN)
  endpoints <- evaluations |>
    group_by(RSN) |>
    summarise(earliest = min(completion_date), latest = max(completion_date),
              .groups = "drop")
  checked <- scorechange |> left_join(endpoints, by = "RSN")
  expect_equal(checked$first_date, checked$earliest)
  expect_equal(checked$latest_date, checked$latest)
  first_rows <- evaluations |>
    select(RSN, first_date = completion_date,
           recorded_first = `TENANT SERVICE REQUEST LOG`)
  last_rows <- evaluations |>
    select(RSN, latest_date = completion_date,
           recorded_latest = `TENANT SERVICE REQUEST LOG`,
           recorded_units = `CONFIRMED UNITS`)
  checked <- checked |>
    left_join(first_rows, by = c("RSN", "first_date")) |>
    left_join(last_rows, by = c("RSN", "latest_date"))
  expect_equal(checked$first_score, checked$recorded_first)
  expect_equal(checked$latest_score, checked$recorded_latest)
  expect_equal(checked[["CONFIRMED UNITS"]], checked$recorded_units)
  expect_equal(sum(checked$first_date == checked$latest_date), 444)
})

test_that("the follow-up filter excludes buildings without a later evaluation", {
  followup <- scorechange |>
    filter(first_date < latest_date, first_score == 1, latest_score %in% c(1, 3))
  single_visit <- evaluations |> count(RSN) |> filter(n == 1)
  expect_gt(nrow(followup), 0)
  expect_false(any(followup$RSN %in% single_visit$RSN))
})
