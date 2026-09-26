#### Preamble ####
# Purpose: Cleans the raw Apartment Building Evaluation data from 2023 to present
# Author: Kexin Liu
# Date: 24 September 2026
# Contact: ws1nn2lj3@gmail.com
# License: 
# Pre-requisites: 

# Any other information needed?

#### Workspace setup ####

library(tidyverse)
raw_data <- read_csv("data/01-raw_data/raw_data.csv")


#### Clean data ####

variable_interest <- raw_data |>
  select("RSN", "EVALUATION COMPLETED ON", "CONFIRMED UNITS", "YEAR BUILT", 
         "PROPERTY TYPE", "TENANT SERVICE REQUEST LOG", "PEST CONTROL LOG",
         "MAINTENANCE LOG") |>
  mutate("YEAR EVALUATED" = as.integer(
    format(as.Date(raw_data[["EVALUATION COMPLETED ON"]]), "%Y")
  ))

sorted <- variable_interest[order(variable_interest$RSN, 
                                  -variable_interest[["YEAR EVALUATED"]]), ]
apartment <- sorted[!duplicated(sorted$RSN), ]
row.names(apartment) <- NULL

colSums(is.na(apartment)) ## Check for missing value in all columns


#### Additional dataframe for evaluating score change only ####

# Keep buildings that were evaluated more than once

df.history <- raw_data |>
  select("RSN", "EVALUATION COMPLETED ON", "CONFIRMED UNITS",
         "TENANT SERVICE REQUEST LOG") |>
  mutate(completion_date = as.Date(`EVALUATION COMPLETED ON`))

colSums(is.na(df.history)) ## Check for missing value in all columns

scorechange <- df.history |>
  arrange(RSN, completion_date) |>
  group_by(RSN) |>
  summarise(first_date = first(completion_date),
            latest_date = last(completion_date),
            first_score = first(`TENANT SERVICE REQUEST LOG`),
            latest_score = last(`TENANT SERVICE REQUEST LOG`),
            `CONFIRMED UNITS` = last(`CONFIRMED UNITS`), .groups = "drop")

#### Save data ####
write_csv(apartment, "data/02-analysis_data/analysis_data.csv")
write_csv(scorechange, "data/02-analysis_data/score_change_data.csv")