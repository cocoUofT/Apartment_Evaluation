#### Preamble ####
# Purpose: Cleans the raw Apartment Building Evaluation dataset to a ready for
# analysis dataset
# Author: Kexin Liu
# Date: 24 September 2026
# Contact: ws1nn2lj3@gmail.com
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed


#### Workspace setup ####

library(tidyverse)
raw_data <- read_csv("data/01-raw_data/raw_data.csv")


#### Clean data ####

variable_interest <- raw_data |>
  select("_id", "RSN", "EVALUATION COMPLETED ON", "CONFIRMED UNITS",
         "TENANT SERVICE REQUEST LOG", "WARDNAME") |>
  mutate(completion_date = as.Date(`EVALUATION COMPLETED ON`))

sorted <- variable_interest[order(variable_interest$RSN, 
                                  -as.numeric(variable_interest$completion_date),
                                  -variable_interest[["_id"]]), ]
apartment <- sorted[!duplicated(sorted$RSN), ]
row.names(apartment) <- NULL

colSums(is.na(apartment)) ## Check for missing value in all columns


#### Additional dataframe for evaluating score change only ####

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