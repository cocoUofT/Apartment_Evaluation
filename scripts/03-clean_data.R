#### Preamble ####
# Purpose: Cleans the raw Apartment Building Evaluation data from 2025 to present
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
  mutate(completion_year = as.integer(
    format(as.Date(raw_data[["EVALUATION COMPLETED ON"]]), "%Y")
  ))

sorted <- variable_interest[order(variable_interest$RSN, 
                                  -variable_interest[["completion_year"]]), ]
apartment <- sorted[!duplicated(sorted$RSN), ]
row.names(apartment) <- NULL

# Check for missing value in all columns

colSums(is.na(apartment))


#### Save data ####
write_csv(apartment, "data/02-analysis_data/analysis_data.csv")
