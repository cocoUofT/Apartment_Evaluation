#### Preamble ####
# Purpose: 
# Author: Kexin Liu
# Date: 24 September 2026
# Contact: ws1nn2lj3@gmail.com
# License: 
# Pre-requisites: 
  # The `tidyverse` package must be installed
# Any other information needed? 


#### Workspace setup ####

library(tidyverse)
apartment <- read_csv("data/02-analysis_data/analysis_data.csv")

#### Test data ####

# Check for unique row for each building
if (anyDuplicated(apartment$RSN) == 0){
  message("Test Passed: Each building only appears once.")
} else{
  stop("Test Failed")
}

# Check if all completion_year matches the year of "EVALUATION COMPLETED ON"
if (all(apartment[["completion_year"]] == 
        as.integer(format(as.Date(apartment[["EVALUATION COMPLETED ON"]]), 
                          "%Y"))) == TRUE){
  message("Test Passed: completion_year created accurately")
} else{
  stop("Test Failed")
}
