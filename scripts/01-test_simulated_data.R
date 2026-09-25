#### Preamble ####
# Purpose: Tests the structure and validity of the simulated Australian 
  #electoral divisions dataset.
# Author: Kexin Liu
# Date: 24 September 2026
# Contact: ws1nn2lj3@gmail.com
# License: 
# Pre-requisites: 
  # - 00-simulate_data.R must have been run
# Any other information needed?


#### Workspace setup ####

test_simulate <- read_csv("data/00-simulated_data/simulated_data.csv")

# Test if the data was successfully loaded

if (exists("test_simulate")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}

#### Test data ####

# Check unique row for each building

if (anyDuplicated(test_simulate$RSN) == 0){
  message("Test Passed: Each building only appears once.")
} else{
  stop("Test Failed")
}

# Check if building sizes are divided approximately evenly

table(test_simulate$building_size)