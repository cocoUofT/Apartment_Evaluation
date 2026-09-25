#### Preamble ####
# Purpose: Downloads and saves Apartment Building Evaluation data from 2023 to 2025
# from Open Data Toronto
# Author: Kexin Liu
# Date: 24 September 2026
# Contact: ws1nn2lj3@gmail.com
# License: 
# Pre-requisites: 
  # The `tidyverse` package must be installed and loaded
  # The `opendatatoronto` package must be installed and loaded


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)

#### Download data ####

package <- show_package("4ef82789-e038-44ef-a478-a8f3590c3eb1")

resources <- list_package_resources("4ef82789-e038-44ef-a478-a8f3590c3eb1")

datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

raw_data <- filter(datastore_resources, row_number()==1) %>% get_resource()


#### Save data ####

write_csv(raw_data, "data/01-raw_data/raw_data.csv") 
