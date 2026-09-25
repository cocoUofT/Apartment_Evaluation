#### Preamble ####
# Purpose: Replicated graphs from the paper on Apartment Evaluation
# Author: Kexin Liu
# Date: 25 September 2026
# Contact: ws1nn2lj3@gmail.com
# License: 
# Pre-requisites: 
  # The `tidyverse` package must be installed
  # The 'ggplot2' package must be installed
# Any other information needed? 


#### Workspace setup ####

library(tidyverse)
library(ggplot2)
apartment <- read_csv("data/02-analysis_data/analysis_data.csv")

#### Main Graph ####

# Derive the variable building_size. It will only be used for the main graph.

size_breaks <- quantile(apartment[["CONFIRMED UNITS"]],
                        probs = c(0, 0.25, 0.50, 0.75, 1))
apartment$building_size <- cut(apartment[["CONFIRMED UNITS"]], breaks = size_breaks, 
                               include.lowest = TRUE,
                               labels = c("Small", "Medium", "Large", "Very Large"))


score1_percent <- apartment |>
  filter(
    !is.na(building_size),
    `TENANT SERVICE REQUEST LOG` %in% c(1, 3)
  ) |>
  group_by(building_size) |>
  summarise(
    buildings = n(),
    percent_score1 = mean(`TENANT SERVICE REQUEST LOG` == 1),
    .groups = "drop"
  )

ggplot(score1_percent,
       aes(x = building_size, y = percent_score1)) +
  geom_col(fill = "#B85C50", width = 0.7) +
  geom_text(
    aes(label = scales::percent(percent_score1, accuracy = 0.1)),
    vjust = -0.5
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, max(score1_percent$percent_score1) * 1.25),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Tenant service request log score 1 by building size",
    x = "Building size",
    y = "Percentage receiving score 1",
    caption = "Percentages are among buildings rated 1 or 3; scores 0 and 2 are excluded."
  ) +
  theme_minimal()






# Build a data frame without observations that are missing the "year built" value.
# It will only be used for analysis relating to building age.

building_age_full <- apartment |>
  filter(!is.na(`YEAR BUILT`))
building_age_full[["BUILDING AGE"]] <-
  building_age_full[["completion_year"]] - building_age_full[["YEAR BUILT"]]




