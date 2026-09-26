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


#### Introductory Graph ####

ggplot(apartment, aes(x = `CONFIRMED UNITS`)) +
  geom_histogram(
    binwidth = 60,
    boundary = 0,
    closed = "left",
    fill = "#89C2D9",
    colour = "#89C2D9") +
  scale_x_continuous(breaks = seq(0, 840, by = 120)) +
  labs(title = "Distribution of apartment building sizes",
       x = "Confirmed units per building", y = "Number of buildings") +
  theme_minimal()


#### Main Graph ####

# Derive the variable building_size. It will only be used for the main graph.

size_breaks <- quantile(apartment[["CONFIRMED UNITS"]],
                        probs = c(0, 0.25, 0.50, 0.75, 1))
apartment$building_size <- cut(apartment[["CONFIRMED UNITS"]], breaks = size_breaks, 
                               include.lowest = TRUE,
                               labels = c("Small", "Medium", "Large", "Very Large"))
service <- apartment |>
  filter(!is.na(building_size), `TENANT SERVICE REQUEST LOG` %in% c(1, 3)) |>
  group_by(building_size) |>
  summarise(buildings = n(), 
            service_percent = mean(`TENANT SERVICE REQUEST LOG` == 1),
            .groups = "drop")

ggplot(service, aes(x = building_size, y = service_percent)) +
  geom_col(fill = "#E8A0BF", width = 0.7) +
  geom_text(aes(label = scales::percent(service_percent, accuracy = 0.1)),
            vjust = -0.5) +
  scale_y_continuous( labels = scales::label_percent(accuracy = 1),
                      limits = c(0, max(service$service_percent) * 1.25),
                      expand = expansion(mult = c(0, 0))) +
  labs(title = "Tenant service request log score 1 by building size",
       x = "Building size", y = "Percentage receiving score 1") +
  theme_minimal()


# Two-panel graph comparing building size with maintenance and pest control score

pest <- apartment |>
  filter(!is.na(building_size),`PEST CONTROL LOG` %in% c(1, 3)) |>
  group_by(building_size) |>
  summarise(buildings = n(), score1_percent = mean(`PEST CONTROL LOG` == 1),
    .groups = "drop") |>
  mutate(log_type = "Pest control log")

maintenance <- apartment |>
  filter(!is.na(building_size), `MAINTENANCE LOG` %in% c(1, 3)) |>
  group_by(building_size) |>
  summarise(buildings = n(), score1_percent = mean(`MAINTENANCE LOG` == 1),
    .groups = "drop") |>
  mutate(log_type = "Maintenance log")

both_logs <- bind_rows(pest, maintenance)

ggplot(both_logs, aes(x = building_size, y = score1_percent)) +
  geom_col(fill = "#E8A0BF", width = 0.7) +
  geom_text( aes(label = scales::percent(score1_percent, accuracy = 0.1)),
             vjust = -0.5) +
  facet_wrap(~log_type, nrow = 1) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0, max(both_logs$score1_percent) * 1.25),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Log score 1 by building size", x = "Building size",
       y = "Percentage receiving score 1") + 
  theme_minimal()


#### Appendix Graph ####

# Build a data frame without observations that are missing the "year built" value.
# It will only be used for analysis relating to building age.

building_age_full <- apartment |>
  filter(!is.na(`YEAR BUILT`))
building_age_full[["BUILDING AGE"]] <-
  building_age_full[["completion_year"]] - building_age_full[["YEAR BUILT"]]




