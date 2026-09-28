#### Preamble ####
# Purpose: Replicated graphs from the paper on Apartment Evaluation study
# Author: Kexin Liu
# Date: 25 September 2026
# Contact: ws1nn2lj3@gmail.com
# License: MIT
# Pre-requisites: 
  # The `tidyverse` package must be installed
  # The 'ggplot2' package must be installed
  # The 'analysis_data.csv' is saved and ready to be read
  # The 'score_change_data.csv' is saved and ready to be read


#### Workspace setup ####

library(tidyverse)
library(ggplot2)
apartment <- read_csv("data/02-analysis_data/analysis_data.csv")
scorechange <- read_csv("data/02-analysis_data/score_change_data.csv")

#### Introductory Graphs ####

ggplot(apartment, aes(x = `CONFIRMED UNITS`)) +
  geom_histogram(binwidth = 60, boundary = 0, closed = "left", fill = "grey",
                 colour = "grey") +
  scale_x_continuous(breaks = seq(0, 840, by = 120)) +
  labs(x = "Confirmed Units Per Building", y = "Number of Buildings") +
  theme_minimal()


#### Data Section Graphs ####

# Derive the variable building_size. The building size break will be used universally
# in the data section.

# Create a barplot for building size and proportion of score 1 in service log.

size_breaks <- c(9, 19, 49, 99, Inf)
size_labels <- c("10–19", "20–49", "50–99", "100+")

apartment$building_size <- cut(
  apartment[["CONFIRMED UNITS"]],
  breaks = size_breaks,
  labels = size_labels)

service <- apartment |>
  filter(`TENANT SERVICE REQUEST LOG` %in% c(1, 3)) |>
  group_by(building_size) |>
  summarise(buildings = n(), 
            service_percent = mean(`TENANT SERVICE REQUEST LOG` == 1),
            .groups = "drop")

ggplot(service, aes(x = building_size, y = service_percent)) +
  geom_col(fill = "grey", width = 0.7) +
  geom_text(aes(label = scales::percent(service_percent, accuracy = 0.1)),
            vjust = -0.5) +
  scale_y_continuous( labels = scales::label_percent(accuracy = 1),
                      limits = c(0, max(service$service_percent) * 1.25),
                      expand = expansion(mult = c(0, 0))) +
  labs(x = "Building Size", y = "Percentage Receiving Score 1") +
  theme_minimal()

# Stacked barplot containing all scores

stacked_data <- apartment |>
  filter(`TENANT SERVICE REQUEST LOG` %in% 0:3) |>
  mutate(log_score = factor(`TENANT SERVICE REQUEST LOG`, 
                            levels = c(1, 0, 2, 3)))

stacked_data$building_size <- cut(
  stacked_data[["CONFIRMED UNITS"]],
  breaks = size_breaks,
  labels = size_labels)

size_totals <- stacked_data |> count(building_size)

ggplot(stacked_data, aes(x = building_size, fill = log_score)) +
  geom_bar(width = 0.72, position = position_stack(reverse = TRUE)) +
  geom_text(data = size_totals, 
            aes(x = building_size, y = n, label = scales::comma(n)),
            inherit.aes = FALSE, vjust = -0.5) +
  scale_fill_manual(name = "Score: ",
                    values = c("0" = "#BC5085", "1" = "grey", "2" = "#FDE2A7",
                               "3" = "#BDE0FE"), breaks = c("0", "1", "2", "3"),
                    labels = c("0: Undefined", "1: Not Completed / Outdated", 
                               "2: N/A", "3: Completed")) +
  scale_y_continuous(labels = scales::label_comma(), 
                     expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Building Size (Number of Units)", y = "Number of Buildings") +
  theme_minimal() +
  theme(legend.position = "right", legend.title = element_text(size = 9),
        legend.text = element_text(size = 6),
        legend.key.size = grid::unit(0.5, "cm")) +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE))


# Create a barplot for building size and proportion of improvement in service log

scorechange$building_size <- cut(scorechange[["CONFIRMED UNITS"]],
                                 breaks = size_breaks, labels = size_labels)
score1_change <- scorechange |>
  filter(first_date < latest_date, first_score == 1, latest_score %in% c(1, 3))

size_change_summary <- score1_change |>
  group_by(building_size) |>
  summarise(total = n(), stayed_at_1 = sum(latest_score == 1),
            percent_stayed_1 = stayed_at_1 / total, .groups = "drop")

ggplot(size_change_summary, aes(x = building_size, y = percent_stayed_1)) +
  geom_col(fill = "grey", width = 0.7) +
  geom_text(aes(label = scales::percent(percent_stayed_1, accuracy = 0.1)),
            vjust = -1, colour = "black") +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 0.32),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = "Building Size (Number of Units)",
       y = "Percentage Remaining at Score 1") +
  theme_minimal()

# Toronto city wise segment plot

ward_data <- apartment |>
  filter(`TENANT SERVICE REQUEST LOG` %in% c(1, 3)) |>
  mutate(size_band = cut(`CONFIRMED UNITS`, breaks = size_breaks,
                         labels = size_labels),
         score1 = `TENANT SERVICE REQUEST LOG` == 1)

city_rates <- ward_data |>
  group_by(size_band) |>
  summarise(city_rate = mean(score1), .groups = "drop")

ward_summary <- ward_data |>
  left_join(city_rates, by = "size_band") |>
  group_by(WARDNAME) |>
  summarise(buildings = n(), observed = mean(score1), 
            benchmark = mean(city_rate), .groups = "drop") |>
  filter(buildings >= 50) |>
  mutate(gap = observed - benchmark) |>
  arrange(gap) |>
  mutate(ward_label = paste0(WARDNAME, " (n=", buildings, ")"),
         ward_label = factor(ward_label, levels = ward_label))

ggplot(ward_summary, aes(y = ward_label)) +
  geom_segment(aes(x = benchmark, xend = observed, yend = ward_label),
               color = "grey70", linewidth = 0.9) +
  geom_point(aes(x = benchmark, shape = "Size-mix benchmark",
                 color = "Size-mix benchmark"), size = 3, stroke = 1.2) +
  geom_point(aes(x = observed, shape = "Observed rate", 
                 color = "Observed rate"), size = 2) +
  scale_shape_manual(name = NULL, values = c("Observed rate" = 16, 
                                             "Size-mix benchmark" = 1)) +
  scale_color_manual(name = NULL, values = c("Observed rate" = "#A94C41",
                                             "Size-mix benchmark" = "#4F6874")) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0, 0.15), breaks = seq(0, 0.14, 0.02)) +
  labs(x = "Percentage of Buildings Receiving Score 1", y = NULL) +
  theme_minimal() +
  theme(legend.position = "top")