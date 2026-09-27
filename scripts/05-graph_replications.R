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
scorechange <- read_csv("data/02-analysis_data/score_change_data.csv")

#### Introductory Graphs ####

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


#### Data Section Graphs ####

# Derive the variable building_size. The building size break will be used universally
# in the data section.
# Create a barplot for building size and proportion of score 1 in service log.

size_breaks <- c(9, 19, 49, 99, Inf)
size_labels <- c("10–19", "20–49", "50–99", "100+")

apartment$building_size <- cut(
  apartment[["CONFIRMED UNITS"]],
  breaks = size_breaks,
  labels = size_labels
)

service <- apartment |>
  filter(`TENANT SERVICE REQUEST LOG` %in% c(1, 3)) |>
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


# Stacked barplot containing all scores

stacked_data <- apartment |>
  filter(`TENANT SERVICE REQUEST LOG` %in% 0:3) |>
  mutate(log_score = factor(`TENANT SERVICE REQUEST LOG`, 
                            levels = c(1, 0, 2, 3)))
stacked_data$building_size <- cut(
  stacked_data[["CONFIRMED UNITS"]],
  breaks = size_breaks,
  labels = size_labels
)
size_totals <- stacked_data |> count(building_size)

ggplot(stacked_data, aes(x = building_size, fill = log_score)) +
  geom_bar(width = 0.72, position = position_stack(reverse = TRUE)) +
  geom_text(data = size_totals, 
            aes(x = building_size, y = n, label = scales::comma(n)),
            inherit.aes = FALSE, vjust = -0.5) +
  scale_fill_manual(
    name = "Tenant service request log score",
    values = c(
      "0" = "#71808A",
      "1" = "#B85C50",
      "2" = "#C5A654",
      "3" = "#A8B99D"),
    breaks = c("0", "1", "2", "3"),
    labels = c("0: not specified", "1: not completed/updated", "2: N/A", 
               "3: completed/full")) +
  scale_y_continuous(labels = scales::label_comma(), 
                     expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Building size (confirmed units)", y = "Number of buildings") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))


# Create a barplot for building size and proportion of improvement in service log

scorechange$building_size <- cut(scorechange[["CONFIRMED UNITS"]],
                                 breaks = size_breaks, labels = size_labels)
score1_change <- scorechange |>
  filter(first_score == 1, latest_score %in% c(1, 3))

size_change_summary <- score1_change |>
  group_by(building_size) |>
  summarise(total = n(), changed_to_3 = sum(latest_score == 3),
            stayed_at_1 = sum(latest_score == 1),
            percent_stayed_1 = stayed_at_1 / total, 
            percent_improved = changed_to_3 / total, .groups = "drop")

ggplot(size_change_summary, aes(x = building_size, y = percent_stayed_1)) +
  geom_col(fill = "#B7C4A8", width = 0.7) +
  geom_text(aes(label = scales::percent(percent_stayed_1, accuracy = 0.1)),
            vjust = 1.5, colour = "#B7C4A8") +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 0.5),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Tenant service log scores remaining at 1 by building size",
       x = "Building size",
       y = "Percentage remaining at score 1") +
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
                 color = "Observed rate"), size = 3) +
  scale_shape_manual(name = NULL, values = c("Observed rate" = 16, 
                                             "Size-mix benchmark" = 1)) +
  scale_color_manual(name = NULL, values = c("Observed rate" = "#A94C41",
                                             "Size-mix benchmark" = "#4F6874")) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0, 0.15), breaks = seq(0, 0.14, 0.02)) +
  labs(x = "Percentage of buildings receiving tenant-log score 1", y = NULL) +
  theme_minimal() +
  theme(legend.position = "top")


#### Appendix Graph ####

# Derive the variable building_age. It will only be used for the main graph.
# Create a barplot for building age and proportion of score 1 in service log

df.age <- apartment |>
  filter(!is.na(`YEAR BUILT`))
df.age$building_age <- df.age[["YEAR EVALUATED"]] - df.age[["YEAR BUILT"]]

df.age$age_group <- cut(df.age$building_age,
                        breaks = c(-1, 19, 39, 59, 79, 99, Inf),
                        labels = c("0–19", "20–39", "40–59",
                                   "60–79", "80–99", "100+"))
age_score1 <- df.age |>
  filter(!is.na(age_group), `TENANT SERVICE REQUEST LOG` %in% c(1, 3)) |>
  group_by(age_group) |>
  summarise(buildings = n(), 
            score1_percent = mean(`TENANT SERVICE REQUEST LOG` == 1),
            .groups = "drop")
ggplot(age_score1, aes(x = age_group, y = score1_percent)) +
  geom_col(fill = "#B7C4A8", width = 0.7) +
  geom_text(aes(label = scales::percent(score1_percent, accuracy = 0.1)),
            vjust = -0.5) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0, max(age_score1$score1_percent) * 1.25),
                     expand = expansion(mult = c(0, 0))) +
  labs(title = "Tenant service request log score 1 by building age",
       x = "Building age at evaluation (years)",
       y = "Percentage receiving score 1") +
  theme_minimal()