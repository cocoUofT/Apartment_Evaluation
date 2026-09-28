# Tenant-Service Record Keeping in Toronto

Author: Kexin Liu

## Overview

This repository contains the data, R scripts, and paper for a descriptive study of 
tenant-service-log ratings in Toronto apartment buildings. The analysis examines 
how ratings differ by building size, how ratings change among buildings initially 
receiving a low score, and how ward results compare with benchmarks based on their 
mix of building sizes.

- [Read the paper](paper/paper.pdf)
- [Quarto source](paper/paper.qmd)
- [References](paper/references.bib)
- [GitHub repository](https://github.com/cocoUofT/Apartment_Evaluation.git)

## Data

The source is the City of Toronto's [Apartment Building Evaluation dataset](https://open.toronto.ca/dataset/apartment-building-evaluation/), 
specifically the 2023–current resource. The download date is September 24, 2026. 
The saved snapshot contains 6,731 evaluation records for 3,587 buildings, with 
evaluation dates from June 5, 2023, to September 22, 2026.

## Repository structure

| Location | Contents |
|---|---|
| `data/00-simulated_data/` | Simulated building records. |
| `data/01-raw_data/` | Saved source data downloaded from Open Data Toronto. |
| `data/02-analysis_data/` | Latest-evaluation records and earliest/latest evaluation pairs. |
| `scripts/` | Simulation, downloading, preparation, checking, and figure/table scripts. |
| `paper/` | Quarto source, PDF, and BibTeX bibliography. |
| `other/sketches/` | Initial dataset and figure sketches. |
| `other/llm_usage/` | Saved records of AI assistance. |

## Reproduce

Requires R, Quarto, and LaTeX (e.g., TinyTeX). Open `ApartmentLogRating.Rproj` and run from the project root.

Use the saved data. Skip `02-download_data.R`, which replaces the snapshot with current data.

Run in the R console:

```r
install.packages(c("tidyverse", "ggplot2", "opendatatoronto", "testthat", "tinytable", "scales")) # Once

source("scripts/00-simulate_data.R")
testthat::test_file("scripts/01-test_simulated_data.R")
source("scripts/03-clean_data.R")
testthat::test_file("scripts/04-test_analysis_data.R")
source("scripts/05-graph_replications.R", print.eval = TRUE)
source("scripts/06-table_replications.R", print.eval = TRUE)
```

Then run in the terminal:

```sh
quarto render paper/paper.qmd --to pdf
```

## AI assistance

OpenAI ChatGPT assisted with exploring the dataset, developing and revising 
R code, and troubleshooting Quarto rendering. It also assisted with editing the 
paragraph structure, as well as fixing some grammatical mistakes for the paper. 
GPT-6 Sol was the only model used for this study.

The chat history is stored in [other/llm_usage/usage.txt](other/llm_usage/usage.txt).

## Licence

The original code and accompanying code documentation are licensed under the
[MIT License](LICENSE). Copyright (c) 2026 Kexin Liu.

The City of Toronto source data, including the information contained in the cleaned
datasets, are covered by the
[Open Government Licence – Toronto](https://www.toronto.ca/city-government/data-research-maps/open-data/open-data-licence/),
not the MIT License. Contains information licensed under the Open Government
Licence – Toronto.
