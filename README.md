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

## AI assistance

OpenAI ChatGPT assisted with exploring the dataset, developing and revising 
R code, and troubleshooting Quarto rendering. It also assisted with editing the 
paragraph structure, as well as fixing some grammatical mistakes for the paper. 
GPT-6 Sol was the only model used for this study.

The chat history is stored in [other/llm_usage/usage.txt](other/llm_usage/usage.txt). 