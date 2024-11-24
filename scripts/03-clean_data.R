#### Preamble ####
# Purpose: Cleans the CES 2022 data and prepares analysis datasets
# Author: [Your name]
# Date: 23 November 2024
# Contact: [Your contact]
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(janitor)
library(arrow)

#### Clean data ####
raw_data <- read_csv(
  "data/01-raw_data/raw_ces_2022.csv",
  show_col_types = FALSE
)

# Create demographic analysis dataset
demographics_analysis <- raw_data |>
  clean_names() |>
  # Select key demographic variables
  select(
    presvote20post, birthyr, gender4, educ, race, 
    urbancity, religpew, gunown, edloan, votereg_post
  ) |>
  # Filter for valid registration info first
  filter(
    !is.na(presvote20post),
    !is.na(votereg_post)
  ) |>
  mutate(
    vote_choice = case_when(
      presvote20post == 1 ~ "democrat",
      presvote20post == 2 ~ "republican",
      presvote20post == 6 ~ "other",
      TRUE ~ "did not vote"
    ),
    registered = votereg_post == "Yes"
  )

# Create directory if needed
dir.create("data/02-analysis_data", recursive = TRUE, showWarnings = FALSE)

#### Save data ####
# Save demographic analysis dataset
write_csv(demographics_analysis, 
          "data/02-analysis_data/analysis_data_demographics.csv")
write_parquet(demographics_analysis, 
              "data/02-analysis_data/analysis_data_demographics.parquet")

# Print summary
cat("\nDemographic Analysis Dataset Summary:\n")
cat("Number of observations:", nrow(demographics_analysis), "\n")
print(summary(demographics_analysis))

