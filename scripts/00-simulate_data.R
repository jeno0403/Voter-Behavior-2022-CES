#### Preamble ####
# Purpose: Simulates data for CES 2022 analysis about voting patterns and demographics
# Author: Jinyan Wei
# Date: 23 November 2024
# Contact: jinyan.wei@mail.utoronto.ca
# License: MIT
# Pre-requisites:The `tidyverse` package must be installed
# Any other information needed? Make sure you are in the `ces_2022_demographic_analysisRproj`rproj

#### Workspace setup ####
library(tidyverse)
library(janitor)

#### Define value labels ####
# Create lookup tables for variable meanings
vote_labels <- c("Democrat" = 1, "Republican" = 2, "Other" = 6)
gender_labels <- c("Male" = 1, "Female" = 2, "Other" = 3)
educ_labels <- c("No HS" = 1, "HS Grad" = 2, "Some College" = 3, 
                 "2-Year" = 4, "4-Year" = 5, "Post-Grad" = 6)
race_labels <- c("White" = 1, "Black" = 2, "Hispanic" = 3, "Asian" = 4,
                 "Native American" = 5, "Middle Eastern" = 6, "Other" = 7)
urban_labels <- c("Urban" = 1, "Suburban" = 2, "Rural" = 3, "Other" = 4)

#### Simulate data ####
set.seed(2022)

n_samples <- 1000

simulated_ces_data <- tibble(
  # Presidential vote with realistic proportions
  presvote20post = sample(c(1, 2, 6), n_samples, replace = TRUE, 
                          prob = c(0.46, 0.44, 0.10)),
  
  # Birth year considering voting age
  birthyr = sample(1940:2004, n_samples, replace = TRUE, 
                   prob = dnorm(1940:2004, mean = 1975, sd = 15)),
  
  # Gender with realistic proportions
  gender4 = sample(1:3, n_samples, replace = TRUE, 
                   prob = c(0.48, 0.48, 0.04)),
  
  # Education with age-appropriate constraints
  educ = pmin(6, pmax(1, round(rnorm(n_samples, mean = 3.5, sd = 1.2)))),
  
  # Race with census-like proportions
  race = sample(1:7, n_samples, replace = TRUE,
                prob = c(0.57, 0.14, 0.19, 0.06, 0.01, 0.01, 0.02)),
  
  # Urban/rural split
  urbancity = sample(1:4, n_samples, replace = TRUE,
                     prob = c(0.31, 0.45, 0.20, 0.04)),
  
  # Religious views
  religpew = sample(1:12, n_samples, replace = TRUE),
  
  # Gun ownership
  gunown = sample(1:3, n_samples, replace = TRUE,
                  prob = c(0.32, 0.48, 0.20)),
  
  # Education loans considering age and education
  edloan = sample(1:2, n_samples, replace = TRUE,
                  prob = c(0.35, 0.65))
)

# Add logical constraints
simulated_ces_data <- simulated_ces_data %>%
  # Ensure voting age
  mutate(birthyr = if_else(2022 - birthyr < 18, 
                           birthyr - sample(20:40, n(), replace = TRUE),
                           birthyr)) %>%
  # More likely to have education loans with higher education
  mutate(edloan = if_else(educ >= 4 & birthyr >= 1970, 
                          sample(1:2, n(), replace = TRUE, prob = c(0.6, 0.4)),
                          edloan))

# Save the simulated data
write_csv(simulated_ces_data, "data/00-simulated_data/simulated_ces_data.csv")

