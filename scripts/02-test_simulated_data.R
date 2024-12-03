#### Preamble ####
# Purpose: Tests the structure and validity of the 2022 CES Voting pattern 
# simulated dataset.
# Author: Jinyan Wei
# Date: 28 November 2024 
# Contact: jinyan.wei@mail.utoronto.ca;
# License: MIT
# Pre-requisites: 
# - The `tidyverse`, `testthat`,`janitor` package must be installed and loaded
# Any other information needed? Make sure you are in the `ces_2022_demographic_analysis`rproj

library(testthat)
library(tidyverse)
library(janitor)

# Load the simulated data
simulated_data <- read_csv("data/00-simulated_data/simulated_ces_data.csv")

# Test basic data structure and integrity
test_that("Data structure meets basic requirements", {
  # Check if we have the expected number of rows
  expect_equal(nrow(simulated_data), 1000, 
               info = "Dataset should contain exactly 1000 observations")
  
  # Check if all required columns are present
  expected_columns <- c("presvote20post", "birthyr", "gender4", "educ", 
                        "race", "urbancity", "religpew", "gunown", "edloan")
  expect_true(all(expected_columns %in% names(simulated_data)),
              info = "All required columns should be present")
  
  # Check for missing values
  expect_true(all(!is.na(simulated_data)),
              info = "Dataset should not contain any missing values")
})

# Test value ranges and distributions
test_that("Variables are within expected ranges", {
  # Presidential vote should be 1, 2, or 6
  expect_true(all(simulated_data$presvote20post %in% c(1, 2, 6)),
              info = "Presidential vote should only contain values 1, 2, or 6")
  
  # Birth year should be between 1940 and 2004
  expect_true(all(simulated_data$birthyr >= 1940 & simulated_data$birthyr <= 2004),
              info = "Birth years should be between 1940 and 2004")
  
  # Gender should be 1, 2, or 3
  expect_true(all(simulated_data$gender4 %in% 1:3),
              info = "Gender should only contain values 1, 2, or 3")
  
  # Education should be 1-6
  expect_true(all(simulated_data$educ %in% 1:6),
              info = "Education should only contain values 1 through 6")
  
  # Race should be 1-7
  expect_true(all(simulated_data$race %in% 1:7),
              info = "Race should only contain values 1 through 7")
  
  # Urban/rural should be 1-4
  expect_true(all(simulated_data$urbancity %in% 1:4),
              info = "Urban/rural should only contain values 1 through 4")
})

# Test logical constraints
test_that("Logical constraints are maintained", {
  # Test voting age constraint
  election_year <- 2022
  expect_true(all(election_year - simulated_data$birthyr >= 18),
              info = "All respondents should be at least 18 years old in 2022")
  
  # Test education loan relationships
  high_educ_recent <- simulated_data %>%
    filter(educ >= 4, birthyr >= 1970)
  
  # Calculate proportion with loans in this group
  loan_prop <- mean(high_educ_recent$edloan == 1)
  
  # Should be roughly 60% (allowing for some random variation)
  expect_true(abs(loan_prop - 0.6) < 0.1,
              info = "Recent graduates with higher education should have ~60% loan rate")
})

# Test distribution properties
test_that("Variable distributions match expected proportions", {
  # Test presidential vote distribution
  vote_props <- prop.table(table(simulated_data$presvote20post))
  expect_true(abs(vote_props[1] - 0.46) < 0.05 &&
                abs(vote_props[2] - 0.44) < 0.05 &&
                abs(vote_props[3] - 0.10) < 0.05,
              info = "Presidential vote proportions should roughly match expected values")
  
  # Test gender distribution
  gender_props <- prop.table(table(simulated_data$gender4))
  expect_true(abs(gender_props[1] - 0.48) < 0.05 &&
                abs(gender_props[2] - 0.48) < 0.05 &&
                abs(gender_props[3] - 0.04) < 0.05,
              info = "Gender proportions should roughly match expected values")
  
  # Test urban/rural distribution
  urban_props <- prop.table(table(simulated_data$urbancity))
  expect_true(abs(urban_props[1] - 0.31) < 0.05 &&
                abs(urban_props[2] - 0.45) < 0.05 &&
                abs(urban_props[3] - 0.20) < 0.05 &&
                abs(urban_props[4] - 0.04) < 0.05,
              info = "Urban/rural proportions should roughly match expected values")
})

# Test data type consistency
test_that("Variables have correct data types", {
  expect_true(is.numeric(simulated_data$presvote20post),
              info = "Presidential vote should be numeric")
  expect_true(is.numeric(simulated_data$birthyr),
              info = "Birth year should be numeric")
  expect_true(is.numeric(simulated_data$gender4),
              info = "Gender should be numeric")
  expect_true(is.numeric(simulated_data$educ),
              info = "Education should be numeric")
})