#### Preamble ####
# Purpose: Tests simulated CES 2022 dataset for structure and validity
# Author: [Your name]
# Date: 23 November 2024
# Contact: [Your contact]
# License: MIT
# Pre-requisites: tidyverse, testthat

#### Workspace setup ####
library(tidyverse)
library(testthat)

# Load the simulated data
simulated_ces_data <- read_csv("data/00-simulated_data/simulated_ces_data.csv")

#### Basic data structure tests ####
test_that("Data has correct structure", {
  expect_true(is.numeric(simulated_ces_data$presvote20post))
  expect_true(is.numeric(simulated_ces_data$birthyr))
  expect_true(is.numeric(simulated_ces_data$gender4))
  expect_true(is.numeric(simulated_ces_data$educ))
  expect_true(is.numeric(simulated_ces_data$race))
  expect_true(is.numeric(simulated_ces_data$urbancity))
  expect_true(is.numeric(simulated_ces_data$religpew))
  expect_true(is.numeric(simulated_ces_data$gunown))
  expect_true(is.numeric(simulated_ces_data$edloan))
})

#### Value range tests ####
test_that("Variables are within valid ranges", {
  # Presidential vote values
  expect_true(all(simulated_ces_data$presvote20post %in% c(1, 2, 6)))
  
  # Birth year range
  expect_true(all(simulated_ces_data$birthyr >= 1940 & 
                    simulated_ces_data$birthyr <= 2004))
  
  # Gender values
  expect_true(all(simulated_ces_data$gender4 %in% 1:3))
  
  # Education levels
  expect_true(all(simulated_ces_data$educ %in% 1:6))
  
  # Race values
  expect_true(all(simulated_ces_data$race %in% 1:7))
  
  # Urban/rural values
  expect_true(all(simulated_ces_data$urbancity %in% 1:4))
  
  # Religious views
  expect_true(all(simulated_ces_data$religpew %in% 1:12))
  
  # Gun ownership
  expect_true(all(simulated_ces_data$gunown %in% 1:3))
  
  # Education loan status
  expect_true(all(simulated_ces_data$edloan %in% 1:2))
})

#### Logical relationship tests ####
test_that("Data has logical relationships", {
  # Test voting age
  expect_true(all(2022 - simulated_ces_data$birthyr >= 18))
  
  # Test education loan patterns
  young_educated <- simulated_ces_data %>%
    filter(birthyr >= 1980, educ >= 4) %>%
    summarise(loan_rate = mean(edloan == 1)) %>%
    pull(loan_rate)
  expect_true(young_educated > 0.3)
})

#### Distribution tests ####
test_that("Distributions are reasonable", {
  # Gender distribution roughly equal except for non-binary
  gender_props <- prop.table(table(simulated_ces_data$gender4))
  expect_true(abs(gender_props[1] - 0.48) < 0.1)
  expect_true(abs(gender_props[2] - 0.48) < 0.1)
  
  # Urban/suburban/rural distribution
  urban_props <- prop.table(table(simulated_ces_data$urbancity))
  expect_true(urban_props[2] > urban_props[1]) # More suburban than urban
})

#### Missing value tests ####
test_that("No missing values in required fields", {
  expect_true(all(!is.na(simulated_ces_data)))
})

