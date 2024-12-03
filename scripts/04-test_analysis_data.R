#### Preamble ####
# Purpose: Tests the structure and validity of the cleaned 2022 CES demographic
# analysis dataset.
# Author: Jinyan Wei
# Date: 28 November 2024 
# Contact: jinyan.wei@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse`, `testthat`,`arrow` package must be installed and loaded
# - 02-clean_data.R must have been run

#### Workspace setup ####
library(tidyverse)
library(testthat)
library(arrow)

# Read the cleaned demographic data
cleaned_data <- read_parquet("data/02-analysis_data/cleaned_demographic_data.parquet")

#### Test data completeness and structure ####
test_that("Dataset has no missing values", {
  # Check for NA values across all columns
  expect_false(any(is.na(cleaned_data)), 
               info = "The dataset should not contain any NA values")
  
  # Check for empty strings in character columns
  character_columns <- cleaned_data %>% 
    select_if(is.character) %>% 
    names()
  
  for (col in character_columns) {
    expect_false(any(cleaned_data[[col]] == ""), 
                 info = paste("Column", col, "contains empty strings"))
  }
})

test_that("Dataset has correct structure", {
  # Test the number of rows (assuming 1000 based on simulation)
  expect_equal(nrow(cleaned_data), 1000,
               info = "Dataset should contain exactly 1000 observations")
  
  # Test the number of columns (15 columns as shown in the data)
  expect_equal(ncol(cleaned_data), 15,
               info = "Dataset should contain exactly 15 columns")
  
  # Test column names and order
  expected_colnames <- c(
    "presvote20post", "birthyr", "gender4", "educ", "race", 
    "urbancity", "religpew", "gunown", "edloan",
    "vote_choice", "age", "gender", "education", "race_group", "location"
  )
  expect_equal(colnames(cleaned_data), expected_colnames,
               info = "Column names and order should match expected structure")
})

#### Test data types and value ranges ####
test_that("Columns have correct data types", {
  # Test numeric columns
  numeric_columns <- c("presvote20post", "birthyr", "gender4", "educ", 
                       "race", "urbancity", "religpew", "gunown", "edloan", "age")
  
  for (col in numeric_columns) {
    expect_true(is.numeric(cleaned_data[[col]]),
                info = paste("Column", col, "should be numeric"))
  }
  
  # Test character columns
  character_columns <- c("vote_choice", "gender", "education", 
                         "race_group", "location")
  
  for (col in character_columns) {
    expect_true(is.character(cleaned_data[[col]]),
                info = paste("Column", col, "should be character"))
  }
})

test_that("Values are within expected ranges", {
  # Test vote categories
  expect_true(all(cleaned_data$presvote20post %in% c(1, 2, 6)),
              info = "Presidential vote should only contain values 1, 2, or 6")
  
  expect_true(all(cleaned_data$vote_choice %in% 
                    c("democrat", "republican", "other")),
              info = "Vote choice should only contain 'democrat', 'republican', or 'other'")
  
  # Test birth year and age ranges
  expect_true(all(cleaned_data$birthyr >= 1940 & cleaned_data$birthyr <= 2004),
              info = "Birth years should be between 1940 and 2004")
  
  expect_true(all(cleaned_data$age >= 18),
              info = "All respondents should be at least 18 years old")
  
  # Test education values
  expect_true(all(cleaned_data$educ %in% 1:6),
              info = "Education codes should be between 1 and 6")
  
  valid_education <- c("high school or less", "some college", 
                       "college graduate", "postgraduate")
  expect_true(all(cleaned_data$education %in% valid_education),
              info = "Education categories should match expected values")
})

#### Test logical relationships ####
test_that("Demographic relationships are consistent", {
  # Test age calculation accuracy
  expect_true(all(cleaned_data$age == 2022 - cleaned_data$birthyr),
              info = "Age should be correctly calculated from birth year")
  
  # Test vote choice consistency
  expect_true(all(
    (cleaned_data$presvote20post == 1 & cleaned_data$vote_choice == "democrat") |
      (cleaned_data$presvote20post == 2 & cleaned_data$vote_choice == "republican") |
      (cleaned_data$presvote20post == 6 & cleaned_data$vote_choice == "other")
  ), info = "Vote choice labels should match their numeric codes")
  
  # Test education level consistency
  expect_true(all(
    (cleaned_data$educ <= 2 & cleaned_data$education == "high school or less") |
      (cleaned_data$educ == 3 & cleaned_data$education == "some college") |
      (cleaned_data$educ %in% c(4,5) & cleaned_data$education == "college graduate") |
      (cleaned_data$educ == 6 & cleaned_data$education == "postgraduate")
  ), info = "Education labels should match their numeric codes")
})

#### Test demographic distributions ####
test_that("Demographic distributions are reasonable", {
  # Test gender distribution
  gender_props <- prop.table(table(cleaned_data$gender))
  expect_true(abs(gender_props["male"] - 0.48) < 0.05 &&
                abs(gender_props["female"] - 0.48) < 0.05,
              info = "Gender proportions should be roughly equal with small non-binary proportion")
  
  # Test urban/rural distribution
  location_props <- prop.table(table(cleaned_data$location))
  expect_true(location_props["urban"] >= 0.2 && location_props["suburban"] >= 0.2,
              info = "Urban and suburban populations should each be at least 20%")
  
  # Test education distribution
  expect_true(mean(cleaned_data$educ >= 4) >= 0.2,
              info = "At least 20% should have college degree or higher")
})