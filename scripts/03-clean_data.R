#### Preamble ####
# Purpose: Clean demographic data and create informative visualizations
# Author: [Your Name]
# Date: 27 November 2024
# License: MIT


# Load required libraries
library(tidyverse)
library(janitor)

# Create the cleaning function
clean_demographic_data <- function(data) {
  # Read the data if it's a file path
  if (is.character(data)) {
    data <- read_csv(data)
  }
  
  # Clean and transform the data
  cleaned_data <- data %>%
    # First ensure we have clean column names
    clean_names() %>%
    # Transform the data to match desired format
    mutate(
      # Calculate age from birth year
      age = 2022 - birthyr,
      
      # Convert vote choice codes to text
      vote_choice = case_when(
        presvote20post == 1 ~ "democrat",
        presvote20post == 2 ~ "republican",
        presvote20post == 6 ~ "other"
      ),
      
      # Convert gender codes to text
      gender = case_when(
        gender4 == 1 ~ "male",
        gender4 == 2 ~ "female",
        gender4 == 3 ~ "non-binary"
      ),
      
      # Convert education codes to text
      education = case_when(
        educ %in% c(1, 2) ~ "high school or less",
        educ == 3 ~ "some college",
        educ %in% c(4, 5) ~ "college graduate",
        educ == 6 ~ "postgraduate"
      ),
      
      # Convert race codes to text
      race_group = case_when(
        race == 1 ~ "white",
        race == 2 ~ "black",
        race == 3 ~ "hispanic",
        race == 4 ~ "asian",
        race == 5 ~ "native american",
        race == 6 ~ "middle eastern",
        race == 7 ~ "other"
      ),
      
      # Convert location codes to text
      urbancity = case_when(
        urbancity == 1 ~ "urban",
        urbancity == 2 ~ "suburban",
        urbancity == 3 ~ "rural",
        urbancity == 4 ~ "other"
      )
    ) %>%
    # Select and arrange columns in the exact order shown
    select(
      # Original coded columns first
      presvote20post, religpew, gunown, edloan,
      vote_choice, age, gender, education, race_group, urbancity
    )
  
  return(cleaned_data)
}

# Example usage:
# Read the raw data
raw_data <- read_csv("data/00-simulated_data/simulated_ces_data.csv")

# Clean the data
cleaned_data <- clean_demographic_data(raw_data)

# Save the cleaned data
write_csv(cleaned_data, "data/02-analysis_data/cleaned_demographic_data.csv")
write_parquet(cleaned_data, "data/02-analysis_data/cleaned_demographic_data.parquet")


print(head(cleaned_data))

# Verify column names and order
cat("\nColumn names in order:\n")
cat(paste(colnames(cleaned_data), collapse = ", "))

