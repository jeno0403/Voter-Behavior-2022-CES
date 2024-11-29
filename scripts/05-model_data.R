#### Preamble ####
# Purpose: Logistic regression models for vote choice
# Author: [Your Name]
# Date: [Today's Date]
# Contact: [Your Contact Information]
# License: MIT
# Pre-requisites: Install tidyverse, rstanarm, arrow libraries.
# Any other information needed: Models binary outcome (Democrat/Republican).

#### Workspace Setup ####
library(nnet)  # For multinomial logistic regression
library(tidyverse)  # For data manipulation

#### Read and Prepare Data ####
# Load your cleaned dataset
analysis_data <- read_parquet("data/02-analysis_data/cleaned_demographic_data.parquet")

# Prepare data for multinomial logistic regression
analysis_data <- analysis_data %>%
  filter(vote_choice %in% c("democrat", "republican", "other")) %>%
  mutate(
    vote_choice = factor(vote_choice, levels = c("republican", "democrat", "other")),  # Set levels for outcome
    gender = as.factor(gender),  # Ensure categorical variables are factors
    race_group = as.factor(race_group),
    education = as.factor(education),
    urbancity = as.factor(urbancity)  # Include urbancity as a predictor if justified
  )

# Optionally create an interaction term between education and race_group
analysis_data <- analysis_data %>%
  mutate(education_race_interaction = interaction(education, race_group))

#### Fit Multinomial Logistic Regression ####
# Without interaction term
first_model<- multinom(
  vote_choice ~ gender + education + race_group + urbancity + gunown + edloan + age,
  data = analysis_data
)

# With interaction term
second_model <- multinom(
  vote_choice ~ gender + education + race_group + urbancity + gunown + edloan + age +
    education:race_group,
  data = analysis_data
)

#### Summarize Results ####
summary(first_model)  # For the main model
summary(second_model)  # For the interaction model (if used)

#### Export Models ####
saveRDS(first_model, file = "models/first_model.rds")
saveRDS(second_model, file = "models/second_model.rds")

