#### Preamble ####
# Purpose: Create a Bayesian logistic regression models for vote choice
# Author: Jinyan Wei
# Date: 28 November 2024 
# Contact: jinyan.wei@mail.utoronto.ca;
# License: MIT
# Pre-requisites: Install `arrow`,`dplyr` package must be installed and loaded.


library(dplyr)
library(arrow)

analysis_data <- analysis_data %>%
  mutate(
    age_cohort = factor(cut(age, breaks=c(18,29,49,64,90))),
    vote_choice = factor(vote_choice, levels = c("republican", "democrat")),
    region = case_when(
      state %in% c("Maine", "Vermont", "New Hampshire", "Massachusetts", 
                   "Rhode Island", "Connecticut", "New York", "New Jersey", 
                   "Pennsylvania") ~ "Northeast",
      state %in% c("Wisconsin", "Michigan", "Illinois", "Indiana", "Ohio",
                   "Minnesota", "Iowa", "Missouri", "North Dakota", 
                   "South Dakota", "Nebraska", "Kansas") ~ "Midwest",
      state %in% c("Delaware", "Maryland", "Virginia", "West Virginia",
                   "North Carolina", "South Carolina", "Georgia", "Florida",
                   "Kentucky", "Tennessee", "Mississippi", "Alabama", 
                   "Arkansas", "Louisiana") ~ "South",
      TRUE ~ "West"
    ),
    region = factor(region)
  )

first_model <- glm(
  vote_choice ~ age_cohort + gender + education + income_tier + 
    religion + race + urbanicity + region,
  data = analysis_data,
  family = binomial(link = "logit")
)

saveRDS(first_model, "models/first_model.rds")

