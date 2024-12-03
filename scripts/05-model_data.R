#### Preamble ####
# Purpose: Create a Logistic regression models and Tree model for vote choice
# Author: Jinyan Wei
# Date: 28 November 2024 
# Contact: jinyan.wei@mail.utoronto.ca;
# License: MIT
# Pre-requisites: Install `arrow`,`dplyr` ,`rpart` package must be installed and loaded.


library(dplyr)
library(arrow)
library(rpart)


first_model <- glm(
  vote_choice ~ age_cohort + gender + education + income_tier + 
    religion + race + urbanicity + region,
  data = analysis_data,
  family = binomial(link = "logit")
)

second_model <- rpart(
  vote_choice ~ age_cohort + gender + education + income_tier + 
    religion + race + urbanicity + region,
  data = analysis_data,
  method = "class"  # Classification tree
)

saveRDS(first_model, "models/first_model.rds")
saveRDS(second_model, "models/second_model.rds")

