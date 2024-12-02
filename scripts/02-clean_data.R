#### Preamble ####
# Purpose: Cleans the raw 2022 CES data
# Author: Jinyan Wei
# Date: 28 November 2024
# Contact: jinyan.wei@mail.utoronto.ca
# License: MIT
# Pre-requisites:  
# - The `tidyverse`, `arrow` packages must be installed and loaded

#### Workspace setup ####
library(tidyverse)
library(arrow)


#### Download data ####
# Note: First manually download "CES22_Common_OUTPUT_vv.csv" from:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PR4L8P

data <-
  read_csv(
    "data/01-raw_data/CES22_Common_OUTPUT_vv.csv",
    show_col_types = FALSE
  )
cleaned_data <- raw_data %>%
  filter(
    votereg == 1, # Registered voters only
    presvote20post %in% c(1, 2),
    pid7 %in% 1:7,
    gender4 %in% c(1, 2),
    urbancity %in% c(1, 4),
    faminc_new %in% c(1, 16)
  ) %>%
  
  # Step 2: Create or clean variables
  mutate(
    # Calculate age
    age = 2022 - birthyr,
    
    # Convert vote choice codes to meaningful text
    vote_choice = case_when(
      presvote20post == 1 ~ "democrat",
      presvote20post == 2 ~ "republican",
      TRUE ~ NA_character_
    ),
    
    # Categorize family income into tiers for visualization
    income_tier = case_when(
      faminc_new %in% c(1, 2, 3) ~ "Lower income",
      faminc_new %in% c(4, 5, 6) ~ "Lower-middle income",
      faminc_new %in% c(7, 8, 9) ~ "Middle income",
      faminc_new %in% c(10, 11, 12) ~ "Upper-middle income",
      faminc_new %in% c(13, 14, 15, 16) ~ "Upper income",
      TRUE ~ NA_character_
    ),
    
    # Clean race/ethnicity
    race = case_when(
      race == 1 ~ "White",
      race == 2 ~ "Black",
      race == 3 ~ "Hispanic",
      race == 4 ~ "Asian",
      race == 5 ~ "Native American",
      race == 6 ~ "Middle Eastern",
      race == 7 ~ "Other",
      TRUE ~ NA_character_
    ),
    
    # Clean urbanicity
    urbanicity = case_when(
      urbancity == 1 ~ "Urban",
      urbancity == 2 ~ "Suburban",
      urbancity == 3 ~ "Town",
      urbancity == 4 ~ "Rural"
    ),
    
    # Clean education level
    education = factor(
      case_when(
        educ == 1 ~ "No High School",
        educ == 2 ~ "High School Graduate",
        educ == 3 ~ "Some College",
        educ == 4 ~ "2-year College Degree",
        educ == 5 ~ "4-year College Degree",
        educ == 6 ~ "Postgraduate Degree",
        TRUE ~ NA_character_
      ),
      levels = c(
        "No High School", "High School Graduate", "Some College",
        "2-year College Degree", "4-year College Degree", "Postgraduate Degree"
      )
    ),
    
    # Clean gender
    gender = case_when(
      gender4 == 1 ~ "Male",
      gender4 == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    
    # Clean religious affiliation
    religion = case_when(
      religpew == 1 ~ "Protestant",
      religpew == 2 ~ "Roman Catholic",
      religpew == 3 ~ "Mormon",
      religpew == 4 ~ "Eastern or Greek Orthodox",
      religpew == 5 ~ "Jewish",
      religpew == 6 ~ "Muslim",
      religpew == 7 ~ "Buddhist",
      religpew == 8 ~ "Hindu",
      religpew == 9 ~ "Atheist",
      religpew == 10 ~ "Agnostic",
      religpew == 11 ~ "Nothing in particular",
      religpew == 12 ~ "Something else",
      TRUE ~ NA_character_
    ),
    
    # Clean state of residence
    # Clean state of residence
    state = case_when(
      inputstate == 1 ~ "Alabama",
      inputstate == 2 ~ "Alaska",
      inputstate == 3 ~ "Arizona",
      inputstate == 4 ~ "Arkansas",
      inputstate == 5 ~ "California",
      inputstate == 6 ~ "Colorado",
      inputstate == 7 ~ "Connecticut",
      inputstate == 8 ~ "Delaware",
      inputstate == 9 ~ "District of Columbia",
      inputstate == 10 ~ "Florida",
      inputstate == 11 ~ "Georgia",
      inputstate == 12 ~ "Hawaii",
      inputstate == 13 ~ "Idaho",
      inputstate == 14 ~ "Illinois",
      inputstate == 15 ~ "Indiana",
      inputstate == 16 ~ "Iowa",
      inputstate == 17 ~ "Kansas",
      inputstate == 18 ~ "Kentucky",
      inputstate == 19 ~ "Louisiana",
      inputstate == 20 ~ "Maine",
      inputstate == 21 ~ "Maryland",
      inputstate == 22 ~ "Massachusetts",
      inputstate == 23 ~ "Michigan",
      inputstate == 24 ~ "Minnesota",
      inputstate == 25 ~ "Mississippi",
      inputstate == 26 ~ "Missouri",
      inputstate == 27 ~ "Montana",
      inputstate == 28 ~ "Nebraska",
      inputstate == 29 ~ "Nevada",
      inputstate == 30 ~ "New Hampshire",
      inputstate == 31 ~ "New Jersey",
      inputstate == 32 ~ "New Mexico",
      inputstate == 33 ~ "New York",
      inputstate == 34 ~ "North Carolina",
      inputstate == 35 ~ "North Dakota",
      inputstate == 36 ~ "Ohio",
      inputstate == 37 ~ "Oklahoma",
      inputstate == 38 ~ "Oregon",
      inputstate == 39 ~ "Pennsylvania",
      inputstate == 40 ~ "Rhode Island",
      inputstate == 41 ~ "South Carolina",
      inputstate == 42 ~ "South Dakota",
      inputstate == 43 ~ "Tennessee",
      inputstate == 44 ~ "Texas",
      inputstate == 45 ~ "Utah",
      inputstate == 46 ~ "Vermont",
      inputstate == 47 ~ "Virginia",
      inputstate == 48 ~ "Washington",
      inputstate == 49 ~ "West Virginia",
      inputstate == 50 ~ "Wisconsin",
      inputstate == 51 ~ "Wyoming"
    )
  )
  
  # Step 3: Select only relevant variables
analysis_data <- cleaned_data|> select(age, vote_choice, race, income_tier, urbanicity, education, gender, religion, state,pid7)
#### Save data ####
analysis_data <- na.omit(analysis_data)
write_csv(analysis_data, "data/02-analysis_data/analysis_data.csv")
write_parquet(analysis_data, "data/02-analysis_data/analysis_data.parquet")

