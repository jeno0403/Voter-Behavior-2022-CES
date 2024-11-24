#### Preamble ####
# Purpose: Downloads and saves the CES 2022 data from Harvard Dataverse
# Author: [Your name]
# Date: 23 November 2024
# Contact: [Your contact]
# License: MIT
# Pre-requisites: None


#### Workspace setup ####
library(tidyverse)
library(arrow)


#### Download data ####
# Note: First manually download "CES22_Common_OUTPUT_vv.csv" from:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PR4L8P
ces_2022 <-
  read_csv(
    "data/01-raw_data/CES22_Common_OUTPUT_vv.csv",  # Changed to match your actual file name
    show_col_types = FALSE
  )

#### Save data ####
write_parquet(ces_2022, "data/01-raw_data/raw_ces_2022.parquet")
write_csv(ces_2022, "data/01-raw_data/raw_ces_2022.csv")
         
