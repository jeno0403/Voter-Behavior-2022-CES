#### Preamble ####
# Purpose: Downloads and saves the data from
# Author: Jinyan Wei
# Date: 16 April 2024
# Contact: jinyan.wei@mail.utorotnto.ca
# License: MIT
# Pre-requisites: None


#### Workspace setup ####
library(tidyverse)
library(arrow)


#### Download data ####
# Note: First manually download "CES22_Common_OUTPUT_vv.csv" from:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PR4L8P
# Save the download file in the folder 'data/01-raw_data'

raw_data <-
  read_csv(
    "data/01-raw_data/CES22_Common_OUTPUT_vv.csv",
    show_col_types = FALSE
  )

  
#### Save data ####
# change the_raw_data to whatever name you assigned when you downloaded it.
write_csv(raw_data, "data/01-raw_data/raw_data.csv")

