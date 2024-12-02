# The Generational and Demographic Determinants of Voting Behavior: Evidence from the 2022 CES

## Overview

This paper presents the analysis and methodology used to examine voter behavior and preferences in the 2022 U.S. midterm elections. The primary objective is to explore the factors influencing political party affiliation and voting patterns, focusing on demographic, socioeconomic, and geographic variables. The study incorporates statistical modeling, data visualization, and a detailed discussion on the relationships between voter characteristics and party alignment. This paper provides all the necessary insights, data, and methodologies to understand the dynamics of voter preferences during the 2022 midterm elections.

## File Structure

The repository is structured as follows:

-   `data/raw_data` :This folder contains the raw CES 2022 Common Output Data obtained from [Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PR4L8P).  
  To download the data:
  1. Navigate to the dataset page.
  2. Download the file named **`CES22_Common_OUTPUT_vv.csv`** either manually or by using the direct download link:  
     [Direct File Download](https://dataverse.harvard.edu/api/access/datafile/10140882).
  3. Alternatively, you can use the following `wget` command to avoid download interruptions:  
     ```bash
     wget https://dataverse.harvard.edu/api/access/datafile/10140882 -O data/raw_data/CES22_Common_OUTPUT_vv.csv
     ```
  Once downloaded, move the file to the **`data/raw_data`** directory.
  
-   `data/02-analysis_data` contains the cleaned dataset constructed in `scripts/02-clean_data`.  
-   The `scripts` folder contains the R scripts and code that simulated, tested, downloaded, and cleaned the data.
-   `model` contains fitted models.
-   `other` contains details about LLM chat interactions, sketches and datasheet.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper.


## Statement on LLM usage

Aspects of the code were written with the help of Claude Sonnet3.5 and ChatGPT4o. Part of the writing process, including generating and polishing was written with the help of ChatGPT4o, and the entire chat history is available in `other/llms/usage.txt`.
