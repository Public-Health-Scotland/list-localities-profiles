##### LOCALITY PROFILES MASTER DOC RENDER CODE #####

library(readxl)
library(janitor)
library(tidyverse)
library(knitr)
library(markdown)
library(rmarkdown)

rm(list = ls())

# system unmask function so files have read-write permissions
Sys.umask("006")

# Set file path
lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Source in functions code
source("Master RMarkdown Document & Render Code/Global Script.R")

## Specify HSCP here
## NOTE - make sure that the formatting of the partnership's name matches the lookup
HSCP <- "Angus"

# Below creates locality list of all the localities in a chosen HSCP
lookup <- read_in_localities()

HSCP_list <- unique(lookup$hscp2019name)

# Create list of localities in chosen HSCP
locality_list <- lookup |>
  filter(hscp2019name == HSCP) |>
  pull(hscp_locality)


## Loop to create the profiles for all the localities in the list

## There are several stages to the profiles:
# 1. Producing the HSCP services map (this takes a while to run so it is produced separately)
# 2. Looping through each locality in the HSCP doing the following:
# 2a. Run each section script for that locality
# 2b. Run the Rmd for the main body of the profiles
# 2c. Run the Rmd for the summary tables

# 1. HSCP Services Map

#map <- paste0(lp_path, "Master RMarkdown Document & Render Code/Output/maps/", HSCP, ".png")

#stopifnot(file.exists(map)) # Error if the file path doesn't exist.

loop_env <- c(ls(), "loop_env")

# 2. Loop through each locality to create the main body of the profiles and the summary table
for (LOCALITY in locality_list) {
  ## 2a) Source in all the scripts for a given LOCALITY

  # demographics
  source("Demographics/1. Demographics - Population.R")
  source("./Demographics/2. Demographics - SIMD.R")

  # housing
  source("./Households/Households Code.R")

  # services
  source("./Services/2. Services data manipulation & table.R")
  source("./Services/3. Service HSCP map.R")
#plot(service_map)
  # general health
  source("./General Health/3. General Health Outputs.R")

  # lifestyle & risk factors
  source("./Lifestyle & Risk Factors/2. Lifestyle & Risk Factors Outputs.R")

  # unscheduled care
  source("./Unscheduled Care/2. Unscheduled Care outputs.R")

  # appendices
  source("./Master RMarkdown Document & Render Code/Tables for Appendix.R")

  # Remove tidylog package which messes up outputs
  # detach(package:tidylog, unload = TRUE)

  ## 2b) Create the main body of the profiles

  rmarkdown::render("./Master RMarkdown Document & Render Code/Locality_Profiles_Master_Markdown.Rmd",
    output_file = paste0(LOCALITY, " - Locality Profile_test_tc.docx"),
    output_dir = paste0(lp_path, "Master RMarkdown Document & Render Code/Output/")
  )

  ## 2c) Create the summary tables
  rmarkdown::render("Summary Table/Summary-Table-Markdown.Rmd",
    output_file = paste0(LOCALITY, " - Summary Table_test_tc.docx"),
    output_dir = paste0(lp_path, "Master RMarkdown Document & Render Code/Output/Summary Tables/")
  )

  # Clean up the environment by restoring it to the 'pre-loop' state.
  rm(list = setdiff(ls(), loop_env))
  # Force garbage collection to free up memory
  gc()
}
