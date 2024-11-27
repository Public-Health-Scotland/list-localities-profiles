##### LOCALITY PROFILES MASTER DOC RENDER CODE #####

library(readxl)
library(janitor)
library(tidyverse)
library(knitr)
library(markdown)
library(rmarkdown)
library(fs)

rm(list = ls())

# system unmask function so files have read-write permissions
Sys.umask("006")

# Set file path
lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Source in functions code
source("Master RMarkdown Document & Render Code/Global Script.R")

# Source custom localities function to overwrite the standard
source("Master RMarkdown Document & Render Code/overwrite_with_custom_functions.R")

# Below creates locality list of all the localities in a chosen HSCP
lookup <- read_in_localities()

# Specify HSCP(s) ----
# use `unique(lookup$hscp2019name)` for all
# or create a vector for multiple e.g. `c("Angus", "West Lothian")`
hscp_list <- "Moray"

# NOTE - This checks that it exactly matches the lookup
stopifnot(all(hscp_list %in% unique(lookup$hscp2019name)))

# We don't want to create a profile doc for 'other areas'
locality_list <- locality_list[locality_list != "Other areas"]

# Loop over HSCP ----
# 'looping' over one HSCP is fine.
for (HSCP in hscp_list) {
  # Create list of localities in chosen HSCP
  locality_list <- lookup |>
    filter(hscp2019name == HSCP) |>
    pull(hscp_locality)

  # Loop to create the profiles for all the localities in the list

  # There are several stages to the profiles:
  # 1. Looping through each locality in the HSCP doing the following:
  # 1a. Run each section script for that locality
  # 1b. Run the Rmd for the main body of the profiles
  # 1c. Run the Rmd for the summary tables

  loop_env <- c(ls(), "loop_env")

  # 1. Loop through each locality to create the main body of the profiles and the summary table
  for (LOCALITY in locality_list) {
    # 1a) Source in all the scripts for a given LOCALITY

    # Demographics
    source("Demographics/1. Demographics - Population.R")
    source("Demographics/2. Demographics - SIMD.R")

    # Housing
    source("Households/Households Code.R")

    # Services
    source("Services/2. Services data manipulation & table.R")
    source("Services/3. Service HSCP map.R")

    # General Health
    source("General Health/3. General Health Outputs.R")

    # Lifestyle & Risk Factors
    source("Lifestyle & Risk Factors/2. Lifestyle & Risk Factors Outputs.R")

    # Unscheduled Care
    source("Unscheduled Care/2. Unscheduled Care outputs.R")

    # Appendices
    source("Master RMarkdown Document & Render Code/Tables for Appendix.R")

    # 1b) Create the main body of the profiles

    rmarkdown::render("Master RMarkdown Document & Render Code/Locality_Profiles_Master_Markdown.Rmd",
      output_file = paste0(LOCALITY, " - Locality Profile.docx"),
      output_dir = paste0(lp_path, "Master RMarkdown Document & Render Code/Output/")
    )

    ## 1c) Create the summary tables
    rmarkdown::render("Summary Table/Summary-Table-Markdown.Rmd",
      output_file = paste0(LOCALITY, " - Summary Table.docx"),
      output_dir = paste0(lp_path, "Master RMarkdown Document & Render Code/Output/Summary Tables/")
    )

    # Clean up the environment by restoring it to the 'pre-loop' state.
    rm(list = setdiff(ls(), loop_env))
    # Force garbage collection to free up memory
    gc()
  }
}
