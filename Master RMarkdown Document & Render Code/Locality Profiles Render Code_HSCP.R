##### LOCALITY PROFILES MASTER DOC RENDER CODE #####

library(readxl)
library(janitor)
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
HSCP <- "Clackmannanshire and Stirling"

# Below creates locality list of all the localities in a chosen HSCP
lookup <- read_in_localities()

# HSCP_list <- unique(lookup$hscp2019name)

other_locs <- lookup %>%
  select(hscp_locality, hscp2019name) %>%
  filter(hscp2019name == HSCP) %>%
  arrange(hscp_locality)


# Demographics
source("Demographics/1. Demographics - Population.R")
source("Demographics/2. Demographics - SIMD.R")

# Housing
source("Households/Households code.R")

# Services
source("Services/2. Services data manipulation & table.R")
source("Services/3. Service HSCP map.R") # Map
# General Health
source("General Health/3. General Health Outputs.R")

# Lifestyle & risk factors
source("Lifestyle and risk/2. Lifestyle & Risk Factors Outputs.R")

# Unscheduled Care
source("unschedule care/2. Unscheduled Care outputs.R")

# Appendices
source("Master RMarkdown Document & Render Code/Tables for Appendix.R")

# Remove tidylog package which messes up outputs
detach(package:tidylog, unload = TRUE)

## 2b) Create the main body of the profiles
rmarkdown::render(
  input = "Master RMarkdown Document & Render Code/Locality_Profiles_Master_Markdown_HSCP.Rmd",
  output_file = paste0(HSCP, " - Locality Profile.docx"),
  output_dir = paste0(lp_path, "Master RMarkdown Document & Render Code/Output/")
)

## 2c) Create the summary tables
rmarkdown::render(
  input = "Summary Table/Summary-Table-Markdown - HSCP.Rmd",
  output_file = paste0(HSCP, " - Summary Table.docx"),
  output_dir = paste0(lp_path, "Master RMarkdown Document & Render Code/Output/Summary Tables/")
)
