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
HSCP <- "Western Isles"

# Below creates locality list of all the localities in a chosen HSCP
lookup <- read_in_localities()

# HSCP_list <- unique(lookup$hscp2019name)

other_locs <- lookup %>%
  select(hscp_locality, hscp2019name) %>%
  filter(hscp2019name == HSCP) %>%
  arrange(hscp_locality)


# demographics
source("hscp_level/Demographics/1. Demographics - Population.R")
source("hscp_level/Demographics/2. Demographics - SIMD.R")

# housing
source("hscp_level/Households/HSCP level/Households code.R")

# services
source("hscp_level/Services/3. Service HSCP map.R") # Map
# source(paste0(lp_path, "Services/Scripts/HSCP level/2. Services data manipulation & table.R"))
# source("hscp_level/Services/") # Nothing in this folder

# general health
source("hscp_level/General Health/3. General Health Outputs.R")

# lifestyle & risk factors
source("hscp_level/Lifestyle and risk/HSCP level/2. Lifestyle & Risk Factors Outputs.R")

# unscheduled care
source("hscp_level/unschedule care/2. Unscheduled Care outputs.R")

# appendices
source("Master RMarkdown Document & Render Code/Tables for Appendix.R")

# Remove tidylog package which messes up outputs
detach(package:tidylog, unload = TRUE)

## 2b) Create the main body of the profiles
rmarkdown::render(
  input = "hscp_level/Render script/Locality_Profiles_Master_Markdown_HSCP.Rmd",
  output_file = paste0(HSCP, " - Locality Profile.docx"),
  output_dir = paste0(lp_path, "Master RMarkdown Document & Render Code/Output/")
)

## 2c) Create the summary tables
rmarkdown::render(
  input = "hscp_level/Summary-Table-Markdown - HSCP.Rmd",
  output_file = paste0(HSCP, " - Summary Table.docx"),
  output_dir = paste0(lp_path, "Master RMarkdown Document & Render Code/Output/Summary Tables/")
)
