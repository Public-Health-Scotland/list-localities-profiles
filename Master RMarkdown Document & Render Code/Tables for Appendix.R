##### LOCALITY PROFILES TABLES FOR APPENDIX #####

## Reads in indicator info from "Indicator Tracker" excel doc and formats tables for profiles

# Packages
library(flextable)
library(tidyverse)
library(janitor)
library(readxl)
library(magrittr)
library(kableExtra)
library(dplyr)

#Set year of data extracts for folder
ext_year <- 2022

#Set file path
lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# testing locality
# LOCALITY <- "Forres"

## Indicator Definitions ----

<<<<<<< HEAD
indicator_defs <- read_excel(paste0(lp_path, "Project Info & Indicators/Indicator Tracker ", ext_year, ".xlsx"),
=======
indicator_defs <- read_excel("Project Info & Indicators/Indicator Tracker ", ext_year, ".xlsx",
>>>>>>> 55314fccbe9c9bb769ecc04250e748924f543690
                             sheet = "Definitions")
indicator_defs$format <- "**"
indicator_defs$Indicator <- paste0(indicator_defs$format, indicator_defs$Indicator, indicator_defs$format)

#indicator_defs <- as_tibble(indicator_defs)
indicator_defs <- dplyr::select(indicator_defs, -format)


## Data extraction dates ----

<<<<<<< HEAD
dates_extract <- read_excel(paste0(lp_path, "Project Info & Indicators/Indicator Tracker ", ext_year, ".xlsx"),
=======
dates_extract <- read_excel("Project Info & Indicators/Indicator Tracker ", ext_year, ".xlsx",
>>>>>>> 55314fccbe9c9bb769ecc04250e748924f543690
                            sheet = "Overview", 
                            skip = 2) %>% 
  clean_names() %>% 
  mutate(date_extracted_downloaded = as.Date(as.numeric(date_extracted_downloaded), origin = "1899-12-30")) %>% 
  mutate(date_extracted_downloaded = if_else(is.na(date_extracted_downloaded), Sys.Date(), date_extracted_downloaded)) %>% 
  select("Section" = chapter_heading, "Indicator" = indicator, "Date of data extraction" = date_extracted_downloaded)


dates_extract$format <- "**"
dates_extract$Section <- paste0(dates_extract$format, dates_extract$Section, dates_extract$format)

dates_extract <- dplyr::select(dates_extract, -format)


## PPA conditions included ----

<<<<<<< HEAD
ppa_def <- read_excel(paste0(lp_path, "Project Info & Indicators/Indicator Tracker ", ext_year, ".xlsx"),
=======
ppa_def <- read_excel("Project Info & Indicators/Indicator Tracker ", ext_year, ".xlsx",
>>>>>>> 55314fccbe9c9bb769ecc04250e748924f543690
                               sheet = "PPA")

