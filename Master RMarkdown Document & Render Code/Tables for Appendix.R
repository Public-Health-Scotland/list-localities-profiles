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

# Set year of data extracts for folder
ext_year <- 2023

# Set file path
# lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# testing locality
# LOCALITY <- "Forres"

## Indicator Definitions ----

indicator_defs <- read_excel(paste0(lp_path, "Project Info & Indicators/Indicator Tracker ", ext_year, ".xlsx"),
                             sheet = "Definitions"
)
indicator_defs$format <- "**"
indicator_defs$Indicator <- paste0(indicator_defs$format, indicator_defs$Indicator, indicator_defs$format)

# indicator_defs <- as_tibble(indicator_defs)
indicator_defs <- dplyr::select(indicator_defs, -format)


## Data extraction dates ----

dates_extract <- read_excel(paste0(lp_path, "Project Info & Indicators/Indicator Tracker ", ext_year, ".xlsx"),
                            sheet = "Overview",
                            skip = 2
) %>%
  clean_names() %>%
  mutate(date_extracted_downloaded = as.Date(as.numeric(date_extracted_downloaded), origin = "1899-12-30")) %>%
  mutate(date_extracted_downloaded = if_else(is.na(date_extracted_downloaded), Sys.Date(), date_extracted_downloaded)) %>%
  select("Section" = chapter_heading, "Indicator" = indicator, "Date of data extraction" = date_extracted_downloaded)


dates_extract$format <- "**"
dates_extract$Section <- paste0(dates_extract$format, dates_extract$Section, dates_extract$format)

dates_extract <- dplyr::select(dates_extract, -format)


## PPA conditions included ----

ppa_def <- read_excel(paste0(lp_path, "Project Info & Indicators/Indicator Tracker ", ext_year, ".xlsx"),
                      sheet = "PPA"
)