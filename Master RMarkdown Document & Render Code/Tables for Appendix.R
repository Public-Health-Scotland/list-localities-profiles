##### LOCALITY PROFILES TABLES FOR APPENDIX #####

## Reads in indicator info from "Indicator Tracker" excel doc and formats tables for profiles

# Packages
library(readxl)
library(dplyr)
library(tidyr)
library(fs)
library(glue)

# Set year of data extracts for folder
ext_year <- 2024

# Set file path
# lp_path <- path("/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles")

# testing locality
# LOCALITY <- "Forres"

indicator_workbook_path <- path(
  lp_path,
  "Project Info & Indicators",
  glue("Indicator Tracker {ext_year}.xlsx")
)

## Indicator Definitions ----

indicator_defs <- read_excel(
  path = indicator_workbook_path,
  sheet = "Definitions",
  col_types = "text"
)

## Data extraction dates ----

dates_extract <- read_excel(
  path = indicator_workbook_path,
  sheet = "Overview",
  # Extract the the columns A:C (col 1:col 3) but skip the first 4 rows
  range = cell_limits(ul = c(4, 1), lr = c(NA, 3)),
  # Set the headings
  col_names = c("Section", "Indicator", "Date of data extraction"),
  # Set the col types (also parse the dates)
  col_types = c("text", "text", "date")
) |>
  replace_na(list("Date of data extraction" = Sys.Date())) |>
  mutate(
    `Date of data extraction` = format(`Date of data extraction`, "%d/%m/%Y")
  )

## PPA conditions included ----

ppa_def <- read_excel(
  path = indicator_workbook_path,
  sheet = "PPA",
  col_types = "text"
)

rm(indicator_workbook_path)
