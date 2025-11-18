############################################################################################# .
#                                                                                           #
#                   LOCALITY PROFILES LIFESTYLE & RISK FACTORS OUTPUTS CODE                 #
#                                                                                           #
############################################################################################# .

## Code used to create infographics, charts, and figures for the Lifestyle & Risk factors
#  section of the locality profiles.

## Original script Lucy Dewhurst
## Original date 10/01/2020
## October 2022 rewrote parts for smoother process (edits by C.Puech)

############# 1) PACKAGES, DIRECTORY, LOOKUPS, DATA IMPORT + CLEANING #############

# Determine locality (for testing only)
# LOCALITY <- "Falkirk West"
# LOCALITY <- "Stirling City with the Eastern Villages Bridge of Allan and Dunblane"
# LOCALITY <- "Mid-Argyll, Kintyre and Islay"
# LOCALITY <- "City of Dunfermline"
# LOCALITY <- "Barra"

# Set year of data extracts for folder
ext_year <- 2024

# Set file path
# lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Source in functions code
# source("Master RMarkdown Document & Render Code/Global Script.R")

### Geographical lookups and objects ----

# Locality lookup
lookup <- read_in_localities()

# Determine HSCP and HB based on Loc
HSCP <- as.character(filter(lookup, hscp_locality == LOCALITY)$hscp2019name)
HB <- as.character(filter(lookup, hscp_locality == LOCALITY)$hb2019name)

# Determine other localities based on LOCALITY object
other_locs <- lookup %>%
  select(hscp_locality, hscp2019name) %>%
  filter(hscp2019name == HSCP & hscp_locality != LOCALITY) %>%
  arrange(hscp_locality)

# Find number of locs per partnership
n_loc <- count_localities(lookup, HSCP)


### Import + clean datasets ----

## Drug-related hospital admissions
drug_hosp <- readRDS(paste0(
  lp_path,
  "Lifestyle & Risk Factors/Data ",
  ext_year,
  "/scotpho_data_extract_drug_hosp.RDS"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

check_missing_data_scotpho(drug_hosp)


# Alcohol-related hospital admissions
alcohol_hosp <- readRDS(paste0(
  lp_path,
  "Lifestyle & Risk Factors/Data ",
  ext_year,
  "/scotpho_data_extract_alcohol_hosp.RDS"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = substr(period, 1, 7))

check_missing_data_scotpho(alcohol_hosp)


## Alcohol-specific deaths
alcohol_deaths <- readRDS(paste0(
  lp_path,
  "Lifestyle & Risk Factors/Data ",
  ext_year,
  "/scotpho_data_extract_alcohol_deaths.RDS"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 12), fixed = TRUE))

check_missing_data_scotpho(alcohol_deaths)

## Bowel screening uptake
bowel_screening <- readRDS(paste0(
  lp_path,
  "Lifestyle & Risk Factors/Data ",
  ext_year,
  "/scotpho_data_extract_bowel_screening.RDS"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 12), fixed = TRUE))

check_missing_data_scotpho(bowel_screening)

### check if there is any drug death data

############################### 2) OUTPUTS ####################################

##### 2a Drug-related hospital admissions #####

## Create variables for latest year
max_year_drug_hosp <- max(drug_hosp[["year"]])
min_year_drug_hosp <- min(drug_hosp[["year"]])
latest_period_drug_hosp <- drug_hosp[["period_short"]][which.max(drug_hosp[[
  "year"
]])]
earliest_period_drug_hosp <- drug_hosp[["period_short"]][which.min(drug_hosp[[
  "year"
]])]
# ScotPHO time trend will only be latest 10 years
trend_years <- 10
earliest_period_drug_hosp_trend <- drug_hosp[["period_short"]][match(
  max_year_drug_hosp - trend_years,
  drug_hosp[["year"]]
)]


## Time trend
drug_hosp_time_trend <- scotpho_time_trend(
  data = drug_hosp,
  chart_title = "Drug-related Hospital Admissions Time Trend",
  xaxis_title = "Financial Year Groups (3-year aggregates)",
  yaxis_title = "Drug-related admissions\n(Standardised rates per 100,000)",
  string_wrap = 10,
  trend_years = trend_years,
  rotate_xaxis = TRUE
)

drug_hosp_time_trend

## Bar chart
drug_hosp_bar <- drug_hosp %>%
  scotpho_bar_chart(
    chart_title = paste0(
      "Drug-related Hospital Admissions by Area, ",
      latest_period_drug_hosp
    ),
    xaxis_title = "Drug-related admissions (Standardised rates per 100,000)"
  )

drug_hosp_bar

## Numbers for text
drug_hosp_latest <- filter(
  drug_hosp,
  year == max_year_drug_hosp,
  area_name == LOCALITY,
  area_type == "Locality"
) |>
  pull(measure)

drug_hosp_earliest <- filter(
  drug_hosp,
  year == min_year_drug_hosp,
  area_name == LOCALITY,
  area_type == "Locality"
) |>
  pull(measure)

drug_hosp_change <- abs(
  (drug_hosp_latest - drug_hosp_earliest) / drug_hosp_earliest * 100
)
drug_hosp_change_word <- if_else(
  drug_hosp_latest > drug_hosp_earliest,
  "increase",
  "decrease"
)

scot_drug_hosp <- filter(
  drug_hosp,
  year == max_year_drug_hosp,
  area_name == "Scotland"
) |>
  pull(measure)

drug_hosp_diff_scot <- if_else(
  drug_hosp_latest > scot_drug_hosp,
  "higher",
  "lower"
)


##### 2b Alcohol-related hospital admissions #####

## Create variables for latest year
latest_period_alcohol_hosp <- unique(
  filter(alcohol_hosp, year == max(alcohol_hosp$year))$period_short
)
earliest_period_alcohol_hosp <- unique(
  filter(alcohol_hosp, year == min(alcohol_hosp$year))$period_short
)


## Time trend
alcohol_hosp_time_trend <- scotpho_time_trend(
  data = alcohol_hosp,
  chart_title = "Alcohol-related Hospital Admissions Time Trend",
  xaxis_title = "Financial Year",
  yaxis_title = "Alcohol-related admissions\n(Standardised rates per 100,000)",
  string_wrap = 10,
  rotate_xaxis = TRUE
)

alcohol_hosp_time_trend

## Bar chart
alcohol_hosp_bar <- alcohol_hosp %>%
  scotpho_bar_chart(
    data = .,
    chart_title = paste0(
      "Alcohol-related Hospital Admissions by Area, ",
      max(.$period_short)
    ),
    xaxis_title = "Alcohol-related admissions (Standardised rates per 100,000)"
  )

alcohol_hosp_bar


## Numbers for text

alcohol_hosp_latest <- filter(
  alcohol_hosp,
  year == max(alcohol_hosp$year) &
    (area_name == LOCALITY & area_type == "Locality")
)$measure

alcohol_hosp_earliest <- filter(
  alcohol_hosp,
  (year == min(alcohol_hosp$year)) &
    (area_name == LOCALITY & area_type == "Locality")
)$measure

alcohol_hosp_change <- abs(
  (alcohol_hosp_latest - alcohol_hosp_earliest) / alcohol_hosp_earliest * 100
)
alcohol_hosp_change_word <- if_else(
  alcohol_hosp_latest > alcohol_hosp_earliest,
  "increase",
  "decrease"
)

scot_alcohol_hosp <- filter(
  alcohol_hosp,
  year == max(alcohol_hosp$year) & area_name == "Scotland"
)$measure

alcohol_hosp_diff_scot <- if_else(
  alcohol_hosp_latest > scot_alcohol_hosp,
  "higher",
  "lower"
)


##### 2c Alcohol specific deaths #####

## Create variables for latest year
latest_period_alcohol_deaths <- unique(
  filter(alcohol_deaths, year == max(alcohol_deaths$year))$period_short
)
earliest_period_alcohol_deaths <- unique(
  filter(alcohol_deaths, year == min(alcohol_deaths$year))$period_short
)


## Time trend
alcohol_deaths_time_trend <- scotpho_time_trend(
  data = alcohol_deaths,
  chart_title = "Alcohol-specific Deaths Time Trend",
  xaxis_title = "Year Groups (5-year aggregates)",
  yaxis_title = "Alcohol-specific deaths\n(Standardised rates per 100,000)",
  string_wrap = 10
)

alcohol_deaths_time_trend

## Bar chart
alcohol_deaths_bar <- alcohol_deaths %>%
  scotpho_bar_chart(
    data = .,
    chart_title = paste0(
      "Alcohol-specific Deaths by Area, ",
      max(.$period_short)
    ),
    xaxis_title = "Alcohol-specific deaths (Standardised rates per 100,000)"
  )

alcohol_deaths_bar


## Numbers for text

alcohol_deaths_latest <- filter(
  alcohol_deaths,
  year == max(alcohol_deaths$year) &
    (area_name == LOCALITY & area_type == "Locality")
)$measure

alcohol_deaths_earliest <- filter(
  alcohol_deaths,
  (year == min(alcohol_deaths$year)) &
    (area_name == LOCALITY & area_type == "Locality")
)$measure

alcohol_deaths_change <- abs(
  (alcohol_deaths_latest - alcohol_deaths_earliest) /
    alcohol_deaths_earliest *
    100
)
alcohol_deaths_change_word <- if_else(
  alcohol_deaths_latest > alcohol_deaths_earliest,
  "higher",
  "lower"
)

scot_alcohol_deaths <- filter(
  alcohol_deaths,
  year == max(alcohol_deaths$year) & area_name == "Scotland"
)$measure

alcohol_deaths_diff_scot <- if_else(
  alcohol_deaths_latest > scot_alcohol_deaths,
  "higher",
  "lower"
)


##### 2d Bowel Screening Uptake #####

## Create variables for latest year
latest_period_bowel_screening <- unique(
  filter(bowel_screening, year == max(bowel_screening$year))$period_short
)
earliest_period_bowel_screening <- unique(
  filter(bowel_screening, year == min(bowel_screening$year))$period_short
)


## Time trend
bowel_screening_time_trend <- scotpho_time_trend(
  data = bowel_screening,
  chart_title = "Bowel Screening Uptake Time Trend",
  xaxis_title = "Year Groups (3-year aggregates)",
  yaxis_title = "Bowel screening uptake (%)",
  string_wrap = 10
)

bowel_screening_time_trend

## Bar chart
bowel_screening_bar <- bowel_screening %>%
  scotpho_bar_chart(
    data = .,
    chart_title = paste0(
      "Bowel Screening Uptake by Area, ",
      max(.$period_short)
    ),
    xaxis_title = "Bowel screening uptake (%)"
  )

bowel_screening_bar


## Numbers for text

bowel_screening_latest <- filter(
  bowel_screening,
  year == max(bowel_screening$year) &
    (area_name == LOCALITY & area_type == "Locality")
)$measure

bowel_screening_earliest <- filter(
  bowel_screening,
  (year == min(bowel_screening$year)) &
    (area_name == LOCALITY & area_type == "Locality")
)$measure

bowel_screening_change <- abs(
  (bowel_screening_latest - bowel_screening_earliest) /
    bowel_screening_earliest *
    100
)
bowel_screening_change_word <- if_else(
  bowel_screening_latest > bowel_screening_earliest,
  "increase",
  "decrease"
)


scot_bowel_screening <- filter(
  bowel_screening,
  year == max(bowel_screening$year) & area_name == "Scotland"
)$measure

bowel_screening_diff_scot <- if_else(
  bowel_screening_latest > scot_bowel_screening,
  "higher",
  "lower"
)


############################### 3) CODE FOR SUMMARY TABLE ###############################

## Make GH objects table for hscp, scot AND other localities in the partnership

# Function to get latest data from scotpho for other localities

other_locs_summary_table <- function(data, latest_year) {
  data %>%
    filter(year == latest_year) %>%
    filter(area_type == "Locality") %>%
    rename("hscp_locality" = "area_name") %>%
    right_join(other_locs, by = join_by(hscp_locality)) %>%
    arrange(hscp_locality) %>%
    select(hscp_locality, measure) %>%
    mutate(measure = round_half_up(measure, 1)) %>%
    pivot_wider(names_from = hscp_locality, values_from = measure)
}

# 1. Other locs

other_locs_drug_hosp <- other_locs_summary_table(
  drug_hosp,
  latest_year = max(drug_hosp$year)
)

other_locs_alcohol_hosp <- other_locs_summary_table(
  alcohol_hosp,
  latest_year = max(alcohol_hosp$year)
)

other_locs_alcohol_deaths <- other_locs_summary_table(
  alcohol_deaths,
  latest_year = max(alcohol_deaths$year)
)

other_locs_bowel_screening <- other_locs_summary_table(
  bowel_screening,
  latest_year = max(bowel_screening$year)
)


# 2. HSCP

hscp_drug_hosp <- round_half_up(
  filter(
    drug_hosp,
    year == max(year) &
      (area_name == HSCP & area_type == "HSCP")
  )$measure,
  1
)

hscp_alcohol_hosp <- round_half_up(
  filter(
    alcohol_hosp,
    year == max(year) &
      (area_name == HSCP & area_type == "HSCP")
  )$measure,
  1
)

hscp_alcohol_deaths <- round_half_up(
  filter(
    alcohol_deaths,
    year == max(year) &
      (area_name == HSCP & area_type == "HSCP")
  )$measure,
  1
)

hscp_bowel_screening <- round_half_up(
  filter(
    bowel_screening,
    year == max(year) &
      (area_name == HSCP & area_type == "HSCP")
  )$measure,
  1
)


# 3. Scotland

scot_drug_hosp <- round_half_up(scot_drug_hosp, 1)

scot_alcohol_hosp <- round_half_up(scot_alcohol_hosp, 1)

scot_alcohol_deaths <- round_half_up(scot_alcohol_deaths, 1)

scot_bowel_screening <- round_half_up(scot_bowel_screening, 1)

# Housekeeping ----
# These objects are left over after the script is run
# but don't appear to be used in any 'downstream' process:
# Main markdown, Summary Table, Excel data tables, SDC output.
# TODO: Investigate if these can be removed earlier or not created at all.
rm(
  alcohol_deaths_earliest,
  alcohol_hosp_earliest,
  bowel_screening_earliest,
  drug_hosp_earliest,
  max_year_drug_hosp,
  min_year_drug_hosp,
  trend_years
)
gc()
