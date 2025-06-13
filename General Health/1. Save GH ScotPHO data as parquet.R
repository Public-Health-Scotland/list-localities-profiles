# Save the GH ScotPHO data
# Get the data from https://scotland.shinyapps.io/ScotPHO_profiles_tool/
# Choose Scotland, Council Area, Health Board, HSC Partnership, HSC Locality and Intermediate Zone
# Choose the indicators listed below (in the case_match)
# Download the data as a parquet (file name should be ScotPHO_datatab_extract_***.parquet)
# Put this file in the 'new' data folder e.g. 'General Health/DATA 20XX'

library(fs)
library(glue)
library(purrr)
library(arrow)
library(dplyr)
library(tidyr)

# Change year to be the year in the data folder name
ext_year <- 2024

# Set file path
lp_path <- path(
  "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles"
)
gen_health_data_dir <- path(lp_path, "General Health", glue("DATA {ext_year}"))

# Error if the directory doesn't exist
stopifnot(dir_exists(gen_health_data_dir))

data_extract_file <- dir_ls(
  gen_health_data_dir,
  glob = "*ScotPHO_datatab_extract_*"
)

# Error if there's not a single file
stopifnot(file_exists(data_extract_file))
stopifnot(length(data_extract_file) == 1)

# Read the data and split (nest) it by indicator so we can write out each one separately
read_parquet(data_extract_file) |>
  nest(.by = indicator) |>
  mutate(
    # Put the indicator column back in the data
    data = map2(
      data,
      indicator,
      \(data, indicator) mutate(data, indicator = indicator)
    ),
    # Use the indicator to determine the file name
    file_name = paste0(
      "scotpho_data_extract_",
      case_match(
        indicator,
        "Asthma patient hospitalisations" ~ "asthma_hosp",
        "Cancer registrations" ~ "cancer_reg",
        "Chronic obstructive pulmonary disease (COPD) patient hospitalisations" ~
          "copd_hosp",
        "Coronary heart disease (CHD) patient hospitalisations" ~ "chd_hosp",
        "Deaths, aged 15-44 years" ~ "deaths_15_44",
        "Early deaths from cancer, aged <75 years" ~ "early_deaths_cancer",
        "Life expectancy, females" ~ "life_exp_fem",
        "Life expectancy, males" ~ "life_exp_male",
        "Population prescribed drugs for anxiety/depression/psychosis" ~
          "adp_presc",
        .default = NA_character_
      ),
      ".parquet"
    )
  ) |>
  # Write each indicator's data to individual files
  pwalk(
    \(indicator, data, file_name) {
      write_parquet(
        data,
        path(gen_health_data_dir, file_name),
        compression = "zstd"
      )
    },
    .progress = TRUE
  )

# Delete the original data extract
file_delete(data_extract_file)

# Clean up the environment
rm(list = ls())
gc()
