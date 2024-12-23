# Save General Health ScotPHO data as parquet

# This script takes all the CSV files in the data folder,
# saves as parquet and deletes the original CSV to save space.

library(fs)
library(glue)
library(purrr)
library(readr)
library(arrow)

# Change year to be the year in the data folder name
ext_year <- 2023

# Set file path
lp_path <- path("/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles")
gen_health_data_dir <- path(lp_path, "General Health", glue("DATA {ext_year}"))

# Error if the directory doesn't exist
stopifnot(dir_exists(gen_health_data_dir))

# Map over every CSV file in the directory
dir_ls(path = gen_health_data_dir, type = "file", regexp = ".csv", fixed = TRUE) |>
  # For each file
  # 1) Read the data
  # 2) Write it out as a compressed parquet file
  # 3) Delete the original CSV file
  walk(
    function(file_path) {
      read_csv(file_path, show_col_types = FALSE) |>
        write_parquet(path_ext_set(file_path, "parquet"), compression = "zstd")

      file_delete(file_path)
    },
    .progress = TRUE
  )

# Clean up the environment
rm(list = ls())
gc()
