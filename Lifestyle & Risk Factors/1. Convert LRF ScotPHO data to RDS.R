##### Script to save Lifestyle & risk Factors ScotPHO to RDS #####

## This script takes all the csv files in the data folder,
# saves RDS versions and deletes the csv versions to save space.

library(tidyverse)

# Change year to be the year in the data folder name
ext_year <- 2024

lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/Lifestyle & Risk Factors/Data "

# Create function to do the following:
# Import ScotPHO data extract
# Save RDS version of the data
# Delete the full csv extract

filt_and_save <- function(file_name) {
  data <- read_csv(paste0(lp_path, ext_year, "/", file_name, ".csv"))

  saveRDS(data, paste0(lp_path, ext_year, "/", file_name, ".RDS"))

  unlink(paste0(lp_path, ext_year, "/", file_name, ".csv"))
}

# Extract all file names from the ScotPHO folder
my_files <- list.files(paste0(lp_path, ext_year), pattern = ".csv")

# Remove .csv from file names
file_names <- as.list(gsub(".csv", "", my_files))

# Apply "filt_and_save" function created earlier to each element of the file_names list
lapply(file_names, filt_and_save)
