##### Script to convert Excel data for Services section and save to RDS #####

## This script takes all the csv files in the data folder,
# saves RDS versions and deletes the csv versions to save space.

library(tidyverse)
library(fs)

# Change year to be the year in the data folder name
# ext_year <- 2024

# lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Create function to do the following:

# Import services csv data extract
# Save RDS version of the data
# Delete the full csv extract

filt_and_save <- function(file_name) {
  data <- read_csv(paste0(lp_path, "Services/DATA ", ext_year, "/", file_name, ".csv"))

  saveRDS(data, paste0(lp_path, "Services/DATA ", ext_year, "/", file_name, ".RDS"))

  unlink(paste0(lp_path, "Services/DATA ", ext_year, "/", file_name, ".csv"))
}

# Extract all file names that have .csv within the services data folder (at any folder level)

my_files <- list.files(paste0(lp_path, "Services/DATA ", ext_year, "/CSV"),
  pattern = ".csv",
  recursive = TRUE,
  full.names = TRUE
)

# save CSV information in list

csv_Files <- sapply(
  X = my_files,
  FUN = read_csv,
  simplify = FALSE,
  USE.NAMES = TRUE
)

# Get name of each CSV file (without the CSV bit)

new_file_names <- path_file(path_ext_set(my_files, "RDS"))


for (i in 1:length(csv_Files)) {
  data_i <- csv_Files[[i]]

  saveRDS(data_i, paste0(lp_path, "Services/DATA ", ext_year, "/", new_file_names[i]))
}
