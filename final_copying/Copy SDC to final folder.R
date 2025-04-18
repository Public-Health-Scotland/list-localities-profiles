# Copy Finalised SDC from development area to HSCP specific folders
# This should be ran after the LPs and SDC workbooks have been checked.
# Update the 'year' below, then run all the code.
# Run time: approx 5 min.

# Update the year as needed. The directory will be named '{year} Final Profiles'
year <- "2025-April"

# Source in functions code
source("Master RMarkdown Document & Render Code/Global Script.R")

# Set top level file path
lp_path <- path("/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles")

# Set paths for the existing / new locations
output_dir <- path(lp_path, "Master RMarkdown Document & Render Code", "Output", "background data")
# The directory will be created automatically.
final_dir <- path(lp_path, "Final Profiles", str_glue("{year} Final Profiles"))

# Get the list of HSCPs
hscp_list <- read_in_localities() |>
  pull(hscp2019name) |>
  unique()

# Create a dataframe with some details about the files
file_lookup <- tibble(
  path = dir_ls(path = output_dir, glob = "* - HSCP Profile SDC highlight.xlsx$"),
  file_name = path_file(path),
  hscp = str_extract(
    string = file_name,
    # Regular expression, the brackets create a 'capture group'
    pattern = "^([A-Z].+?) - HSCP Profile SDC highlight.xlsx$",
    # We only want 'group 1' i.e. the bit in the brackets
    group = 1
  )
) |>
  # Drop any rows which didn't match a hscp (usually temp files etc.)
  drop_na(hscp) |>
  # Add columns for the new directory (HSCP name) and the new path
  mutate(
    new_dir = path(final_dir, hscp),
    new_path = path(new_dir, file_name)
  )

# Create the new directories (if needed)
# Set the permissions correctly so we can edit the files if needed
dir_create(unique(pull(file_lookup, new_dir)), mode = "ug=rwx,o=rx")

# Copy files to the relevant new directory
file_copy(
  path = pull(file_lookup, path),
  new_path = pull(file_lookup, new_path)
)
