library(fs)
library(zip)
library(stringr)
library(purrr)

# This script will zip any 'DATA xxxx' folders more than
# 2 years older than the current year
current_year <- 2026
cutoff_year <- current_year - 2

lp_path <- path(
  "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles"
)

chapters <- c(
  "Demographics",
  "General Health",
  "Households",
  "Lifestyle & Risk Factors",
  "Population Health",
  "Services",
  "Unscheduled Care"
)
chapter_dirs <- path(lp_path, chapters)

# Check all the expected chapter directories exist
stopifnot(dir_exists(chapter_dirs))

# Find all DATA yyyy directories
unzipped_data_dirs <- dir_ls(
  chapter_dirs,
  type = "directory",
  recurse = TRUE,
  regexp = "DATA [0-9]{4}$"
)

# Extract year and filter to >2 years old
data_years <- as.integer(str_extract(unzipped_data_dirs, "[0-9]{4}"))

unzipped_data_dirs <- unzipped_data_dirs[data_years <= cutoff_year]

# Corresponding zip files
new_zip_files <- path_ext_set(unzipped_data_dirs, "zip")

message(
  "About to zip these directories:\n",
  paste("  -", path_rel(unzipped_data_dirs, lp_path), collapse = "\n")
)

walk2(unzipped_data_dirs, new_zip_files, function(data_dir, zip_file) {
  if (file_exists(zip_file)) {
    message("Zip already exists, skipping: ", path_rel(zip_file, lp_path))
    invisible(zip_file)
  }

  # Create zip
  suppressWarnings(
    zip::zip(
      zipfile = zip_file,
      files = data_dir
    )
  )

  # Stop if zip file wasn't created
  stopifnot(file_exists(zip_file))

  expected <- path_rel(dir_ls(data_dir, recurse = TRUE), "/")

  actual <- path(zip_list(zip_file)$filename)

  missing <- setdiff(expected, actual)

  if (length(missing) != 0) {
    file_delete(zip_file)

    stop(
      "Zip validation failed for ",
      data_dir,
      "\n",
      "Missing files:\n",
      paste("  -", missing, collapse = "\n"),
      call. = FALSE
    )
  }

  dir_delete(data_dir)

  invisible(zip_file)
})
