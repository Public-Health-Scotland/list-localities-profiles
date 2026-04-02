library(knitr)
library(bookdown)
library(phstemplates)

rlang::check_installed(
  pkg = "phstemplates",
  reason = "v1.3.0 is needed to apply sensitivity labels",
  version = "1.3.0",
  action = \(pkg, ...) {
    remotes::install_github(paste0("Public-Health-Scotland/", pkg))
  }
)

rm(list = ls())

# Source in functions code
source("Master RMarkdown Document & Render Code/Global Script.R")

# Set file path
lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"
output_dir <- path(lp_path, "Profiles Output")

# Below creates locality list of all the localities
lookup <- read_in_localities()

# Specify HSCP(s) ----
# use `unique(lookup$hscp2019name)` for all
# or create a vector for multiple e.g. `c("Angus", "West Lothian")`
# For a larger test, use the below to produce profiles for HSCPs likely to cause issues.
# source("Master RMarkdown Document & Render Code/find_hscp_outliers.R")
# hscp_list <- outlier_hscps
hscp_list <- unique(lookup$hscp2019name)

# NOTE - This checks that it exactly matches the lookup
stopifnot(all(hscp_list %in% unique(lookup[["hscp2019name"]])))

# Loop over HSCP ----
# 'looping' over one HSCP is fine.

# There are several stages to the profiles:
# 1. Looping through each HSCP doing the following:
# 1a. Run each section script for that locality
# 1b. Run the Rmd for the main body of the profiles
# 1c. Run the Rmd for the summary tables

loop_env <- c(ls(), "loop_env")

# 1. Loop through each HSCP to create the main body of the profiles and the summary table
for (HSCP in hscp_list) {
  # 1a) Source in all the scripts for a given HSCP

  # Demographics ----
  source("Demographics/1. Demographics - Population.R")
  source("Demographics/2. Demographics - SIMD.R")

  # Housing ----
  source("Households/Households Code.R")

  # Services ----
  source("Services/2. Services data manipulation & table.R")
  source("Services/3. Service HSCP map.R")

  # General Health ----
  source("General Health/3. General Health Outputs.R")

  # Lifestyle & Risk Factors ----
  source("Lifestyle & Risk Factors/2. Lifestyle & Risk Factors Outputs.R")

  # Unscheduled Care ----
  source("Unscheduled Care/2. Unscheduled Care outputs.R")

  # Appendices ----
  source("Master RMarkdown Document & Render Code/Tables for Appendix.R")

  main_title <- glue("{HSCP} - HSCP Profile")
  output_doc_name <- path_ext_set(main_title, "docx")

  # Make sure your working directory is the project root
  bookdown::render_book(
    input = "lp_bookdown",
    output_dir = output_dir,
    output_file = output_doc_name,
    new_session = FALSE,
    output_format = "bookdown::word_document2",
    config_file = "_bookdown.yaml"
  )

  document_path <- path(output_dir, output_doc_name)

  orient(document_path)

  cover_page_path <- path(
    lp_path,
    "templates",
    "phs-mngtinfo-cover.docx"
  )

  add_cover_page(
    document_path,
    cover_page_path,
    main_title
  )

  apply_sensitivity_label(
    document_path,
    "OFFICIAL_SENSITIVE_VMO"
  )

  # End of loop housekeeping ----
  # Clean up the environment by restoring it to the 'pre-loop' state.
  rm(list = setdiff(ls(), loop_env))
  # Force garbage collection to free up memory
  gc()
}
