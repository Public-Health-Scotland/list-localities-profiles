library(knitr)
library(bookdown)
library(phstemplates)

rm(list = ls())
rlang::check_installed(
  pkg = "phstemplates",
  reason = "v1.3.0 is needed to apply sensitivity labels",
  version = "1.3.0",
  action = \(pkg, ...) {
    remotes::install_github(paste0("Public-Health-Scotland/", pkg))
  }
)

# Source in functions code
source("Master RMarkdown Document & Render Code/Global Script.R")

# Set file path
lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"
output_dir <- path(lp_path, "Profiles Output")


# Below creates locality list of all the localities in a chosen HSCP
lookup <- read_in_localities()

# Specify HSCP(s) ----
# use `unique(lookup$hscp2019name)` for all
# or create a vector for multiple e.g. `c("Angus", "West Lothian")`
# For a larger test, use the below to produce profiles for HSCPs likely to cause issues.
# source("Master RMarkdown Document & Render Code/find_hscp_outliers.R")
# hscp_list <- outlier_hscps
hscp_list <- "West Dunbartonshire"

# NOTE - This checks that it exactly matches the lookup
stopifnot(all(hscp_list %in% unique(lookup[["hscp2019name"]])))

# Create temporary local locations
temp_root <- file_temp(pattern = "lp-profile-build-")
dir_create(temp_root)

tmp_inputs_dir <- path(temp_root, "inputs")
tmp_docs_dir <- path(temp_root, "docs")

dir_create(c(tmp_inputs_dir, tmp_docs_dir))

local_lp_bookdown <- path(tmp_inputs_dir, "lp_bookdown")
dir_copy("lp_bookdown", local_lp_bookdown)

cover_page_path <- path(
  lp_path,
  "templates",
  "phs-mngtinfo-cover.docx"
)

tmp_cover_page_path <- path(tmp_inputs_dir, "phs-mngtinfo-cover.docx")
file_copy(cover_page_path, tmp_cover_page_path, overwrite = TRUE)

local_lp_bookdown <- file_temp(pattern = "lp_bookdown-")
dir_copy("lp_bookdown", local_lp_bookdown)

# Loop over HSCP ----
# 'looping' over one HSCP is fine.
for (HSCP in hscp_list) {
  # Create list of localities in chosen HSCP
  locality_list <- lookup |>
    filter(hscp2019name == HSCP) |>
    pull(hscp_locality)

  # Loop to create the profiles for all the localities in the list

  # There are several stages to the profiles:
  # 1. Looping through each locality in the HSCP doing the following:
  # 1a. Run each section script for that locality
  # 1b. Run the Rmd for the main body of the profiles
  # 1c. Run the Rmd for the summary tables

  loop_env <- c(ls(), "loop_env")

  # 1. Loop through each locality to create the main body of the profiles and the summary table
  for (LOCALITY in locality_list) {
    # 1a) Source in all the scripts for a given LOCALITY

    # Demographics ----
    source("Demographics/1. Demographics - Population.R")
    source("Demographics/2. Demographics - SIMD.R")

    # Housing ----
    source("Households/Households Code.R")

    # Services ----
    source("Services/2. Services data manipulation & table.R")
    source("Services/3. Service HSCP map.R")

    # Population Health ----
    source("Population Health/3. Population Health Outputs.R")

    # Lifestyle & Risk Factors ----
    source("Lifestyle & Risk Factors/2. Lifestyle & Risk Factors Outputs.R")

    # Unscheduled Care ----
    source("Unscheduled Care/2. Unscheduled Care outputs.R")

    # Appendices ----
    source("Master RMarkdown Document & Render Code/Tables for Appendix.R")

    main_title <- glue("{LOCALITY} - Locality Profile")
    safe_locality <- gsub("[/\\\\]", "-", LOCALITY)
    doc_title <- glue(safe_locality, "- Locality Profile")
    output_doc_name <- path_ext_set(doc_title, "docx")

    tmp_document_path <- path(tmp_docs_dir, output_doc_name)
    final_document_path <- path(output_dir, output_doc_name)

    bookdown::render_book(
      input = local_lp_bookdown,
      output_dir = tmp_docs_dir,
      output_file = output_doc_name,
      new_session = FALSE,
      output_format = "bookdown::word_document2",
      config_file = "_bookdown.yaml"
    )

    orient(tmp_document_path)

    add_cover_page(
      tmp_document_path,
      tmp_cover_page_path,
      main_title
    )

    apply_sensitivity_label(
      tmp_document_path,
      "OFFICIAL_SENSITIVE_VMO"
    )

    if (file_exists(final_document_path)) {
      file_delete(final_document_path)
    }

    file_move(
      tmp_document_path,
      final_document_path
    )
    # End of loop housekeeping ----
    # Clean up the environment by restoring it to the 'pre-loop' state.
    rm(list = setdiff(ls(), loop_env))
    # Force garbage collection to free up memory
    gc()
  }
}

if (dir_exists(temp_root)) {
  dir_delete(temp_root)
}
