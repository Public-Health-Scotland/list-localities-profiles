# This script iterates through HSCPs and their localities.
# For each locality, it processes and extracts data related for:
#   - Unscheduled Care
#   - General Health
#
# The script identifying instances where counts are less than 10
# The processed data is then saved to separate Excel files for each HSCP,
# with sheets for SMR01 based indicators, SMR01 indicators by age group, and
# Long Term Conditions data for Statistical Disclosure Control - SDC.
rm(list = ls())

# Make sure files have read-write permissions
Sys.umask("006")

# Load Global packages and functions
source("Master RMarkdown Document & Render Code/Global Script.R")

# Set file path for output
lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Read in locality lookup table
lookup <- read_in_localities()

# Specify HSCP(s) to process ----
# By default, process all HSCPs.
# To process specific HSCPs, replace `unique(lookup$hscp2019name)`,
# with a vector of HSCP names e.g., `c("Angus", "West Lothian")`
hscp_list <- unique(lookup[["hscp2019name"]])

# Check: Ensure specified HSCP list exactly matches HSCP names in the lookup
stopifnot(all(hscp_list %in% unique(lookup[["hscp2019name"]])))

# Loop over HSCP ----
for (HSCP in hscp_list) {
  message(glue("Starting processing for HSCP: {HSCP} ({which(hscp_list == HSCP)}/{length(hscp_list)})"))
  # Create list of localities within the current HSCP
  locality_list <- lookup |>
    filter(hscp2019name == HSCP) |>
    pull(hscp_locality)

  # Initialise empty lists to store data for all localities in the current HSCP
  smr01_based_all <- vector("list", length(locality_list)) |>
    set_names(locality_list)
  smr01_age_all <- vector("list", length(locality_list)) |>
    set_names(locality_list)
  ltc_all <- vector("list", length(locality_list)) |>
    set_names(locality_list)

  # Save the current environment variables before processing each HSCP
  # This is used to clear up variables created within the locality loop later
  loop_env <- c(ls(), "loop_env")

  for (LOCALITY in locality_list) {
    message(glue("  Processing locality: {LOCALITY} ({which(locality_list == LOCALITY)}/{length(locality_list)})) HSCP: {HSCP}"))
    # **Unscheduled Care Data Processing** ----
    # Extract and filter unscheduled care data for the current locality
    source("Unscheduled Care/2. Unscheduled Care outputs.R")

    # **Data Validation - Unscheduled Care**
    stopifnot(
      exists("ppa_areas"),
      exists("emergency_adm_areas"),
      exists("bed_days_areas"),
      exists("readmissions_areas"),
      exists("ae_att_areas"),
      exists("delayed_disch_areas"),
      exists("falls_areas"),
      exists("bed_days_mh_areas"),
      exists("emergency_adm_age"),
      exists("bed_days_age"),
      exists("readmissions_age"),
      exists("ae_att_age"),
      exists("bed_days_mh_age")
    )

    smr01_based_loc <- pmap(
      list(
        data = list(
          ppa_areas,
          emergency_adm_areas,
          bed_days_areas,
          readmissions_areas,
          ae_att_areas,
          delayed_disch_areas,
          falls_areas,
          bed_days_mh_areas
        ),
        name = c(
          "PPA",
          "emergency admissions",
          "Bed days",
          "readmissions",
          "ae attendances",
          "Delayed Discharges",
          "Falls",
          "MH Bed Days"
        )
      ),
      \(data, name) {
        data |>
          filter(location == LOCALITY) |>
          select(financial_year, n = any_of(c("read_28", "n"))) |>
          mutate(
            locality = LOCALITY,
            name = name,
            .before = everything()
          ) |>
          filter(n < 10L) # Apply SDC filter (counts less than 10)
      }
    ) |>
      set_names(c(
        "Preventable_admission_PPA",
        "Emergency_Admissions",
        "Unscheduled_Bed_Days",
        "Readmissions",
        "AE_attendances",
        "Delayed_Discharge",
        "Fall_Admissions",
        "MH_bed_days"
      ))

    smr01_age_loc <- pmap(
      list(
        data = list(
          filter(emergency_adm_age, hscp_locality == LOCALITY),
          filter(bed_days_age, hscp_locality == LOCALITY),
          readmissions_age,
          filter(ae_att_age, hscp_locality == LOCALITY),
          filter(bed_days_mh_age, hscp_locality == LOCALITY)
        ),
        name = c(
          "emergency admissions age",
          "bed days age",
          "readmissions age",
          "AE attendances age",
          "mh bed days age"
        )
      ),
      \(data, name) {
        data |>
          select(
            financial_year,
            age_group,
            n = any_of(c("adm", "bed_days", "read_28", "attendances"))
          ) |>
          mutate(
            locality = LOCALITY,
            name = name,
            .before = everything()
          ) |>
          filter(n < 10L) # Apply SDC filter (counts less than 10)
      }
    ) |>
      set_names(c(
        "Emergency_Admissions_Age",
        "Unscheduled_Bed_Days_Age",
        "Readmissions_Age",
        "AE_attendances_Age",
        "bed_days_mh_age"
      ))

    # Append locality-specific SMR01 data to the HSCP-level lists
    smr01_based_all[[LOCALITY]] <- smr01_based_loc |> list_rbind()
    smr01_age_all[[LOCALITY]] <- smr01_age_loc |> list_rbind()

    # Clear out Unscheduled Care data objects to free up memory
    rm(list = setdiff(ls(), c(loop_env, "LOCALITY")))
    gc() # Run garbage collection to further free up memory


    # **General Health Data Processing** ----
    # Extract and filter general health data for the current locality
    source("General Health/3. General Health Outputs.R")

    stopifnot(
      exists("ltc_multimorbidity"),
      exists("ltc_types"),
      exists("top5ltc_loc")
    )

    ltc_loc <- pmap(
      list(
        data = list(
          ltc_multimorbidity,
          ltc_types,
          top5ltc_loc
        ),
        name = c(
          "ltc_multimorbidity",
          "ltc types",
          "top5ltc"
        )
      ),
      \(data, name) {
        data |>
          select(
            measure = any_of(c("total_ltc", "key", "Prevalence")),
            any_of("age_group"),
            n = any_of(c("people", "value"))
          ) |>
          mutate(
            locality = LOCALITY,
            name = name,
            .before = everything()
          ) |>
          filter(n < 10L) # Apply SDC filter (counts less than 10)
      }
    ) |>
      set_names(c("Long_Term_Conditions", "LTC_Types", "Top5_LTC"))

    # Append locality-specific LTC data to the HSCP-level list
    ltc_all[[LOCALITY]] <- ltc_loc |> list_rbind()

    # Clear out General Health data objects to free up memory
    rm(list = setdiff(ls(), loop_env))
    gc() # Run garbage collection
  } # End of LOCALITY loop

  # **Write data to Excel file** ----
  # Save the processed data for all localities in the HSCP to an Excel file
  save_dataframes_to_excel(
    dataframes = map(
      list(smr01_based_all, smr01_age_all, ltc_all),
      bind_rows # Combine lists of dataframes into single dataframes
    ),
    sheet_names = c("SMR01_based", "SMR01_Age_Groups", "LTCs_Age_Groups"),
    file_path = path(
      lp_path,
      "background data 2024",
      "SDC",
      glue("{HSCP} - Locality Profile SDC highlight.xlsx")
    )
  )

  message(glue("Finished processing for HSCP: {HSCP} - Excel file saved."))
} # End of HSCP loop

message("Script completed processing all HSCPs.") # Final completion message
