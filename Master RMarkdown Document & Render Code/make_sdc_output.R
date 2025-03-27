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
output_dir <- path(lp_path, "Master RMarkdown Document & Render Code", "Output", "background data")

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
#for (HSCP in hscp_list) {
#  message(glue("Starting processing for HSCP: {HSCP} ({which(hscp_list == HSCP)}/{length(hscp_list)})"))
  # Create list of localities within the current HSCP
 # locality_list <- lookup |>
 #   filter(hscp2019name == HSCP) |>
 #   pull(hscp_locality)

  # Initialise empty lists to store data for all localities in the current HSCP
  smr01_based_all <- vector("list", length(hscp_list)) |>
    set_names(hscp_list)
  smr01_age_all <- vector("list", length(hscp_list)) |>
    set_names(hscp_list)
  ltc_all <- vector("list", length(hscp_list)) |>
    set_names(hscp_list)

  # Save the current environment variables before processing each HSCP
  # This is used to clear up variables created within the locality loop later
  loop_env <- c(ls(), "loop_env")

  for (HSCP in hscp_list) {
    message(glue("  Processing hscp: {HSCP} ({which(hscp_list == HSCP)}/{length(hscp_list)})) HSCP: {HSCP}"))
    # **Unscheduled Care Data Processing** ----
    # Extract and filter unscheduled care data for the current hscp
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

    smr01_based_loc <- imap(
      list(
        "Preventable_Admissions_PPA" = ppa_areas,
        "Emergency_Admissions" = emergency_adm_areas,
        "Unscheduled_Bed_Days" = bed_days_areas,
        "Readmissions" = readmissions_areas,
        "AE_attendances" = ae_att_areas,
        "Delayed_Discharges" = delayed_disch_areas,
        "Fall_Admissions" = falls_areas,
        "MH_bed_days" = bed_days_mh_areas
      ),
      \(data, name) {
        data |>
          filter(location == HSCP) |>
          select(financial_year, n = any_of(c("read_28", "n"))) |>
          mutate(
            HSCP = HSCP,
            name = name,
            .before = everything()
          ) |>
          filter(n < 10L) # Apply SDC filter (counts less than 10)
      }
    )

    smr01_age_loc <- imap(
      list(
        "Emergency_Admissions_Age" = filter(emergency_adm_age, hscp2019name == HSCP),
        "Unscheduled_Bed_Days_Age" = filter(bed_days_age, hscp2019name == HSCP),
        "Readmissions_Age" = readmissions_age,
        "AE_Attendances_Age" = filter(ae_att_age, hscp2019name == HSCP),
        "Bed_Days_MH_Age" = filter(bed_days_mh_age, hscp2019name == HSCP)
      ),
      \(data, name) {
        data |>
          select(
            financial_year,
            age_group,
            n = any_of(c("adm", "bed_days", "read_28", "attendances"))
          ) |>
          mutate(
            HSCP = HSCP,
            name = name,
            .before = everything()
          ) |>
          filter(n < 10L) # Apply SDC filter (counts less than 10)
      }
    )

    # Append locality-specific SMR01 data to the HSCP-level lists
    smr01_based_all[[HSCP]] <- smr01_based_loc |> list_rbind()
    smr01_age_all[[HSCP]] <- smr01_age_loc |> list_rbind()

    # Clear out Unscheduled Care data objects to free up memory
    rm(list = setdiff(ls(), c(loop_env, "HSCP")))
    gc() # Run garbage collection to further free up memory


    # **General Health Data Processing** ----
    # Extract and filter general health data for the current locality
    source("General Health/3. General Health Outputs.R")

    stopifnot(
      exists("ltc_multimorbidity"),
      exists("ltc_types"),
      exists("top5ltc_hscp")
    )

    ltc_loc <- imap(
      list(
        "Long_Term_Conditions" = ltc_multimorbidity,
        "LTC_Types" = ltc_types,
        "LTC_Top_5_Prevalence" = top5ltc_loc
      ),
      \(data, name) {
        data |>
          select(
            measure = any_of(c("total_ltc", "key", "Prevalence")),
            any_of("age_group"),
            n = any_of(c("people", "value"))
          ) |>
          mutate(
            HSCP = HSCP,
            name = name,
            .before = everything()
          ) |>
          filter(n < 10L) # Apply SDC filter (counts less than 10)
      }
    )

    # Append locality-specific LTC data to the HSCP-level list
    ltc_all[[HSCP]] <- ltc_loc |> list_rbind()

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
      output_dir,
      glue("{HSCP} - HSCP Profile SDC highlight.xlsx")
    )
  )

  message(glue("Finished processing for HSCP: {HSCP} - Excel file saved."))
} # End of HSCP loop

message("Script completed processing all HSCPs.") # Final completion message
