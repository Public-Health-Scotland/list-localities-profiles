##### LOCALITY PROFILES MASTER DOC RENDER CODE #####

library(readxl)
library(writexl)
library(openxlsx)

rm(list = ls())

# system unmask function so files have read-write permissions
Sys.umask("006")

# Set file path
lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Source in functions code
source("Master RMarkdown Document & Render Code/Global Script.R")

# Below creates locality list of all the localities in a chosen HSCP
lookup <- read_in_localities()

# Add HSCP name

HSCP <- "Orkney Islands"

# Create list of localities in chosen HSCP
locality_list <- lookup %>%
  filter(hscp2019name == HSCP) %>%
  pull(hscp_locality)


## Loop to create the relevant dataframes for all the localities in the list


# Initialize excel_output as a named list to store output data frames
df_names <- c(
  "Preventable_admission_PPA", "Emergency_Admissions",
  "Unscheduled_Bed_Days", "Readmissions",
  "AE_attendances", "Delayed_Discharge", "Fall_Admissions", "MH_bed_days"
)

excel_output <- setNames(vector("list", length(df_names)), df_names)

# 2. Loop through each locality to create the appended excel output
for (LOCALITY in locality_list) {
  ## 2a) Source in all the scripts for a given LOCALITY

  # unscheduled care
  source("./Unscheduled Care/2. Unscheduled Care outputs.R")

  ppa_areas <- ppa_areas |>
    mutate(name = "PPA")
  emergency_adm_areas <- emergency_adm_areas |>
    mutate(name = "emergency admissions")
  bed_days_areas <- bed_days_areas |>
    mutate(name = "Bed days")
  readmissions_areas <- readmissions_areas |>
    rename("n" = "read_28") |>
    mutate(name = "readmissions")
  ae_att_areas <- ae_att_areas |>
    mutate(name = "ae attendances")
  delayed_disch_areas <- delayed_disch_areas |>
    mutate(name = "Delayed Discharges")
  falls_areas <- falls_areas |>
    mutate(name = "Falls")
  bed_days_mh_areas <- bed_days_mh_areas |>
    mutate(name = "MH Bed Days")

  # Define data frames and their corresponding sheet names
  df <- list(
    "Preventable_admission_PPA" = ppa_areas[ppa_areas$location == LOCALITY, c("financial_year", "n", "name")],
    "Emergency_Admissions" = emergency_adm_areas[emergency_adm_areas$location == LOCALITY, c("financial_year", "n", "name")],
    "Unscheduled_Bed_Days" = bed_days_areas[bed_days_areas$location == LOCALITY, c("financial_year", "n", "name")],
    "Readmissions" = readmissions_areas[readmissions_areas$location == LOCALITY, c("financial_year", "n", "name")],
    "AE_attendances" = ae_att_areas[ae_att_areas$location == LOCALITY, c("financial_year", "n", "name")],
    "Delayed_Discharge" = delayed_disch_areas[delayed_disch_areas$location == LOCALITY, c("financial_year", "n", "name")],
    "Fall_Admissions" = falls_areas[falls_areas$location == LOCALITY, c("financial_year", "n", "name")],
    "MH_bed_days" = bed_days_mh_areas[bed_days_mh_areas$location == LOCALITY, c("financial_year", "n", "name")]
  )


  for (name in df_names) {
    # Get the current dataframe
    output <- df[[name]]

    # Add locality name to the output dataframe
    output$locality <- LOCALITY

    # Append the current dataframe to corresponding element in excel_output
    excel_output[[name]] <- rbind(excel_output[[name]], output)
  }
}


flag_low_numbers <- function(df) {
  df$flag <- ifelse(df[, 2] < 10, 1, 0)
  return(df)
}


# Apply the function to each dataframe in the list
flagged_dfs <- lapply(excel_output, flag_low_numbers)

# Extract rows flagged as 1 from each dataframe and combine them
flagged_rows1 <- do.call(rbind, lapply(flagged_dfs, function(df) {
  df[df$flag == 1, ]
})) |>
  ungroup() |>
  select("financial_year", "locality", "name", "n")



#### Second section extracting age breakdown small numbers#####

# Initialize excel_output as a named list to store output data frames
df_names2 <- c("Emergency_Admissions_Age", "Unscheduled_Bed_Days_Age", "Readmissions_Age", "AE_attendances_Age", "bed_days_mh_age")

excel_output2 <- setNames(vector("list", length(df_names2)), df_names2)


# 2. Loop through each locality to create the appended excel output
for (LOCALITY in locality_list) {
  ## 2a) Source in all the scripts for a given LOCALITY

  # unscheduled care
  source("./Unscheduled Care/2. Unscheduled Care outputs.R")
  emergency_adm_age <- emergency_adm_age |>
    rename("n" = "adm") |>
    mutate(name = "emergency admissions age") |>
    filter(financial_year != "2023/24")
  bed_days_age <- bed_days_age |>
    rename("n" = "bed_days") |>
    mutate(name = "bed days age") |>
    filter(financial_year != "2023/24")
  readmissions_age <- readmissions_age |>
    rename("n" = "read_28") |>
    mutate(name = "readmissions age") |>
    filter(financial_year != "2023/24")
  ae_att_age <- ae_att_age |>
    rename("n" = "attendances") |>
    mutate(name = "AE attendances age") |>
    filter(financial_year != "2023/24")
  bed_days_mh_age <- bed_days_mh_age |>
    rename("n" = "bed_days") |>
    mutate(name = "mh bed days age") |>
    filter(financial_year != "2023/24")

  # Define data frames and their corresponding sheet names
  df2 <- list( # "Long_Term_Conditions" = ltc[ltc$hscp_locality == LOCALITY,],
    "Emergency_Admissions_Age" = emergency_adm_age[emergency_adm_age$hscp_locality == LOCALITY, c("financial_year", "n", "age_group", "name")],
    "Unscheduled_Bed_Days_Age" = bed_days_age[bed_days_age$hscp_locality == LOCALITY, c("financial_year", "n", "age_group", "name")],
    "Readmissions_Age" = readmissions_age[, c("financial_year", "n", "age_group", "name")],
    "AE_attendances_Age" = ae_att_age[ae_att_age$hscp_locality == LOCALITY, c("financial_year", "n", "age_group", "name")],
    "bed_days_mh_age" = bed_days_mh_age[bed_days_mh_age$hscp_locality == LOCALITY, c("financial_year", "n", "age_group", "name")]
  )


  for (name in df_names2) {
    # Get the current dataframe
    output2 <- df2[[name]]

    # Add locality name to the output dataframe
    output2$locality <- LOCALITY

    # Append the current dataframe to corresponding element in excel_output
    excel_output2[[name]] <- rbind(excel_output2[[name]], output2)
  }
}


# Apply the function to each dataframe in the list
flagged_dfs2 <- lapply(excel_output2, flag_low_numbers)

# Extract rows flagged as 1 from each dataframe and combine them
flagged_rows2 <- do.call(rbind, lapply(flagged_dfs2, function(df) {
  df[df$flag == 1, ]
})) |>
  ungroup() |>
  select("locality", "financial_year", "name", "age_group", "n")


#### Flag LTCs and save out to excel####



# Initialize excel_output as a named list to store output data frames
df_names3 <- c("Long_Term_Conditions", "LTC_Types", "Top5_LTC")

excel_output3 <- setNames(vector("list", length(df_names3)), df_names3)


# 2. Loop through each locality to create the appended excel output
for (LOCALITY in locality_list) {
  ## 2a) Source in all the scripts for a given LOCALITY

  # general health
  source("./General Health/3. General Health Outputs.R")
  ltc_multimorbidity <- ltc_multimorbidity |>
    rename(
      "n" = "people",
      "measure" = "total_ltc"
    ) |>
    mutate(name = "ltc_multimorbidity")

  ltc_types <- ltc_types |>
    rename(
      "n" = "value",
      "measure" = "key"
    ) |>
    mutate(name = "ltc types")

  top5ltc_loc <- top5ltc_loc |>
    rename(
      "n" = "value",
      "measure" = "Prevalence"
    ) |>
    mutate(
      name = "top5ltc",
      age_group = "all ages"
    )



  # Define data frames and their corresponding sheet names
  df3 <- list(
    "Long_Term_Conditions" = ltc_multimorbidity[, c("measure", "n", "age_group", "name")],
    "LTC_Types" = ltc_types[, c("measure", "n", "age_group", "name")],
    "Top5_LTC" = top5ltc_loc[, c("measure", "n", "age_group", "name")]
  )




  for (name in df_names3) {
    # Get the current dataframe
    output3 <- df3[[name]]

    # Add locality name to the output dataframe
    output3$locality <- LOCALITY

    # Append the current dataframe to corresponding element in excel_output
    excel_output3[[name]] <- rbind(excel_output3[[name]], output3)
  }
}



# Apply the function to each dataframe in the list
flagged_dfs3 <- lapply(excel_output3, flag_low_numbers)

# Extract rows flagged as 1 from each dataframe and combine them
flagged_rows3 <- do.call(rbind, lapply(flagged_dfs3, function(df) {
  df[df$flag == 1, ]
})) |>
  select("locality", "name", "measure", "age_group", "n")

### Write to excel



# Define a function to save multiple dataframes to an Excel workbook
save_dataframes_to_excel <- function(dataframes, sheet_names, file_name) {
  # Create a new workbook
  wb <- createWorkbook()

  # Loop over each dataframe and corresponding sheet name
  for (i in seq_along(dataframes)) {
    # Get the current dataframe
    output <- dataframes[[i]]

    # Get the sheet name
    dataframe_name <- sheet_names[[i]]

    # Create a new sheet with the dataframe name as the sheet name
    addWorksheet(wb, sheetName = dataframe_name)

    # Write the dataframe to the current sheet
    writeData(wb, sheet = dataframe_name, x = output)
  }

  # Save the workbook to a file
  saveWorkbook(wb, file_name, overwrite = TRUE)
}

# List of dataframes
excel_output <- list(flagged_rows1, flagged_rows2, flagged_rows3)

# Corresponding sheet names
excel_names <- c("SMR01_based", "SMR01_Age_Groups", "LTCs_Age_Groups")

# Call the function to save the dataframes to an Excel file
save_dataframes_to_excel(excel_output, excel_names, paste0("/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/background data/SDC/", HSCP, ".xlsx"))


# Save the workbook to a file
openxlsx::saveWorkbook(wb, paste0("/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/background data/", HSCP, ".xlsx"), overwrite = TRUE)
