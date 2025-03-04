##### LOCALITY PROFILES MASTER DOC RENDER CODE #####

library(readxl)
library(janitor)
library(tidyverse)
library(knitr)
library(markdown)
library(rmarkdown)
library(writexl)

rm(list = ls())

# system unmask function so files have read-write permissions
Sys.umask("006")

# Set file path
lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Source in functions code
source("Master RMarkdown Document & Render Code/Global Script.R")
lookup <- read_in_localities()
# Specify HSCP(s) ----
# use `unique(lookup$hscp2019name)` for all
# hscp_list <- unique(lookup$hscp2019name)
hscp_list <- "Angus"

# NOTE - This checks that it exactly matches the lookup
stopifnot(all(hscp_list %in% unique(lookup$hscp2019name)))

# Loop over HSCP ----
# 'looping' over one HSCP is fine.
for (HSCP in hscp_list) {
  # Create list of localities in chosen HSCP
  locality_list <- lookup |>
    filter(hscp2019name == HSCP) |>
    distinct(hscp_locality) |> 
    pull(hscp_locality)
  
  loop_env <- c(ls(), "loop_env")

  ## Loop to create the profiles for all the localities in the list


  # Determine the number of outputs
  # num_outputs <- length(demo_list(LOCALITY[[1]]))

  # Create an empty list to store dataframes for each output
  excel_output <- vector("list", length = 38)

  # excel_output <- list()

  # 2. Loop through each locality to create the appended excel output
  for (LOCALITY in locality_list) {
    ## 2a) Source in all the scripts for a given LOCALITY
    # demographics
    source("Demographics/1. Demographics - Population.R")
    source("Demographics/2. Demographics - SIMD.R")
    # lifestyle & risk factors
    source("Lifestyle & Risk Factors/2. Lifestyle & Risk Factors Outputs.R")

    # unscheduled care
    source("Unscheduled Care/2. Unscheduled Care outputs.R")

    # general health
    source("General Health/3. General Health Outputs.R")

    # housing
    source("Households/Households Code.R")

    # services
    source("Services/2. Services data manipulation & table.R")

    # Define data frames and their corresponding sheet names
    df <- list(
      "Population_Estimates" = pops[pops$hscp_locality == LOCALITY, ],
      "Population_Projections" = pop_proj_dat,
      "SIMD_Locality_2022" = simd_perc_breakdown,
      "SIMD_Domains_2016" = simd2016_dom,
      "SIMD_Domains_2022" = simd2020_dom,
      "Care_Home" = markers_care_home[markers_care_home$hscp_locality == LOCALITY, ],
      "Emergency_Dep" = markers_emergency_dep[markers_emergency_dep$hscp_locality == LOCALITY, ],
      "GP" = markers_gp[markers_gp$hscp_locality == LOCALITY, ],
      "Minor_Injuries_Unit" = markers_miu[markers_miu$hscp_locality == LOCALITY, ],
      "Housing_Data" = house_dat1,
      "Housing_Council_Tax_Band" = house_dat2,
      "Life_Expectancy" = life_exp[life_exp$area_name == LOCALITY, ],
      "Deaths_Aged_15_44" = deaths_15_44[deaths_15_44$area_name == LOCALITY, ],
      "Long_Term_Conditions" = ltc[ltc$hscp_locality == LOCALITY, ],
      "Cancer_Registrations" = cancer_reg[cancer_reg$area_name == LOCALITY, ],
      "Early_Deaths_Cancer" = early_deaths_cancer[early_deaths_cancer$area_name == LOCALITY, ],
      "Asthma_Hospitalisations" = asthma_hosp[asthma_hosp$area_name == LOCALITY, ],
      "CHD_Hospitalisations" = chd_hosp[chd_hosp$area_name == LOCALITY, ],
      "COPD_Hospitalisations" = copd_hosp[copd_hosp$area_name == LOCALITY, ],
      "MH_Prescriptions" = adp_presc[adp_presc$area_name == LOCALITY, ],
      "Alcohol_Admissions" = alcohol_hosp[alcohol_hosp$area_name == LOCALITY, ],
      "Alcohol_Deaths" = alcohol_deaths[alcohol_deaths$area_name == LOCALITY, ],
      "Drug_Admissions" = drug_hosp[drug_hosp$area_name == LOCALITY, ],
      "Bowel_Screening" = bowel_screening[bowel_screening$area_name == LOCALITY, ],
      "Emergency_Admissions" = emergency_adm_areas[emergency_adm_areas$location == LOCALITY, ],
      "Emergency_Admissions_Age " = emergency_adm_age[emergency_adm_age$hscp_locality == LOCALITY, ],
      "Unscheduled_Bed_Days" = bed_days_areas[bed_days_areas$location == LOCALITY, ],
      "Unscheduled_Bed_Days_Age" = bed_days_age[bed_days_age$hscp_locality == LOCALITY, ],
      "AE_attendances" = ae_att_areas[ae_att_areas$location == LOCALITY, ],
      "AE_attendances_Age" = ae_att_age[ae_att_age$hscp_locality == LOCALITY, ],
      "Readmissions" = readmissions_areas[readmissions_areas$location == LOCALITY, ],
      "Readmissions_Age" = readmissions_age,
      "Delayed_Discharge" = delayed_disch_areas[delayed_disch_areas$location == LOCALITY, ],
      "Fall_Admissions" = falls_areas[falls_areas$location == LOCALITY, ], # Need to add Scotland & HB to this
      "Preventable_admission_PPA" = ppa_areas[ppa_areas$location == LOCALITY, ], # Need to add Scotland & HB to this
      "psych_admissions" = psych_hosp[psych_hosp$area_name == LOCALITY, ], # Need to add Scotland & HB to this
      "MH_bed_days" = bed_days_mh_areas[bed_days_mh_areas$location == LOCALITY, ],
      "bed_days_mh_age" = bed_days_mh_age[bed_days_mh_age$hscp_locality == LOCALITY, ]
      
    )

    # Loop over each dataframe in the df list to add locality and append to the output list
    for (i in seq_along(df)) {
      # Get the current dataframe
      output <- df[[i]]

      # Add locality name to the output dataframe
      output$locality <- LOCALITY

      # Combine the current dataframe with existing dataframes for the same output
      excel_output[[i]] <- rbind(excel_output[[i]], output) # append(excel_output[[i]], output)
    }
  }

  wb <- openxlsx::createWorkbook()

  names(excel_output) <- names(df)

  excel_names <- names(df)



  # Loop over each combined dataframe
  for (i in seq_along(excel_output)) {
    # Get the current combined dataframe
    output <- excel_output[[i]]

    # Get the dataframe name
    dataframe_name <- excel_names[[i]]

    # Create a new sheet with the dataframe name as the sheet name
    openxlsx::addWorksheet(wb, sheetName = dataframe_name)

    # Write the combined dataframe to the current sheet
    openxlsx::writeData(wb, sheet = dataframe_name, x = output)
    
    openxlsx::setColWidths(wb, sheet = dataframe_name, cols = 1:ncol(output), widths = "auto")
    
  }

  index_data <- data.frame(Sheet_name = excel_names)

  excel_output <- c(index_data, excel_output)

  openxlsx::addWorksheet(wb, sheetName = "Index")

  openxlsx::writeData(wb, sheet = "Index", x = index_data)
  
  
  # Save the workbook to a file
  openxlsx::saveWorkbook(wb, paste0(lp_path, "Master RMarkdown Document & Render Code/Output/background data/", HSCP, " - Locality Profile data.xlsx"), overwrite = TRUE)
  rm(list = setdiff(ls(), loop_env))
  # Force garbage collection to free up memory
  gc()

}

