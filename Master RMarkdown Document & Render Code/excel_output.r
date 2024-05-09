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

# Below creates locality list of all the localities in a chosen HSCP
lookup <- read_in_localities()

#just for testing. 
HSCP_list <- c("Orkney Islands")#unique(lookup$hscp2019name)

# Create list of localities in chosen HSCP
locality_list <- lookup %>%
  filter(hscp2019name %in% HSCP_list) %>%
  pull(hscp_locality)


## Loop to create the profiles for all the localities in the list


# Determine the number of outputs
#num_outputs <- length(demo_list(LOCALITY[[1]]))

# Create an empty list to store dataframes for each output
excel_output <- vector("list", length = 38)

#excel_output <- list()

# 2. Loop through each locality to create the appended excel output
for (LOCALITY in locality_list) {
  ## 2a) Source in all the scripts for a given LOCALITY
  
  # demographics
  source("Demographics/1. Demographics - Population.R")
  source("./Demographics/2. Demographics - SIMD.R")
  # lifestyle & risk factors
  source("./Lifestyle & Risk Factors/2. Lifestyle & Risk Factors Outputs.R")
  
  # unscheduled care
  source("./Unscheduled Care/2. Unscheduled Care outputs.R")
  
  # general health
  source("./General Health/3. General Health Outputs.R")
  
  # housing
  source("./Households/Households Code.R")
  
  # services
  source("./Services/2. Services data manipulation & table.R")
  
  # Define data frames and their corresponding sheet names
  df <- list(
    "Ch1_Population_Estimates" = pops[pops$hscp_locality == LOCALITY,],
    "Ch1_Population_Projections" = pop_proj_dat,
    "Ch1_SIMD_Locality_2021" = simd_perc_breakdown,
    "Ch1_SIMD_Domains_2016" = simd2016_dom,
    "Ch1_SIMD_Domains_2021" = simd2020_dom,
    "Ch_Alcohol_Admissions_2122" = alcohol_hosp[alcohol_hosp$area_name == LOCALITY,],
    "Ch_Alcohol_Deaths_2017-2021" = alcohol_deaths[alcohol_deaths$area_name == LOCALITY,],
    "Ch_Drug_Admissions_1920-2122" = drug_hosp[drug_hosp$area_name == LOCALITY,],
    "Ch_Bowel_Screening_2019-2021" = bowel_screening[bowel_screening$area_name == LOCALITY,],
    "Life_Expectancy" = life_exp[life_exp$area_name == LOCALITY,],
    "Deaths_Aged_15_44" = deaths_15_44[deaths_15_44$area_name == LOCALITY,],
    "Cancer_Registrations" = cancer_reg[cancer_reg$area_name == LOCALITY,],
    "Early_Deaths_Cancer" = early_deaths_cancer[early_deaths_cancer$area_name == LOCALITY,],
    "Asthma_Hospitalisations" = asthma_hosp[asthma_hosp$area_name == LOCALITY,],
    "CHD_Hospitalisations" = chd_hosp[chd_hosp$area_name == LOCALITY,],
    "COPD_Hospitalisations" = copd_hosp[copd_hosp$area_name == LOCALITY,],
    "MH_Prescritpions" = adp_presc[adp_presc$area_name == LOCALITY,],
    "Long_Term_Conditions" = ltc[ltc$hscp_locality == LOCALITY,],
    "Emergency_Admissions" = emergency_adm_areas[emergency_adm_areas$location == LOCALITY,],
    "Emergency_Admissions_Age " = emergency_adm_age[emergency_adm_age$hscp_locality == LOCALITY,],
    "Unscheduled_Bed_Days" = bed_days_areas[bed_days_areas$location == LOCALITY,],
    "Unscheduled_Bed_Days_Age" = bed_days_age[bed_days_age$hscp_locality ==LOCALITY,],
    "Readmissions" = readmissions_areas[readmissions_areas$location == LOCALITY,],
    "Readmissions_Age"= readmissions_age,
    "AE_attendances" = ae_att_areas[ae_att_areas$location == LOCALITY,],
    "AE_attendances_Age" =  ae_att_age[ae_att_age$hscp_locality ==LOCALITY,],
    "Delayed_Discharge" =  delayed_disch_areas[delayed_disch_areas$location == LOCALITY,],
    "Fall_Admissions" = falls_areas[falls_areas$location == LOCALITY,], #Need to add Scotland & HB to this 
    "Preventable_admission_PPA" = ppa_areas[ppa_areas$location == LOCALITY,],#Need to add Scotland & HB to this
    "psych_admissions" = psych_hosp[psych_hosp$area_name == LOCALITY,],#Need to add Scotland & HB to this
    "MH_bed_days" = bed_days_mh_areas[bed_days_mh_areas$location == LOCALITY,],
    "bed_days_mh_age" =  bed_days_mh_age[bed_days_mh_age$hscp_locality ==LOCALITY,],
    "Housing_Data" = house_dat1,
    "Housing_Council_Tax_Band" = house_dat2,
    "Care_Home" = markers_care_home[markers_care_home$hscp_locality == LOCALITY,],
    "Emergency_Dep" = markers_emergency_dep[markers_emergency_dep$hscp_locality == LOCALITY,],
    "GP" = markers_gp[markers_gp$hscp_locality == LOCALITY,],
    "Minor_Injuries_Unit" = markers_miu[markers_miu$hscp_locality == LOCALITY,]

  )

  # Loop over each dataframe in the df list to add locality and append to the output list
 for (i in seq_along(df)) {
    # Get the current dataframe
   output <- df[[i]]
    
    # Add locality name to the output dataframe
    output$locality <- LOCALITY
   
    # Combine the current dataframe with existing dataframes for the same output
    excel_output[[i]] <- rbind(excel_output[[i]], output) #append(excel_output[[i]], output)
    
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
  dataframe_name <-  excel_names[[i]]
  
  # Create a new sheet with the dataframe name as the sheet name
  openxlsx::addWorksheet(wb, sheetName = dataframe_name)
  
  # Write the combined dataframe to the current sheet
  openxlsx::writeData(wb, sheet = dataframe_name, x = output)
}

index_data <- data.frame(Sheet_name = excel_names)

excel_output <- c(index_data, excel_output)

openxlsx::addWorksheet(wb, sheetName = "Index")

openxlsx::writeData(wb, sheet = 'Index', x = index_data)


# Save the workbook to a file
openxlsx::saveWorkbook(wb, "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/background data/output.xlsx", overwrite = TRUE)



#SDC Check 

SDC <- list( #"Long_Term_Conditions" = ltc[ltc$hscp_locality == LOCALITY,],
"Emergency_Admissions" = emergency_adm_areas[emergency_adm_areas$location == LOCALITY,],
"Emergency_Admissions_Age " = emergency_adm_age[emergency_adm_age$hscp_locality == LOCALITY,],
"Unscheduled_Bed_Days" = bed_days_areas[bed_days_areas$location == LOCALITY,],
"Unscheduled_Bed_Days_Age" = bed_days_age[bed_days_age$hscp_locality ==LOCALITY,],
"Readmissions" = readmissions_areas[readmissions_areas$location == LOCALITY,],
"Readmissions_Age"= readmissions_age,
"AE_attendances" = ae_att_areas[ae_att_areas$location == LOCALITY,],
"AE_attendances_Age" =  ae_att_age[ae_att_age$hscp_locality ==LOCALITY,],
"Delayed_Discharge" =  delayed_disch_areas[delayed_disch_areas$location == LOCALITY,],
"Fall_Admissions" = falls_areas[falls_areas$location == LOCALITY,], 
"MH_bed_days" = bed_days_mh_areas[bed_days_mh_areas$location == LOCALITY,],
"bed_days_mh_age" =  bed_days_mh_age[bed_days_mh_age$hscp_locality ==LOCALITY,])





# housing
#source("./Households/Households Code.R")

# services
# source("./Services/2. Services data manipulation & table.R")

# general health
# source("./General Health/3. General Health Outputs.R")

# lifestyle & risk factors
# source("./Lifestyle & Risk Factors/2. Lifestyle & Risk Factors Outputs.R")

# unscheduled care
# source("./Unscheduled Care/2. Unscheduled Care outputs.R")

# Remove tidylog package which messes up outputs
#detach(package:tidylog, unload = TRUE)

# Define data frames and their corresponding sheet names

## 2b) save and append to master excel output 

#append_to_master_excel("/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/background data/Data Demographics.xlsx", demo_list)
##add front end function
##tidy up file path, or function for creating workbook 
##use readxl::readsheets 
#existing_data <- openxlsx::getSheetNames("/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/background data/Data Demographics.xlsx")
