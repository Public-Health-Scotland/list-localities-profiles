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

#Add HSCP name
HSCP_list <- c("Orkney Islands")#unique(lookup$hscp2019name)

# Create list of localities in chosen HSCP
locality_list <- lookup %>%
  filter(hscp2019name %in% HSCP_list) %>%
  pull(hscp_locality)


## Loop to create the profiles for all the localities in the list


#excel_output <- vector("list", length = 14)
#"Emergency_Admissions_Age","Unscheduled_Bed_Days_Age","Readmissions_Age","AE_attendances_Age",, "bed_days_mh_age"
#excel_output <- list()

# Initialize excel_output as a named list to store output data frames
df_names <- c("Preventable_admission_PPA", "Emergency_Admissions",
              "Unscheduled_Bed_Days", "Readmissions", 
              "AE_attendances", "Delayed_Discharge", "Fall_Admissions", "MH_bed_days")

excel_output <- setNames(vector("list", length(df_names)), df_names)

# 2. Loop through each locality to create the appended excel output
for (LOCALITY in locality_list) {
  ## 2a) Source in all the scripts for a given LOCALITY
  
  # unscheduled care
  source("./Unscheduled Care/2. Unscheduled Care outputs.R")  
    
    ppa_areas <- ppa_areas |> 
      mutate(name = 'PPA')
    emergency_adm_areas <- emergency_adm_areas |> 
      mutate(name = 'emergency admissions')
    bed_days_areas <- bed_days_areas |> 
      mutate(name = 'Bed days')
    readmissions_areas <- readmissions_areas |> 
      rename('n' = 'read_28') |> 
      mutate(name = 'readmissions')
    ae_att_areas <- ae_att_areas |> 
      mutate(name = 'ae attendances')
    delayed_disch_areas <- delayed_disch_areas |> 
      mutate(name = 'Delayed Discharges')
    falls_areas <- falls_areas |> 
      mutate(name = 'Falls')
    bed_days_mh_areas <- bed_days_mh_areas |> 
      mutate(name = 'MH Bed Days')
  
  # general health
  source("./General Health/3. General Health Outputs.R")
  

  # Define data frames and their corresponding sheet names
  df <- list( #"Long_Term_Conditions" = ltc[ltc$hscp_locality == LOCALITY,],
                 "Preventable_admission_PPA" = ppa_areas[ppa_areas$location == LOCALITY,c("financial_year","n",'name')],
                 "Emergency_Admissions" = emergency_adm_areas[emergency_adm_areas$location == LOCALITY,c("financial_year","n",'name')],
                 #"Emergency_Admissions_Age" = emergency_adm_age[emergency_adm_age$hscp_locality == LOCALITY,c("financial_year","adm","age_group")],
                 "Unscheduled_Bed_Days" = bed_days_areas[bed_days_areas$location == LOCALITY,c("financial_year","n",'name')],
                # "Unscheduled_Bed_Days_Age" = bed_days_age[bed_days_age$hscp_locality ==LOCALITY,c("financial_year","bed_days","age_group")],
                 "Readmissions" = readmissions_areas[readmissions_areas$location == LOCALITY,c("financial_year","n",'name')],
                 #"Readmissions_Age"= readmissions_age[,c("financial_year","read_28","age_group")],
                 "AE_attendances" = ae_att_areas[ae_att_areas$location == LOCALITY,c("financial_year","n",'name')],
                 #"AE_attendances_Age" =  ae_att_age[ae_att_age$hscp_locality ==LOCALITY,c("financial_year","attendances","age_group")],
                 "Delayed_Discharge" =  delayed_disch_areas[delayed_disch_areas$location == LOCALITY,c("financial_year","n",'name')],
                 "Fall_Admissions" = falls_areas[falls_areas$location == LOCALITY,c("financial_year","n",'name')], 
                 "MH_bed_days" = bed_days_mh_areas[bed_days_mh_areas$location == LOCALITY,c("financial_year","n",'name')])#,)
                 #"bed_days_mh_age" =  bed_days_mh_age[bed_days_mh_age$hscp_locality ==LOCALITY,c("financial_year","bed_days","age_group")])
    

  
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
}))

####Second section extracting age breakdown small numbers

# Initialize excel_output as a named list to store output data frames
df_names2 <- c("Emergency_Admissions_Age","Unscheduled_Bed_Days_Age","Readmissions_Age","AE_attendances_Age", "bed_days_mh_age")

excel_output2 <- setNames(vector("list", length(df_names2)), df_names2)


# 2. Loop through each locality to create the appended excel output
for (LOCALITY in locality_list) {
  ## 2a) Source in all the scripts for a given LOCALITY
  
  # unscheduled care
  source("./Unscheduled Care/2. Unscheduled Care outputs.R")  
  emergency_adm_age <- emergency_adm_age |> 
    rename('n' = 'adm') |> 
    mutate(name = 'emergency admissions age') |> 
    filter(financial_year != '2023/24')
  bed_days_age <- bed_days_age|> 
    rename('n' = 'bed_days') |> 
    mutate(name = 'bed days age')|> 
    filter(financial_year != '2023/24')
  readmissions_age <- readmissions_age|> 
    rename('n' = 'read_28') |> 
    mutate(name = 'readmissions age')|> 
    filter(financial_year != '2023/24')
  ae_att_age <- ae_att_age|> 
    rename('n' = 'attendances') |> 
    mutate(name = 'AE attendances age')|> 
    filter(financial_year != '2023/24')
  bed_days_mh_age <- bed_days_mh_age|> 
    rename('n' = 'bed_days') |> 
    mutate(name = 'mh bed days age')|> 
    filter(financial_year != '2023/24')
 
  # Define data frames and their corresponding sheet names
  df2 <- list( #"Long_Term_Conditions" = ltc[ltc$hscp_locality == LOCALITY,],
     "Emergency_Admissions_Age" = emergency_adm_age[emergency_adm_age$hscp_locality == LOCALITY,c("financial_year","n","age_group","name")],
     "Unscheduled_Bed_Days_Age" = bed_days_age[bed_days_age$hscp_locality ==LOCALITY,c("financial_year","n","age_group","name")],
     "Readmissions_Age"= readmissions_age[,c("financial_year","n","age_group","name")],
     "AE_attendances_Age" =  ae_att_age[ae_att_age$hscp_locality ==LOCALITY,c("financial_year","n","age_group","name")],
     "bed_days_mh_age" =  bed_days_mh_age[bed_days_mh_age$hscp_locality ==LOCALITY,c("financial_year","n","age_group","name")])
  
  
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
}))


####Got to here, need to flag LTCs and save out to excel
###save out to excel workbook 


wb <- openxlsx::createWorkbook()

names(excel_output) <- names(df)

flagged_rows <- names(df)

names(excel_output2) <- names(df2)

excel_names2 <- names(df2)

excel_names <- c(excel_names1,excel_names2)

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
openxlsx::saveWorkbook(wb, paste0("/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/background data/SDC/",HSCP,".xlsx"), overwrite = TRUE)
