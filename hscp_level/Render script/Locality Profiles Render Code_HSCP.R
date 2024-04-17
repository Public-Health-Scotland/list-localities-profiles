##### LOCALITY PROFILES MASTER DOC RENDER CODE #####

library(readxl)
library(janitor)
library(knitr)
library(markdown)
library(rmarkdown)

rm(list = ls())

# system unmask function so files have read-write permissions
Sys.umask("006")

#Set file path
lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

#Source in functions code
source(paste0(lp_path, "Master RMarkdown Document & Render Code/Global Script.R"))

##Specify HSCP here
## NOTE - make sure that the formatting of the partnership's name matches the lookup  
HSCP <- "Western Isles"

#Below creates locality list of all the localities in a chosen HSCP
lookup <- read_in_localities() 

#HSCP_list <- unique(lookup$hscp2019name)

#Create list of localities in chosen HSCP
#locality_list <- lookup %>% 
 # filter(hscp2019name == HSCP) %>% 
 # pull(hscp_locality)

other_locs <- lookup %>% 
  select(hscp_locality, hscp2019name) %>% 
  filter(hscp2019name == HSCP) %>% 
  arrange(hscp_locality)


##Loop to create the profiles for all the localities in the list

## There are several stages to the profiles:
    # 1. Producing the HSCP services map (this takes a while to run so it is produced separately)
    # 2. Looping through each locality in the HSCP doing the following:
          #2a. Run each section script for that locality
          #2b. Run the Rmd for the main body of the profiles
          #2c. Run the Rmd for the summary tables

# 1. HSCP Services Map
#source(paste0(lp_path, "Services/Scripts/3. Service HSCP map.R"))


# 2. Loop through each locality to create the main body of the profiles and the summary table
#for (LOCALITY in locality_list){
  
  ## 2a) Source in all the scripts for a given LOCALITY
  
  #demographics
  source(paste0(lp_path, "Demographics/Scripts/HSCP level/1. Demographics - Population.R"))
  source(paste0(lp_path, "Demographics/Scripts/HSCP level/2. Demographics - SIMD.R"))
  
  #housing
  source(paste0(lp_path, "Households/Scripts/HSCP level/Households code.R"))
  
  #services
  source(paste0(lp_path, "Services/Scripts/HSCP level/2. Services data manipulation & table.R"))

  #general health
  source(paste0(lp_path, "General Health/Scripts/HSCP level/3. General Health Outputs.R"))
  
  #lifestyle & risk factors
  source(paste0(lp_path, "Lifestyle & Risk Factors/Scripts/HSCP level/2. Lifestyle & Risk Factors Outputs.R"))
  
  #unscheduled care
  source(paste0(lp_path, "Unscheduled Care/Scripts/HSCP level/2. Unscheduled Care outputs.R"))
  
  #appendices
  source(paste0(lp_path, "Master RMarkdown Document & Render Code/Tables for Appendix.R"))
  
  # Remove tidylog package which messes up outputs
  detach(package:tidylog, unload=TRUE)

  ## 2b) Create the main body of the profiles
  rmarkdown::render(paste0(lp_path, "Master RMarkdown Document & Render Code/HSCP level/Locality_Profiles_Master_Markdown_HSCP.Rmd"),
                    output_file =  paste0(HSCP, " - Locality Profile.docx"),
                    output_dir = paste0(lp_path, "Master RMarkdown Document & Render Code/Output/"))
  
  ## 2c) Create the summary tables
  #rmarkdown::render(paste0(lp_path, "Summary Table/Summary-Table-Markdown - HSCP.Rmd"),
                   # output_file = paste0(HSCP, " - Summary Table.docx"),
                   # output_dir = paste0(lp_path, "Master RMarkdown Document & Render Code/Output/Summary Tables/"))
#}

 