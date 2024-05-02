#############################################################################################.
#                                                                                           #
#                         LOCALITY PROFILES SERVICES MAP & TABLE CODE                       #
#                                                                                           #
#############################################################################################.

##Code used to manipulate services data for locality profiles.
# Also produces a table of what services are in the locality.
# The map is created in script "3. Services HSCP Map" - this is so that it does not have to run
# for every locality

## Written by C.Puech
## Created on 24/02/2020
## Latest update August 2022 - rewrote parts of code for smoother process

###### 1. Set up ######

##Load packages

library(tidyverse)
library(readxl)
library(janitor)
library(knitr)
library(gridExtra)
library(grid)
library(data.table)

#Change year to be the year in the data folder name
ext_year <- 2022

##Set Locality (for testing only)
#HSCP <- "Moray"

##Set file path
lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

#Source in functions code
source(paste0(lp_path, "Master RMarkdown Document & Render Code/Global Script.R"))


### Geographical lookups and objects ----

#Locality lookup
lookup <- read_in_localities(dz_level = T)

#Lookup without datazones
lookup2 <- read_in_localities()

##Determine HSCP
#HSCP <- as.character(filter(lookup2, hscp_locality == LOCALITY)$hscp2019name)

#Get number of localities in HSCP
n_loc <- lookup2 %>% 
  group_by(hscp2019name) %>% 
  summarise(locality_n = n()) %>% 
  filter(hscp2019name == HSCP) %>% 
  pull(locality_n)


###### 2. Read in services data ######

## Read in Postcode file for latitudes and longitudes

postcode_lkp <- read_in_postcodes() %>% 
  mutate(postcode = gsub(" ", "", pc7)) %>% 
  select(postcode, grid_reference_easting, grid_reference_northing, latitude, longitude, datazone2011, 
         hscp_locality, hscp2019name, hscp2019, hb2019name, hb2019)


## Read in all data in services folder

services_file_names <- list.files(paste0(lp_path, "Services/DATA ", ext_year), pattern = "RDS")

for (file in services_file_names) {
  
  name <- substr(x = file, 1, 4)
  
  data <- readRDS(paste0(lp_path, "Services/DATA ", ext_year, "/", file)) %>% 
    clean_names()
  
  assign(name, data)
  
}

#Change to more straightforward names
access_dep <- scot
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF, scot)


###### 3. Manipulate services data ######

## Access deprivtion ----
access_dep <- clean_scotpho_dat(access_dep) 

latest_year_access_dep <- max(access_dep$year)

access_dep_latest <- filter(access_dep, 
                            year == max(access_dep$year) & 
                              (area_name == HSCP & area_type == "HSCP"))$measure

## GP Practices ----

prac <- prac %>% 
  select(practice_code, gp_practice_name, practice_list_size, postcode) %>% 
  mutate(postcode = gsub(" ", "", postcode)) 

# Merge practice data with postcode and locality lookups
markers_gp <- left_join(prac, postcode_lkp, by = "postcode") %>% 
  mutate(type = "GP Practice") %>% 
  #filter out HSCP for map
  filter(hscp2019name == HSCP) 


## Emergency Departments and MIUs ----

hosp_lookup <- hosp_types %>% 
  filter(status == "Open") %>% 
  select(name = treatment_location_name, location = treatment_location_code, type = current_department_type) %>% 
  left_join(select(hosp_postcodes, location, postcode)) %>% 
  mutate(postcode = gsub(" ", "", postcode)) %>% 
  left_join(postcode_lkp, by = "postcode") 

# MIUs
markers_miu <- hosp_lookup %>% 
  filter(type == "Minor Injury Unit or Other") %>% 
  filter(hscp2019name == HSCP)

#EDs
markers_emergency_dep <- hosp_lookup %>% 
  filter(type == "Emergency Department") %>% 
  filter(hscp2019name == HSCP) 

Clacks_Royal <- hosp_lookup %>%
  filter(name == "Forth Valley Royal Hospital")

# Ninewells hospital is incorrectly mapped even though postcode ok - so corrected coords here

if(HSCP == "Dundee City"){
  markers_emergency_dep <- markers_emergency_dep %>% 
    mutate(latitude = if_else(latitude == 56.4617, 56.4659308, latitude),
           longitude = if_else(longitude == -2.991432, -3.0378506, longitude))
}

if(HSCP == "Clackmannanshire & Stirling"){
  markers_emergency_dep <- rbind(markers_emergency_dep, Clacks_Royal)
}

## Care Homes ----

markers_care_home <- care_homes %>% 
  select(type = care_service, subtype, name = service_name, service_postcode) %>% 
  filter(type == "Care Home Service") %>% 
  filter(subtype == "Older People") %>%  
  mutate(postcode = gsub(" ", "", service_postcode)) %>% 
  left_join(postcode_lkp, by = "postcode") %>% 
  filter(hscp2019name == HSCP)


###### 4. Table ######

# Subset care which is not Elderly care for table
other_care_type <- care_homes %>% 
  select(type = care_service, subtype, name = service_name, service_postcode) %>% 
  filter(type == "Care Home Service") %>% 
  filter(subtype != "Older People") %>%  
  mutate(postcode = gsub(" ", "", service_postcode)) %>% 
  left_join(postcode_lkp, by = "postcode") %>% 
  filter(hscp2019name == HSCP)

# Create table
services_tibble <- tibble(Type = c("**Primary Care**", "**A&E**", "", "**Care Home**", ""), 
                          Service = c("GP Practice", "Emergency Department", "Minor Injuries Unit", "Elderly Care", "Other"),
                          Number = c(nrow(filter(markers_gp, hscp2019name == HSCP)), 
                                     nrow(filter(markers_emergency_dep, hscp2019name == HSCP)), 
                                     nrow(filter(markers_miu, hscp2019name == HSCP)), 
                                     nrow(filter(markers_care_home, hscp2019name == HSCP)), 
                                     nrow(other_care_type)))


# detach(package:tidyverse, unload=TRUE)
# detach(package:janitor, unload=TRUE)
# detach(package:mapview, unload=TRUE)
# detach(package:data.table, unload=TRUE)
