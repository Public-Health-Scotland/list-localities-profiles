##################### LOCALITY PROFILES UNSCHEDULED CARE: DATA EXTRACTION ######################.

# Original author: Will Clayton
# Updated Oct/Nov 2022 by Adam Rennie to use Global Script functions
# Last edits December 2022 by C Puech to improve data quality and process
    # All indicators for which MSG files are available now directly use MSG data


####################### SECTION 1: Packages, file paths, lookups, etc #########################

## Manually set year that the profiles are being run (extract year)
ext_year <- 2022

## Manually set the name of the latest MSG folder
latest_msg_folder <- "2022-10 October"

#Set locality profiles file path
lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

##Packages
library(tidyverse)
library(janitor)
library(tidylog)
library(magrittr)
library(lubridate)
library(scales)
library(broom)
library(reshape2)
library(haven)
library(fst)
library(odbc)

##Functions
source(paste0(lp_path,"Master RMarkdown Document & Render Code/Global Script.R"))

#Read/write permissions
Sys.umask("006")

Sys.getenv("R_ZIPCMD", "zip")

# Folder to export to
exportfolder <- "Unscheduled Care/DATA ", ext_year, "/"


## Lookups ----

# Postcodes
postcodes <- read_in_postcodes() %>% 
  rename(postcode = pc8) 

# Localities/Datazones
datazones <- read_in_localities(dz_level = T)
localities <- read_in_localities()


########################## SECTION 2: MSG Data ###############################

# Read in MSG data
msg_emerg_adm_raw <- read_sav(paste0("/conf/LIST_analytics/MSG/", latest_msg_folder, 
                                     "/Breakdowns/1a-Admissions-breakdown.sav")) 

msg_beddays_raw <- read_sav(paste0("/conf/LIST_analytics/MSG/", latest_msg_folder, 
                                   "/Breakdowns/2a-Acute-Beddays-breakdown.sav"))

msg_ae_raw <- read_sav(paste0("/conf/LIST_analytics/MSG/", latest_msg_folder, 
                              "/Breakdowns/3-A&E-Breakdowns.sav")) 

msg_dd_raw <- read_sav(paste0("/conf/LIST_analytics/MSG/", latest_msg_folder, 
                              "/Breakdowns/4-Delayed-Discharge-Breakdowns.sav"))

msg_mh_beddays_raw <- read_sav(paste0("/conf/LIST_analytics/MSG/", latest_msg_folder, 
                                      "/Breakdowns/2c-MH-Beddays-breakdown.sav"))


# 1. Emergency Admissions ----
#_________________________________________________________________________

msg_emergency_adm <- msg_emerg_adm_raw %>% 
  mutate(age_group = age_group_1(age_group)) %>%
  mutate(financial_year = fy(month)) %>%
  mutate(hscp_locality = gsub("&", "and", locality)) %>%
  
  #join with localities lookup to get hscp
  left_join(localities, by = "hscp_locality") %>% 
  
  #aggregate
  group_by(financial_year, hscp2019name, hscp_locality, age_group) %>%
  summarise(admissions = sum(admissions)) %>%
  ungroup()


# 2a. Unscheduled bed days ----
#_________________________________________________________________________

msg_bed_days <- msg_beddays_raw %>% 
  mutate(age_group = age_group_1(age_group)) %>%
  mutate(financial_year = fy(month)) %>%
  mutate(hscp_locality = gsub("&", "and", locality)) %>%
  
  #join with localities lookup to get hscp
  left_join(localities, by = "hscp_locality") %>% 
  
  #aggregate
  group_by(financial_year,hscp2019name,hscp_locality,age_group) %>%
  summarise(bed_days = sum(unplanned_beddays)) %>%
  ungroup()


# 2b. Unscheduled bed days - Mental Health ----
#_________________________________________________________________________

msg_bed_days_mh <- msg_mh_beddays_raw %>% 
  mutate(age_group = age_group_1(age_group)) %>%
  mutate(financial_year = fy(month)) %>%
  mutate(hscp_locality = gsub("&", "and", locality)) %>%
  
  #join with localities lookup to get hscp
  left_join(localities, by = "hscp_locality") %>% 
  
  #aggregate
  group_by(financial_year, hscp2019name, hscp_locality, age_group) %>%
  summarise(bed_days = sum(unplanned_beddays)) %>%
  ungroup()


# 3. A&E Attendances ----
#_________________________________________________________________________

msg_ae <- msg_ae_raw %>%
  mutate(age_group = age_group_1(age_group)) %>%
  mutate(financial_year = fy(month)) %>%
  mutate(hscp_locality = gsub("&", "and", locality)) %>%
  
  #join with localities lookup to get hscp
  left_join(localities, by = "hscp_locality") %>% 
  
  #aggregate
  group_by(financial_year, hscp2019name, hscp_locality, age_group) %>%
  summarise(attendances = sum(attendances)) %>%
  ungroup()



# 4. Delayed Discharges ----
#_________________________________________________________________________

msg_dd <- msg_dd_raw %>%
  mutate(age_group = age_group_1(age_group)) %>%
  mutate(financial_year = fy(month)) %>%
  mutate(hscp_locality = gsub("&", "and", locality)) %>%
  
  #this data set has some data with partnership but no locality, need to tidy names
  mutate(hscp2019name = gsub("&", "and", council)) %>%
  mutate(hscp2019name = ptsp(hscp2019name)) %>%
  
  group_by(financial_year, hscp2019name, hscp_locality, age_group, reason_for_delay) %>% 
  summarise(dd_people = n(),
            dd_bed_days = sum(delayed_bed_days)) %>%
  ungroup()


## Save Data ----
#_________________________________________________________________________


# Emergency admissions
saveRDS(msg_emergency_adm, paste0(exportfolder, "emergency_admissions_msg.rds"))

# Bed days
saveRDS(msg_bed_days, paste0(exportfolder, "bed_days_msg.rds"))

# Bed days MH
saveRDS(msg_bed_days_mh, paste0(exportfolder, "bed_days_mh_msg.rds"))

# A&E
saveRDS(msg_ae, paste0(exportfolder, "ae_attendances_msg.rds"))

# Delayed discharges
saveRDS(msg_dd, paste0(exportfolder, "delayed_discharges_msg.rds"))



############################# SECTION 3: SMR Data ###################################

## Extract SMR data ----
#_________________________________________________________________________

## SMR Queries


query_smr1 = paste("SELECT ADMISSION_DATE, ADMISSION, ADMISSION_TYPE, OLD_SMR1_TADM_CODE, CIS_MARKER, COUNCIL_AREA_2019, LOCATION, DATAZONE_2011, 
                   DISCHARGE_DATE, DISCHARGE, DISCHARGE_TYPE, MAIN_CONDITION, AGE_IN_YEARS, INPATIENT_DAYCASE_IDENTIFIER, 
                   OTHER_CONDITION_1,OTHER_CONDITION_2, OTHER_CONDITION_3,OTHER_CONDITION_4, OTHER_CONDITION_5, 
                   LINK_NO, LENGTH_OF_STAY, RECORD_TYPE,URI, SORT_MARKER, UPI_NUMBER",
                   "FROM ANALYSIS.SMR01_PI WHERE (DISCHARGE_DATE >= TO_DATE('2016-04-01','YYYY-MM-DD'))")

# query_smr1e = paste("SELECT ADMISSION_DATE, ADMISSION, ADMISSION_TYPE, OLD_SMR1_TADM_CODE, CIS_MARKER, COUNCIL_AREA_2019, LOCATION, DATAZONE_2011, 
#                     DISCHARGE_DATE, DISCHARGE, DISCHARGE_TYPE, MAIN_CONDITION, AGE_IN_YEARS,INPATIENT_DAYCASE_IDENTIFIER, 
#                     OTHER_CONDITION_1,OTHER_CONDITION_2, OTHER_CONDITION_3,OTHER_CONDITION_4, OTHER_CONDITION_5, 
#                     LINK_NO, LENGTH_OF_STAY, RECORD_TYPE,URI, SORT_MARKER, UPI_NUMBER",
#                     "FROM ANALYSIS.SMR01_1E_PI WHERE (DISCHARGE_DATE >= TO_DATE('2016-04-01','YYYY-MM-DD'))")

# query_smr4 = paste("SELECT PATIENT_IDENTIFIER, ADMISSION_DATE, ADMISSION, ADMISSION_TYPE,SPECIALTY, CIS_MARKER, 
#                    HBRES_CURRENTDATE, HBTREAT_CURRENTDATE, COUNCIL_AREA_2019, LOCATION, DATAZONE_2011, HSCP_2019, DR_POSTCODE,
#                    DISCHARGE_DATE, DISCHARGE, MANAGEMENT_OF_PATIENT, DISCHARGE_TYPE, MAIN_CONDITION, SEX, AGE_IN_YEARS,
#                    OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3,OTHER_CONDITION_4, OTHER_CONDITION_5, 
#                    LINK_NO, LENGTH_OF_STAY,RECORD_TYPE,URI, SORT_MARKER, UPI_NUMBER",
#                    "FROM ANALYSIS.SMR04_PI WHERE (ADMISSION_DATE >= TO_DATE('2016-04-01','YYYY-MM-DD'))")

query_deaths = paste("SELECT LINK_NO, DATE_OF_DEATH,AGE, DATE_OF_BIRTH, POSTCODE, COUNCIL_AREA_2019, CHI, 
                     UNDERLYING_CAUSE_OF_DEATH, CAUSE_OF_DEATH_CODE_0, CAUSE_OF_DEATH_CODE_1, CAUSE_OF_DEATH_CODE_2, 
                     CAUSE_OF_DEATH_CODE_3, CAUSE_OF_DEATH_CODE_4, CAUSE_OF_DEATH_CODE_5, CAUSE_OF_DEATH_CODE_6, 
                     CAUSE_OF_DEATH_CODE_7, CAUSE_OF_DEATH_CODE_8,CAUSE_OF_DEATH_CODE_9",
                     "FROM ANALYSIS.GRO_DEATHS_C WHERE (DATE_OF_DEATH >= TO_DATE('2016-04-01','YYYY-MM-DD'))")


# Connect to ODBC
channel = suppressWarnings(
  dbConnect(drv = odbc::odbc(),
            dsn = "SMRA",
            uid = rstudioapi::showPrompt(title = "Username", message = "Username:"),
            pwd = .rs.askForPassword("What is your LDAP password?"))
)

# Run queries
smr1_extract = as_tibble(dbGetQuery(channel,
                                    statement = query_smr1)) %>%
  clean_names()

# smr1e_extract = as_tibble(dbGetQuery(channel,
#                                      statement = query_smr1e)) %>%
#   clean_names()

# smr4_extract = as_tibble(dbGetQuery(channel,
#                                     statement = query_smr4)) %>%
#   clean_names()

deaths_extract = as_tibble(dbGetQuery(channel,
                                      statement = query_deaths)) %>%
  clean_names()

dbDisconnect(channel)

deaths_extract = as.data.frame(deaths_extract)
deaths_extract[is.na(deaths_extract)] <- 0
deaths <- deaths_extract %>%
  group_by(link_no) %>%
  summarise(date_of_death = first(date_of_death))

# 5. Fall Admissions -----
#_________________________________________________________________________

# Edited by CP december 2022 to match Core suite code and hscp level figures

smr_falls <- smr1_extract %>%
  filter(admission_type %in% c(33,34,35)) %>%
  mutate(fall_flag = purrr::pmap_dbl(select(., contains("condition")),
                                     ~any(grepl("^W[00-19]", c(...)),
                                          na.rm = TRUE) * 1)) %>%
  filter(fall_flag == 1) %>%
  select(-fall_flag) %>% 
  rename(datazone2011 = datazone_2011, age = age_in_years) %>% 
  mutate(year = year(discharge_date)) %>%
  mutate(month_num = month(discharge_date)) %>%         
  mutate(AdDate = ISOdate(year,month_num,1,0,0,0,'GMT')) %>%
  arrange(datazone2011) %>%
  left_join(datazones, by = "datazone2011") %>%
  mutate(age_group = age_group_2(age)) %>%
  mutate(financial_year = fy(discharge_date)) %>% 
  drop_na(financial_year) %>% 
  group_by(financial_year, hscp2019name, hscp_locality, age_group) %>%
  summarise(admissions = n()) %>%
  ungroup() 

# 6. Readmissions (28 days) ----
#_________________________________________________________________________

# Edited by CP december 2022 to use data tables and match Core suite code

smr1_extract_read <- smr1_extract %>%
  filter(age_in_years >= 18) %>%
  arrange(link_no, cis_marker, admission_date, 
          record_type, discharge_date, desc(admission_type)) 

# Convert to data table, aggregate to stay level
read_dt <- as.data.table(smr1_extract_read)
read_table <- read_dt[, .(
  age = min(age_in_years),
  admission_date = min(admission_date),
  discharge_date = max(discharge_date),
  admission_type = first(admission_type),
  discharge_type = last(discharge_type),
  datazone2011 = last(datazone_2011)
), by = c("link_no", "cis_marker")]

# Convert back to data frame & sort
read_dataframe <- as.data.frame(read_table) %>%
  ungroup() %>%
  arrange(link_no, cis_marker, admission_date, discharge_date, desc(admission_type))

smr_readmissions <- read_dataframe %>% 
  mutate(discharges=1) %>% 
  left_join(deaths, by = "link_no") %>%
  mutate(discharge_dead = if_else(substr(discharge_type,1,1)=="4",1,0)) %>%
  mutate(discharge_to_death = time_length(interval(discharge_date, date_of_death),
                                          unit = "day")) %>% 
  mutate(discharge_dead = if_else(discharge_to_death <= 0 & 
                                    date_of_death >= admission_date & 
                                    !(is.na(date_of_death)), 1, discharge_dead)) %>% 
  
  mutate(discharges = if_else(discharge_dead != 1 & !(is.na(discharge_dead)), 1, 0)) %>% 

  mutate(year = year(discharge_date)) %>%
  mutate(month_num = month(discharge_date)) %>%         
  mutate(AdDate = make_datetime(year,month_num,1)) %>%
  arrange(datazone2011) %>%
  left_join(datazones, by = "datazone2011") %>% 
  mutate(age_group = age_group_2(age)) %>%
  arrange(link_no,desc(discharge_date),desc(cis_marker)) %>%
  
  mutate(read_28 = if_else(link_no == lag(link_no) &
                             lag(admission_type) %in% c(20,21,22,30,31,32,33,34,35,36,38,39) &
                             time_length(interval(discharge_date, lag(admission_date)),
                                         unit = "day") <= 28, 1, 0)) %>%
  
  mutate(read_28 = if_else(is.na(read_28),0,read_28)) %>%
  mutate(financial_year = fy(discharge_date)) %>% 
  drop_na(financial_year) %>%
  group_by(financial_year,hscp2019name,hscp_locality,age_group) %>%
  summarise(discharges = sum(discharges),
            read_28 = sum(read_28)) %>%
  ungroup()

rm(smr1_extract_read, read_dt, read_table, read_dataframe)

# 7. Comm 6 months -----
#___________________________________________________________
# 
# #codes to exclude
# location_codes <- c('A240V', 'F821V', 'G105V', 'G518V', 'G203V', 'G315V', 'G424V', 'G541V', 'G557V', 
#                     'H239V', 'L112V', 'L213V', 'L215V', 'L330V', 'L365V', 'N465R', 'N498V', 
#                     'S312R', 'S327V', 'T315S', 'T337V', 'Y121V')
# # remove selected locations from extract
# smr1e_temp <- smr1e_extract %>%
#   filter(!(location %in% location_codes)) %>%
#   mutate(recid = "50B")
# # remove selected locations from extract
# smr1_temp <- smr1_extract %>%
#   filter(!(location %in% location_codes)) %>%
#   mutate(recid = "01B")
# # remove selected locations from extract
# smr4_temp <- smr4_extract %>%
#   filter(!(location %in% location_codes)) %>%
#   mutate(recid = "04B")
# 
# comm_temp <- rbind(smr1_temp,smr1e_temp)
# 
# comm_smr1 <- comm_temp %>%
#   # Arrange data in order of patient identifier then chronologically by admission date then discharge
#   arrange(link_no,admission_date,discharge_date,admission,discharge,uri) %>%
#   filter(inpatient_daycase_identifier == "I") %>% # filter for inpatients
#   # aggregate to stay level, using first ep admission code & date, first ep recid type and date of stay discharge
#   group_by(link_no,cis_marker) %>%
#   summarise(old_smr1_tadm_code = first(old_smr1_tadm_code),
#             admission_date = first(admission_date),
#             recid = first(recid),
#             discharge_date = last(discharge_date)) %>%
#   ungroup() %>%
#   arrange(admission_date,discharge_date,link_no) %>% # sort data chronologically
#   mutate(duplicate = ifelse(admission_date==lag(admission_date) &
#                               discharge_date==lag(discharge_date) &
#                               link_no==lag(link_no),1,0)) %>% # add flag to highlight duplicate records to be removed
#   filter(duplicate == 0) %>%
#   mutate(zerostay = ifelse(admission_date==discharge_date,1,0)) %>% # add flag for day case in IP records
#   arrange(admission_date,link_no,desc(discharge_date)) %>% 
#   mutate(duplicate_doa = ifelse(admission_date==lag(admission_date) & # remove records of stays admitted on the same day
#                                   link_no==lag(link_no) &
#                                   zerostay==0,1,0)) %>%
#   filter(duplicate_doa == 0) %>%
#   arrange(discharge_date,link_no,admission_date) %>% # remove records of stays discharged on the same day 
#   mutate(duplicate_dod = ifelse(discharge_date==lag(discharge_date) &
#                                   link_no==lag(link_no) &
#                                   zerostay==0,1,0)) %>%
#   filter(duplicate_dod == 0) %>% 
#   arrange(link_no)
# 
# comm_smr4 <- smr4_temp %>%
#   # filter(age_in_years >= 75) %>%
#   arrange(link_no,admission_date,discharge_date,admission,discharge,uri) %>% # sort by patient then chronologically
#   mutate(include = ifelse(discharge_date >= dmy(01042014),1,ifelse(is.na(discharge_date),2,0))) %>% #exclude dates before boundary
#   filter(include == 1 | include == 2) %>%
#   filter(management_of_patient %in% c("1","3","5","7","A")) %>%
#   mutate(duplicate = ifelse(admission_date==lag(admission_date) & # remove duplicate admissions
#                               discharge_date==lag(discharge_date) &
#                               link_no==lag(link_no),1,0)) %>%
#   filter(duplicate == 0) %>%
#   mutate(zerostay = ifelse(admission_date==discharge_date,1,0)) %>% 
#   arrange(admission_date,link_no,desc(discharge_date)) %>% # remove stays with admission day
#   mutate(duplicate_doa = ifelse(admission_date==lag(admission_date) &
#                                   link_no==lag(link_no) &
#                                   zerostay==0,1,0)) %>%
#   filter(duplicate_doa == 0) %>%
#   arrange(discharge_date,link_no,admission_date) %>% # remove stays with duplicate discharge day
#   mutate(duplicate_dod = ifelse(discharge_date==lag(discharge_date) &
#                                   link_no==lag(link_no) &
#                                   zerostay==0,1,0)) %>%
#   filter(duplicate_dod == 0) %>% 
#   arrange(link_no) %>%
#   select(link_no, recid,patient_identifier,link_no,admission_date,discharge_date,location)
# 
# comm_combo = bind_rows(comm_smr1,comm_smr4) # link acute and psychiatric data sets
# 
# commdeaths <- deaths_extract %>% # create deaths list to match against
#   arrange(link_no,date_of_death) %>%
#   mutate(flag = ifelse(link_no==lag(link_no),1,0)) %>%
#   filter(flag == 0) %>%
#   
#   ## Conditions to fill 
#   
#   # death column starts with V or X and 2nd and 3rd character between 0-99 OR starts with Y and ends with 0-84 OR
#   # deaths column starts with W and 2nd and 4rd character between 20-99 or
#   mutate(external = purrr::pmap_dbl(select(., contains("cause_of_death")),
#                                     ~any(grepl("^V[00-99]|^X[00-99]|^Y[00-84]|^W[20-99]", c(...)),
#                                          na.rm = TRUE) * 1)) %>%
#   # Conditions for flag to remove deaths including cause W0-19:
#   # Any cause of death column starts with W followed by 0-19 and above external flag is 1 (i.e. TRUE)
#   mutate(external = ifelse(
#     ((substr(underlying_cause_of_death,1,1) == "W" & between(as.numeric(substr(underlying_cause_of_death, 2, 3)), 0, 19) & external == 1) |
#        (substr(cause_of_death_code_0,1,1) == "W" & between(as.numeric(substr(cause_of_death_code_0, 2, 3)), 0, 19) & external == 1) |
#        (substr(cause_of_death_code_1,1,1) == "W" & between(as.numeric(substr(cause_of_death_code_1, 2, 3)), 0, 19) & external == 1) |       
#        (substr(cause_of_death_code_2,1,1) == "W" & between(as.numeric(substr(cause_of_death_code_2, 2, 3)), 0, 19) & external == 1) |
#        (substr(cause_of_death_code_3,1,1) == "W" & between(as.numeric(substr(cause_of_death_code_3, 2, 3)), 0, 19) & external == 1) | 
#        (substr(cause_of_death_code_4,1,1) == "W" & between(as.numeric(substr(cause_of_death_code_4, 2, 3)), 0, 19) & external == 1) |                
#        (substr(cause_of_death_code_5,1,1) == "W" & between(as.numeric(substr(cause_of_death_code_5, 2, 3)), 0, 19) & external == 1) |    
#        (substr(cause_of_death_code_6,1,1) == "W" & between(as.numeric(substr(cause_of_death_code_6, 2, 3)), 0, 19) & external == 1) |
#        (substr(cause_of_death_code_7,1,1) == "W" & between(as.numeric(substr(cause_of_death_code_7, 2, 3)), 0, 19) & external == 1) | 
#        (substr(cause_of_death_code_8,1,1) == "W" & between(as.numeric(substr(cause_of_death_code_8, 2, 3)), 0, 19) & external == 1) | 
#        (substr(cause_of_death_code_9,1,1) == "W" & between(as.numeric(substr(cause_of_death_code_9, 2, 3)), 0, 19) & external == 1)),0,external)) %>%
#   filter(external == 0) %>%
#   
#   arrange(postcode) %>%
#   left_join(postcodes, by = "postcode") %>%
#   select(link_no,date_of_death:external,hb2019,hb2019name,hscp2019,hscp2019name,hscp_locality) %>%
#   mutate(financial_death = fy(date_of_death)) %>% # add financial year for death date
#   drop_na(financial_death) %>%
#   mutate(month = month(date_of_death)) %>%
#   mutate(year = year(date_of_death)) %>%
#   mutate(AdDate = make_datetime(year,month,1)) %>% # create month/year variable
#   arrange(link_no) %>% 
#   select(AdDate,link_no,hscp2019name,hscp_locality,date_of_death,financial_death)
# 
# comm_activity <- comm_combo %>%
#   arrange(link_no) %>%
#   left_join(commdeaths, by = "link_no") %>%
#   drop_na(link_no) %>%
#   mutate(discharge_date = if_else(recid == "04B" & is.na(discharge_date),date_of_death,discharge_date)) %>%
#   mutate(sixmonth_date = date_of_death - days(183)) %>%
#   mutate(admission_date = if_else(admission_date <= sixmonth_date & discharge_date >= sixmonth_date,sixmonth_date,admission_date)) %>%
#   mutate(death_adm_diff = time_length(interval(admission_date,date_of_death),unit = "day")) %>%
#   mutate(monthsix = ifelse(death_adm_diff <= 183,1,0)) %>%
#   filter(monthsix == 1) %>%
#   mutate(invalid_death = ifelse(admission_date > date_of_death,0,1)) %>%
#   filter(invalid_death == 1) %>%
#   mutate(discharge_date = if_else(discharge_date > date_of_death,date_of_death,discharge_date)) %>%
#   mutate(los = time_length(interval(admission_date,discharge_date),unit = "day"))
# 
# comm_agg <- comm_activity %>%
#   group_by(AdDate,financial_death,link_no) %>%
#   summarise(totallos = sum(los)) %>%
#   ungroup() %>%
#   mutate(los = ifelse(totallos > 182.5, 182.5,totallos)) %>%
#   select(-totallos)
# 
# smr_comm_6mo <- commdeaths %>%
#   arrange(link_no, AdDate, financial_death) %>%
#   left_join(comm_agg, by=c("link_no","AdDate","financial_death")) %>%
#   mutate(los = if_else(is.na(los),0,los)) %>%
#   # filter(los != 0) %>%
#   group_by(hscp2019name,hscp_locality,financial_death) %>%
#   summarise(total_bddys = sum(los),
#             total_deaths = n()) %>%
#   ungroup() %>%
#   mutate(percent = 1-(total_bddys/total_deaths/182.5)) %>%
#   select(financial_death,hscp2019name,hscp_locality,total_bddys,total_deaths,percent) %>%
#   na.omit() %>%
#   as.data.frame()
# 
# 
# # 8. Potentially Preventable Admissions -----
# #_________________________________________________________________________________
# 
# #identify PPAs
 ppa_id <- smr1_extract %>%
   arrange(link_no,cis_marker) %>%
   filter(admission_type %in% c(30,31,32,33,34,35,36,38,39,20,21,22,18)) %>% # AR - added 18 (planned transfers) and removed 37 as discovery metadata
   
   mutate(op1a13 = substr(main_condition,1,3)) %>%
   mutate(d1c14 = substr(main_condition,1,4)) %>%
   mutate(d2c14 = substr(other_condition_1,1,4)) %>%
   mutate(d3c14 = substr(other_condition_2,1,4)) %>%
   mutate(d4c14 = substr(other_condition_3,1,4)) %>%
   mutate(d5c14 = substr(other_condition_4,1,4)) %>%
   mutate(d6c14 = substr(other_condition_5,1,4)) %>%
   mutate(d1c13 = substr(d1c14,1,3)) %>%
   mutate(d2c13 = substr(d2c14,1,3)) %>%
   mutate(d3c13 = substr(d3c14,1,3)) %>%
   mutate(d4c13 = substr(d4c14,1,3)) %>%
   mutate(d5c13 = substr(d5c14,1,3)) %>%
   mutate(d6c13 = substr(d6c14,1,3)) %>%
   mutate(op1a13 = replace_na(op1a13,'0'),d1c14 = replace_na(d1c14,'0'),d2c14 = replace_na(d2c14,'0'),
          d3c14 = replace_na(d3c14,'0'),d4c14 = replace_na(d4c14,'0'),d5c14 = replace_na(d5c14,'0'),
          d6c14 = replace_na(d6c14,'0'),d1c13 = replace_na(d1c13,'0'),d2c13 = replace_na(d2c13,'0'),
          d3c13 = replace_na(d3c13,'0'),d4c13 = replace_na(d4c13,'0'),d5c13 = replace_na(d5c13,'0'),d6c13 = replace_na(d6c13,'0')) %>%
   
   mutate(ent = ifelse((d1c13 %in% c('H66','J06')| d1c14 %in% c('J028','J029','J038','J039','J321')),1,0)) %>%
   mutate(dent = ifelse(d1c13 %in% c('K02','K03','K04','K05','K06','K08'),1,0)) %>%
   mutate(conv = ifelse(d1c13 %in% c('G40','G41','R56','O15'),1,0)) %>%
   mutate(gang = ifelse((d1c13 == 'R02'| d2c13 == 'R02'| d3c13 == 'R02'| d4c13 == 'R02'| d5c13 == 'R02'| d6c13 == 'R02'),1,0)) %>%
   mutate(nutridef = ifelse((d1c13 %in% c('E40','E41','E43')| d1c14 %in% c('E550','E643','M833')),1,0)) %>%
   mutate(dehyd = ifelse((d1c13 == 'E86'| d1c14 %in% c('K522','K528','K529')),1,0)) %>%
   mutate(pyelon = ifelse(d1c13 %in% c('N10','N11','N12'),1,0)) %>%
   mutate(perf = ifelse((d1c13 %in% c('K25','K26','K27','K28') & substr(d1c14,4,4) %in% c('0','1','2','4','5','6')),1,0)) %>%
   mutate(cell = ifelse((d1c13 %in% c('L03','L04')| d1c14 %in% c('L080','L088','L089','L980')),1,0)) %>%
   mutate(cell = ifelse(cell == 1 & op1a13 %in% c('S06','S57','S68','S70','W90','X11'),0,cell)) %>%
   mutate(pelvic = ifelse(d1c13 %in% c('N70','N73'),1,0)) %>%
   mutate(flu = ifelse((d1c13 %in% c('J10','J11','J13')| d2c13 %in% c('J10','J11','J13')| d3c13 %in% c('J10','J11','J13')| d4c13 %in% c('J10','J11','J13')| d5c13 %in% c('J10','J11','J13')| d6c13 %in% c('J10','J11','J13')| d1c14 == 'J181' | d2c14 == 'J181' | d3c14 == 'J181' | d4c14 == 'J181' | d5c14 == 'J181' | d6c14 == 'J181'),1,0)) %>%
   mutate(othvacc = ifelse((d1c13 %in% c('A35','A36','A80','B05','B06','B26')| d2c13 %in% c('A35','A36','A80','B05','B06','B26')| d3c13 %in% c('A35','A36','A80','B05','B06','B26')| d4c13 %in% c('A35','A36','A80','B05','B06','B26')| d5c13 %in% c('A35','A36','A80','B05','B06','B26')| d6c13 %in% c('A35','A36','A80','B05','B06','B26')| 
                              d1c14 %in% c('A370','A379','B161','B169')| d2c14 %in% c('A370','A379','B161','B169')| d3c14 %in% c('A370','A379','B161','B169')| d4c14 %in% c('A370','A379','B161','B169')| d5c14 %in% c('A370','A379','B161','B169')| d6c14 %in% c('A370','A379','B161','B169')),1,0)) %>%
   mutate(iron = ifelse(d1c14 %in% c('D501','D508','D509'),1,0)) %>%
   mutate(asthma = ifelse(d1c13 %in% c('J45','J46'),1,0)) %>%
   
   mutate(diabetes = ifelse((d1c13 == 'E10' & d1c14 != 'E109')|(d1c13 == 'E11' & d1c14 != 'E119')|(d1c13 == 'E12' & d1c14 != 'E129') | (d1c13 == 'E13' & d1c14 != 'E139')|(d1c13 == 'E14' & d1c14 != 'E149')|
                              (d2c13 == 'E10' & d2c14 != 'E109')|(d2c13 == 'E11' & d2c14 != 'E119')|(d2c13 == 'E12' & d2c14 != 'E129') | (d2c13 == 'E13' & d2c14 != 'E139')|(d2c13 == 'E14' & d2c14 != 'E149')|
                              (d3c13 == 'E10' & d3c14 != 'E109')|(d3c13 == 'E11' & d3c14 != 'E119')|(d3c13 == 'E12' & d3c14 != 'E129') | (d3c13 == 'E13' & d3c14 != 'E139')|(d3c13 == 'E14' & d3c14 != 'E149')|
                              (d4c13 == 'E10' & d4c14 != 'E109')|(d4c13 == 'E11' & d4c14 != 'E119')|(d4c13 == 'E12' & d4c14 != 'E129') | (d4c13 == 'E13' & d4c14 != 'E139')|(d4c13 == 'E14' & d4c14 != 'E149')|
                              (d5c13 == 'E10' & d5c14 != 'E109')|(d5c13 == 'E11' & d5c14 != 'E119')|(d5c13 == 'E12' & d5c14 != 'E129') | (d5c13 == 'E13' & d5c14 != 'E139')|(d5c13 == 'E14' & d5c14 != 'E149')|
                              (d6c13 == 'E10' & d6c14 != 'E109')|(d6c13 == 'E11' & d6c14 != 'E119')|(d6c13 == 'E12' & d6c14 != 'E129') | (d6c13 == 'E13' & d6c14 != 'E139')|(d6c13 == 'E14' & d6c14 != 'E149'),1,0)) %>%
   mutate(hypert = ifelse((d1c13 == 'I10'| d1c14 == 'I119'),1,0)) %>%
   mutate(hypert = ifelse((hypert == 1 & (op1a13 %in% range('K01','K50') | op1a13 %in% c('K56','K60','K61'))),0,hypert)) %>%
   mutate(angina = ifelse(d1c13 == 'I20',1,0)) %>%
   mutate(angina = ifelse((angina == 1 & op1a13 %in% c('K40','K45','K49','K60','K65','K66')),0,angina)) %>%
   mutate(copd = ifelse((d1c13 %in% c('J41','J42','J43','J44')) | (d1c13 == 'J20' & d2c13 %in% c('J41','J42','J43','J44')),1,0)) %>%
   mutate(chf = ifelse((d1c13 == 'I50'| d1c13 == 'J81' | d1c14 == 'I110'),1,0)) %>%
   mutate(chf = ifelse(chf == 1 & (op1a13 %in% range('K01','K50') | op1a13 %in% c('K56','K60','K61')),0,chf)) %>%
   mutate(allcond = ifelse((ent == 1 | dent == 1 |conv == 1 | gang == 1 | nutridef == 1 | dehyd == 1 | pyelon == 1 | 
                              perf == 1 | cell == 1 | pelvic == 1 | flu == 1 | othvacc == 1 | iron == 1 | asthma == 1 | 
                              diabetes == 1 | hypert == 1 | angina == 1 | copd == 1 | chf == 1), 1, 0)) %>%
   filter(allcond == 1) 
 
 # aggregate
 ppa <- ppa_id %>% 
   group_by(link_no,cis_marker) %>%
   summarise(ent=sum(ent),dent=sum(dent),conv=sum(conv),gang=sum(gang),nutridef=sum(nutridef),dehyd=sum(dehyd),pyelon=sum(pyelon),perf=sum(perf),cell=sum(cell),pelvic=sum(pelvic),flu=sum(flu),othvacc=sum(othvacc),iron=sum(iron),asthma=sum(asthma),diabetes=sum(diabetes),hypert=sum(hypert),angina=sum(angina),copd=sum(copd),chf=sum(chf),allcond=sum(allcond),
             age = first(age_in_years),
             dod = max(discharge_date),
             datazone2011 = last(datazone_2011)) %>%
   ungroup() %>%
   
   mutate(ent=ifelse(ent>0,1,0),dent=ifelse(dent>0,1,0),conv=ifelse(conv>0,1,0),gang=ifelse(gang>0,1,0),nutridef=ifelse(nutridef>0,1,0),dehyd=ifelse(dehyd>0,1,0),pyelon=ifelse(pyelon>0,1,0),perf=ifelse(perf>0,1,0),cell=ifelse(cell>0,1,0),pelvic=ifelse(pelvic>0,1,0),flu=ifelse(flu>0,1,0),othvacc=ifelse(othvacc>0,1,0),iron=ifelse(iron>0,1,0),asthma=ifelse(asthma>0,1,0),diabetes=ifelse(diabetes>0,1,0),
          hypert=ifelse(hypert>0,1,0),angina=ifelse(angina>0,1,0),copd=ifelse(copd>0,1,0),chf=ifelse(chf>0,1,0)) %>%
   
   group_by(link_no,cis_marker) %>%
   summarise(ent=first(ent),dent=first(dent),conv=first(conv),gang=first(gang),nutridef=first(nutridef),dehyd=first(dehyd),pyelon=first(pyelon),perf=first(perf),cell=first(cell),pelvic=first(pelvic),flu=first(flu),othvacc=first(othvacc),iron=first(iron),asthma=first(asthma),diabetes=first(diabetes),hypert=first(hypert),angina=first(angina),copd=first(copd),chf=first(chf),allcond = 
               sum(ent,dent,conv,gang,nutridef,dehyd,pyelon,perf,cell,pelvic,flu,othvacc,iron,asthma,diabetes,hypert,angina,copd,chf),
             age = first(age),
             dod = max(dod),
             datazone2011 = last(datazone2011)) %>%
   ungroup() %>%
   
   arrange(datazone2011) %>%
   left_join(datazones,by="datazone2011") %>%
   arrange(link_no,cis_marker) %>%
   mutate(financial_year = fy(dod)) %>%
   drop_na(financial_year) %>%
   mutate(age_group = age_group_2(age)) %>%
   mutate(admissions = 1) %>%
   
   group_by(financial_year,age_group,hscp2019name,hscp_locality) %>%
   summarise(admissions = sum(admissions)) %>% 
   drop_na(hscp2019name,age_group) %>%
   as.data.frame()

 
# 9. MH Emergency Admissions ----
#(Calculated using "Stays" method from MH Inpatient Activity Publication)
#_____________________________________________________________________________
# 
# 
# future_date <- "2029-01-01"
# 
# 
# smr4_prep <- smr4_extract %>% 
#   # recode missing discharge date to something in the future
#   mutate(discharge_date = substr(discharge_date, 1, 10), 
#          discharge_date = ifelse(is.na(discharge_date) == T, future_date,  
#                                  discharge_date)) %>%
#   
#   arrange(link_no,admission_date,discharge_date) %>%
#   # remove missing link_no
#   filter(is.na(link_no)==F) #%>%
#   
#   
# smr_mh_emergency_adm <- smr4_prep %>%
#     filter(specialty %in% c('G1', 'G2', 'G3', 'G4', 
#                           #  'G5',  # publication code excludes G5
#                             'G6','G21','G22')) %>%
#   group_by(link_no,cis_marker) %>%
#   summarise(admission_date=min(admission_date),
#             discharge_date=max(discharge_date),
#             datazone2011=first(datazone_2011),
#             hb2019 = first(hbres_currentdate),
#             los=sum(length_of_stay),
#             admission_type=first(admission_type),
#             age=first(age_in_years),
#             main_condition=first(main_condition),
#             location = first(location),
#             episodes=n()) %>%
#   ungroup() %>%
#   mutate(hb2019name = hbres(hb2019)) %>%
#   mutate(adm = 1) %>%
#   mutate(age_group = case_when(
#     between(age, 0, 17) ~ "0 - 17",
#     between(age, 18, 44) ~ "18 - 44",
#     between(age, 45, 64) ~ "45 - 64",
#     age >= 65 ~ "65+")) %>%
#   mutate(month_num = month(admission_date)) %>% # use admission date for dates to allign with emergency ADMISSIONS
#   mutate(year = year(admission_date)) %>%
#   mutate(AdDate = make_datetime(year,month_num,1)) %>%
#   mutate(financial_year = fy(AdDate)) %>%
#   drop_na(financial_year) %>%
#   filter(admission_type %in% c('30','31','32','33','34','35','36','37','38','39','20','21','22','18')) %>% # emergency admission only (frist ep)
#   mutate(main_con = substr(main_condition,1,3)) %>%
#   mutate(diag = case_when(
#     str_detect(main_con,'F[00-09]') == T ~ "Dementia",
#     str_detect(main_con, 'F[10-19]') == T ~ "Psychoactive Substance Misuse",
#     str_detect(main_con, 'F[20-29]') == T ~ "Schizophrenia",
#     str_detect(main_con, 'F[30-49]') == T ~ "Mood (affective) disorders",
#     str_detect(main_con, 'F[60-69]') == T ~ "Adult personality and behavioural disorders",    
#     str_detect(main_con, 'F[50-59]') == T ~ "Adolescent behavioural and emotional disorders",
#     str_detect(main_con, 'F[70-99]') == T ~ "Adolescent behavioural and emotional disorders")) %>%
#   # mutate(diag = fct_explicit_na(diag, na_level = "Other")) #%>%
#   mutate(diag = ifelse(diag %in% c("Dementia","Psychoactive Substance Misuse","Schizophrenia",
#                                    "Mood (affective) disorders","Adult personality and behavioural disorders",
#                                    "Adolescent behavioural and emotional disorders"),diag,"Other")) %>%
#   arrange(datazone2011) %>%
#   left_join(datazones, by = "datazone2011") %>%
#   arrange(link_no, cis_marker, admission_date, discharge_date,desc(admission_type)) %>%
#   group_by(financial_year, hscp2019name,hscp_locality,age_group,diag) %>%
#   summarise(admissions = sum(adm)) %>%
#   as.data.frame() %>%
#   filter(financial_year != "2022/23")


## Save Data ----
#_________________________________________________________________________

# Falls
saveRDS(smr_falls, paste0(exportfolder, "falls_smr.rds"))

# Readmissions
saveRDS(smr_readmissions, paste0(exportfolder, "readmissions_smr.rds"))

# PPA
saveRDS(ppa, paste0(exportfolder, "ppa_smr.rds"))

