################################################################################
## Codename - 001_SMR01SMR04_basefile                                         ##
## Data Release - Annual Mental Health Inpatient Publication                  ##
## Original Authors - Ciara Gribben                                           ##
## Orginal Date - February 2019                                               ##
## Latest Update Author - Alex Bruce                                          ##
## Latest Update Date - October 2021                                          ##
## Updates to script (if any):                                                ##
##                                                                            ##
## 2022 Changes:                                                              ##
##                                                                            ##
## - DISCHARGE_TYPE added to all data import variable lists                   ##
##                                                                            ##
## 2021 Changes                                                               ##
##                                                                            ##
## - COUNCIL_AREA was changed to COUNCIL_AREA_2019 by DMT, the format changed ##
##  from a number to a string and accounted for the 8 postcodes which moved   ##
##  from GGC to North Lanarkshire in 2019. Adjusted code to account, CA2019   ##
##  codes have been converted to the same numberic format used in other       ##
##  scripts for this publication.                                             ##
##                                                                            ##
## - Health Board codes were updated throughout SMR datasets to use HB2019    ##
##   codes. HB2014 codes have been replaced by HB2019 codes in the script to  ##
##   account for this.                                                        ##
##                                                                            ##
## - Basefile only includes State Hospital info up till Dec 2020 due to       ##
##   staffing issues                                                          ##
##                                                                            ##
## Type - Extraction/preparation                             ###################
## Written/run on - R Studio SERVER                          ##
## R version - 3.5.1 (2021-10-30)                            ##
## Versions of packages -                                    ##
## tidyverse_1.3.1                                           ##
## odbc_1.2.2                                                ##
## lubridate_1.7.10                                          ##
##                                                           ##
## Description - Extracts SMR01 and SMR04 data and creates   ##
##               basefile for subsequent work                ##
##                                                           ##
## Approximate run time: 60 minutes                          ##
###############################################################


##################################
### SECTION 1 - HOUSE KEEPING ----
##################################

### 1 - Load packages ----
library(Rcpp)
library(odbc) # for accessing SMRA
library(dplyr) # tidyverse package
library(tidyr) # tidyverse package
library(lubridate) # for handling dates
library(data.table)


### 2 -  Publication Date variable to define filepath ----
# MUST BE UPDATED EACH TIME WE PUBLISH
pub_date <- "20221213"


### 3 - Set filepaths ----
output <- paste0(
  "//PHI_conf/MentalHealth1/Inpatient care/Publications/Hospital-Inpatient-Care-of-People-with-Mental-Health-Problems-in-Scotland/",
  pub_date, "/Data/"
)


### 4 - Define variables - update as necessary ----
# Minimum financial year we want to publish
min_fyear <- 1996
# Maximum financial year we want to publish
max_fyear <- 2021
# Far away future date for recoding blank discharge dates
# SMR04 contains open records of people still in hopsital
future_date <- "2029-01-01"


### 5 - Define the database connection with SMRA ----
# set it up so that a pop up appears to enter user name and password
suppressWarnings(channel <- dbConnect(odbc(),
  dsn = "SMRA",
  uid = Sys.getenv("USER"),
  pwd = .rs.askForPassword("SMRA Password:")
))


###################################
### SECTION 2 - DATA EXTRACTION----
###################################

### 1 - SMR01 Data Extract ----
# Define SQL query
Query_SMR01 <- paste(
  "SELECT LINK_NO, ADMISSION_DATE, DISCHARGE_DATE, CIS_MARKER, DOB, SEX,",
  "LOCATION, SPECIALTY, MAIN_CONDITION,",
  "ADMISSION_TYPE, DISCHARGE_TYPE,",
  "HBRES_CURRENTDATE, HBTREAT_CURRENTDATE,",
  "POSTCODE, COUNCIL_AREA_2019, AGE_IN_YEARS,",
  "LENGTH_OF_STAY",
  "FROM ANALYSIS.SMR01_PI"
)

# Extract data from database using SQL query defined above
data_smr01 <- as_tibble(dbGetQuery(channel, Query_SMR01))


### 2 - SMR04 Data Extract ----
# Define SQL query for COPPISH data (data submitted from 01/04/96)
Query_SMR04 <- paste(
  "SELECT LINK_NO, ADMISSION_DATE, DISCHARGE_DATE, CIS_MARKER, DOB, SEX,",
  "LOCATION, SPECIALTY, MAIN_CONDITION, OTHER_CONDITION_1,",
  "OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4,",
  "OTHER_CONDITION_5, ADMISSION_TYPE, ADMISSION_REASON,",
  "ADMISSION_TRANSFER_FROM, STATUS_ON_ADMISSION, DISCHARGE_TYPE,",
  "DISCHARGE_TRANSFER_TO, HBRES_CURRENTDATE, HBTREAT_CURRENTDATE,",
  "ADMISSION_REFERRAL_FROM, POSTCODE, COUNCIL_AREA_2019, AGE_IN_YEARS,",
  "AGE_ON_DISCHARGE, LENGTH_OF_STAY",
  "FROM ANALYSIS.SMR04_PI"
)

# Extract data from database using SQL query above
data_coppish <- as_tibble(dbGetQuery(channel, Query_SMR04))

# Alter DOB type for matching
data_coppish$DOB <- ymd(data_coppish$DOB)

# Create a variable for record id
data_coppish$REC_ID <- "04B"

# Define SQL query for pre COPPISH data
Query_SMR04_historic <- paste(
  "SELECT LINK_NO, ADMISSION_DATE, DISCHARGE_DATE, CIS_MARKER, DOB,",
  "SEX, LOCATION, OLD_DISCHARGE_DIAG_1, OLD_DISCHARGE_DIAG_2,",
  "OLD_DISCHARGE_DIAG_3, OLD_DISCHARGE_DIAG_4, OLD_ADM_STATUS,",
  "OLD_DISCHARGE_TYPE,HBRES_CURRENTDATE,HBTREAT_CURRENTDATE,",
  "POSTCODE,HOSPITAL_TYPE, AGE_IN_YEARS, AGE_ON_DISCHARGE,",
  "LENGTH_OF_STAY",
  "FROM ANALYSIS.SMR04_HISTORIC"
)

# Extract data from database using SQL query defined above
data_historic <- as_tibble(dbGetQuery(channel, Query_SMR04_historic))

# Rename columns of historic data to match coppish data
data_historic <- data_historic %>%
  rename(
    MAIN_CONDITION = OLD_DISCHARGE_DIAG_1,
    OTHER_CONDITION_1 = OLD_DISCHARGE_DIAG_2,
    OTHER_CONDITION_2 = OLD_DISCHARGE_DIAG_3,
    OTHER_CONDITION_3 = OLD_DISCHARGE_DIAG_4,
    STATUS_ON_ADMISSION = OLD_ADM_STATUS,
    DISCHARGE_TYPE = OLD_DISCHARGE_TYPE
  )

# Create a variable for record id
data_historic$REC_ID <- "04A"

# Alter DOB type to date
data_historic$DOB <- ymd(data_historic$DOB)

# Add the SMR04 tibbles together
data_smr04 <- dplyr::bind_rows(data_coppish, data_historic)
rm(data_coppish)
rm(data_historic)



###########################################
### SECTION 3 - DATA PREPARATION SMR01 ----
###########################################

### 1 - All var names to lower case ----
names(data_smr01) <- tolower(names(data_smr01))


### 2 - Restrict to F codes only ----
# F codes are the ICD10 codes beginning with F that cover Mental Health
data_smr01 <- data_smr01 %>%
  filter(substr(main_condition, 1, 1) == "F")


### 3 -  Derive Financial Year ----
data_smr01 <- data_smr01 %>%
  mutate(
    fyear = paste0(
      ifelse(month(discharge_date) >= 4, year(discharge_date), year(discharge_date) - 1),
      "/",
      ifelse(month(discharge_date) >= 4, year(discharge_date) + 1, year(discharge_date))
    ),
    fyear_pt1_dis = substr(fyear, 1, 4),
    adm_fyear = paste0(
      ifelse(month(admission_date) >= 4, year(admission_date), year(admission_date) - 1),
      "/",
      ifelse(month(admission_date) >= 4, year(admission_date) + 1, year(admission_date))
    ),
    fyear_pt1_adm = substr(adm_fyear, 1, 4)
  )

# Keep relevant financial years only
data_smr01 <- data_smr01 %>%
  filter(fyear_pt1_dis >= min_fyear & fyear_pt1_dis <= max_fyear)


### 4 - Create dataset identifying variable ----
data_smr01 <- data_smr01 %>%
  mutate(SMR01 = 1)


### 5 - Altering variable types ----
data_smr01 <- data_smr01 %>%
  mutate(
    admission_date = as.Date(admission_date, "%Y-%m-%d"),
    discharge_date = as.Date(discharge_date, "%Y-%m-%d"),
    dob = as.Date(dob, "%Y-%m-%d"),
    sex = as.numeric(sex)
  )

### 6 - Deal with duplicates ----
data_smr01 <- data_smr01 %>%
  group_by(link_no, admission_date, discharge_date) %>%
  filter(row_number() == 1 | admission_date == discharge_date)


### 7 - Remove any record where link no is missing ----
data_smr01 <- data_smr01 %>%
  filter(!is.na(link_no))


###########################################
### SECTION 4 - DATA PREPARATION SMR04 ----
###########################################

### 1 - Ensure variable names are all lowercase ----
names(data_smr04) <- tolower(names(data_smr04))


### 2 - Recode missing discharge dates ----
# Set the missing discharge dates to something very large
data_smr04 <- data_smr04 %>%
  mutate(
    discharge_date = substr(discharge_date, 1, 10),
    discharge_date = ifelse(is.na(discharge_date), future_date,
      discharge_date
    )
  )


### 3 - Updating variable format ----
data_smr04 <- data_smr04 %>%
  mutate(
    sex = as.numeric(sex),
    dob = ymd(dob),
    status_on_admission = as.numeric(status_on_admission),
    age_in_years = as.numeric(age_in_years),
    age_on_discharge = as.numeric(age_on_discharge),
    length_of_stay = as.numeric(length_of_stay),
    admission_type = as.character(admission_type),
    hospital_type = as.numeric(hospital_type),
    admission_date = ymd(admission_date),
    discharge_date = ymd(discharge_date),
    admission_type = as.numeric(admission_type)
  )


### 4 - Deal with duplicates ----
data_smr04 <- data_smr04 %>%
  group_by(link_no, admission_date, discharge_date) %>%
  filter(row_number() == 1 | admission_date == discharge_date)


### 5 - Remove any record where link no is missing ----
data_smr04 <- data_smr04 %>%
  filter(!is.na(link_no))


### 6 - Hospital Type Recode ----
# Hospital Types
data_smr04 <- data_smr04 %>%
  mutate(hospital_type = ifelse(is.na(hospital_type), 0, hospital_type))


### 7 - Financial Year Derivation ----
data_smr04 <- data_smr04 %>%
  mutate(
    fyear = paste0(
      ifelse(month(discharge_date) >= 4, year(discharge_date), year(discharge_date) - 1),
      "/",
      ifelse(month(discharge_date) >= 4, year(discharge_date) + 1, year(discharge_date))
    ),
    fyear_pt1_dis = substr(fyear, 1, 4),
    adm_fyear = paste0(
      ifelse(month(admission_date) >= 4, year(admission_date), year(admission_date) - 1),
      "/",
      ifelse(month(admission_date) >= 4, year(admission_date) + 1, year(admission_date))
    ),
    fyear_pt1_adm = substr(adm_fyear, 1, 4)
  )


### 8 - Create dataset identifying variable ----
data_smr04 <- data_smr04 %>%
  mutate(SMR04 = 1)



#####################################################
### SECTION 5 - DATA PREPARATION SMR04 RESIDENTS ----
#####################################################

### 1 - Create a resident flag ----
# A patient is a resident if they stay overnight at the change of a financial year
# ie the night of 31st March
# so a patient is counted as a resident in 1980/81 if they stay over the night of 31/3/81
# first create several variables which indicate if a patient was resident in a particular year
data_res <- data_smr04 %>%
  mutate(
    r1996_1997 = 0,
    r1997_1998 = 0,
    r1998_1999 = 0,
    r1999_2000 = 0,
    r2000_2001 = 0,
    r2001_2002 = 0,
    r2002_2003 = 0,
    r2003_2004 = 0,
    r2004_2005 = 0,
    r2005_2006 = 0,
    r2006_2007 = 0,
    r2007_2008 = 0,
    r2008_2009 = 0,
    r2009_2010 = 0,
    r2010_2011 = 0,
    r2011_2012 = 0,
    r2012_2013 = 0,
    r2013_2014 = 0,
    r2014_2015 = 0,
    r2015_2016 = 0,
    r2016_2017 = 0,
    r2017_2018 = 0,
    r2018_2019 = 0,
    r2019_2020 = 0,
    r2020_2021 = 0,
    r2021_2022 = 0
  )

data_res <- data.table::data.table(data_res)

data_res[
  admission_date <= ymd("19970331") & discharge_date >= ymd("19970401"),
  r1996_1997 := 1
]
data_res[
  admission_date <= ymd("19980331") & discharge_date >= ymd("19980401"),
  r1997_1998 := 1
]
data_res[
  admission_date <= ymd("19990331") & discharge_date >= ymd("19990401"),
  r1998_1999 := 1
]
data_res[
  admission_date <= ymd("20000331") & discharge_date >= ymd("20000401"),
  r1999_2000 := 1
]
data_res[
  admission_date <= ymd("20010331") & discharge_date >= ymd("20010401"),
  r2000_2001 := 1
]
data_res[
  admission_date <= ymd("20020331") & discharge_date >= ymd("20020401"),
  r2001_2002 := 1
]
data_res[
  admission_date <= ymd("20030331") & discharge_date >= ymd("20030401"),
  r2002_2003 := 1
]
data_res[
  admission_date <= ymd("20040331") & discharge_date >= ymd("20040401"),
  r2003_2004 := 1
]
data_res[
  admission_date <= ymd("20050331") & discharge_date >= ymd("20050401"),
  r2004_2005 := 1
]
data_res[
  admission_date <= ymd("20060331") & discharge_date >= ymd("20060401"),
  r2005_2006 := 1
]
data_res[
  admission_date <= ymd("20070331") & discharge_date >= ymd("20070401"),
  r2006_2007 := 1
]
data_res[
  admission_date <= ymd("20080331") & discharge_date >= ymd("20080401"),
  r2007_2008 := 1
]
data_res[
  admission_date <= ymd("20090331") & discharge_date >= ymd("20090401"),
  r2008_2009 := 1
]
data_res[
  admission_date <= ymd("20100331") & discharge_date >= ymd("20100401"),
  r2009_2010 := 1
]
data_res[
  admission_date <= ymd("20110331") & discharge_date >= ymd("20110401"),
  r2010_2011 := 1
]
data_res[
  admission_date <= ymd("20120331") & discharge_date >= ymd("20120401"),
  r2011_2012 := 1
]
data_res[
  admission_date <= ymd("20130331") & discharge_date >= ymd("20130401"),
  r2012_2013 := 1
]
data_res[
  admission_date <= ymd("20140331") & discharge_date >= ymd("20140401"),
  r2013_2014 := 1
]
data_res[
  admission_date <= ymd("20150331") & discharge_date >= ymd("20150401"),
  r2014_2015 := 1
]
data_res[
  admission_date <= ymd("20160331") & discharge_date >= ymd("20160401"),
  r2015_2016 := 1
]
data_res[
  admission_date <= ymd("20170331") & discharge_date >= ymd("20170401"),
  r2016_2017 := 1
]
data_res[
  admission_date <= ymd("20180331") & discharge_date >= ymd("20180401"),
  r2017_2018 := 1
]
data_res[
  admission_date <= ymd("20190331") & discharge_date >= ymd("20190401"),
  r2018_2019 := 1
]
data_res[
  admission_date <= ymd("20200331") & discharge_date >= ymd("20200401"),
  r2019_2020 := 1
]
data_res[
  admission_date <= ymd("20210331") & discharge_date >= ymd("20210401"),
  r2020_2021 := 1
]
data_res[
  admission_date <= ymd("20220331") & discharge_date >= ymd("20220401"),
  r2021_2022 := 1
]

data_res <- as_tibble(data_res)

# remove those who are not flagged as being a resident in any year (resident=0)
# we do this just to reduce the size of the dataset.
data_res <- data_res %>%
  filter(r1996_1997 == 1 | r1997_1998 == 1 |
    r1998_1999 == 1 | r1999_2000 == 1 |
    r2000_2001 == 1 | r2001_2002 == 1 |
    r2002_2003 == 1 | r2003_2004 == 1 |
    r2004_2005 == 1 | r2005_2006 == 1 |
    r2006_2007 == 1 | r2007_2008 == 1 |
    r2008_2009 == 1 | r2009_2010 == 1 |
    r2010_2011 == 1 | r2011_2012 == 1 |
    r2012_2013 == 1 | r2013_2014 == 1 |
    r2014_2015 == 1 | r2015_2016 == 1 |
    r2016_2017 == 1 | r2017_2018 == 1 |
    r2018_2019 == 1 | r2019_2020 == 1 |
    r2020_2021 == 1 | r2021_2022 == 1)

# Next pull all these variables together so we have multiple rows per patient
# which shows every year they were resident.
data_res <- data_res %>%
  pivot_longer(
    cols = c("r1996_1997":"r2021_2022"),
    names_to = "resident_year",
    values_to = "residents"
  )

# Correct the resident year variable so it has the format 1997/1998
data_res <- data_res %>%
  mutate(year = paste0(substr(resident_year, 2, 5), "/", substr(resident_year, 7, 11)))



###########################################
### SECTION 6 - JOINING DATA FILES ----
###########################################

### 1 - Join the SMR01 and SMR04 files to make basefile ----
data_smr_all <- dplyr::bind_rows(data_smr01, data_smr04)


### 2 - Create a variable to sum over ----
data_smr_all <- data_smr_all %>%
  mutate(count_var = 1)

data_res <- data_res %>%
  mutate(count_var = 1)


### 3 - Updating identifying variables ----
# 'Fill in' the SMR01/04 variables with 0s if case is not from relevant dataset
data_smr_all <- data_smr_all %>%
  mutate(
    SMR01 = ifelse(is.na(SMR01), 0, SMR01),
    SMR04 = ifelse(is.na(SMR04), 0, SMR04)
  )


### 4 - Create new council area variable to work with other scripts ----
# DMT added 2019 HB and CA codes to SMR datasets so no longer need to recode for
# 2019 boundaries

# N.B. - Pre COPPISH data from SMR04 does not contain Council Area codes.

data_smr_all <- data_smr_all %>%
  mutate(council_area = ifelse(council_area_2019 == "S12000033", 1,
    ifelse(council_area_2019 == "S12000034", 2,
      ifelse(council_area_2019 == "S12000041", 3,
        ifelse(council_area_2019 == "S12000035", 4,
          ifelse(council_area_2019 == "S12000026", 5,
            ifelse(council_area_2019 == "S12000005", 6,
              ifelse(council_area_2019 == "S12000039", 7,
                ifelse(council_area_2019 == "S12000006", 8,
                  ifelse(council_area_2019 == "S12000042", 9,
                    ifelse(council_area_2019 == "S12000008", 10,
                      ifelse(council_area_2019 == "S12000045", 11,
                        ifelse(council_area_2019 == "S12000010", 12,
                          ifelse(council_area_2019 == "S12000011", 13,
                            ifelse(council_area_2019 == "S12000036", 14,
                              ifelse(council_area_2019 == "S12000014", 15,
                                ifelse(council_area_2019 == "S12000015", 16,
                                  ifelse(council_area_2019 == "S12000047", 16,
                                    ifelse(council_area_2019 == "S12000046", 17,
                                      ifelse(council_area_2019 == "S12000049", 17,
                                        ifelse(council_area_2019 == "S12000017", 18,
                                          ifelse(council_area_2019 == "S12000018", 19,
                                            ifelse(council_area_2019 == "S12000019", 20,
                                              ifelse(council_area_2019 == "S12000020", 21,
                                                ifelse(council_area_2019 == "S12000021", 22,
                                                  ifelse(council_area_2019 == "S12000044", 23,
                                                    ifelse(council_area_2019 == "S12000050", 23,
                                                      ifelse(council_area_2019 == "S12000023", 24,
                                                        ifelse(council_area_2019 == "S12000024", 25,
                                                          ifelse(council_area_2019 == "S12000048", 25,
                                                            ifelse(council_area_2019 == "S12000038", 26,
                                                              ifelse(council_area_2019 == "S12000027", 27,
                                                                ifelse(council_area_2019 == "S12000028", 28,
                                                                  ifelse(council_area_2019 == "S12000029", 29,
                                                                    ifelse(council_area_2019 == "S12000030", 30,
                                                                      ifelse(council_area_2019 == "S12000040", 31,
                                                                        ifelse(council_area_2019 == "S12000013", 32, NA)
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )) %>%
  mutate(council_area = as.numeric(council_area))


### 5 - NHS Board Name Allocation ----
data_smr_all <- data_smr_all %>%
  mutate(hbtreat_name = case_when(
    hbtreat_currentdate == "S08000015" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "A" ~ "NHS Ayrshire & Arran",
    hbtreat_currentdate == "S08000016" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "B" ~ "NHS Borders",
    hbtreat_currentdate == "S08000017" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "Y" ~ "NHS Dumfries & Galloway",
    hbtreat_currentdate == "S08000029" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "F" ~ "NHS Fife",
    hbtreat_currentdate == "S08000019" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "V" ~ "NHS Forth Valley",
    hbtreat_currentdate == "S08000020" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "N" ~ "NHS Grampian",
    hbtreat_currentdate == "S08000031" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "G" ~ "NHS Greater Glasgow & Clyde",
    hbtreat_currentdate == "S08000022" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "H" ~ "NHS Highland",
    hbtreat_currentdate == "S08000032" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "L" ~ "NHS Lanarkshire",
    hbtreat_currentdate == "S08000024" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "S" ~ "NHS Lothian",
    hbtreat_currentdate == "S08000025" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "R" ~ "NHS Orkney",
    hbtreat_currentdate == "S08000026" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "Z" ~ "NHS Shetland",
    hbtreat_currentdate == "S08000030" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "T" ~ "NHS Tayside",
    hbtreat_currentdate == "S08000028" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "W" ~ "NHS Western Isles",
    hbtreat_currentdate == "S08100001" ~ "National Waiting Times Centre",
    hbtreat_currentdate == "S08100008" ~ "State Hospital",
    hbtreat_currentdate == "S27000001" ~ "Non-NHS Provider",
    TRUE ~ "Null"
  ))


# repeat for hb residence
data_smr_all <- data_smr_all %>%
  mutate(hbres_name = case_when(
    hbres_currentdate == "S08000015" ~ "NHS Ayrshire & Arran",
    hbres_currentdate == "S08000016" ~ "NHS Borders",
    hbres_currentdate == "S08000017" ~ "NHS Dumfries & Galloway",
    hbres_currentdate == "S08000029" ~ "NHS Fife",
    hbres_currentdate == "S08000019" ~ "NHS Forth Valley",
    hbres_currentdate == "S08000020" ~ "NHS Grampian",
    hbres_currentdate == "S08000031" ~ "NHS Greater Glasgow & Clyde",
    hbres_currentdate == "S08000022" ~ "NHS Highland",
    hbres_currentdate == "S08000032" ~ "NHS Lanarkshire",
    hbres_currentdate == "S08000024" ~ "NHS Lothian",
    hbres_currentdate == "S08000025" ~ "NHS Orkney",
    hbres_currentdate == "S08000026" ~ "NHS Shetland",
    hbres_currentdate == "S08000030" ~ "NHS Tayside",
    hbres_currentdate == "S08000028" ~ "NHS Western Isles",
    TRUE ~ "Other"
  ))

# repeat for data_res hb treat
data_res <- data_res %>%
  mutate(hbtreat_name = case_when(
    hbtreat_currentdate == "S08000015" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "A" ~ "NHS Ayrshire & Arran",
    hbtreat_currentdate == "S08000016" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "B" ~ "NHS Borders",
    hbtreat_currentdate == "S08000017" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "Y" ~ "NHS Dumfries & Galloway",
    hbtreat_currentdate == "S08000029" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "F" ~ "NHS Fife",
    hbtreat_currentdate == "S08000019" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "V" ~ "NHS Forth Valley",
    hbtreat_currentdate == "S08000020" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "N" ~ "NHS Grampian",
    hbtreat_currentdate == "S08000031" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "G" ~ "NHS Greater Glasgow & Clyde",
    hbtreat_currentdate == "S08000022" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "H" ~ "NHS Highland",
    hbtreat_currentdate == "S08000032" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "L" ~ "NHS Lanarkshire",
    hbtreat_currentdate == "S08000024" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "S" ~ "NHS Lothian",
    hbtreat_currentdate == "S08000025" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "R" ~ "NHS Orkney",
    hbtreat_currentdate == "S08000026" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "Z" ~ "NHS Shetland",
    hbtreat_currentdate == "S08000030" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "T" ~ "NHS Tayside",
    hbtreat_currentdate == "S08000028" |
      hbtreat_currentdate == "S27000001" &
        substr(location, 1, 1) == "W" ~ "NHS Western Isles",
    hbtreat_currentdate == "S08100001" ~ "National Waiting Times Centre",
    hbtreat_currentdate == "S08100008" ~ "State Hospital",
    hbtreat_currentdate == "S27000001" ~ "Non-NHS Provider",
    TRUE ~ "Null"
  ))


# repeat for data_res hb residence
data_res <- data_res %>%
  mutate(hbres_name = case_when(
    hbres_currentdate == "S08000015" ~ "NHS Ayrshire & Arran",
    hbres_currentdate == "S08000016" ~ "NHS Borders",
    hbres_currentdate == "S08000017" ~ "NHS Dumfries & Galloway",
    hbres_currentdate == "S08000029" ~ "NHS Fife",
    hbres_currentdate == "S08000019" ~ "NHS Forth Valley",
    hbres_currentdate == "S08000020" ~ "NHS Grampian",
    hbres_currentdate == "S08000031" ~ "NHS Greater Glasgow & Clyde",
    hbres_currentdate == "S08000022" ~ "NHS Highland",
    hbres_currentdate == "S08000032" ~ "NHS Lanarkshire",
    hbres_currentdate == "S08000024" ~ "NHS Lothian",
    hbres_currentdate == "S08000025" ~ "NHS Orkney",
    hbres_currentdate == "S08000026" ~ "NHS Shetland",
    hbres_currentdate == "S08000030" ~ "NHS Tayside",
    hbres_currentdate == "S08000028" ~ "NHS Western Isles",
    TRUE ~ "Other"
  ))



###############################
### SECTION 7 - SAVE OUTPUT----
###############################

# Sort data before saving output files
data_smr_all <- data_smr_all %>%
  arrange(link_no, cis_marker, admission_date, discharge_date)

data_res <- data_res %>%
  arrange(link_no, cis_marker, admission_date, discharge_date)

# Save basefile so it can be used in subsequent syntaxes
saveRDS(data_smr_all, file = paste0(output, "SMR01_SMR04_basefile.rds"))
saveRDS(data_res, file = paste0(output, "SMR04_basefile_RESIDENTS.rds"))

rm(data_smr01)
rm(data_smr04)
rm(data_res)
rm(data_smr_all)

# Clear environment
rm(list = ls())
### END OF SCRIPT###
