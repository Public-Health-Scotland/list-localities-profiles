############################################################
## Codename - 011_SMR01SMR04_geography                    ##
## Data Release - Annual SMR04 Publication                ##
## Original Authors - Cormac Murray                       ##
## Orginal Date - July 2019                               ##
## Latest Update Author - Alex Bruce                      ##
## Latest Update Date - November 2022                     ##
## Updates to script (if any):                            ##
## 2021 - AB                                              ##
## - Changed some wording that incorrectly refered to HBs ##
## instead of Council Areas                               ##
##                                                        ##
## Type - Extraction/preparation                          ##
## Written/run on - R Studio SERVER                       ##
## R version - 3.6.1                                      ##
## Versions of packages -                                 ##
## tidyverse_1.3.0                                        ##
## lubridate_1.7.9                                        ##
##                                                        ##
## Description - Extracts SMR01 data and creates basefile ##
## by adding it to SMR04 basefile for subsequent work     ##
##                                                        ##
## Approximate run time:6 minutes                         ##
############################################################

###################################
### SECTION 1 - HOUSE KEEPING  ----
###################################

### 1 - Load packages ----
library(tidyverse) # tidyr_1.1.2; dplyr_1.0.2
library(lubridate) # for formatting dates
# library(haven)          # for reading spss files
# library(foreign)        # for accessing SPSS data


### 2 -  Publication Date variable to define filepath ----
# MUST BE UPDATED EACH TIME WE PUBLISH
pub_date <- "20221213"


### 3 -  Set Filepaths ----
input <- paste0(
  "//PHI_conf/MentalHealth1/Inpatient care/Publications/Hospital-Inpatient-Care-of-People-with-Mental-Health-Problems-in-Scotland/",
  pub_date, "/Data/"
)
output <- paste0(
  "//PHI_conf/MentalHealth1/Inpatient care/Publications/Hospital-Inpatient-Care-of-People-with-Mental-Health-Problems-in-Scotland/",
  pub_date, "/Output/"
)
lookups <- paste0(
  "//PHI_conf/MentalHealth1/Inpatient care/Publications/Hospital-Inpatient-Care-of-People-with-Mental-Health-Problems-in-Scotland/",
  pub_date, "/Lookups/"
)


### 4 - Define min and max years ----
# must be updated each time we publish
min_year <- 1997
max_year <- 2021



################################
### SECTION 2 - DATA LOADING----
################################

### 1 - Read in basefiles ----
data <- readRDS(paste0(input, "SMR01_SMR04_basefile.rds"))

ca_pop <- readRDS(paste0(lookups, "CA Populations.rds"))

### 2 - Data preparation.
# First sort and recode missing specialties & hospital types so we don't lose these in the subset
data <- data %>%
  arrange(link_no, cis_marker, admission_date, discharge_date) %>%
  mutate(
    specialty = ifelse(is.na(specialty), "NA", specialty),
    hospital_type = ifelse(is.na(hospital_type), "NA",
      as.character(hospital_type)
    )
  )


######################################
### SECTION 3 - DATA ANALYSIS  ----
######################################

### 1 - Produce discharges, broken down by financial year and health board ----
discharge_data <- data %>%
  filter(fyear_pt1_dis >= min_year & fyear_pt1_dis <= max_year) %>%
  filter(specialty != "G5") %>%
  filter(hospital_type != 2) %>%
  group_by(fyear, council_area) %>%
  summarise(
    DischargesTotal = sum(count_var),
    DischargesNonPsychiatric = sum(SMR01),
    DischargesPsychiatric = sum(SMR04)
  ) %>%
  ungroup() %>%
  complete(fyear, council_area, fill = list(DischargesTotal = 0, DischargesNonPsychiatric = 0, DischargesPsychiatric = 0))

## Checks (Should all return 'TRUE')
sum(discharge_data$DischargesNonPsychiatric) + sum(discharge_data$DischargesPsychiatric) == sum(discharge_data$DischargesTotal)
# The total number of entries in the discharges file should be the number of years covered,
# multiplied by the number of health boards (both treatment and residents).
(max_year - min_year + 1) * length(unique(data$council_area)) == nrow(discharge_data)


### 2 - Produce patients, broken down by financial year and council area ----
patients_data <- data %>%
  filter(fyear_pt1_dis >= min_year & fyear_pt1_dis <= max_year) %>%
  filter(specialty != "G5") %>%
  filter(hospital_type != 2) %>%
  group_by(link_no, fyear, council_area, SMR01, SMR04) %>%
  summarise(
    specialty = last(specialty),
    hospital_type = last(hospital_type)
  ) %>%
  mutate(count_var = 1) %>%
  ungroup()

# second aggregation to count number of patients
patients_data <- patients_data %>%
  group_by(link_no, fyear, council_area) %>%
  summarise(
    count_var = max(count_var),
    SMR01 = max(SMR01),
    SMR04 = max(SMR04)
  ) %>%
  group_by(fyear, council_area) %>%
  summarise(
    PatientsTotal = sum(count_var),
    PatientsNonPsychiatric = sum(SMR01),
    PatientsPsychiatric = sum(SMR04)
  ) %>%
  ungroup() %>%
  complete(fyear, council_area, fill = list(PatientsTotal = 0, PatientsNonPsychiatric = 0, PatientsPsychiatric = 0))


## Checks (Should all return 'TRUE')

# Patients can appear in both SMR01 and SMR04 for a given year/council area.
sum(patients_data$PatientsNonPsychiatric) + sum(patients_data$PatientsPsychiatric) >= sum(patients_data$PatientsTotal)

# The total number of entries in the patients file should be the number of years covered,
# multiplied by the number of health boards (both treatment and residents).
(max_year - min_year + 1) * length(unique(data$council_area)) == nrow(patients_data)


### 3 - Combine patient and discharge data ----
combined_data <- left_join(discharge_data, patients_data, by = c("fyear", "council_area"))


### 4 - Exclude rows with NA values among council areas ----
combined_data <- combined_data %>%
  filter(council_area >= 1 & council_area <= 32)


### 5 - Produce financial years for council area populations ----
ca_pop <- ca_pop %>%
  filter(Year >= min_year & Year <= max_year) %>%
  mutate(fyear = paste0(Year, "/", Year + 1))


### 6 - Add CA labels for matching ----
ca_pop <- ca_pop %>%
  mutate(ca = ifelse(council_area == "S12000033", 1,
    ifelse(council_area == "S12000034", 2,
      ifelse(council_area == "S12000041", 3,
        ifelse(council_area == "S12000035", 4,
          ifelse(council_area == "S12000026", 5,
            ifelse(council_area == "S12000005", 6,
              ifelse(council_area == "S12000039", 7,
                ifelse(council_area == "S12000006", 8,
                  ifelse(council_area == "S12000042", 9,
                    ifelse(council_area == "S12000008", 10,
                      ifelse(council_area == "S12000045", 11,
                        ifelse(council_area == "S12000010", 12,
                          ifelse(council_area == "S12000011", 13,
                            ifelse(council_area == "S12000036", 14,
                              ifelse(council_area == "S12000014", 15,
                                ifelse(council_area == "S12000015", 16,
                                  ifelse(council_area == "S12000047", 16,
                                    ifelse(council_area == "S12000046", 17,
                                      ifelse(council_area == "S12000049", 17,
                                        ifelse(council_area == "S12000017", 18,
                                          ifelse(council_area == "S12000018", 19,
                                            ifelse(council_area == "S12000019", 20,
                                              ifelse(council_area == "S12000020", 21,
                                                ifelse(council_area == "S12000021", 22,
                                                  ifelse(council_area == "S12000044", 23,
                                                    ifelse(council_area == "S12000050", 23,
                                                      ifelse(council_area == "S12000023", 24,
                                                        ifelse(council_area == "S12000024", 25,
                                                          ifelse(council_area == "S12000048", 25,
                                                            ifelse(council_area == "S12000038", 26,
                                                              ifelse(council_area == "S12000027", 27,
                                                                ifelse(council_area == "S12000028", 28,
                                                                  ifelse(council_area == "S12000029", 29,
                                                                    ifelse(council_area == "S12000030", 30,
                                                                      ifelse(council_area == "S12000040", 31,
                                                                        ifelse(council_area == "S12000013", 32, "Null")
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
  ))


### 7 - Refine Dataset ----
ca_pop <- ca_pop %>%
  select(fyear, ca, Pop) %>%
  mutate(ca = as.numeric(ca)) %>%
  rename(council_area = ca)


### 6 - Join populations onto combined data ----
# Join population onto combined data
data_with_pops <- left_join(combined_data, ca_pop, by = c("fyear", "council_area"))


### 7 - Apply Rounding to all figures - round to nearest 10 ----
data_with_pops <- data_with_pops %>%
  mutate(
    DischargesTotal = plyr::round_any(DischargesTotal, 10),
    DischargesNonPsychiatric = plyr::round_any(DischargesNonPsychiatric, 10),
    DischargesPsychiatric = plyr::round_any(DischargesPsychiatric, 10),
    PatientsTotal = plyr::round_any(PatientsTotal, 10),
    PatientsNonPsychiatric = plyr::round_any(PatientsNonPsychiatric, 10),
    PatientsPsychiatric = plyr::round_any(PatientsPsychiatric, 10)
  )


### 8 - Calculate rates ----
data_with_pops <- data_with_pops %>%
  mutate(
    DischargesTotal_Rates = round((DischargesTotal / Pop * 100000), 2),
    DischargesNonPsychiatric_Rates = round((DischargesNonPsychiatric / Pop * 100000), 2),
    DischargesPsychiatric_Rates = round((DischargesPsychiatric / Pop * 100000), 2),
    PatientsTotal_Rates = round((PatientsTotal / Pop * 100000), 2),
    PatientsNonPsychiatric_Rates = round((PatientsNonPsychiatric / Pop * 100000), 2),
    PatientsPsychiatric_Rates = round((PatientsPsychiatric / Pop * 100000), 2)
  ) %>%
  select(fyear, council_area, DischargesTotal_Rates:PatientsPsychiatric_Rates)


### 9 - Gather rates into a long format, and apply council area labels ----
data_with_pops <- data_with_pops %>%
  mutate(ca = ifelse(council_area == 1, "Aberdeen City",
    ifelse(council_area == 2, "Aberdeenshire",
      ifelse(council_area == 3, "Angus",
        ifelse(council_area == 4, "Argyll and Bute",
          ifelse(council_area == 5, "Scottish Borders",
            ifelse(council_area == 6, "Clackmannanshire",
              ifelse(council_area == 7, "West Dunbartonshire",
                ifelse(council_area == 8, "Dumfries and Galloway",
                  ifelse(council_area == 9, "Dundee City",
                    ifelse(council_area == 10, "East Ayrshire",
                      ifelse(council_area == 11, "East Dunbartonshire",
                        ifelse(council_area == 12, "East Lothian",
                          ifelse(council_area == 13, "East Renfrewshire",
                            ifelse(council_area == 14, "City of Edinburgh",
                              ifelse(council_area == 15, "Falkirk",
                                ifelse(council_area == 16, "Fife",
                                  ifelse(council_area == 17, "Glasgow City",
                                    ifelse(council_area == 18, "Highland",
                                      ifelse(council_area == 19, "Inverclyde",
                                        ifelse(council_area == 20, "Midlothian",
                                          ifelse(council_area == 21, "Moray",
                                            ifelse(council_area == 22, "North Ayrshire",
                                              ifelse(council_area == 23, "North Lanarkshire",
                                                ifelse(council_area == 24, "Orkney Islands",
                                                  ifelse(council_area == 25, "Perth and Kinross",
                                                    ifelse(council_area == 26, "Renfrewshire",
                                                      ifelse(council_area == 27, "Shetland Islands",
                                                        ifelse(council_area == 28, "South Ayrshire",
                                                          ifelse(council_area == 29, "South Lanarkshire",
                                                            ifelse(council_area == 30, "Stirling",
                                                              ifelse(council_area == 31, "West Lothian",
                                                                ifelse(council_area == 32, "Na h-Eileanan an Iar", "Null")
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
  ))


data_with_pops_long <- data_with_pops %>%
  pivot_longer(
    c(
      DischargesTotal_Rates, DischargesNonPsychiatric_Rates, DischargesPsychiatric_Rates,
      PatientsTotal_Rates, PatientsNonPsychiatric_Rates, PatientsPsychiatric_Rates
    ),
    names_to = "RateType", values_to = "Value"
  )

data_with_pops_long <- data_with_pops_long %>%
  mutate(measure = ifelse(RateType == "DischargesTotal_Rates", "Crude rate of discharges (per 100,000 population)",
    ifelse(RateType == "DischargesNonPsychiatric_Rates", "Crude rate of discharges (per 100,000 population)",
      ifelse(RateType == "DischargesPsychiatric_Rates", "Crude rate of discharges (per 100,000 population)",
        ifelse(RateType == "PatientsTotal_Rates", "Crude rate of patients (per 100,000 population)",
          ifelse(RateType == "PatientsNonPsychiatric_Rates", "Crude rate of patients (per 100,000 population)",
            ifelse(RateType == "PatientsPsychiatric_Rates", "Crude rate of patients (per 100,000 population)", "Null")
          )
        )
      )
    )
  )) %>%
  mutate(dataset = ifelse(RateType == "DischargesTotal_Rates", "Total",
    ifelse(RateType == "DischargesNonPsychiatric_Rates", "Non-psychiatric",
      ifelse(RateType == "DischargesPsychiatric_Rates", "Psychiatric",
        ifelse(RateType == "PatientsTotal_Rates", "Total",
          ifelse(RateType == "PatientsNonPsychiatric_Rates", "Non-psychiatric",
            ifelse(RateType == "PatientsPsychiatric_Rates", "Psychiatric", "Null")
          )
        )
      )
    )
  )) %>%
  select(fyear, ca, measure, dataset, Value) %>%
  arrange(fyear, ca, measure, dataset, Value)


######################################
### SECTION 4 - SAVE OUT DATA    ----
######################################

# to export into pub folder
write.csv(data_with_pops_long, file = paste0(output, "SMR01SMR04_geographyrates.csv"), row.names = FALSE)

### END OF SCRIPT ###
