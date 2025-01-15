# SMR04 Quert

query_smr4 <- paste(
  "SELECT PATIENT_IDENTIFIER, ADMISSION_DATE, ADMISSION, ADMISSION_TYPE,SPECIALTY, CIS_MARKER,
                   HBRES_CURRENTDATE, HBTREAT_CURRENTDATE, COUNCIL_AREA_2019, LOCATION, DATAZONE_2011,
                   DISCHARGE_DATE, DISCHARGE, MANAGEMENT_OF_PATIENT, DISCHARGE_TYPE, MAIN_CONDITION, SEX, AGE_IN_YEARS,
                   OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3,OTHER_CONDITION_4, OTHER_CONDITION_5,
                   LINK_NO, LENGTH_OF_STAY,RECORD_TYPE,URI, SORT_MARKER, UPI_NUMBER",
  "FROM ANALYSIS.SMR04_PI WHERE (DISCHARGE_DATE >= TO_DATE('2016-04-01','YYYY-MM-DD'))"
)

channel <- odbc::dbConnect(
  drv = odbc::odbc(),
  dsn = "SMRA",
  uid = Sys.getenv("USER"),
  pwd = rstudioapi::askForPassword("Enter LDAP password:")
)

smr4_extract <- as_tibble(dbGetQuery(channel,
  statement = query_smr4
)) %>%
  clean_names()

dbDisconnect(channel)

# 9. MH Emergency Admissions ---- Rate of discharges for psychiatric by financial year
# (Calculated using "Stays" method from MH Inpatient Activity Publication)
# _____________________________________________________________________________

smr_mh_emergency_adm <- smr4_extract %>%
  arrange(link_no, admission_date, discharge_date) %>% # arrange chronologically
  filter(specialty %in% c("G1", "G2", "G3", "G4", "G5", "G6", "G21", "G22")) %>% # Choose all psychiatric specialties
  group_by(link_no, cis_marker) %>% # group by stay
  summarise(
    admission_date = min(admission_date), # first admission date
    discharge_date = max(discharge_date), # final discharge date
    datazone2011 = first(datazone_2011),
    los = sum(length_of_stay),
    admission_type = first(admission_type), # first admission type (for emergency adm)
    age = first(age_in_years), # age at admission
    main_condition = first(main_condition), # main condition @ admissions
    location = first(location),
    episodes = n()
  ) %>%
  ungroup() %>%
  mutate(adm = 1) %>%
  mutate(age_group = case_when( # our required age groupings
    between(age, 0, 17) ~ "0 - 17",
    between(age, 18, 44) ~ "18 - 44",
    between(age, 45, 64) ~ "45 - 64",
    age >= 65 ~ "65+"
  )) %>%
  mutate(month_num = month(discharge_date)) %>%
  mutate(year = year(discharge_date)) %>%
  mutate(AdDate = make_datetime(year, month_num, 1)) %>%
  mutate(financial_year = phsmethods::extract_fin_year(AdDate)) %>% # creates variable for fin year
  drop_na(financial_year) %>%
  filter(admission_type %in% c("30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "20", "21", "22", "18")) %>% # emergency admissions only
  mutate(main_con = substr(main_condition, 1, 3)) %>%
  mutate(diag = case_when(
    main_con %in% "F[00-09]" ~ "Dementia",
    main_con %in% "F[10-19]" ~ "Psychoactive Substance Misuse",
    main_con %in% "F[20-29]" ~ "Schizophrenia",
    main_con %in% "F[30-49]" ~ "Mood (affective) disorders",
    main_con %in% "F[60-69]" ~ "Adult personality and behavioural disorders",
    main_con %in% c("F[50-59]", "F[70-99]") ~ "Adolescent behavioural and emotional disorders"
  )) %>%
  # mutate(diag = fct_explicit_na(diag, na_level = "Other")) %>%
  mutate(diag = ifelse(diag %in% c(
    "Dementia", "Psychoactive Substance Misuse", "Schizophrenia",
    "Mood (affective) disorders", "Adult personality and behavioural disorders",
    "Adolescent behavioural and emotional disorders"
  ), diag, "Other")) %>%
  arrange(datazone2011) %>%
  left_join(datazones, by = "datazone2011") %>%
  arrange(link_no, cis_marker, admission_date, discharge_date, desc(admission_type)) %>%
  group_by(financial_year, hscp2019name, hscp_locality, age_group, diag) %>%
  summarise(admissions = sum(adm)) %>%
  as.data.frame()
