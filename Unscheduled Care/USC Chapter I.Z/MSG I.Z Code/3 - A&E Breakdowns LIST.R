# MSG Indicators - Indicator 3: A&E Attendances
# Uses queries from A&E datamart to produce LIST breakdown file

# set current year
#current_year <- "2223"


#### 2. LIST breakdown file ----

# Use this section only when you have updated A&E data from a previous year
# i <- 1718
# old_temp <- list()
# while (i < as.numeric(current_year)){
#   finyear <- as.character(i)
#   old_temp[[finyear]] <- read_rds(glue("Data/3-A&E-{finyear}.rds")) %>% clean_names()
#   i <- i + 101}
# old_temp <- bind_rows(old_temp) %>%
#   clean_names() %>%
#   filter(!(month %in% c("Jan 2017", "Feb 2017", "Mar 2017")))
# write_rds(old_temp, "Data/3-Complete-Years.rds", compress = "gz")

# Note that you will have to have a copy of the A&E data from all previous years for this to work

col_names <- c("cal_year","month_num","month_year","council","age_group","ref_source","area_treated","location","location_name","datazone2011","attendances","admissions","number_meeting_target")

#old_admissions <- read_rds("Data/3-Complete-Years.rds")

ad_1718 <- read_csv(
  # Read directly from zip file from BOXI
  glue("./Unscheduled Care/DATA 2022/MSG-MonthlyUpdate-Breakdowns-1718.zip"),
  # Data starts on row 4
  skip = 4,
  # Apply old column names to the new data
  col_names = col_names,
  # Define column types (n = numeric, c = character)
  col_types = "nnccccccccnnn"
) %>% 
  # Remove empty councils
 filter(council != "" | !is.na(council))

ad_1819 <- read_csv(
  # Read directly from zip file from BOXI
  glue("./Unscheduled Care/DATA 2022/MSG-MonthlyUpdate-BreakdownsCF-1819.zip"),
  # Data starts on row 4
  skip = 4,
  # Apply old column names to the new data
  col_names = col_names,
  # Define column types (n = numeric, c = character)
  col_types = "nnccccccccnnn"
) %>% 
  # Remove empty councils
  filter(council != "" | !is.na(council))


ad_1920 <- read_csv(
  # Read directly from zip file from BOXI
  glue("./Unscheduled Care/DATA 2022/MSG-MonthlyUpdate-BreakdownsCF-1920.zip"),
  # Data starts on row 4
  skip = 4,
  # Apply old column names to the new data
  col_names = col_names,
  # Define column types (n = numeric, c = character)
  col_types = "nnccccccccnnn"
) %>% 
  # Remove empty councils
  filter(council != "" | !is.na(council))

ad_2021 <- read_csv(
  # Read directly from zip file from BOXI
  glue("./Unscheduled Care/DATA 2022/MSG-MonthlyUpdate-BreakdownsCF-2021.zip"),
  # Data starts on row 4
  skip = 4,
  # Apply old column names to the new data
  col_names = col_names,
  # Define column types (n = numeric, c = character)
  col_types = "nnccccccccnnn"
) %>% 
  # Remove empty councils
  filter(council != "" | !is.na(council))

ad_2122 <- read_csv(
  # Read directly from zip file from BOXI
  glue("./Unscheduled Care/DATA 2022/MSG-MonthlyUpdate-Breakdowns-2122.zip"),
  # Data starts on row 4
  skip = 4,
  # Apply old column names to the new data
  col_names = col_names,
  # Define column types (n = numeric, c = character)
  col_types = "nnccccccccnnn"
) %>% 
  # Remove empty councils
  filter(council != "" | !is.na(council))


#new_admissions <- read_csv(
  # Read directly from zip file from BOXI
 # glue("./Unscheduled Care/DATA 2022/MSG-MonthlyUpdate-Breakdowns-1718.zip"),
  # Data starts on row 4
 # skip = 4,
  # Apply old column names to the new data
 # col_names = colnames(old_admissions),
  # Define column types (n = numeric, c = character)
#  col_types = "nnccccccccnnn"
#) %>% 
  # Remove empty councils
 # filter(council != "" | !is.na(council))

admissions <- bind_rows(ad_1718,ad_1819,ad_1920,ad_2021,ad_2122)

iz_lkp <- read_in_localitiesiz2(dz_level = T)

# Bind rows of old data and new data together
#temp_ae <- bind_rows(old_admissions, new_admissions) %>%
temp_ae <- admissions %>%
  # Get locality names
  left_join(., iz_lkp, by = c("datazone2011")) %>%
  # Define empty localities as unknown
  mutate(hscp_locality = replace_na(hscp_locality, "Unknown")) %>%
  relocate(hscp_locality, .before = datazone2011) %>%
  # Turn month into a date
  mutate(month = dmy(glue("01-{month_num}-{cal_year}"))) %>% 
  select(month,council,age_group,ref_source,area_treated,location,location_name,hscp_locality,intzone2011,attendances,admissions,number_meeting_target)

# save out temp file
arrow::write_parquet(temp_ae,path(data_folder,'A&E_temp.rds'))

# aggregate to get breakdown file
final_output <-  temp_ae %>%
  # Convert to lazy data table for quick aggregate
  lazy_dt() %>% 
  # Aggregate
  group_by(across(month:intzone2011)) %>% 
  summarise(across(attendances:number_meeting_target, sum, na.rm = TRUE)) %>% 
  # Change output to tibble
  as_tibble() %>% 
  # Quick rename for ease
  rename(locality = hscp_locality) %>%
  # Get rid of missing age groups
  mutate(age_group = if_else(is.na(age_group), "", age_group))

# save out R breakdown file
#write_rds(final_output, "Data/3-A&E-Breakdowns.rds", compress = "gz")

arrow::write_parquet(final_output,path(data_folder,'3-A&E-Breakdowns.parquet'))


