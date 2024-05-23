# MSG Indicators - Indicator 3: A&E Attendances
# Uses queries from A&E datamart to produce LIST breakdown file


#### 2. LIST breakdown file ----

# Note that you will have to have a copy of the A&E data from all previous years for this to work

col_names <- c(
  "cal_year",
  "month_num",
  "month_year",
  "council",
  "age_group",
  "ref_source",
  "area_treated",
  "location",
  "location_name",
  "datazone2011",
  "attendances",
  "admissions",
  "number_meeting_target"
)

years <- c("1718", "1819", "1920", "2021", "2122", "2223")

# For the below to work all the data for the years (above) must be in the same 
# `data_folder`. It also must be named: `MSG-MonthlyUpdate-Breakdowns-{year}.zip`
admissions <- map(
  years,
    \(year) {
      read_csv(
        # Read directly from zip file from BOXI
        path(data_folder, "AE", glue("MSG-MonthlyUpdate-Breakdowns-{year}.zip")),
        # Data starts on row 4
        skip = 4,
        # Apply old column names to the new data
        col_names = col_names,
        # Define column types (n = numeric, c = character)
        col_types = "nnccccccccnnn",
        lazy = TRUE
      ) |> 
        # Remove empty councils
        filter(council != "" | !is.na(council))
    }
) |> 
  list_rbind()

iz_lkp <- read_in_localitiesiz2(dz_level = T)

# Bind rows of old data and new data together
# temp_ae <- bind_rows(old_admissions, new_admissions) %>%
temp_ae <- admissions %>%
  # Get locality names
  left_join(iz_lkp, by = c("datazone2011")) %>%
  # Define empty localities as unknown
  mutate(hscp_locality = replace_na(hscp_locality, "Unknown")) %>%
  relocate(hscp_locality, .before = datazone2011) %>%
  # Turn month into a date
  mutate(month = dmy(glue("01-{month_num}-{cal_year}"))) %>%
  select(month, council, age_group, ref_source, area_treated, location, location_name, hscp_locality, intzone2011, attendances, admissions, number_meeting_target)

# aggregate to get breakdown file
final_output <- temp_ae %>%
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
write_parquet(final_output, path(data_folder, "3-A&E-Breakdowns.parquet"))
