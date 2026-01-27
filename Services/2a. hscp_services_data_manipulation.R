# This script prepares the services data at the HSCP level.
# The locality-specific table is created in "2b. locality_services_table.R".
# The map is created in script "3. Services HSCP Map".

###### 1. Set up ----

# Change year to be the year in the data folder name
ext_year <- 2024

## Set HSCP (for testing only)
# HSCP <- "Falkirk"

## Set file path
# lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Source in functions code
# source("Master RMarkdown Document & Render Code/Global Script.R")

### Geographical lookups and objects ----

# Locality lookup
lookup <- read_in_localities()

# Get number of localities in HSCP
# This script assumes that 'HSCP' is an object available in the environment
n_loc <- count_localities(lookup, HSCP)


###### 2. Read in services data ----

## Read in Postcode file for latitudes and longitudes

postcode_lkp <- read_in_postcodes() %>%
  mutate(postcode = gsub(" ", "", pc7, fixed = TRUE)) %>%
  select(
    postcode,
    grid_reference_easting,
    grid_reference_northing,
    latitude,
    longitude,
    datazone2011,
    hscp_locality,
    hscp2019name,
    hscp2019,
    hb2019name,
    hb2019
  )


## Read in all data in services folder

services_file_names <- list.files(
  paste0(lp_path, "Services/DATA ", ext_year),
  pattern = "RDS"
)

for (file in services_file_names) {
  name <- substr(x = file, 1, 4)

  data <- readRDS(paste0(lp_path, "Services/DATA ", ext_year, "/", file)) %>%
    clean_names()

  assign(name, data)
}

# Change to more straightforward names
hosp_postcodes <- curr
hosp_types <- hosp
care_homes <- MDSF

rm(curr, hosp, MDSF)


###### 3. Manipulate services data ######

## GP Practices ----

prac <- prac %>%
  select(practice_code, gp_practice_name, practice_list_size, postcode) %>%
  mutate(postcode = gsub(" ", "", postcode, fixed = TRUE))

# Merge practice data with postcode and locality lookups
markers_gp <- left_join(prac, postcode_lkp, by = "postcode") %>%
  mutate(type = "GP Practice") %>%
  # filter out HSCP for map
  filter(hscp2019name == HSCP)

## Emergency Departments and MIUs ----

# rename for hospital_code to location
hosp_postcodes <- rename(hosp_postcodes, location = hospital_code)

# create hospital lookup table
hosp_lookup <- hosp_types %>%
  filter(status == "Open") %>%
  select(
    name = treatment_location_name,
    location = treatment_location_code,
    type = current_department_type
  ) %>%
  left_join(
    select(hosp_postcodes, location, postcode),
    by = join_by(location)
  ) %>%
  mutate(postcode = gsub(" ", "", postcode, fixed = TRUE)) %>%
  left_join(postcode_lkp, by = "postcode")

# MIUs
markers_miu <- hosp_lookup %>%
  filter(type == "Minor Injury Unit or Other") %>%
  filter(hscp2019name == HSCP)

# EDs
markers_emergency_dep <- hosp_lookup %>%
  filter(type == "Emergency Department") %>%
  filter(hscp2019name == HSCP)

Clacks_Royal <- filter(hosp_lookup, name == "Forth Valley Royal Hospital")

# Ninewells hospital is incorrectly mapped even though postcode ok - so corrected coords here

if (HSCP == "Dundee City") {
  markers_emergency_dep <- markers_emergency_dep %>%
    mutate(
      latitude = if_else(latitude == 56.4617, 56.4659308, latitude),
      longitude = if_else(longitude == -2.991432, -3.0378506, longitude)
    )
}

if (HSCP == "Clackmannanshire & Stirling") {
  markers_emergency_dep <- rbind(markers_emergency_dep, Clacks_Royal)
}

## Care Homes ----

markers_care_home <- care_homes %>%
  select(
    type = care_service,
    subtype,
    name = service_name,
    service_postcode
  ) %>%
  filter(type == "Care Home Service") %>%
  filter(subtype == "Older People") %>%
  mutate(postcode = gsub(" ", "", service_postcode, fixed = TRUE)) %>%
  left_join(postcode_lkp, by = "postcode") %>%
  filter(hscp2019name == HSCP)


# Housekeeping ----
rm(
  data,
  file,
  hosp_lookup,
  name,
  prac,
  services_file_names
)
gc()
