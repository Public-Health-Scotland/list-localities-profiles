# Code used to produce a table of what services are in the locality.
# The data manipulation is done in script "2a. hscp_services_data_manipulation.R".
# The map is created in script "3. Services HSCP Map".

###### 1. Set up ----

## Set Locality (for testing only)
# LOCALITY <- "Falkirk West"

## Set file path
# lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

##Source in functions code
# source("Master RMarkdown Document & Render Code/Global Script.R")

## Locality lookup
# lookup <- read_in_localities(dz_level = TRUE)

## Determine HSCP (for testing only)
# HSCP <- as.character(filter(lookup, hscp_locality == LOCALITY)$hscp2019name)

## Source the HSCP level data for completeness
# source("Services/2a. hscp_services_data_manipulation.R")

###### 2. Table ----

# Subset care which is not Elderly care for table
other_care_type <- care_homes %>%
  select(
    type = care_service,
    subtype,
    name = service_name,
    service_postcode
  ) %>%
  filter(type == "Care Home Service") %>%
  filter(subtype != "Older People") %>%
  mutate(postcode = gsub(" ", "", service_postcode, fixed = TRUE)) %>%
  left_join(postcode_lkp, by = "postcode") %>%
  filter(hscp_locality == LOCALITY)

# Create table
services_tibble <- tibble(
  Type = c("Primary Care", "A&E", "", "Care Home", ""),
  Service = c(
    "GP Practice",
    "Emergency Department",
    "Minor Injuries Unit",
    "Elderly Care",
    "Other"
  ),
  Number = c(
    sum(markers_gp[["hscp_locality"]] == LOCALITY),
    sum(markers_emergency_dep[["hscp_locality"]] == LOCALITY),
    sum(markers_miu[["hscp_locality"]] == LOCALITY),
    sum(markers_care_home[["hscp_locality"]] == LOCALITY),
    nrow(other_care_type)
  )
)

# Housekeeping ----
rm(
  other_care_type
)
