# LOCALITY PROFILES SOCIAL CARE SLF DATA CODE

# Code used to create a Social Care extract from the individual
# Source Linkage Files.

# Set-up ----

# Clear environment
rm(list = ls())
gc()


# Set year for data extracts folder
ext_year <- 2025


# Set financial year to use for SLFs
#
# Format example:
# FY 2024/25 = "2425"
fy <- "2425"


# Source Locality Profile functions
source(
  "Master RMarkdown Document & Render Code/Global Script.R"
)


# Additional packages
library(arrow)
library(dplyr)
library(fs)
library(glue)
library(slfhelper)
library(stringr)
library(tidyr)


# Set file paths ----

lp_path <- path(
  "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles"
)


social_care_data_dir <- path(
  lp_path,
  "Social Care",
  glue("DATA {ext_year}")
)


if (!dir_exists(social_care_data_dir)) {
  
  dir_create(
    social_care_data_dir,
    recurse = TRUE
  )
}


# Locality lookup ----

lookup <- read_in_localities(
  dz_level = TRUE
)


# Variables required from the individual SLF ----

social_care_variables <- c(
  
  # Person identifiers
  "anon_chi",
  "social_care_id",
  "person_id",
  
  # Reporting year
  "year",
  
  # Demographics
  "gender",
  "age",
  "postcode",
  "dob",
  "death_date",
  "deceased",
  
  # Geography
  "datazone2011",
  "datazone2022",
  "hscp2018",
  "lca",
  "locality",
  
  # Population denominator
  "keep_population",
  
  # SIMD
  "simd2020v2_rank",
  "simd2020v2_sc_decile",
  "simd2020v2_sc_quintile",
  "simd2020v2_hb2019_decile",
  "simd2020v2_hb2019_quintile",
  "simd2020v2_hscp2019_decile",
  "simd2020v2_hscp2019_quintile",
  
  # Care at Home
  "hc_episodes",
  "hc_total_hours",
  "hc_total_cost",
  
  # Care at Home personal care
  "hc_personal_episodes",
  "hc_personal_hours",
  "hc_personal_hours_cost",
  
  # Care at Home non-personal care
  "hc_non_personal_episodes",
  "hc_non_personal_hours",
  "hc_non_personal_hours_cost",
  
  # Care at Home reablement
  "hc_reablement_episodes",
  "hc_reablement_hours",
  "hc_reablement_hours_cost",
  
  # Care Home
  "ch_cis_episodes",
  "ch_beddays",
  "ch_cost",
  
  # Social-care client groups
  "sc_dementia",
  "sc_learning_disability",
  "sc_mental_health_disorders",
  "sc_physical_and_sensory_disability",
  "sc_drugs",
  "sc_alcohol",
  "sc_palliative_care",
  "sc_carer",
  "sc_elderly_frail",
  "sc_neurological_condition",
  "sc_autism",
  "sc_other_vulnerable_groups",
  
  # Other social-care characteristics
  "sc_living_alone",
  "sc_support_from_unpaid_carer",
  "sc_social_worker",
  "sc_type_of_housing",
  "sc_meals",
  "sc_day_care",
  "sc_latest_submission",
  
  # A&E activity
  "ae_attendances",
  "ae_cost",
  
  # Continuous inpatient journeys
  "cij_total",
  "cij_el",
  "cij_non_el",
  "cij_mat",
  "cij_delay",
  
  # Acute inpatient activity
  "acute_inpatient_episodes",
  "acute_inpatient_beddays",
  "acute_el_inpatient_episodes",
  "acute_el_inpatient_beddays",
  "acute_non_el_inpatient_episodes",
  "acute_non_el_inpatient_beddays",
  
  # Geriatric long-stay activity
  "gls_inpatient_episodes",
  "gls_inpatient_beddays",
  "gls_el_inpatient_episodes",
  "gls_el_inpatient_beddays",
  "gls_non_el_inpatient_episodes",
  "gls_non_el_inpatient_beddays",
  
  # Mental-health inpatient activity
  "mh_inpatient_episodes",
  "mh_inpatient_beddays",
  "mh_el_inpatient_episodes",
  "mh_el_inpatient_beddays",
  "mh_non_el_inpatient_episodes",
  "mh_non_el_inpatient_beddays",
  
  # Delayed discharge activity
  "dd_noncode9_episodes",
  "dd_noncode9_beddays",
  "dd_code9_episodes",
  "dd_code9_beddays"
)


# Read individual SLF ----

slf <- read_slf_individual(
  year = fy,
  col_select = social_care_variables,
  as_data_frame = FALSE
) |>
  # Remove invalid ages
  filter(
    age >= 0
  ) |>
  collect()


# Add Locality Profile geography ----

social_care_slf <- slf |>
  left_join(
    lookup,
    by = "datazone2011",
    relationship = "many-to-one",
    unmatched = "drop"
  ) |>
  # Retain records that can be assigned to a Locality Profile locality
  drop_na(
    hscp_locality
  ) |>
  mutate(
    
    # Standard reporting-year name
    financial_year = as.character(
      year
    ),
    
    # Standard age groups used across the Social Care indicators
    age_group = case_when(
      between(age, 0, 17) ~ "0-17 years",
      between(age, 18, 64) ~ "18-64 years",
      between(age, 65, 74) ~ "65-74 years",
      between(age, 75, 84) ~ "75-84 years",
      age >= 85 ~ "85+ years",
      TRUE ~ "Unknown"
    ),
    
    # Standard sex description
    sex = case_when(
      str_to_lower(
        as.character(gender)
      ) %in% c(
        "1",
        "m",
        "male"
      ) ~ "Male",
      
      str_to_lower(
        as.character(gender)
      ) %in% c(
        "2",
        "f",
        "female"
      ) ~ "Female",
      
      TRUE ~ "Not Known / Unspecified"
    ),
    
    # Care at Home user flag
    #
    # A person is treated as receiving Care at Home where they have at least
    # one annual Care at Home episode or positive recorded Care at Home hours.
    care_at_home_flag = case_when(
      coalesce(
        hc_episodes,
        0
      ) > 0 ~ 1L,
      
      coalesce(
        hc_total_hours,
        0
      ) > 0 ~ 1L,
      
      TRUE ~ 0L
    ),
    
    # Care Home resident flag
    #
    # A person is treated as a Care Home resident where they have at least
    # one annual Care Home CIJ episode or positive Care Home bed days.
    care_home_flag = case_when(
      coalesce(
        ch_cis_episodes,
        0
      ) > 0 ~ 1L,
      
      coalesce(
        ch_beddays,
        0
      ) > 0 ~ 1L,
      
      TRUE ~ 0L
    ),
    
    # Combined Social Care flag
    social_care_flag = case_when(
      care_at_home_flag == 1 ~ 1L,
      care_home_flag == 1 ~ 1L,
      TRUE ~ 0L
    ),
    
    # Annual emergency admission measures
    emergency_admissions = coalesce(
      acute_non_el_inpatient_episodes,
      0
    ),
    
    emergency_admission_beddays = coalesce(
      acute_non_el_inpatient_beddays,
      0
    ),
    
    # Standard Scotland-level SIMD quintile
    simd_quintile = case_when(
      !is.na(
        simd2020v2_sc_quintile
      ) ~ as.character(
        simd2020v2_sc_quintile
      ),
      
      TRUE ~ "Unknown"
    )
  ) |>
  select(
    
    # Reporting year
    financial_year,
    
    # Person identifiers
    anon_chi,
    social_care_id,
    person_id,
    
    # Locality Profile geography
    hscp2019name,
    hscp_locality,
    datazone2011,
    datazone2022,
    
    # Demographics
    age,
    age_group,
    gender,
    sex,
    postcode,
    dob,
    death_date,
    deceased,
    
    # SLF-adjusted population
    keep_population,
    
    # SIMD
    simd2020v2_rank,
    simd2020v2_sc_decile,
    simd2020v2_sc_quintile,
    simd2020v2_hb2019_decile,
    simd2020v2_hb2019_quintile,
    simd2020v2_hscp2019_decile,
    simd2020v2_hscp2019_quintile,
    simd_quintile,
    
    # Social Care flags
    care_at_home_flag,
    care_home_flag,
    social_care_flag,
    
    # Care at Home
    hc_episodes,
    hc_total_hours,
    hc_total_cost,
    
    # Care at Home personal care
    hc_personal_episodes,
    hc_personal_hours,
    hc_personal_hours_cost,
    
    # Care at Home non-personal care
    hc_non_personal_episodes,
    hc_non_personal_hours,
    hc_non_personal_hours_cost,
    
    # Care at Home reablement
    hc_reablement_episodes,
    hc_reablement_hours,
    hc_reablement_hours_cost,
    
    # Care Home
    ch_cis_episodes,
    ch_beddays,
    ch_cost,
    
    # Social-care client groups
    sc_dementia,
    sc_learning_disability,
    sc_mental_health_disorders,
    sc_physical_and_sensory_disability,
    sc_drugs,
    sc_alcohol,
    sc_palliative_care,
    sc_carer,
    sc_elderly_frail,
    sc_neurological_condition,
    sc_autism,
    sc_other_vulnerable_groups,
    
    # Other social-care characteristics
    sc_living_alone,
    sc_support_from_unpaid_carer,
    sc_social_worker,
    sc_type_of_housing,
    sc_meals,
    sc_day_care,
    sc_latest_submission,
    
    # A&E
    ae_attendances,
    ae_cost,
    
    # Continuous inpatient journeys
    cij_total,
    cij_el,
    cij_non_el,
    cij_mat,
    cij_delay,
    
    # Acute inpatient activity
    acute_inpatient_episodes,
    acute_inpatient_beddays,
    acute_el_inpatient_episodes,
    acute_el_inpatient_beddays,
    acute_non_el_inpatient_episodes,
    acute_non_el_inpatient_beddays,
    
    # Standard emergency admission variables
    emergency_admissions,
    emergency_admission_beddays,
    
    # Geriatric long-stay activity
    gls_inpatient_episodes,
    gls_inpatient_beddays,
    gls_el_inpatient_episodes,
    gls_el_inpatient_beddays,
    gls_non_el_inpatient_episodes,
    gls_non_el_inpatient_beddays,
    
    # Mental-health inpatient activity
    mh_inpatient_episodes,
    mh_inpatient_beddays,
    mh_el_inpatient_episodes,
    mh_el_inpatient_beddays,
    mh_non_el_inpatient_episodes,
    mh_non_el_inpatient_beddays,
    
    # Delayed discharge activity
    dd_noncode9_episodes,
    dd_noncode9_beddays,
    dd_code9_episodes,
    dd_code9_beddays
  )


# Save data as parquet ----

write_parquet(
  social_care_slf,
  path(
    social_care_data_dir,
    "Social_Care_from_SLF.parquet"
  )
)


# Clean up the environment ----

rm(list = ls())
gc()