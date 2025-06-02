# LOCALITY PROFILES GENERAL HEALTH SLF DATA CODE

# Code used to extract long-term conditions data from Source Linkage Files

# Set-up ----

# Set year for data extracts folder for saving
ext_year <- 2024

# Set financial year to use for SLFs (format ex: for FY 2021/2022 -> 202122)
# Recommended to use previous year's data for more up to date figures + pop
fy <- "2324"

# Source in functions code
source("Master RMarkdown Document & Render Code/Global Script.R")

# Additional packages
library(slfhelper)

# Set file path
lp_path <- path(
  "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles"
)

gen_health_data_dir <- path(lp_path, "General Health", glue("DATA {ext_year}"))

# Locality lookup ----
lookup <- read_in_localities(dz_level = TRUE)

# Read in SLF individual level data ---
slf <- read_slf_individual(
  year = fy,
  col_select = c(
    "year",
    "datazone2011",
    "hscp2018",
    "age",
    "keep_population",
    ltc_vars
  ),
  as_data_frame = FALSE
) |>
  # remove -1 age
  filter(age >= 0) |>
  # compute age band
  mutate(
    age_group = case_when(
      age < 65 ~ "Under 65",
      between(age, 65, 74) ~ "65-74",
      between(age, 75, 84) ~ "75-84",
      age >= 85 ~ "85+"
    )
  ) |>
  collect() |>
  # Compute total LTCs
  mutate(total_ltc = rowSums(pick(arth:refailure)))


# Aggregate to Locality level ----
ltc_data <- slf |>
  left_join(
    lookup,
    by = "datazone2011",
    relationship = "many-to-one",
    unmatched = "drop"
  ) |>
  drop_na(hscp_locality) |>
  group_by(year, hscp2019name, hscp_locality, age_group, total_ltc) |>
  summarise(
    across(arth:refailure, sum),
    # congen = sum(congen),
    # bloodbfo = sum(bloodbfo),
    # endomet = sum(endomet),
    # digestive = sum(digestive),
    people = n(),
    slf_adj_pop = sum(keep_population),
    .groups = "drop_last"
  ) |>
  # Using adjusted population from SLFs for closer estimates to true population
  mutate(slf_adj_pop = sum(slf_adj_pop)) |>
  ungroup()

# Save data as parquet ----
write_parquet(
  ltc_data,
  path(gen_health_data_dir, "LTC_from_SLF.parquet")
)

# Clean up the environment
rm(list = ls())
gc()
