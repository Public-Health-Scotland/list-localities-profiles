##################### LOCALITY PROFILES UNSCHEDULED CARE: OUTPUTS ######################.

# Original author: Will Clayton
# Updated Oct/Nov 2022 by Adam Rennie to use Global Script functions
# Last edits Late Nov 22 by Luke Taylor and Cecilia Puech to tidy up script and change outputs

####################### SECTION 1: Packages, file paths, etc #########################

## Manually set year that the profiles are being run (year on data folder)
ext_year <- 2024

# Set locality profiles file path
# lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"
import_folder <- paste0(lp_path, "Unscheduled Care/DATA ", ext_year, "/")

### for testing run global script and locality placeholder below

## Packages
library(scales)

## Functions
# source("Master RMarkdown Document & Render Code/Global Script.R")

## Define locality
# LOCALITY <- "Stirling City with the Eastern Villages Bridge of Allan and Dunblane"
# LOCALITY <- "Inverness"
# LOCALITY <- "Ayr North and Former Coalfield Communities"
# LOCALITY <- "Whalsay and Skerries"
# LOCALITY <- "North Perthshire"
# LOCALITY <- "Inverclyde East"
# Set date limit for financial year
# Unless we're in Q4 use the previous FY as the max
# max_fy <- ifelse(
#   lubridate::quarter(Sys.Date(), fiscal_start = 4) != 4,
#   phsmethods::extract_fin_year(Sys.Date() - years(1)),
#   phsmethods::extract_fin_year(Sys.Date())
# )
max_fy <- "2023/24" # TODO Change this to be dynamic and move to general!

########################## SECTION 2: Lookups & Populations ###############################

## 1. Lookups ----

localities <- read_in_localities()

HSCP <- as.character(filter(localities, hscp_locality == LOCALITY)$hscp2019name)
HB <- as.character(filter(localities, hscp_locality == LOCALITY)$hb2019name)

# Determine other localities based on LOCALITY object
other_locs <- localities %>%
  select(hscp_locality, hscp2019name) %>%
  filter(hscp2019name == HSCP & hscp_locality != LOCALITY) %>%
  arrange(hscp_locality)

# Find number of locs per partnership
n_loc <- count_localities(localities, HSCP)


## 2. Populations (for rates) ----

populations <- read_in_dz_pops()

populations_proxy_year <- read_in_dz_pops_proxy_year()

populations <- rbind(populations, populations_proxy_year)


# compute age bands
populations$"Pop0_17" <- rowSums(subset(populations, select = age0:age17))
populations$"Pop18_44" <- rowSums(subset(populations, select = age18:age44))
populations$"Pop45_64" <- rowSums(subset(populations, select = age45:age64))
populations$"Pop65_74" <- rowSums(subset(populations, select = age65:age74))
populations$"Pop75Plus" <- rowSums(subset(
  populations,
  select = age75:age90plus
))
populations$"Pop65Plus" <- rowSums(subset(
  populations,
  select = age65:age90plus
))

pops <- populations %>%
  select(
    year,
    hb2019name,
    hscp2019name,
    hscp_locality,
    Pop0_17,
    Pop18_44,
    Pop45_64,
    Pop65_74,
    Pop75Plus,
    Pop65Plus,
    total_pop
  ) %>%
  mutate(financial_year = paste0(year, "/", substr(year + 1, 3, 4))) %>%
  group_by(financial_year, year, hb2019name, hscp2019name, hscp_locality) %>%
  summarise(across(everything(), sum)) %>%
  ungroup()


# Aggregate and add partnership + HB + Scotland totals

pop_areas <- pops %>%
  filter(hscp_locality == LOCALITY) %>%
  select(-hb2019name, -hscp2019name) %>%
  rename(location = hscp_locality) %>%
  # Add a partnership total
  bind_rows(
    pops %>%
      select(-hscp_locality, -hb2019name) %>%
      filter(hscp2019name == HSCP) %>%
      group_by(financial_year, year, hscp2019name) %>%
      summarise(across(everything(), sum)) %>%
      ungroup() %>%
      rename(location = hscp2019name)
  ) %>%
  # Add HB total
  bind_rows(
    pops %>%
      select(-hscp_locality, -hscp2019name) %>%
      filter(hb2019name == HB) %>%
      group_by(financial_year, year, hb2019name) %>%
      summarise(across(everything(), sum)) %>%
      ungroup() %>%
      rename(location = hb2019name)
  ) %>%
  # Add a Scotland total
  bind_rows(
    pops %>%
      select(-hscp_locality, -hscp2019name, -hb2019name) %>%
      group_by(financial_year, year) %>%
      summarise(across(everything(), sum)) %>%
      ungroup() %>%
      mutate(location = "Scotland")
  ) %>%
  pivot_longer(
    "Pop0_17":"total_pop",
    names_to = "age_group",
    values_to = "pop"
  ) %>%
  mutate(
    age_group = case_when(
      age_group == "Pop0_17" ~ "0 - 17",
      age_group == "Pop18_44" ~ "18 - 44",
      age_group == "Pop45_64" ~ "45 - 64",
      age_group == "Pop65_74" ~ "65 - 74",
      age_group == "Pop75Plus" ~ "75+",
      age_group == "Pop65Plus" ~ "65+",
      age_group == "total_pop" ~ "Total"
    )
  )


loc_pop <- pops %>%
  pivot_longer(
    "Pop0_17":"total_pop",
    names_to = "age_group",
    values_to = "pop"
  ) %>%
  mutate(
    age_group = case_when(
      age_group == "Pop0_17" ~ "0 - 17",
      age_group == "Pop18_44" ~ "18 - 44",
      age_group == "Pop45_64" ~ "45 - 64",
      age_group == "Pop65_74" ~ "65 - 74",
      age_group == "Pop75Plus" ~ "75+",
      age_group == "Pop65Plus" ~ "65+",
      age_group == "total_pop" ~ "Total"
    )
  )

# populations for age group charts
loc_pop_age1 <- loc_pop %>%
  filter(
    hscp_locality == LOCALITY,
    age_group %in% c("0 - 17", "18 - 44", "45 - 64", "65 - 74", "75+")
  )

# pop for MH emergency admissions age group chart
loc_pop_age2 <- loc_pop %>%
  filter(
    hscp_locality == LOCALITY,
    age_group %in% c("0 - 17", "18 - 44", "45 - 64", "65+")
  )

# populations by area - all ages
pop_areas_all_ages <- pop_areas %>%
  filter(age_group == "Total")

# populations by area - 65+
pop_areas_65plus <- pop_areas %>%
  filter(age_group == "65+")

# populations for other localities in the HSCP (for summary table only) - all ages
pops_other_locs <- inner_join(
  loc_pop,
  other_locs,
  by = join_by(hscp2019name, hscp_locality)
) %>%
  filter(
    age_group == "Total",
    year == max(year)
  ) %>%
  select(financial_year, year, hscp_locality, pop)

# populations for other localities in the HSCP (for summary table only) - 65+
pops_other_locs_65plus <- inner_join(
  loc_pop,
  other_locs,
  by = join_by(
    hscp2019name,
    hscp_locality
  )
) %>%
  filter(
    age_group == "65+",
    year == max(year)
  ) %>%
  select(financial_year, year, hscp_locality, pop)


########################## SECTION 3: Functions ###############################

# Functions for aggregating data
# For this function to work, the main variable of the data (ex: number of admissions) must be renamed "n"

aggregate_usc_area_data <- function(data) {
  pts_locality <- data %>%
    filter(hscp_locality == LOCALITY) %>%
    mutate(location = hscp_locality) %>%
    group_by(financial_year, location) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(area_type = "Locality")

  pts_hscp <- data %>%
    filter(hscp2019name == HSCP) %>%
    mutate(location = hscp2019name) %>%
    group_by(financial_year, location) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(area_type = "HSCP")

  pts_hb <- data %>%
    left_join(
      select(localities, hscp_locality, hb2019name),
      by = join_by(hscp_locality)
    ) %>%
    filter(hb2019name == HB) %>%
    mutate(location = hb2019name) %>%
    group_by(financial_year, location) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(area_type = "HB")

  pts_scot <- data %>%
    group_by(financial_year) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(
      location = "Scotland",
      area_type = "Scotland"
    )

  bind_rows(pts_locality, pts_hscp, pts_hb, pts_scot) %>%
    mutate(
      area_type = factor(
        area_type,
        levels = c("Locality", "HSCP", "HB", "Scotland")
      )
    )
}

# Functions for creating time trends
age_group_trend_usc <- function(
  data_for_plot,
  plot_title,
  yaxis_title,
  source
) {
  data_for_plot %>%
    ggplot(aes(
      x = financial_year,
      y = data,
      group = age_group,
      color = age_group
    )) +
    geom_line(linewidth = 1) +
    geom_point() +
    scale_colour_manual(values = c(palette)) +
    scale_x_discrete(breaks = data_for_plot$financial_year) +
    scale_y_continuous(
      labels = comma,
      limits = c(0, 1.1 * max(data_for_plot$data))
    ) +
    theme_profiles() +
    labs(
      title = plot_title,
      y = yaxis_title,
      x = "Financial Year",
      color = "Age Group",
      caption = source
    ) +
    theme(plot.title = element_text(hjust = 0.5, size = 12))
}

area_trend_usc <- function(data_for_plot, plot_title, yaxis_title, source) {
  data_for_plot %>%
    mutate(
      location = fct_reorder(
        as.factor(str_wrap(location, 23)),
        as.numeric(area_type)
      )
    ) %>%
    ggplot() +
    aes(
      x = financial_year,
      y = data,
      group = location,
      fill = location,
      linetype = area_type
    ) +
    geom_line(aes(colour = location), linewidth = 1) +
    geom_point(aes(colour = location), size = 2) +
    scale_fill_manual(values = palette) +
    scale_colour_manual(values = palette) +
    theme_profiles() +
    expand_limits(y = 0) +
    scale_x_discrete(breaks = data_for_plot$financial_year) +
    scale_y_continuous(
      labels = comma,
      limits = c(0, 1.1 * max(data_for_plot$data))
    ) +
    labs(
      title = plot_title,
      y = yaxis_title,
      x = "Financial Year",
      caption = source
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      legend.title = element_blank()
    ) +
    guides(
      linetype = "none",
      shape = "none",
      fill = "none",
      colour = guide_legend(nrow = 1, byrow = TRUE)
    )
}

# Functions for text variables

percent_change_calc <- function(numerator, denominator, digits = 1) {
  round_half_up(
    abs(numerator - denominator) / denominator * 100,
    digits = digits
  )
}

word_change_calc <- function(latest, first) {
  dplyr::case_when(
    dplyr::near(latest, first) ~ "change",
    latest > first ~ "increase",
    latest < first ~ "decrease"
  )
}

####################### SECTION 4: Data manipulation & outputs #########################

# 1. Emergency Admissions ----
# _________________________________________________________________________

emergency_adm <- read_parquet(paste0(
  import_folder,
  "emergency_admissions_msg.parquet"
)) %>%
  filter(financial_year <= max_fy)

# Plotting by age
emergency_adm_age <- emergency_adm %>%
  filter(hscp_locality == LOCALITY) %>%
  drop_na(age_group) %>%
  group_by(financial_year, age_group) %>%
  summarise(adm = sum(admissions)) %>%
  ungroup() %>%
  left_join(
    loc_pop_age1,
    by = join_by(financial_year, age_group)
  ) %>%
  mutate(data = round_half_up(adm / pop * 100000)) %>%
  drop_na(year)


EAs_age_ts <- age_group_trend_usc(
  data_for_plot = emergency_adm_age,
  plot_title = paste(
    "Emergency admissions per 100,000 over time by age group\n for",
    LOCALITY
  ),
  yaxis_title = "Emergency admission rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)

# Plotting by area
emergency_adm_areas <- emergency_adm %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  left_join(
    pop_areas_all_ages,
    by = join_by(financial_year, location)
  ) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

EAs_loc_ts <- area_trend_usc(
  data_for_plot = emergency_adm_areas,
  plot_title = "Emergency admissions per 100,000 over time by residence",
  yaxis_title = "Emergency admission rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)


# Objects for text for Figures and Summary (Emergency Admissions)
min_year_ea <- min(emergency_adm_areas$financial_year)
max_year_ea <- max(emergency_adm_areas$financial_year)

first_fy_rate <- filter(
  emergency_adm_areas,
  financial_year == min(financial_year),
  location == LOCALITY,
  area_type == "Locality"
)$data

latest_emergency_adm_loc <- emergency_adm_areas %>%
  filter(
    location == LOCALITY,
    year == max(year, na.rm = TRUE)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_emergency_adm_loc1 <- latest_emergency_adm_loc %>% pull(formatted_data)
latest_emergency_adm_loc2 <- latest_emergency_adm_loc %>% pull(data)

percent_rate_change <- percent_change_calc(
  latest_emergency_adm_loc2,
  first_fy_rate
)
word_change_rate <- word_change_calc(latest_emergency_adm_loc2, first_fy_rate)

# HSCP
hscp_emergency_adm <- emergency_adm_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hscp_emergency_adm1 <- hscp_emergency_adm %>% pull(formatted_data)
hscp_emergency_adm2 <- hscp_emergency_adm %>% pull(data)

first_fy_hscp <- filter(
  emergency_adm_areas,
  financial_year == min(financial_year),
  area_type == "HSCP"
)$data

hscp_rate_change <- percent_change_calc(hscp_emergency_adm2, first_fy_hscp)
word_change_hscp <- word_change_calc(hscp_emergency_adm2, first_fy_hscp)

# Scotland
scot_emergency_adm <- emergency_adm_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

scot_emergency_adm1 <- scot_emergency_adm %>% pull(formatted_data)
scot_emergency_adm2 <- scot_emergency_adm %>% pull(data)

first_fy_scot <- filter(
  emergency_adm_areas,
  financial_year == min(financial_year),
  location == "Scotland"
)$data

scot_rate_change <- percent_change_calc(scot_emergency_adm2, first_fy_scot)
word_change_scot <- word_change_calc(scot_emergency_adm2, first_fy_scot)

# NHS health board
hb_emergency_adm <- emergency_adm_areas %>%
  filter(
    location == HB,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hb_emergency_adm1 <- hb_emergency_adm %>% pull(formatted_data)
hb_emergency_adm2 <- hb_emergency_adm %>% pull(data)

first_fy_hb <- filter(
  emergency_adm_areas,
  financial_year == min(financial_year),
  location == HB
)$data

hb_rate_change <- percent_change_calc(hb_emergency_adm2, first_fy_hb)
word_change_hb <- word_change_calc(hb_emergency_adm2, first_fy_hb)

# other locations
other_loc_emergency_adm <- emergency_adm %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(adm = sum(admissions)) %>%
  ungroup() %>%
  right_join(
    pops_other_locs,
    by = join_by(financial_year, hscp_locality)
  ) %>%
  mutate(adm = replace_na(adm, 0)) %>%
  mutate(data = round_half_up(adm / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)

# Create objects for text emergency admissions by age group
max_ea_age <- max(emergency_adm_age$financial_year)
min_ea_age <- min(emergency_adm_age$financial_year)

latest_ea_max_age <- emergency_adm_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == max(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ea_max_age1 <- latest_ea_max_age %>% pull(formatted_data)
latest_ea_max_age2 <- latest_ea_max_age %>% pull(data)
age_group_max_ea <- latest_ea_max_age %>% pull(age_group)

first_ea_max_age <- emergency_adm_age %>%
  filter(
    year == min(year),
    age_group == age_group_max_ea
  ) %>%
  pull(data)

max_rate_change_ea <- percent_change_calc(latest_ea_max_age2, first_ea_max_age)
max_word_change_ea <- word_change_calc(latest_ea_max_age2, first_ea_max_age)

latest_ea_min_age <- emergency_adm_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == min(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ea_min_age1 <- latest_ea_min_age %>% pull(formatted_data)
latest_ea_min_age2 <- latest_ea_min_age %>% pull(data)
age_group_min_ea <- latest_ea_min_age %>% pull(age_group)

first_ea_min_age <- emergency_adm_age %>%
  filter(
    age_group == age_group_min_ea
  ) %>%
  filter(year == min(year))

first_ea_min_age1 <- first_ea_min_age %>% pull(data)
min_year_ea_age1 <- first_ea_min_age %>% pull(year)


min_rate_change_ea <- percent_change_calc(latest_ea_min_age2, first_ea_min_age1)
min_word_change_ea <- word_change_calc(latest_ea_min_age2, first_ea_min_age1)

# 2a. Unscheduled bed days ----
# _________________________________________________________________________

bed_days <- read_parquet(paste0(import_folder, "bed_days_msg.parquet")) %>%
  filter(financial_year <= max_fy)

# Plotting by age
bed_days_age <- bed_days %>%
  filter(hscp_locality == LOCALITY) %>%
  drop_na(age_group) %>%
  group_by(financial_year, age_group) %>%
  summarise(bed_days = sum(bed_days)) %>%
  ungroup() %>%
  left_join(loc_pop_age1, by = join_by(financial_year, age_group)) %>%
  mutate(data = round_half_up(bed_days / pop * 100000)) %>%
  drop_na(year)


BDs_age_ts <- age_group_trend_usc(
  data_for_plot = bed_days_age,
  plot_title = paste(
    "Unscheduled bed days per 100,000 over time by age group\n for",
    LOCALITY
  ),
  yaxis_title = "Unscheduled bed day rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)


# Plotting by area
bed_days_areas <- bed_days %>%
  rename(n = bed_days) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

BDs_loc_ts <- area_trend_usc(
  data_for_plot = bed_days_areas,
  plot_title = "Unscheduled bed days per 100,000 over time by residence",
  yaxis_title = "Unscheduled bed day rate\n per 100,000 population",
  source = "Source: PHS SMR01"
)

# Objects for text for Figures and Summary (Unscheduled Bed Days)
min_year_ubd <- min(bed_days_areas$financial_year)
max_year_ubd <- max(bed_days_areas$financial_year)

# LOCALITY
first_fy_rate_ubd <- filter(
  bed_days_areas,
  financial_year == min(financial_year),
  location == LOCALITY & area_type == "Locality"
)$data

latest_bed_days_loc <- bed_days_areas %>%
  filter(location == LOCALITY, year == max(year)) %>%
  mutate(formatted_data = format(data, big.mark = ","))
latest_bed_days_loc1 <- latest_bed_days_loc %>% pull(formatted_data)
latest_bed_days_loc2 <- latest_bed_days_loc %>% pull(data)

rate_change_ubd <- percent_change_calc(latest_bed_days_loc2, first_fy_rate_ubd)
word_change_ubd <- word_change_calc(latest_bed_days_loc2, first_fy_rate_ubd)
# HSCP
first_fy_hscp_ubd <- filter(
  bed_days_areas,
  (financial_year == min(bed_days_areas$financial_year)) & (area_type == "HSCP")
)$data

hscp_bed_days <- bed_days_areas %>%
  filter(location == HSCP, year == max(year)) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hscp_bed_days1 <- hscp_bed_days %>% pull(formatted_data)
hscp_bed_days2 <- hscp_bed_days %>% pull(data)

hscp_rate_ubd <- percent_change_calc(hscp_bed_days2, first_fy_hscp_ubd)
hscp_change_ubd <- word_change_calc(hscp_bed_days2, first_fy_hscp_ubd)

# Scotland
first_fy_scot_ubd <- filter(
  bed_days_areas,
  (financial_year == min(bed_days_areas$financial_year)) &
    (area_type == "Scotland")
)$data

scot_bed_days <- bed_days_areas %>%
  filter(location == "Scotland", year == max(year)) %>%
  mutate(formatted_data = format(data, big.mark = ","))

scot_bed_days1 <- scot_bed_days %>% pull(formatted_data)
scot_bed_days2 <- scot_bed_days %>% pull(data)

scot_rate_ubd <- percent_change_calc(scot_bed_days2, first_fy_scot_ubd)
scot_change_ubd <- word_change_calc(scot_bed_days2, first_fy_scot_ubd)

# NHS health board
hb_bed_days <- bed_days_areas %>%
  filter(
    location == HB,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hb_bed_days1 <- hb_bed_days %>% pull(formatted_data)
hb_bed_days2 <- hb_bed_days %>% pull(data)
first_fy_hb_ubd <- filter(
  bed_days_areas,
  financial_year == min(financial_year),
  location == HB
)$data

hb_rate_change_ubd <- percent_change_calc(hb_bed_days2, first_fy_hb_ubd)
word_change_hb_ubd <- word_change_calc(hb_bed_days2, first_fy_hb_ubd)


other_loc_bed_days <- bed_days %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(bed_days = sum(bed_days)) %>%
  ungroup() %>%
  right_join(pops_other_locs, by = join_by(financial_year, hscp_locality)) %>%
  mutate(adm = replace_na(bed_days, 0)) %>%
  mutate(data = round_half_up(bed_days / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)

# Create objects for text emergency admissions by age group
max_ubd_age <- max(bed_days_age$financial_year)
min_ubd_age <- min(bed_days_age$financial_year)

latest_ubd_max_age <- bed_days_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == max(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ubd_max_age1 <- latest_ubd_max_age %>% pull(formatted_data)
latest_ubd_max_age2 <- latest_ubd_max_age %>% pull(data)
age_group_max_ubd <- latest_ubd_max_age %>% pull(age_group)

first_ubd_max_age <- bed_days_age %>%
  filter(
    year == min(year),
    age_group == age_group_max_ubd
  ) %>%
  pull(data)

max_rate_change_ubd <- percent_change_calc(
  latest_ubd_max_age2,
  first_ubd_max_age
)
max_word_change_ubd <- word_change_calc(latest_ubd_max_age2, first_ubd_max_age)

latest_ubd_min_age <- bed_days_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == min(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ubd_min_age1 <- latest_ubd_min_age %>% pull(formatted_data)
latest_ubd_min_age2 <- latest_ubd_min_age %>% pull(data)
age_group_min_ubd <- latest_ubd_min_age %>% pull(age_group)

first_ubd_min_age <- bed_days_age %>%
  filter(
    age_group == age_group_min_ubd
  ) %>%
  filter(year == min(year))

first_ubd_min_age1 <- first_ubd_min_age %>% pull(data)
min_year_ubd_age1 <- first_ubd_min_age %>% pull(year)


min_rate_change_ubd <- percent_change_calc(
  latest_ubd_min_age2,
  first_ubd_min_age1
)
min_word_change_ubd <- word_change_calc(latest_ubd_min_age2, first_ubd_min_age1)

# 2b. Unscheduled bed days - Mental Health ----
# _________________________________________________________________________

bed_days_mh <- read_parquet(paste0(
  import_folder,
  "bed_days_mh_msg.parquet"
)) %>%
  filter(financial_year <= max_fy)

# Plotting by age
bed_days_mh_age <- bed_days_mh %>%
  filter(hscp_locality == LOCALITY) %>%
  drop_na(age_group) %>%
  group_by(financial_year, age_group) %>%
  summarise(bed_days = sum(bed_days)) %>%
  ungroup() %>%
  left_join(loc_pop_age1, by = join_by(financial_year, age_group)) %>%
  mutate(data = round_half_up(bed_days / pop * 100000)) %>%
  drop_na(year)


BDMH_age_ts <- age_group_trend_usc(
  data_for_plot = bed_days_mh_age,
  plot_title = paste(
    "Unscheduled bed days (MH) per 100,000 over time by age group\n for",
    LOCALITY
  ),
  yaxis_title = "Unscheduled bed day (MH) rate\n per 100,000 population",
  source = "Source: PHS SMR04"
)


# Plotting by area
bed_days_mh_areas <- bed_days_mh %>%
  rename(n = bed_days) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

BDMH_loc_ts <- area_trend_usc(
  data_for_plot = bed_days_mh_areas,
  plot_title = "Unscheduled bed days (MH) per 100,000 over time by residence",
  yaxis_title = "Unscheduled bed day (MH) rate\n per 100,000 population",
  source = "Source: PHS SMR04"
)


# Objects for text and summary table- age
max_year_bd_mh_age <- max(bed_days_mh_age$financial_year)
min_year_bd_mh_age <- min(bed_days_mh_age$financial_year)

latest_bd_mh_max_age <- bed_days_mh_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == max(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_bd_mh_max_age1 <- latest_bd_mh_max_age %>% pull(formatted_data)
latest_bd_mh_max_age2 <- latest_bd_mh_max_age %>% pull(data)
age_group_max_mh <- latest_bd_mh_max_age %>% pull(age_group)

first_bd_mh_max_age <- bed_days_mh_age %>%
  filter(
    year == min(year),
    age_group == age_group_max_mh
  ) %>%
  pull(data)

max_rate_change_beds_mh <- percent_change_calc(
  latest_bd_mh_max_age2,
  first_bd_mh_max_age
)
max_word_change_beds_mh <- word_change_calc(
  latest_bd_mh_max_age2,
  first_bd_mh_max_age
)

latest_bd_mh_min_age <- bed_days_mh_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == min(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_bd_mh_min_age1 <- latest_bd_mh_min_age %>% pull(formatted_data)
latest_bd_mh_min_age2 <- latest_bd_mh_min_age %>% pull(data)
age_group_min_mh <- latest_bd_mh_min_age %>% pull(age_group)

first_bd_mh_min_age <- bed_days_mh_age %>%
  filter(
    age_group == age_group_min_mh
  ) %>%
  filter(year == min(year))

first_bd_mh_min_age1 <- first_bd_mh_min_age %>% pull(data)
min_year_bd_mh_age1 <- first_bd_mh_min_age %>% pull(year)


min_rate_change_beds_mh <- percent_change_calc(
  latest_bd_mh_min_age2,
  first_bd_mh_min_age1
)
min_word_change_beds_mh <- word_change_calc(
  latest_bd_mh_min_age2,
  first_bd_mh_min_age1
)

# Objects for text and summary table- area
max_year_bd_mh_areas <- max(bed_days_mh_areas$financial_year)
min_year_bd_mh_areas <- min(bed_days_mh_areas$financial_year)

latest_bed_days_mh_loc <- bed_days_mh_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_bed_days_mh_loc1 <- latest_bed_days_mh_loc %>% pull(formatted_data)
latest_bed_days_mh_loc2 <- latest_bed_days_mh_loc %>% pull(data)
latest_bed_days_mh_loc1 <- ifelse(
  is_empty(latest_bed_days_mh_loc1),
  "NA",
  latest_bed_days_mh_loc1
)

first_bed_days_mh_loc <- bed_days_mh_areas %>%
  filter(
    location == LOCALITY,
    year == min(year)
  ) %>%
  pull(data)

loc_rate_change_beds_mh <- percent_change_calc(
  latest_bed_days_mh_loc2,
  first_bed_days_mh_loc
)
loc_word_change_beds_mh <- word_change_calc(
  latest_bed_days_mh_loc2,
  first_bed_days_mh_loc
)

hscp_bed_days_mh <- bed_days_mh_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hscp_bed_days_mh1 <- hscp_bed_days_mh %>% pull(formatted_data)
hscp_bed_days_mh2 <- hscp_bed_days_mh %>% pull(data)

first_hscp_bed_days_mh <- bed_days_mh_areas %>%
  filter(
    location == HSCP,
    year == min(year)
  ) %>%
  pull(data)

hscp_rate_change_beds_mh <- percent_change_calc(
  hscp_bed_days_mh2,
  first_hscp_bed_days_mh
)
hscp_word_change_beds_mh <- word_change_calc(
  hscp_bed_days_mh2,
  first_hscp_bed_days_mh
)

scot_bed_days_mh <- bed_days_mh_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

scot_bed_days_mh1 <- scot_bed_days_mh %>% pull(formatted_data)
scot_bed_days_mh2 <- scot_bed_days_mh %>% pull(data)

first_scot_bed_days_mh <- bed_days_mh_areas %>%
  filter(
    location == "Scotland",
    year == min(year)
  ) %>%
  pull(data)

scot_rate_change_beds_mh <- percent_change_calc(
  scot_bed_days_mh2,
  first_scot_bed_days_mh
)
scot_word_change_beds_mh <- word_change_calc(
  scot_bed_days_mh2,
  first_scot_bed_days_mh
)

# NHS health board
hb_mh_beddays <- bed_days_mh_areas %>%
  filter(
    location == HB,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hb_mh_beddays1 <- hb_mh_beddays %>% pull(formatted_data)
hb_mh_beddays2 <- hb_mh_beddays %>% pull(data)

first_fy_hb_mh <- filter(
  bed_days_mh_areas,
  financial_year == min(financial_year),
  location == HB
)$data

hb_rate_change_mh <- round(
  abs(hb_mh_beddays2 - first_fy_hb_mh) / first_fy_hb_mh * 100,
  digits = 1
)
word_change_hb_mh <- word_change_calc(hb_mh_beddays2, first_fy_hb_mh)

other_loc_bed_days_mh <- bed_days_mh %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(bed_days = sum(bed_days)) %>%
  ungroup() %>%
  right_join(pops_other_locs, by = join_by(financial_year, hscp_locality)) %>%
  mutate(adm = replace_na(bed_days, 0)) %>%
  mutate(data = round_half_up(bed_days / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)


# 3. A&E Attendances ----
# _________________________________________________________________________

ae_attendances <- read_parquet(paste0(
  import_folder,
  "ae_attendances_msg.parquet"
)) %>%
  filter(financial_year <= max_fy)

# Plotting by age
ae_att_age <- ae_attendances %>%
  filter(
    hscp_locality == LOCALITY,
    age_group != "NA"
  ) %>%
  group_by(financial_year, age_group) %>%
  summarise(attendances = sum(attendances)) %>%
  ungroup() %>%
  left_join(loc_pop_age1, by = join_by(financial_year, age_group)) %>%
  mutate(data = round_half_up(attendances / pop * 100000)) %>%
  drop_na(year)


AandE_age_ts <- age_group_trend_usc(
  data_for_plot = ae_att_age,
  plot_title = paste(
    "A&E attendances per 100,000 over time by age group\n for",
    LOCALITY
  ),
  yaxis_title = "A&E attendance rate\n per 100,000 population",
  source = "Source: PHS A&E Datamart"
)


# Plotting by area
ae_att_areas <- ae_attendances %>%
  rename(n = attendances) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)


AandE_loc_ts <- area_trend_usc(
  data_for_plot = ae_att_areas,
  plot_title = paste("A&E attendances per 100,000 over time by residence"),
  yaxis_title = "A&E attendance rate\n per 100,000 population",
  source = "Source: PHS A&E Datamart"
)

# Objects for text and summary table- age

min_year_ae_age <- min(ae_att_age$financial_year)
max_year_ae_age <- max(ae_att_age$financial_year)


latest_ae_att_max_age <- ae_att_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == max(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ae_att_loc1_age <- latest_ae_att_max_age %>% pull(formatted_data)
latest_ae_att_loc2_age <- latest_ae_att_max_age %>% pull(data)

age_group_max <- latest_ae_att_max_age %>% pull(age_group)

first_ae_att_max_age <- ae_att_age %>%
  filter(
    year == min(year),
    age_group == age_group_max
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

first_ae_att_max_age_data <- first_ae_att_max_age %>% pull(data)

percent_rate_change_ae_age <- percent_change_calc(
  latest_ae_att_loc2_age,
  first_ae_att_max_age_data
)
word_change_rate_ae_age <- word_change_calc(
  latest_ae_att_loc2_age,
  first_ae_att_max_age_data
)

latest_ae_att_min_age <- ae_att_age %>%
  filter(
    year == max(year)
  ) %>%
  filter(
    data == min(data)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ae_att_loc1_age_min <- latest_ae_att_min_age %>% pull(formatted_data)
latest_ae_att_loc2_age_min <- latest_ae_att_min_age %>% pull(data)
age_group_min <- latest_ae_att_min_age %>% pull(age_group)

first_ae_att_min_age <- ae_att_age %>%
  filter(
    year == min(year),
    age_group == age_group_min
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

first_ae_att_min_data <- first_ae_att_min_age %>% pull(data)

percent_rate_change_ae_age2 <- percent_change_calc(
  latest_ae_att_loc2_age_min,
  first_ae_att_min_data
)
word_change_rate_ae_age2 <- word_change_calc(
  latest_ae_att_loc2_age_min,
  first_ae_att_min_data
)


# Objects for text and summary table- area

min_year_ae_area <- min(ae_att_areas$financial_year)
max_year_ae_area <- max(ae_att_areas$financial_year)

first_fy_rate_ae_areas <- filter(
  ae_att_areas,
  financial_year == min(financial_year),
  location == LOCALITY,
  area_type == "Locality"
)$data

latest_ae_att_loc <- ae_att_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ae_att_loc1 <- latest_ae_att_loc %>% pull(formatted_data)
latest_ae_att_loc2 <- latest_ae_att_loc %>% pull(data)

percent_rate_change_ae_areas <- percent_change_calc(
  latest_ae_att_loc2,
  first_fy_rate_ae_areas
)
word_change_rate_ae_areas <- word_change_calc(
  latest_ae_att_loc2,
  first_fy_rate_ae_areas
)

hscp_ae_att <- ae_att_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hscp_ae_att1 <- hscp_ae_att %>% pull(formatted_data)
hscp_ae_att2 <- hscp_ae_att %>% pull(data)

first_fy_hscp_ae <- filter(
  ae_att_areas,
  financial_year == min(financial_year),
  area_type == "HSCP"
)$data

percent_rate_change_ae_areas_hscp <- percent_change_calc(
  hscp_ae_att2,
  first_fy_hscp_ae
)
word_change_rate_ae_areas_hscp <- word_change_calc(
  hscp_ae_att2,
  first_fy_hscp_ae
)

scot_ae_att <- ae_att_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))
scot_ae_att1 <- scot_ae_att %>% pull(formatted_data)
scot_ae_att2 <- scot_ae_att %>% pull(data)

first_fy_scot_ae <- filter(
  ae_att_areas,
  financial_year == min(financial_year),
  location == "Scotland"
)$data

percent_rate_change_ae_areas_scot <- percent_change_calc(
  scot_ae_att2,
  first_fy_scot_ae
)
word_change_rate_ae_areas_scot <- word_change_calc(
  scot_ae_att2,
  first_fy_scot_ae
)

# NHS health board
hb_ae_att <- ae_att_areas %>%
  filter(
    location == HB,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hb_ae1 <- hb_ae_att %>% pull(formatted_data)
hb_ae2 <- hb_ae_att %>% pull(data)
first_fy_hb_ae <- filter(
  ae_att_areas,
  financial_year == min(financial_year),
  location == HB
)$data

hb_rate_change_ae <- percent_change_calc(hb_ae2, first_fy_hb_ae)
word_change_hb_ae <- word_change_calc(hb_ae2, first_fy_hb_ae)

other_loc_ae_att <- ae_attendances %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(attendances = sum(attendances)) %>%
  ungroup() %>%
  right_join(pops_other_locs, by = join_by(financial_year, hscp_locality)) %>%
  mutate(attendances = replace_na(attendances, 0)) %>%
  mutate(data = round_half_up(attendances / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)


# 4. Delayed Discharges ----
# _________________________________________________________________________

delayed_disch <- read_parquet(paste0(
  import_folder,
  "delayed_discharges_msg.parquet"
)) %>%
  filter(financial_year <= max_fy) %>%
  filter(age_group %in% c("65 - 74", "75+")) %>%
  group_by(financial_year, hscp2019name, hscp_locality) %>%
  summarise(
    dd_people = sum(dd_people),
    dd_bed_days = sum(dd_bed_days)
  ) %>%
  ungroup()


# Plotting by area
delayed_disch_areas <- delayed_disch %>%
  rename(n = dd_bed_days) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_65plus, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

DD_loc_ts <- area_trend_usc(
  data_for_plot = delayed_disch_areas,
  plot_title = paste0(
    "Delayed discharge bed days per 100,000 population aged over 65\n",
    "over time by residence"
  ),
  yaxis_title = "Delayed discharge bed day rate\n per 100,000 population aged 65+",
  source = "Source: PHS Delayed Discharges"
)


# Objects for text and summary table
min_year_dd <- min(delayed_disch_areas$financial_year)
max_year_dd <- max(delayed_disch_areas$financial_year)

latest_dd_loc <- delayed_disch_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_dd_loc1 <- latest_dd_loc %>% pull(formatted_data)
latest_dd_loc2 <- latest_dd_loc %>% pull(data)

first_dd_loc <- delayed_disch_areas %>%
  filter(
    location == LOCALITY,
    year == min(year)
  ) %>%
  pull(data)

percent_rate_change_dd_loc <- percent_change_calc(latest_dd_loc2, first_dd_loc)
word_change_rate_dd_loc <- word_change_calc(latest_dd_loc2, first_dd_loc)


hscp_dd <- delayed_disch_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hscp_dd1 <- hscp_dd %>% pull(formatted_data)
hscp_dd2 <- hscp_dd %>% pull(data)

first_hscp_dd <- delayed_disch_areas %>%
  filter(
    location == HSCP,
    year == min(year)
  ) %>%
  pull(data)

percent_rate_change_dd_hscp <- percent_change_calc(hscp_dd2, first_hscp_dd)
word_change_rate_dd_hscp <- word_change_calc(hscp_dd2, first_hscp_dd)


scot_dd <- delayed_disch_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

scot_dd1 <- scot_dd %>% pull(formatted_data)
scot_dd2 <- scot_dd %>% pull(data)

first_scot_dd <- delayed_disch_areas %>%
  filter(
    location == "Scotland",
    year == min(year)
  ) %>%
  pull(data)

percent_rate_change_dd_scot <- percent_change_calc(scot_dd2, first_scot_dd)
word_change_rate_dd_scot <- word_change_calc(scot_dd2, first_scot_dd)

# NHS health board
hb_dd <- delayed_disch_areas %>%
  filter(
    location == HB,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hb_dd1 <- hb_dd %>% pull(formatted_data)
hb_dd2 <- hb_dd %>% pull(data)
first_fy_hb_dd <- filter(
  delayed_disch_areas,
  financial_year == min(financial_year),
  location == HB
)$data

hb_rate_change_dd <- percent_change_calc(hb_dd2, first_fy_hb_dd)
word_change_hb_dd <- word_change_calc(hb_dd2, first_fy_hb_dd)


other_loc_dd <- delayed_disch %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(dd_bed_days = sum(dd_bed_days)) %>%
  ungroup() %>%
  right_join(
    pops_other_locs_65plus,
    by = join_by(financial_year, hscp_locality)
  ) %>%
  mutate(dd_bed_days = replace_na(dd_bed_days, 0)) %>%
  mutate(data = round_half_up(dd_bed_days / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)


# 5. Fall Admissions ----
# _________________________________________________________________________

falls <- read_parquet(paste0(import_folder, "falls_smr.parquet")) %>%
  filter(financial_year <= max_fy) %>%
  filter(age_group %in% c("65 - 74", "75+"))

# Plotting by area
falls_areas <- falls %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_65plus, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  drop_na(year)

Falls_loc_ts <- area_trend_usc(
  data_for_plot = falls_areas,
  plot_title = paste0(
    "Emergency admissions from falls per 100,000 population aged over 65\n",
    "over time by residence"
  ),
  yaxis_title = "Emergency admissions from falls rate\nper 100,000 population aged 65+",
  source = "Source: PHS SMR01"
)


# Objects for text and summary table
min_year_falls <- min(falls_areas$financial_year)
max_year_falls <- max(falls_areas$financial_year)

latest_falls_loc <- falls_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_falls_loc1 <- latest_falls_loc %>% pull(formatted_data)
latest_falls_loc2 <- latest_falls_loc %>% pull(data)

first_falls_loc <- falls_areas %>%
  filter(
    location == LOCALITY,
    year == min(year)
  ) %>%
  pull(data)

percent_rate_change_falls_loc <- percent_change_calc(
  latest_falls_loc2,
  first_falls_loc
)
word_change_rate_falls_loc <- word_change_calc(
  latest_falls_loc2,
  first_falls_loc
)

hscp_falls <- falls_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hscp_falls1 <- hscp_falls %>% pull(formatted_data)
hscp_falls2 <- hscp_falls %>% pull(data)

first_falls_hscp <- falls_areas %>%
  filter(
    location == HSCP,
    year == min(year)
  ) %>%
  pull(data)

percent_rate_change_falls_hscp <- percent_change_calc(
  hscp_falls2,
  first_falls_hscp
)
word_change_rate_falls_hscp <- word_change_calc(hscp_falls2, first_falls_hscp)

scot_falls <- falls_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

scot_falls1 <- scot_falls %>% pull(formatted_data)
scot_falls2 <- scot_falls %>% pull(data)

first_falls_scot <- falls_areas %>%
  filter(
    location == "Scotland",
    year == min(year)
  ) %>%
  pull(data)

percent_rate_change_falls_scot <- percent_change_calc(
  scot_falls2,
  first_falls_scot
)
word_change_rate_falls_scot <- word_change_calc(scot_falls2, first_falls_scot)

# NHS health board
hb_falls <- falls_areas %>%
  filter(
    location == HB,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hb_falls1 <- hb_falls %>% pull(formatted_data)
hb_falls2 <- hb_falls %>% pull(data)

first_fy_hb_falls <- filter(
  falls_areas,
  financial_year == min(financial_year),
  location == HB
)$data

hb_rate_change_falls <- round(
  abs(hb_falls2 - first_fy_hb_falls) / first_fy_hb_falls * 100,
  digits = 1
)
word_change_hb_falls <- word_change_calc(hb_falls2, first_fy_hb_falls)

# 6. Readmissions (28 days) ----
# _________________________________________________________________________

readmissions <- read_parquet(paste0(
  import_folder,
  "readmissions_smr.parquet"
)) %>%
  filter(financial_year <= max_fy)

# Plotting by age

readmissions_age <- readmissions %>%
  filter(hscp_locality == LOCALITY) %>%
  drop_na(age_group) %>%
  group_by(financial_year, age_group) %>%
  summarise(
    read_28 = sum(read_28),
    discharges = sum(discharges)
  ) %>%
  ungroup() %>%
  mutate(data = round_half_up(read_28 / discharges * 1000, 1))

read_age_ts <- age_group_trend_usc(
  data_for_plot = readmissions_age,
  plot_title = paste(
    "Readmission rate (28 days) per 1,000 discharges by age group\n for",
    LOCALITY
  ),
  yaxis_title = "Readmission rate (28 days)\n per 1,000 discharges",
  source = "Source: PHS SMR01"
)


# Plotting by area

# First use aggregating function on readmissions, then on discharges, then join

read1 <- readmissions %>%
  select(-discharges) %>%
  rename(n = read_28) %>%
  aggregate_usc_area_data() %>%
  rename(read_28 = n)

read2 <- readmissions %>%
  select(-read_28) %>%
  rename(n = discharges) %>%
  aggregate_usc_area_data() %>%
  rename(discharges = n)

readmissions_areas <- left_join(
  read1,
  read2,
  by = join_by(financial_year, location, area_type)
) %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(read_28 / discharges * 1000, 1)) %>%
  drop_na(year)

rm(read1, read2)

read_loc_ts <- area_trend_usc(
  data_for_plot = readmissions_areas,
  plot_title = paste(
    "Readmission rate (28 days) per 1,000 discharges over time by residence"
  ),
  yaxis_title = "Readmission rate (28 days)\n per 1,000 discharges",
  source = "Source: PHS SMR01"
)

# Objects for text and summary table-age
min_year_re_age <- min(readmissions_age$financial_year)
max_year_re_age <- max(readmissions_age$financial_year)

latest_re_max_age <- readmissions_age %>%
  filter(
    financial_year == max(financial_year)
  ) %>%
  filter(
    data == max(data)
  )

latest_re_max_age_data <- latest_re_max_age %>% pull(data)
latest_re_max_age_group <- latest_re_max_age %>% pull(age_group)

first_re_max_age <- readmissions_age %>%
  filter(
    financial_year == min(financial_year),
    age_group == latest_re_max_age_group
  ) %>%
  pull(data)


percent_rate_change_re_age <- percent_change_calc(
  latest_re_max_age_data,
  first_re_max_age
)
word_change_rate_re_age <- word_change_calc(
  latest_re_max_age_data,
  first_re_max_age
)

latest_re_min_age <- readmissions_age %>%
  filter(
    financial_year == max(financial_year)
  ) %>%
  filter(data == min(data))

latest_re_min_age_data <- latest_re_min_age %>% pull(data)
latest_re_min_age_group <- latest_re_min_age %>% pull(age_group)

first_re_min_age <- readmissions_age %>%
  filter(
    financial_year == min(financial_year),
    age_group == latest_re_min_age_group
  ) %>%
  pull(data)

percent_rate_change_re_age_min <- percent_change_calc(
  latest_re_min_age_data,
  first_re_min_age
)
word_change_rate_re_age_min <- word_change_calc(
  latest_re_min_age_data,
  first_re_min_age
)

# Objects for text and summary table-area
min_year_re_area <- min(readmissions_areas$financial_year)
max_year_re_area <- max(readmissions_areas$financial_year)

first_read_loc <- readmissions_areas %>%
  filter(
    location == LOCALITY,
    year == min(year)
  )

first_read_loc1 <- first_read_loc %>% pull(data)

latest_read_loc <- readmissions_areas %>%
  filter(
    location == LOCALITY,
    year == max(year)
  )

latest_read_loc1 <- latest_read_loc %>% pull(data)

percent_rate_change_re_area <- percent_change_calc(
  latest_read_loc1,
  first_read_loc1
)
word_change_rate_re_area <- word_change_calc(latest_read_loc1, first_read_loc1)

first_hscp_read <- readmissions_areas %>%
  filter(
    location == HSCP,
    year == min(year)
  ) %>%
  pull(data)

hscp_read <- readmissions_areas %>%
  filter(
    location == HSCP,
    year == max(year)
  ) %>%
  pull(data)

percent_rate_change_re_area_hscp <- percent_change_calc(
  hscp_read,
  first_hscp_read
)
word_change_rate_re_area_hscp <- word_change_calc(hscp_read, first_hscp_read)

first_scot_read <- readmissions_areas %>%
  filter(
    location == "Scotland",
    year == min(year)
  ) %>%
  pull(data)


scot_read <- readmissions_areas %>%
  filter(
    location == "Scotland",
    year == max(year)
  ) %>%
  pull(data)

percent_rate_change_re_area_scot <- percent_change_calc(
  scot_read,
  first_scot_read
)
word_change_rate_re_area_scot <- word_change_calc(scot_read, first_scot_read)

# NHS health board
hb_read <- readmissions_areas %>%
  filter(
    location == HB,
    year == max(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

hb_read1 <- hb_read %>% pull(formatted_data)
hb_read2 <- hb_read %>% pull(data)

first_fy_hb_read <- filter(
  readmissions_areas,
  financial_year == min(financial_year),
  location == HB
)$data

hb_rate_change_read <- percent_change_calc(hb_read2, first_fy_hb_read)
word_change_hb_read <- word_change_calc(hb_read2, first_fy_hb_read)

# 7. Comm 6 months ----
# _________________________________________________________________________________
#
# comm_6mo <- readRDS(paste0(import_folder, "comm_6mo_smr.rds")) %>%
#   rename(financial_year = financial_death) %>%
#   filter(financial_year <= max_fy)
#
# # Aggregate data
# # First use aggregating function on bed days, then on deaths, then join
#
# comm_6mo1 <- comm_6mo %>%
#   select(-total_deaths) %>%
#   rename(n = total_bddys) %>%
#   aggregate_usc_area_data() %>%
#   rename(total_bddys = n)
#
# comm_6mo2 <- comm_6mo %>%
#   select(-total_bddys) %>%
#   rename(n = total_deaths) %>%
#   aggregate_usc_area_data() %>%
#   rename(total_deaths = n)
#
# comm_6mo_areas <- left_join(comm_6mo1, comm_6mo2) %>%
#   left_join(pop_areas_all_ages) %>%
#   mutate(data = round_half_up(100*(1 - total_bddys/total_deaths/182.5), 1))
#
# #Locality Table
#
# comm_6mo_loc_table <- comm_6mo_areas %>%
#   filter(location == LOCALITY) %>%
#   mutate(data = paste0(data, "%"),
#          financial_year = paste0("**", financial_year, "**"),
#          financial_year = fct_reorder(financial_year, year)) %>%
#   select(location, financial_year, data) %>%
#   pivot_wider(names_from = financial_year, values_from = data) %>%
#   select(-location)
#
# # Bar plot comparing areas in latest year
#
# comm_loc_bar <- comm_6mo_areas %>%
#   filter(year == max(year)) %>%
#   mutate(location = fct_reorder(as.factor(str_wrap(location, 28)), as.numeric(area_type))) %>%
#
#   ggplot(aes(x = location, y = data, fill = location, weight = data)) +
#   geom_col(position = position_dodge()) +
#   geom_text(aes(y = data, label = round_half_up(data, 1)),
#             position=position_dodge(width=0.9),
#             vjust=-0.25, color = "#4a4a4a", size = 4, fontface = "bold") +
#   scale_y_continuous(labels = comma, limits = c(0, 1.1*max(comm_6mo_areas$data))) +
#   scale_fill_manual(values = palette) +
#   theme_profiles() +
#   theme(panel.grid.major.y = element_line(color = "grey85"),
#         panel.grid.major.x = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_text(size = 10)) +
#   labs(y = "% Last 6 months in community",
#        caption = "Source: NRS Death Records, PHS SMR01, SMR01E and SMR04") +
#   guides(fill= "none")
#
#
# # Objects for text and summary table
# latest_comm_6mo_loc <- comm_6mo_areas %>%
#   filter(location == LOCALITY,
#          year == max(year)) %>%
#   pull(data)
#
# hscp_comm_6mo <- comm_6mo_areas %>%
#   filter(location == HSCP,
#          year == max(year)) %>%
#   pull(data)
#
# scot_comm_6mo <- comm_6mo_areas %>%
#   filter(location == "Scotland",
#          year == max(year)) %>%
#   pull(data)
#
# other_loc_comm_6mo <- comm_6mo %>%
#   group_by(financial_year, hscp_locality) %>%
#   summarise(total_bddys = sum(total_bddys),
#             total_deaths = sum(total_deaths)) %>%
#   ungroup() %>%
#   right_join(pops_other_locs) %>%
#   mutate(total_bddys = replace_na(total_bddys, 0),
#          total_deaths = replace_na(total_deaths, 0)) %>%
#   mutate(data = as.character(round_half_up(100*(1 - total_bddys/total_deaths/182.5), 1))) %>%
#   select(hscp_locality, data) %>%
#   pivot_wider(names_from = hscp_locality, values_from = data)

# 8. Potentially Preventable Admissions ----
# _______________________________________________________________________________________________________

ppa <- read_parquet(paste0(import_folder, "ppa_smr.parquet")) %>%
  filter(financial_year <= max_fy)

# % PPAs in locality under and over 65
ppa_total <- ppa %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data()

ppa_65plus <- ppa %>%
  filter(age_group %in% c("65 - 74", "75+")) %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  rename(plus65tot = n) %>%
  left_join(ppa_total, by = join_by(financial_year, location)) %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(plus65tot / n * 100, 1)) %>%
  drop_na(year)

latest_ppa_65plus <- ppa_65plus %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  pull(data)

ppa_under65 <- ppa %>%
  filter(age_group %in% c("0 - 17", "18 - 44", "45 - 64")) %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  rename(under65tot = n) %>%
  left_join(ppa_total, by = join_by(financial_year, location)) %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(under65tot / n * 100, 1)) %>%
  drop_na(year)

latest_ppa_under65 <- ppa_under65 %>%
  filter(
    location == LOCALITY,
    year == max(year)
  ) %>%
  pull(data)


# Plotting by area
ppa_areas <- ppa %>%
  rename(n = admissions) %>%
  aggregate_usc_area_data() %>%
  left_join(pop_areas_all_ages, by = join_by(financial_year, location)) %>%
  mutate(data = round_half_up(n / pop * 100000)) %>%
  mutate(
    location = factor(location, levels = c(LOCALITY, HSCP, HB, "Scotland"))
  ) %>%
  arrange(location) %>%
  drop_na(year)


ppa_loc_ts <- area_trend_usc(
  data_for_plot = ppa_areas,
  plot_title = paste(
    "Potentially Preventable Emergency Admissions per 100,000 by residence"
  ),
  yaxis_title = "PPA rate\nper 100,000 population",
  source = "Source: PHS SMR01"
)


# Objects for text and summary table
max_year_ppa_areas <- max(ppa_areas$financial_year)
min_year_ppa_areas <- min(ppa_areas$financial_year)


latest_ppa_loc <- ppa_areas %>%
  filter(
    location == LOCALITY,
    year == max(year) | year == min(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

latest_ppa_loc1 <- latest_ppa_loc$formatted_data[2]

ppa_diff <- percent_change_calc(latest_ppa_loc$data[2], latest_ppa_loc$data[1])
ppa_word_change <- word_change_calc(
  latest_ppa_loc$data[2],
  latest_ppa_loc$data[1]
)

hscp_ppa <- ppa_areas %>%
  filter(
    location == HSCP,
    year == max(year) | year == min(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

ppa_diff_hscp <- percent_change_calc(hscp_ppa$data[2], hscp_ppa$data[1])
ppa_word_change_hscp <- word_change_calc(hscp_ppa$data[2], hscp_ppa$data[1])

scot_ppa <- ppa_areas %>%
  filter(
    location == "Scotland",
    year == max(year) | year == min(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

diff_scot_ppa <- percent_change_calc(scot_ppa$data[2], scot_ppa$data[1])
word_change_scot_ppa <- word_change_calc(scot_ppa$data[2], scot_ppa$data[1])

# NHS health board
hb_ppa <- ppa_areas %>%
  filter(
    location == HB,
    year == max(year) | year == min(year)
  ) %>%
  mutate(formatted_data = format(data, big.mark = ","))

diff_hb_ppa <- percent_change_calc(hb_ppa$data[2], hb_ppa$data[1])
word_change_hb_ppa <- word_change_calc(hb_ppa$data[2], hb_ppa$data[1])

other_loc_ppa <- ppa %>%
  group_by(financial_year, hscp_locality) %>%
  summarise(admissions = sum(admissions)) %>%
  ungroup() %>%
  right_join(pops_other_locs, by = join_by(financial_year, hscp_locality)) %>%
  mutate(admissions = replace_na(admissions, 0)) %>%
  mutate(data = round_half_up(admissions / pop * 100000)) %>%
  mutate(data = format(data, big.mark = ",")) %>%
  select(hscp_locality, data) %>%
  pivot_wider(names_from = hscp_locality, values_from = data)


# 9. Psychiatric hospital admissions (ScotPHO) ----
# ___________________________________________________________________________

psych_hosp <- read_csv(paste0(
  import_folder,
  "scotpho_data_extract_psychiatric_admissions.csv"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

check_missing_data_scotpho(psych_hosp)

## Create variables for latest year
latest_period_psych_hosp <- unique(
  filter(psych_hosp, year == max(psych_hosp$year))$period_short
)

## Time trend
psych_hosp_time_trend <- psych_hosp %>%
  scotpho_time_trend(
    data = .,
    chart_title = "Psychiatric Patient Hospitalisations Time Trend",
    xaxis_title = "Financial Year Groups (3-year aggregates)",
    yaxis_title = "Psychiatric patient hospitalisations\n(Standardised rates per 100,000)",
    string_wrap = 10,
    rotate_xaxis = TRUE
  )


## Objects for text and summary table

# Locality latest value
psych_hosp_latest <- round_half_up(
  filter(
    psych_hosp,
    year == max(psych_hosp$year) &
      (area_name == LOCALITY & area_type == "Locality")
  )$measure,
  1
)

other_locs_psych_hosp <- psych_hosp %>%
  filter(year == max(year)) %>%
  filter(area_type == "Locality") %>%
  rename("hscp_locality" = "area_name") %>%
  right_join(other_locs, by = join_by(hscp_locality)) %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, measure) %>%
  mutate(measure = as.character(round_half_up(measure, 1))) %>%
  pivot_wider(names_from = hscp_locality, values_from = measure)

hscp_psych_hosp <- round_half_up(
  filter(
    psych_hosp,
    year == max(year) & (area_name == HSCP & area_type == "HSCP")
  )$measure,
  1
)

scot_psych_hosp <- round_half_up(
  filter(psych_hosp, year == max(year) & area_name == "Scotland")$measure,
  1
)


list_years <- unique(psych_hosp_time_trend$data[5])
list_years_latest <- list_years$period


# Locality
loc_psych_hosp <- psych_hosp %>%
  filter(period %in% list_years_latest) %>%
  filter(
    area_name == LOCALITY &
      area_type == "Locality",
    year == min(year) | year == max(year)
  ) %>%
  mutate(measure2 = format(measure, big.mark = ","))

diff_loc_psych <- percent_change_calc(
  loc_psych_hosp$measure[2],
  loc_psych_hosp$measure[1]
)
word_change_loc_psych <- word_change_calc(
  loc_psych_hosp$measure[2],
  loc_psych_hosp$measure[1]
)

# HSCP
hscp_psych_hosp <- psych_hosp %>%
  filter(period %in% list_years_latest) %>%
  filter(
    area_name == HSCP &
      area_type == "HSCP",
    year == min(year) | year == max(year)
  ) %>%
  mutate(measure2 = format(measure, big.mark = ","))

diff_hscp_psych <- percent_change_calc(
  hscp_psych_hosp$measure[2],
  hscp_psych_hosp$measure[1]
)
word_change_hscp_psych <- word_change_calc(
  hscp_psych_hosp$measure[2],
  hscp_psych_hosp$measure[1]
)

# NHS health board
hb_psych_hosp <- psych_hosp %>%
  filter(period %in% list_years_latest) %>%
  filter(
    area_name == HB &
      area_type == "Health board",
    year == min(year) | year == max(year)
  ) %>%
  mutate(measure2 = format(measure, big.mark = ","))

diff_hb_psych <- percent_change_calc(
  hb_psych_hosp$measure[2],
  hb_psych_hosp$measure[1]
)
word_change_hb_psych <- word_change_calc(
  hb_psych_hosp$measure[2],
  hb_psych_hosp$measure[1]
)

# Scotland
scot_psych_hosp <- psych_hosp %>%
  filter(period %in% list_years_latest) %>%
  filter(
    area_name == "Scotland" &
      area_type == "Scotland",
    year == min(year) | year == max(year)
  ) %>%
  mutate(measure2 = format(measure, big.mark = ","))

diff_scot_psych <- percent_change_calc(
  scot_psych_hosp$measure[2],
  scot_psych_hosp$measure[1]
)
word_change_scot_psych <- word_change_calc(
  scot_psych_hosp$measure[2],
  scot_psych_hosp$measure[1]
)

# Housekeeping ----
# These objects are left over after the script is run
# but don't appear to be used in any 'downstream' process:
# Main markdown, Summary Table, Excel data tables, SDC output.
# TODO: Investigate if these can be removed earlier or not created at all.
rm(
  ae_attendances,
  age_group_trend_usc,
  aggregate_usc_area_data,
  area_trend_usc,
  bed_days,
  bed_days_mh,
  delayed_disch,
  emergency_adm,
  falls,
  first_ae_att_max_age,
  first_ae_att_max_age_data,
  first_ae_att_min_age,
  first_ae_att_min_data,
  first_bd_mh_max_age,
  first_bd_mh_min_age,
  first_bd_mh_min_age1,
  first_bed_days_mh_loc,
  first_dd_loc,
  first_ea_max_age,
  first_ea_min_age,
  first_ea_min_age1,
  first_falls_hscp,
  first_falls_loc,
  first_falls_scot,
  first_fy_hb,
  first_fy_hb_ae,
  first_fy_hb_dd,
  first_fy_hb_falls,
  first_fy_hb_mh,
  first_fy_hb_read,
  first_fy_hb_ubd,
  first_fy_hscp,
  first_fy_hscp_ae,
  first_fy_hscp_ubd,
  first_fy_rate,
  first_fy_rate_ae_areas,
  first_fy_rate_ubd,
  first_fy_scot,
  first_fy_scot_ae,
  first_fy_scot_ubd,
  first_hscp_bed_days_mh,
  first_hscp_dd,
  first_hscp_read,
  first_re_max_age,
  first_re_min_age,
  first_read_loc,
  first_read_loc1,
  first_scot_bed_days_mh,
  first_scot_dd,
  first_scot_read,
  first_ubd_max_age,
  first_ubd_min_age,
  first_ubd_min_age1,
  hb_ae_att,
  hb_ae2,
  hb_bed_days,
  hb_bed_days2,
  hb_dd,
  hb_dd2,
  hb_emergency_adm,
  hb_emergency_adm2,
  hb_falls,
  hb_falls2,
  hb_mh_beddays,
  hb_mh_beddays2,
  hb_read,
  hb_read2,
  hscp_ae_att2,
  hscp_bed_days_mh2,
  hscp_bed_days2,
  hscp_dd2,
  hscp_emergency_adm2,
  hscp_falls,
  hscp_falls2,
  import_folder,
  latest_ae_att_loc,
  latest_ae_att_loc2,
  latest_ae_att_loc2_age,
  latest_ae_att_loc2_age_min,
  latest_ae_att_max_age,
  latest_ae_att_min_age,
  latest_bd_mh_max_age,
  latest_bd_mh_max_age2,
  latest_bd_mh_min_age,
  latest_bd_mh_min_age2,
  latest_bed_days_loc,
  latest_bed_days_loc2,
  latest_bed_days_mh_loc2,
  latest_dd_loc,
  latest_dd_loc2,
  latest_ea_max_age,
  latest_ea_max_age2,
  latest_ea_min_age,
  latest_ea_min_age2,
  latest_emergency_adm_loc,
  latest_emergency_adm_loc2,
  latest_falls_loc,
  latest_falls_loc2,
  latest_re_max_age,
  latest_re_min_age,
  latest_read_loc,
  latest_ubd_max_age,
  latest_ubd_max_age2,
  latest_ubd_min_age,
  latest_ubd_min_age2,
  list_years,
  list_years_latest,
  loc_pop,
  loc_pop_age1,
  loc_pop_age2,
  localities,
  # max_fy,
  min_year_ea_age1,
  min_year_ubd_age1,
  percent_change_calc,
  pop_areas_65plus,
  pop_areas_all_ages,
  pops_other_locs,
  pops_other_locs_65plus,
  populations,
  populations_proxy_year,
  ppa,
  ppa_65plus,
  ppa_total,
  ppa_under65,
  readmissions,
  scot_ae_att2,
  scot_bed_days_mh2,
  scot_bed_days2,
  scot_dd2,
  scot_emergency_adm2,
  scot_falls,
  scot_falls2,
  word_change_calc
)
gc()

## Stat disclosure control

# writexl::write_xlsx(x = list("Emergency Adm" = emergency_adm,
#                              "Unsch Bed Days" = bed_days,
#                              "Unsch Bed Days (SMR4)" = bed_days_mh,
#                              "A&E Att" = ae_attendances,
#                              "Delayed Disch" = delayed_disch),
#                     path = paste0(lp_path, "Publishing/MSG Data.xlsx"))

# falls_sdc <- falls %>%
#   group_by(financial_year, hscp2019name, hscp_locality) %>%
#   summarise(falls_admission = sum(admissions)) %>%
#   ungroup()

# writexl::write_xlsx(x = list("Falls" = falls_sdc,
#                              "Readmissions 28" = readmissions,
#                              "PPA" = ppa),
#                     path = paste0(lp_path, "Publishing/SMR Data.xlsx"))
