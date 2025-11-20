# LOCALITY PROFILES DEMOGRAPHICS: SIMD

### First Created: 08/08/2019
### Original Author: Aidan Morrison

### Written for: RStudio Server Pro, R Version 3.6.1

### Description: The purpose of this code is to produce outputs on deprivation to be
###              used for LIST locality profiles produced in RMarkdown.

### Revised Oct/Nov 2022 by Craig Fraser and Luke Taylor for smoother process, ex:
# Incorporated lookup functions so less dependent on static files

### Script restructuring Nov 22 by C Puech

# SECTION 1: Packages, file paths, etc ----

## Libraries
library(reshape2)
library(ggrepel)
library(sf)

# Source in global functions/themes script
# source("Master RMarkdown Document & Render Code/Global Script.R")

## Final document will loop through a list of localities
# Create placeholder for for loop
# LOCALITY <- "Skye, Lochalsh and West Ross"
# LOCALITY <- "Falkirk West"
# LOCALITY <- "Stirling City with the Eastern Villages Bridge of Allan and Dunblane"
# LOCALITY <- "Ayr North and Former Coalfield Communities"
# LOCALITY <- "Helensburgh and Lomond"
# LOCALITY <- "City of Dunfermline"
# LOCALITY <- "Eastwood"

# SECTION 2: Data Imports ----

## Locality/DZ lookup
lookup_dz <- read_in_localities(dz_level = TRUE)

## Population data
pop_raw_data <- read_in_dz_pops()

pop_max_year <- max(pop_raw_data$year)

pop_data <- pop_raw_data %>%
  filter(year == max(year)) %>%
  group_by(
    year,
    datazone2011,
    hscp_locality,
    hscp2019name,
    simd2020v2_sc_quintile
  ) %>%
  summarise(total_pop = sum(total_pop)) %>%
  ungroup()


## SIMD Domains

lookups_dir <- path("/conf/linkage/output/lookups/Unicode")

# 2020
simd_2020_all <- read_rds(path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_simd2020v2.rds"
)) %>%
  select(datazone2011, simd = "simd2020v2_sc_quintile")

simd_2020_dom <- read_rds(path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_domain_level_simd.rds"
)) %>%
  clean_names() %>%
  select(
    datazone2011,
    income = "simd2020v2_inc_quintile",
    employment = "simd2020v2_emp_quintile",
    education = "simd2020v2_educ_quintile",
    access = "simd2020v2_access_quintile",
    housing = "simd2020v2_house_quintile",
    health = "simd2020v2_hlth_quintile",
    crime = "simd2020v2_crime_quintile"
  )

simd2020 <- merge(simd_2020_all, simd_2020_dom, by = "datazone2011") %>%
  left_join(pop_data, by = join_by(datazone2011))

# 2016
simd_2016_all <- read_rds(path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_simd2016.rds"
)) %>%
  select(datazone2011 = "DataZone2011", simd = "simd2016_sc_quintile")

simd_2016_dom <- read_rds(path(
  lookups_dir,
  "Deprivation",
  "DataZone2011_domain_level_simd.rds"
)) %>%
  clean_names() %>%
  select(
    datazone2011,
    income = "simd2016_inc_quintile",
    employment = "simd2016_emp_quintile",
    education = "simd2016_educ_quintile",
    access = "simd2016_access_quintile",
    housing = "simd2016_house_quintile",
    health = "simd2016_hlth_quintile",
    crime = "simd2016_crime_quintile"
  )

simd2016 <- merge(simd_2016_all, simd_2016_dom, by = "datazone2011") %>%
  left_join(lookup_dz, by = join_by(datazone2011))

rm(simd_2020_all, simd_2020_dom, simd_2016_all, simd_2016_dom)


# SECTION 3: Outputs ----

## 5a) SIMD summary ----

# calculate the percent of the population within each deprivation quintile
simd_perc_breakdown <- pop_data %>%
  mutate(simd2020v2_sc_quintile = as.factor(simd2020v2_sc_quintile)) %>%
  filter(hscp_locality == LOCALITY) %>%
  group_by(simd2020v2_sc_quintile, .drop = FALSE) %>%
  summarise(pop = sum(total_pop)) %>%
  mutate(
    total_pop = sum(pop),
    perc = round_half_up(100 * pop / total_pop, 1)
  ) %>%
  mutate(perc = replace_na(perc, 0)) %>%
  arrange(simd2020v2_sc_quintile)

perc_bottom_quintile <- simd_perc_breakdown[1, ]$perc
perc_top_quintile <- simd_perc_breakdown[5, ]$perc


## 5b) SIMD map ----

# load in shapefile for mapping
zones <- read_sf(path(
  lookups_dir,
  "Geography",
  "Shapefiles",
  "Data Zones 2011",
  "SG_DataZone_Bdry_2011.shp"
)) %>%
  st_transform(4326) %>%
  rename(datazone2011 = DataZone)

# merge lookup and shapefile
zones <- merge(zones, lookup_dz, by = "datazone2011")

# subset for Locality
zones <- subset(zones, hscp_locality == LOCALITY)

# Get latitude and longitude co-ordinates for each datazone, find min and max.
zones_coord <-
  zones %>%
  st_coordinates() %>%
  as_tibble() %>%
  select("long" = X, "lat" = Y) %>%
  summarise(
    min_long = min(long),
    max_long = max(long),
    min_lat = min(lat),
    max_lat = max(lat)
  )

# Get min and max longitude for locality
min_long <- zones_coord$min_long
max_long <- zones_coord$max_long
min_lat <- zones_coord$min_lat
max_lat <- zones_coord$max_lat

# get place names
places <- read_csv(path(
  lookups_dir,
  "Geography",
  "Shapefiles",
  "Scottish Places",
  "Places to Data Zone Lookup.csv"
)) %>%
  rename(datazone2011 = DataZone) %>%
  filter(datazone2011 %in% zones$datazone2011) %>%
  # extra filter to remove place names with coordinates outwith locality
  filter(
    between(Longitude, min_long, max_long),
    between(Latitude, min_lat, max_lat)
  ) %>%
  group_by(name) %>%
  summarise(
    Longitude = first(Longitude),
    Latitude = first(Latitude),
    type = first(type),
    datazone2011 = first(datazone2011)
  ) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)


# load in 2020 deprivation data
simd_map_data <- simd2020 %>%
  filter(hscp_locality == LOCALITY) %>%
  select(datazone2011, simd)

# merge with shapefile
zones <- merge(zones, simd_map_data, by = "datazone2011")

# set colours for simd
simd_col <- RColorBrewer::brewer.pal(n = 5, name = "RdYlBu")
simd_cats <- paste("SIMD", 1:5)

# plot
simd_map <- ggplot() +
  geom_sf(
    data = zones,
    aes(fill = ordered(simd, levels = 1:5)),
    colour = "black"
  ) +
  scale_fill_manual(values = simd_col, labels = simd_cats, drop = FALSE) +
  geom_label_repel(
    data = places,
    aes(x = Longitude, y = Latitude, label = name),
    color = "black",
    size = 3.5,
    fill = "#FFFFFF40", # Add a semi-transparent white background to the labels
    label.size = NA # Labels don't have a border
  ) +
  theme_void() +
  guides(fill = guide_legend(title = "SIMD Quintile")) +
  labs(caption = "Source: Scottish Government, Public Health Scotland")

rm(zones, places, simd_map_data)

## 5c) SIMD domains ----

## deprivation domains
plot_labels <- c(
  "Access",
  "Crime",
  "Education",
  "Employment",
  "Health",
  "Housing",
  "Income",
  "General"
)

# SIMD topic breakdown

simd_domains <- simd2020 %>%
  filter(hscp_locality == LOCALITY) %>%
  select(
    income,
    employment,
    education,
    access,
    crime,
    health,
    housing,
    total_pop
  ) %>%
  reshape2::melt(id.vars = "total_pop") %>%
  group_by(variable, value) %>%
  summarise(total_pop = sum(total_pop)) %>%
  ggplot(aes(
    fill = factor(value, levels = 1:5),
    y = total_pop,
    x = factor(variable, levels = tolower(plot_labels))
  )) +
  geom_col(position = "fill") +
  scale_x_discrete(labels = plot_labels) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "",
    y = "Proportion of Population",
    title = paste0("Breakdown of the SIMD Domains in ", str_wrap(LOCALITY, 50)),
    caption = "Source: Scottish Government, Public Health Scotland, National Records Scotland"
  ) +
  scale_fill_manual(
    name = "Quintile",
    labels = simd_cats,
    values = simd_col,
    drop = FALSE
  ) +
  theme_profiles()


## 5d) SIMD 2016 vs 2020 ----

# Deprivation Data 2020
simd2020_dom <- simd2020 %>%
  filter(hscp_locality == LOCALITY) %>%
  select(
    datazone2011,
    simd,
    income,
    employment,
    education,
    access,
    crime,
    health,
    housing
  )

names(simd2020_dom)[2:9] <- paste0(names(simd2020_dom)[2:9], "_20")

# Deprivation Data 2016
simd2016_dom <- simd2016 %>%
  filter(hscp_locality == LOCALITY) %>%
  select(
    datazone2011,
    simd,
    income,
    employment,
    education,
    access,
    crime,
    health,
    housing
  )

names(simd2016_dom)[2:9] <- paste0(names(simd2016_dom)[2:9], "_16")

# Get most up to date datazone populations

pop_16_20 <- pop_raw_data %>%
  filter(year %in% c(2016, pop_max_year)) %>%
  select(year, datazone2011, sex, pop = total_pop) %>%
  group_by(datazone2011, year) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  mutate(simd_rank_year = if_else(year == 2016, "pop_16", "pop_20")) %>%
  select(-year) %>%
  pivot_wider(names_from = simd_rank_year, values_from = pop)

## Data wrangling
simd2016_dom <- simd2016_dom %>%
  left_join(pop_16_20, by = join_by(datazone2011)) %>%
  select(datazone2011, contains("_16")) %>%
  reshape2::melt(id.vars = c("datazone2011", "pop_16")) %>%
  group_by(variable) %>%
  mutate(total_pop = sum(pop_16)) %>%
  group_by(variable, value) %>%
  summarise(
    pop = sum(pop_16),
    total_pop = max(total_pop)
  ) %>%
  mutate(
    perc_16 = pop / total_pop,
    domain = gsub("_16", "", variable, fixed = TRUE)
  ) %>%
  ungroup() %>%
  select(domain, perc_16, quintile = value)

simd2020_dom <- simd2020_dom %>%
  left_join(pop_16_20, by = join_by(datazone2011)) %>%
  select(datazone2011, contains("_20")) %>%
  reshape2::melt(id.vars = c("datazone2011", "pop_20")) %>%
  group_by(variable) %>%
  mutate(total_pop = sum(pop_20)) %>%
  group_by(variable, value) %>%
  summarise(
    pop = sum(pop_20),
    total_pop = max(total_pop)
  ) %>%
  mutate(
    perc_20 = pop / total_pop,
    domain = gsub("_20", "", variable, fixed = TRUE)
  ) %>%
  ungroup() %>%
  select(domain, perc_20, quintile = value)

domains <- unique(simd2020_dom[["domain"]])
base_data <- tibble(
  domain = rep(domains, each = 5),
  quintile = rep(1:5, 8)
)

## Outputs

simd_16_20_dom <- full_join(
  base_data,
  simd2016_dom,
  by = join_by(domain, quintile)
) %>%
  mutate(perc_16 = replace_na(perc_16, 0)) %>%
  full_join(simd2020_dom, by = join_by(domain, quintile)) %>%
  mutate(perc_20 = replace_na(perc_20, 0)) %>%
  mutate(diff = perc_20 - perc_16) %>%
  mutate(
    domain = ifelse(domain == "simd", "SIMD", str_to_title(domain)),
    v_just = ifelse(diff < 0, 1.5, -1)
  )

simd_diff_plot <- ggplot(
  simd_16_20_dom,
  aes(x = quintile, y = diff, fill = factor(quintile))
) +
  facet_wrap(
    ~ factor(
      domain,
      levels = c("SIMD", unique(sort(simd_16_20_dom$domain))[1:7])
    ),
    ncol = 4
  ) +
  geom_line(aes(y = 0, group = 1)) +
  geom_col(
    position = position_dodge(),
    color = "black"
  ) +
  geom_text(
    aes(
      label = paste0(format(round_half_up(100 * diff, 1), nsmall = 1), "%"),
      vjust = v_just
    ),
    color = "black",
    position = position_dodge(0),
    size = 2.8
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(
      -1.4 * max(abs(simd_16_20_dom$diff)),
      1.4 * max(abs(simd_16_20_dom$diff))
    )
  ) +
  scale_fill_manual(
    name = "Population-Weighted Quintile",
    labels = simd_cats,
    values = (simd_col),
    drop = FALSE
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    x = "",
    y = "Difference from 2016",
    title = paste0(
      "Difference in Population Living in Deprivation Quintiles by SIMD Domain\n",
      "in 2016 and ",
      pop_max_year,
      " in ",
      str_wrap(LOCALITY, 50)
    ),
    caption = "Source: Scottish Government, National Records Scotland"
  ) +
  guides(fill = guide_legend(title.position = "top"))

simd_diff_overall <- simd_16_20_dom %>%
  filter(domain == "SIMD") %>%
  mutate(
    Quintile = paste(domain, quintile),
    perc_16 = paste0(format(round_half_up(100 * perc_16, 1), nsmall = 1), "%"),
    perc_20 = paste0(format(round_half_up(100 * perc_20, 1), nsmall = 1), "%"),
    Difference = paste0(format(round_half_up(100 * diff, 1), nsmall = 1), "%")
  ) %>%
  select(Quintile, perc_16, perc_20, Difference)


##################### SECTION 4: Objects for summary table #######################

## Relevant lookups for creating the table objects
lookup <- read_in_localities()

## Relevant lookups for creating the table objects
HSCP <- as.character(filter(lookup, hscp_locality == LOCALITY)$hscp2019name)

# Determine other localities based on LOCALITY object
other_locs <- lookup %>%
  select(hscp_locality, hscp2019name) %>%
  filter(hscp2019name == HSCP & hscp_locality != LOCALITY) %>%
  arrange(hscp_locality)

# Find number of locs per partnership
n_loc <- count_localities(lookup, HSCP)


## Other localities in HSCP objects

other_locs_simd <- pop_data %>%
  mutate(simd2020v2_sc_quintile = as.factor(simd2020v2_sc_quintile)) %>%
  filter(
    year == max(year),
    hscp_locality %in% other_locs$hscp_locality
  ) %>%
  group_by(hscp_locality, simd2020v2_sc_quintile, .drop = FALSE) %>%
  summarise(pop = sum(total_pop)) %>%
  ungroup() %>%
  group_by(hscp_locality) %>%
  mutate(total_pop = sum(pop)) %>%
  ungroup() %>%
  mutate(perc = round_half_up(pop / total_pop * 100, 1)) %>%
  filter(simd2020v2_sc_quintile == 1 | simd2020v2_sc_quintile == 5) %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, simd2020v2_sc_quintile, perc) %>%
  pivot_wider(names_from = hscp_locality, values_from = perc) %>%
  arrange(desc(simd2020v2_sc_quintile)) %>%
  select(-simd2020v2_sc_quintile)


## HSCP objects

hscp_simd <- pop_data %>%
  mutate(simd2020v2_sc_quintile = as.factor(simd2020v2_sc_quintile)) %>%
  filter(
    year == max(year),
    hscp2019name == HSCP
  ) %>%
  group_by(simd2020v2_sc_quintile, .drop = FALSE) %>%
  summarise(pop = sum(total_pop)) %>%
  ungroup() %>%
  mutate(perc = round_half_up(pop / sum(pop) * 100, 1))

hscp_simd_top <- filter(hscp_simd, simd2020v2_sc_quintile == 5)$perc
hscp_simd_bottom <- filter(hscp_simd, simd2020v2_sc_quintile == 1)$perc

# Housekeeping ----
# These objects are left over after the script is run
# but don't appear to be used in any 'downstream' process:
# Main markdown, Summary Table, Excel data tables, SDC output.
# TODO: Investigate if these can be removed earlier or not created at all.
rm(
  base_data,
  domains,
  hscp_simd,
  lookup_dz,
  lookups_dir,
  max_lat,
  max_long,
  min_lat,
  min_long,
  pop_16_20,
  pop_data,
  pop_raw_data,
  simd_cats,
  simd_col,
  simd2016,
  simd2020,
  zones_coord
)
gc()
