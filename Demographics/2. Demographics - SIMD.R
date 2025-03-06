######################## LOCALITY PROFILES DEMOGRAPHICS: SIMD ########################.

### First Created: 08/08/2019
### Original Author: Aidan Morrison

### Written for: RStudio Server Pro, R Version 3.6.1

### Description: The purpose of this code is to produce outputs on deprivation to be
###              used for LIST locality profiles produced in RMarkdown.

### Revised Oct/Nov 2022 by Craig Fraser and Luke Taylor for smoother process, ex:
# Incorporated lookup functions so less dependent on static files

### Script restructuring Nov 22 by C Puech

####################### SECTION 1: Packages, file paths, etc #########################

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
# LOCALITY <- "Inverness"


########################## SECTION 2: Data Imports ###############################

## Locality/DZ lookup
lookup_dz <- read_in_localities(dz_level = TRUE)

## Population data
pop_raw_data <- read_in_dz_pops()

pop_max_year <- max(pop_raw_data$year)

pop_data <- pop_raw_data %>%
  filter(year == max(year)) %>%
  group_by(year, datazone2011, hscp_locality, hscp2019name, simd2020v2_sc_quintile) %>%
  summarise(total_pop = sum(total_pop)) %>%
  ungroup()


## SIMD Domains

# 2020
simd_2020_all <- read_rds("/conf/linkage/output/lookups/Unicode/Deprivation/DataZone2011_simd2020v2.rds") %>%
  select(datazone2011, simd = "simd2020v2_sc_quintile")

simd_2020_dom <- read_rds("/conf/linkage/output/lookups/Unicode/Deprivation/DataZone2011_domain_level_simd.rds") %>%
  clean_names() %>%
  select(datazone2011, income = "simd2020v2_inc_quintile", employment = "simd2020v2_emp_quintile", education = "simd2020v2_educ_quintile", access = "simd2020v2_access_quintile", housing = "simd2020v2_house_quintile", health = "simd2020v2_hlth_quintile", crime = "simd2020v2_crime_quintile")

simd2020 <- merge(simd_2020_all, simd_2020_dom, by = "datazone2011") %>%
  left_join(pop_data)

# 2016
simd_2016_all <- read_rds("/conf/linkage/output/lookups/Unicode/Deprivation/DataZone2011_simd2016.rds") %>%
  select(datazone2011 = "DataZone2011", simd = "simd2016_sc_quintile")

simd_2016_dom <- read_rds("/conf/linkage/output/lookups/Unicode/Deprivation/DataZone2011_domain_level_simd.rds") %>%
  clean_names() %>%
  select(datazone2011, income = "simd2016_inc_quintile", employment = "simd2016_emp_quintile", education = "simd2016_educ_quintile", access = "simd2016_access_quintile", housing = "simd2016_house_quintile", health = "simd2016_hlth_quintile", crime = "simd2016_crime_quintile")

simd2016 <- merge(simd_2016_all, simd_2016_dom, by = "datazone2011") %>%
  left_join(lookup_dz)

rm(simd_2020_all, simd_2020_dom, simd_2016_all, simd_2016_dom)



############################# SECTION 3: Outputs #############################

## 5a) SIMD summary ----

perc_bottom_quintile <- list()
perc_top_quintile <- list()

# calculate the percent of the population within each deprivation quintile
for(loc in locality_list){
  simd_perc_breakdown <- pop_data %>%
    mutate(simd2020v2_sc_quintile = as.factor(simd2020v2_sc_quintile)) %>%
    filter(hscp_locality %in% loc) %>%
    group_by(simd2020v2_sc_quintile, .drop = FALSE) %>%
    dplyr::summarise(pop = sum(total_pop)) %>%
    mutate(
      total_pop = sum(pop),
      perc = round_half_up(100 * pop / total_pop, 1)
    ) %>%
    mutate(perc = replace_na(perc, 0)) %>%
    arrange(simd2020v2_sc_quintile)
  
  perc_bottom_quintile[[loc]] <- simd_perc_breakdown[1, ]$perc
  perc_top_quintile[[loc]] <- simd_perc_breakdown[5, ]$perc
}

rm(simd_perc_breakdown)


## 5b) SIMD map ----

# load in shapefile for mapping
zones_all <- read_sf(dsn = "//conf/linkage/output/lookups/Unicode/Geography/Shapefiles/Data Zones 2011/SG_DataZone_Bdry_2011.shp") %>%
  st_transform(4326) %>%
  rename(datazone2011 = datazone20)

# merge lookup and shapefile
zones_merged <- merge(zones_all, lookup_dz, by = "datazone2011")

simd_map <- list()
for (loc in locality_list){
# subset for Locality
zones <- subset(zones_merged, hscp_locality == loc)

# Get latitude and longitdue co-ordinates for each datazone, find min and max.
zones_coord <-
  zones %>%
  sf::st_coordinates() %>%
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
places <- read_csv(paste0(
  "/conf/linkage/output/lookups/Unicode/Geography/",
  "Shapefiles/Scottish Places/Places to Data",
  " Zone Lookup.csv"
)) %>%
  rename(datazone2011 = DataZone) %>%
  filter(datazone2011 %in% zones$datazone2011) %>%
  # extra filter to remove place names with coordinates outwith locality
  filter(Longitude >= min_long & Longitude <= max_long &
    Latitude >= min_lat & Latitude <= max_lat) %>%
  group_by(name) %>%
  dplyr::summarise(
    Longitude = first(Longitude),
    Latitude = first(Latitude),
    type = first(type),
    datazone2011 = first(datazone2011)
  ) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)


# load in 2020 deprivation data
simd_map_data <- simd2020 %>%
  filter(hscp_locality == loc) %>%
  dplyr::select(datazone2011, simd)

# merge with shapefile
zones <- merge(zones, simd_map_data, by = "datazone2011")

# set colours for simd
simd_col <- RColorBrewer::brewer.pal(n = 5, name = "RdYlBu")
simd_cats <- paste("SIMD", 1:5)

# plot
simd_map[[loc]] <- ggplot() +
  geom_sf(
    data = zones,
    aes(fill = ordered(simd, levels = 1:5)), colour = "black"
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
  guides(fill = guide_legend(title = "SIMD Quintile"))# +
  #labs(caption = "Source: Scottish Government, Public Health Scotland")
}
rm(zones, places, simd_map_data)

## 5c) SIMD domains ----

## deprivation domains
plot_labels <- c(
  "Access", "Crime", "Education", "Employment",
  "Health", "Housing", "Income", "General"
)

# SIMD topic breakdown

simd_domains <-  
  map(locality_list,
      ~simd2020 %>%
        filter(hscp_locality == .x) %>%
  select(income, employment, education, access, crime, health, housing, total_pop) %>%
  reshape2::melt(id.vars = "total_pop") %>%
  group_by(variable, value) %>%
  # mutate(grand_avg_pop = sum(total_pop)) %>% 
  #   group_by(variable, value, grand_avg_pop) %>% 
  dplyr::summarise(total_pop = sum(total_pop)) %>%
    group_by(variable) %>% 
    mutate(grand_sum = sum(total_pop),
    proportion = total_pop/grand_sum) %>% 
    
    mutate(value = factor(value, levels = rev(1:5), ordered = T)) %>% 
  ggplot(aes(
    fill = value, y = proportion,
    x = factor(variable, levels = tolower(plot_labels))
  )) +
  geom_col(position = "stack") +
    coord_flip()+
  scale_x_discrete(labels = plot_labels) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "", y = "Proportion of Population",
    title = paste0("Breakdown of the SIMD Domains in\n ", str_wrap(.x, 50))#,
   # caption = "Source: Scottish Government, Public Health Scotland, National Records Scotland"
  ) +
  scale_fill_manual(
    name = "Quintile",
    labels = rev(simd_cats),
    values = rev(simd_col), drop = FALSE
  ) +
  theme_profiles()+
   guides(fill = guide_legend(title = NULL, 
                              nrow = 1,
                              reverse = T))
  )


## 5d) SIMD 2016 vs 2020 ----

simd2020_dom <- list()
simd2016_dom <- list()
for(loc in locality_list){
  # Deprivation Data 2020
  simd2020_dom[[loc]] <- simd2020 %>%
    filter(hscp_locality == loc) %>%
    select(datazone2011, simd, income, employment, education, access, crime, health, housing)
  
  names(simd2020_dom[[loc]])[2:9] <- paste0(names(simd2020_dom[[loc]])[2:9], "_20")
  
  # Deprivation Data 2016
  simd2016_dom[[loc]] <- simd2016 %>%
    filter(hscp_locality == loc) %>%
    select(datazone2011, simd, income, employment, education, access, crime, health, housing)
  
  names(simd2016_dom[[loc]])[2:9] <- paste0(names(simd2016_dom[[loc]])[2:9], "_16")
}
# Get most up to date datazone populations

pop_16_20 <- pop_raw_data %>%
  filter(year %in% c(2016, pop_max_year)) %>%
  select(year, datazone2011, sex, pop = total_pop) %>%
  dplyr::group_by(datazone2011, year) %>%
  dplyr::summarise(pop = sum(pop)) %>%
  ungroup() %>%
  mutate(simd_rank_year = if_else(year == 2016, "pop_16", "pop_20")) %>%
  select(-year) %>%
  pivot_wider(names_from = simd_rank_year, values_from = pop)

## Data wrangling
simd_diff_plot <- list()
simd_diff_overall <- list()
simd_16_20_dom <- list()
for(loc in locality_list){
  
  simd2016_dom[[loc]] <- simd2016_dom[[loc]]  %>%
    left_join(pop_16_20, join_by(datazone2011)) %>%
    select(datazone2011, contains("_16")) %>%
    reshape2::melt(id.vars = c("datazone2011", "pop_16")) %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(total_pop = sum(pop_16)) %>%
    dplyr::group_by(variable, value) %>%
    dplyr::summarise(
      pop = sum(pop_16),
      total_pop = max(total_pop)
    ) %>%
    dplyr::mutate(
      perc_16 = pop / total_pop,
      domain = gsub("_16", "", variable)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(domain, perc_16, quintile = value)
  
  simd2020_dom[[loc]] <- simd2020_dom[[loc]] %>%
    left_join(pop_16_20) %>%
    select(datazone2011, contains("_20")) %>%
    reshape2::melt(id.vars = c("datazone2011", "pop_20")) %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(total_pop = sum(pop_20)) %>%
    dplyr::group_by(variable, value) %>%
    dplyr::summarise(
      pop = sum(pop_20),
      total_pop = max(total_pop)
    ) %>%
    dplyr::mutate(
      perc_20 = pop / total_pop,
      domain = gsub("_20", "", variable)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(domain, perc_20, quintile = value)
  
  domains <- simd2020_dom[[loc]]$domain %>% unique()
  base_data <- tibble(
    domain = rep(domains, each = 5),
    quintile = rep(1:5, 8)
  )
}
  
## Outputs

for(loc in locality_list){
  simd_16_20_dom[[loc]] <- full_join(base_data, simd2016_dom[[loc]]) %>%
  mutate(perc_16 = replace_na(perc_16, 0)) %>%
  full_join(simd2020_dom[[loc]]) %>%
  mutate(perc_20 = replace_na(perc_20, 0)) %>%
  mutate(diff = perc_20 - perc_16) %>%
  mutate(
    domain = ifelse(domain == "simd", "SIMD", tools::toTitleCase(domain)),
    v_just = ifelse(diff < 0, 1.5, -1)
  )

simd_diff_plot[[loc]] <- ggplot(simd_16_20_dom[[loc]], aes(x = quintile, y = diff, fill = factor(quintile))) +
  facet_wrap(~ factor(domain, levels = c("SIMD", unique(sort(simd_16_20_dom[[loc]]$domain))[1:7])), ncol = 4) +
  geom_line(aes(y = 0, group = 1)) +
  geom_col(
    position = position_dodge(),
    color = "black"
  ) +
  geom_text(
    aes(
      label = paste0(format(janitor::round_half_up(100 * diff, 1), nsmall = 1), "%"),
      vjust = v_just
    ),
    color = "black",
    position = position_dodge(0), size = 2.8
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(-1.4 * max(abs(simd_16_20_dom[[loc]]$diff)), 1.4 * max(abs(simd_16_20_dom[[loc]]$diff)))
  ) +
  scale_fill_manual(
    name = "Population-Weighted Quintile",
    labels = simd_cats,
    values = (simd_col), drop = FALSE
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    x = "", y = "Difference from 2016",
    title = paste0(
      "Difference in Population Living in Deprivation Quintiles by SIMD Domain\n",
      "in 2016 and ", pop_max_year, " in ", loc)#,
    # caption = "Source: Scottish Government, National Records Scotland"
  ) +
  guides(fill = guide_legend(title.position = "top"))

simd_diff_overall[[loc]] <- simd_16_20_dom[[loc]] %>%
  filter(domain == "SIMD") %>%
  mutate(
    Quintile = paste(domain, quintile),
    perc_16 = paste0(format(janitor::round_half_up(100 * perc_16, 1), nsmall = 1), "%"),
    perc_20 = paste0(format(janitor::round_half_up(100 * perc_20, 1), nsmall = 1), "%"),
    Difference = paste0(format(janitor::round_half_up(100 * diff, 1), nsmall = 1), "%")
  ) %>%
  select(Quintile, perc_16, perc_20, Difference)
}


##################### SECTION 4: Objects for summary table #######################

## Relevant lookups for creating the table objects
lookup <- read_in_localities()

## Relevant lookups for creating the table objects
HSCP <- as.character(filter(lookup, hscp_locality %in% locality_list)$hscp2019name) %>% 
  unique()

# Determine other localities based on LOCALITY object
other_locs <- lookup %>%
  select(hscp_locality, hscp2019name) %>%
  filter(hscp2019name == HSCP & hscp_locality == locality_list[2]) %>%
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
