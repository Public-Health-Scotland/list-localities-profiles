# LOCALITY PROFILES SERVICES MAP & TABLE CODE
# Code for creating the HSCP services map for the locality profiles

# 0. Testing Set up ----

## Select HCSP (for testing only)
# HSCP <- "Renfrewshire"

## Set file path

# lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Source in functions code (for testing only)
# source("Master RMarkdown Document & Render Code/Global Script.R")

## Select a locality based on the HSCP (for source code "2. Services Outputs" to run - it does not matter which one is chosen)
# LOCALITY <- read_in_localities() |> filter(hscp2019name == HSCP) |> slice(1) |> pull(hscp_locality)

# Source the data manipulation script for services
# source("Services/2. Services data manipulation & table.R")

# 1. Set up ----

## Load packages
library(readr)
library(dplyr)
library(sf)
library(ggrepel)
library(ggmap)
library(patchwork)

# 2. Read in locality shape files ----

shp <- read_sf(
  "/conf/linkage/output/lookups/Unicode/Geography/Shapefiles/HSCP Locality (Datazone2011 Base)/HSCP_Locality.shp"
)
shp <- st_transform(shp, 4326) |>
  select(hscp_local, HSCP_name, Shape_Leng, Shape_Area, geometry)

shp <- shp |>
  mutate(hscp_locality = gsub("&", "and", hscp_local, fixed = TRUE)) |>
  merge(lookup2, by = "hscp_locality")

shp_hscp <- shp |>
  filter(hscp2019name == HSCP) |>
  mutate(
    hscp_locality = stringr::str_wrap(hscp_locality, 24),
    hscp_local = stringr::str_wrap(hscp_local, 24)
  )

# 3. Map Code ----
# 3.1 Palettes ----

# Create colour palettes for different numbers of localities
if (n_loc < 5) {
  col_palette <- c("#3F3685", "#9B4393", "#0078D4", "#83BB26")
} else if (n_loc %in% c(5, 6)) {
  col_palette <- c(
    "#3F3685",
    "#9B4393",
    "#0078D4",
    "#83BB26",
    "#948DA3",
    "#1E7F84"
  )
} else if (n_loc == 7) {
  col_palette <- c(
    "#3F3685",
    "#9B4393",
    "#0078D4",
    "#83BB26",
    "#948DA3",
    "#1E7F84",
    "#6B5C85"
  )
} else if (n_loc == 8) {
  col_palette <- c(
    "#3F3685",
    "#9B4393",
    "#0078D4",
    "#83BB26",
    "#948DA3",
    "#1E7F84",
    "#6B5C85",
    "#C73918"
  )
} else if (n_loc == 9) {
  col_palette <- c(
    "#3F3685",
    "#9B4393",
    "#0078D4",
    "#83BB26",
    "#948DA3",
    "#1E7F84",
    "#6B5C85",
    "#C73918",
    "orchid3"
  )
}

# 3.2 Locality shapes ----
# Get latitude and longitude coordinates for each data locality, find min and max.
zones_coord <- shp_hscp |>
  st_coordinates() |>
  as_tibble() |>
  select("long" = X, "lat" = Y) |>
  summarise(
    min_long = min(long),
    max_long = max(long),
    min_lat = min(lat),
    max_lat = max(lat)
  )

# Get min and max longitude for locality, add a 0.01 extra to add a border to map.
min_long <- zones_coord$min_long - 0.01
max_long <- zones_coord$max_long + 0.01
min_lat <- zones_coord$min_lat - 0.01
max_lat <- zones_coord$max_lat + 0.01

# get data zones in HSCP
hscp_loc <- read_csv(
  "/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/HSCP Localities_DZ11_Lookup_20240513.csv"
) |>
  select(datazone2011, hscp2019name) |>
  filter(hscp2019name == HSCP)

# get place names of cities, towns and villages within locality
places <- read_csv(paste0(
  "/conf/linkage/output/lookups/Unicode/Geography/",
  "Shapefiles/Scottish Places/Places to Data Zone Lookup.csv"
)) |>
  rename(datazone2011 = DataZone) |>
  filter(datazone2011 %in% hscp_loc$datazone2011) |>
  # extra filter to remove place names with coordinates outwith locality
  filter(
    Longitude >= min_long &
      Longitude <= max_long &
      Latitude >= min_lat &
      Latitude <= max_lat
  ) |>
  group_by(name) |>
  summarise(
    Longitude = first(Longitude),
    Latitude = first(Latitude),
    type = first(type)
  ) |>
  st_as_sf(coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326) |>
  filter(!grepl("_", name, fixed = TRUE)) |> # filter incorrect name types
  filter(type != "hamlet" & type != "village") # remove smaller places

# 3.3 Background map ----
locality_map_id <- read_csv(paste0(lp_path, "Services/", "locality_map_id.csv"))
api_key <- locality_map_id$id
# upload map background from stadia maps, enter registration key, filter for max and min long/lat
register_stadiamaps(key = api_key)
service_map_background <- get_stadiamap(
  bbox = c(
    min_long,
    min_lat,
    max_long,
    max_lat
  ),
  maptype = "stamen_terrain_background"
)

# preview map
# ggmap(service_map_background)

# 3.4 Map markers ----
# add locality polygons and service markers to map where services are located
service_map <- ggmap(service_map_background) +
  geom_sf(
    data = shp_hscp,
    mapping = aes(
      fill = hscp_local
    ),
    colour = "black",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  labs(fill = "Locality")

# check if services markers exist for locality
if (nrow(markers_gp) > 0) {
  service_map <- service_map +
    geom_point(
      data = markers_gp,
      mapping = aes(
        x = longitude,
        y = latitude,
        colour = "GP Practice"
      ),
      size = 2,
      shape = 21,
      stroke = 0.5,
      fill = "red"
    )
}
if (nrow(markers_care_home) > 0) {
  service_map <- service_map +
    geom_point(
      data = markers_care_home,
      mapping = aes(
        x = longitude,
        y = latitude,
        colour = "Care Home"
      ),
      size = 2,
      shape = 22,
      stroke = 0.5,
      fill = "yellow"
    )
}
if (nrow(markers_emergency_dep) > 0) {
  service_map <- service_map +
    geom_point(
      data = markers_emergency_dep,
      mapping = aes(
        x = longitude,
        y = latitude,
        colour = "Emergency Department"
      ),
      size = 2,
      shape = 23,
      stroke = 0.5,
      fill = "blue"
    )
}
if (nrow(markers_miu) > 0) {
  service_map <- service_map +
    geom_point(
      data = markers_miu,
      mapping = aes(
        x = longitude,
        y = latitude,
        colour = "Minor Injuries Unit"
      ),
      size = 2,
      shape = 24,
      stroke = 0.5,
      fill = "green"
    )
}

# preview HSCP map with service markers added and localities outlined
# plot(service_map)

# 3.5 Final map ----
# create final service map WITHOUT LEGEND

service_map <- service_map +
  labs(colour = "Service Type") +
  scale_color_manual(
    values = c(
      "GP Practice" = "black",
      "Care Home" = "black",
      "Emergency Department" = "black",
      "Minor Injuries Unit" = "black"
    )
  ) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = col_palette) +
  geom_text_repel(
    data = places,
    aes(x = Longitude, y = Latitude, label = name),
    color = "black",
    size = 3.5,
    max.overlaps = 18,
    max.time = 2,
    max.iter = 100000
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(caption = "Source: Public Health Scotland") +
  theme(legend.position = "none")

# Create Map of Just the Locality Areas in order to take its legend (of locality colours) ----

service_map_1 <- ggmap(service_map_background) +
  geom_sf(
    data = shp_hscp,
    mapping = aes(
      fill = hscp_local
    ),
    colour = "black",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  labs(fill = "Locality") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = col_palette) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(caption = "Source: Public Health Scotland")

leg1 <- cowplot::get_legend(service_map_1)

# Create Map of Just the Locations in order to use its legend (of location colours and shapes) ----

all_markers <- dplyr::select(markers_miu, name, latitude, longitude) %>%
  mutate(type = "Minor Injury Unit") %>%
  bind_rows(
    dplyr::select(markers_care_home, name, latitude, longitude) %>%
      mutate(type = "Care Home")
  ) %>%
  bind_rows(
    dplyr::select(markers_emergency_dep, name, latitude, longitude) %>%
      mutate(type = "Emergency Department")
  ) %>%
  bind_rows(
    dplyr::select(markers_gp, name = gp_practice_name, latitude, longitude) %>%
      mutate(type = "GP Practice")
  )

service_map_2 <- ggmap(service_map_background) +
  geom_point(
    data = all_markers,
    aes(x = longitude, y = latitude, colour = type, fill = type, shape = type)
  ) +
  scale_color_manual(
    values = c(
      "GP Practice" = "black",
      "Care Home" = "black",
      "Emergency Department" = "black",
      "Minor Injury Unit" = "black"
    )
  ) +
  scale_fill_manual(
    values = c(
      "GP Practice" = "red",
      "Care Home" = "yellow",
      "Emergency Department" = "blue",
      "Minor Injury Unit" = "green"
    )
  ) +
  scale_shape_manual(
    values = c(
      "GP Practice" = 21,
      "Care Home" = 22,
      "Emergency Department" = 23,
      "Minor Injury Unit" = 24
    )
  ) +
  theme(legend.title = element_blank()) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  ) +
  labs(caption = "Source: Public Health Scotland")


leg2 <- cowplot::get_legend(service_map_2)

# Create new legend which combines other legends ----

blank_leg <- patchwork::plot_spacer() + theme_void()

leg12 <- cowplot::plot_grid(
  blank_leg,
  leg1,
  blank_leg,
  leg2,
  blank_leg,
  ncol = 1
)

# Combine plot of locations and localities with corrected legends

service_map <- cowplot::plot_grid(
  service_map,
  leg12,
  nrow = 1,
  align = "h",
  axis = "t",
  rel_widths = c(1.7, 1.0)
)

# preview final service map
# plot(service_map)

# 4 Cleanup ----
# remove unnecessary objects
rm(
  blank_leg,
  Clacks_Royal,
  data,
  hosp_postcodes,
  hosp_types,
  leg1,
  leg2,
  leg12,
  markers_care_home,
  markers_emergency_dep,
  markers_gp,
  markers_miu,
  other_care_type,
  postcode_lkp,
  service_map_1,
  service_map_2,
  service_map_background,
  shp,
  shp_hscp,
  zones_coord
)

# Housekeeping ----
# These objects are left over after the script is run
# but don't appear to be used in any 'downstream' process:
# Main markdown, Summary Table, Excel data tables, SDC output.
# TODO: Investigate if these can be removed earlier or not created at all.
rm(
  all_markers,
  api_key,
  col_palette,
  ext_year,
  hscp_loc,
  locality_map_id,
  lookup2,
  max_lat,
  max_long,
  min_lat,
  min_long,
  places
)
gc()
