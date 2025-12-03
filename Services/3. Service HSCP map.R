# LOCALITY PROFILES SERVICES MAP & TABLE CODE
# Code for creating the HSCP services map for the locality profiles

# 0. Testing Set up ----

## Select HCSP (for testing only)
# HSCP <- "Aberdeenshire"

## Set file path
# lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

## Source in functions code (for testing only)
#source("Master RMarkdown Document & Render Code/Global Script.R")

## Select a locality based on the HSCP (for source code "2. Services Outputs" to run - it does not matter which one is chosen)
# LOCALITY <- read_in_localities() |>
#   filter(hscp2019name == HSCP) |>
#   slice(1) |>
#   pull(hscp_locality)

## Source the data manipulation script for services
# source("Services/2. Services data manipulation & table.R")

# 1. Set up ----

## Load packages
library(sf)
library(ggrepel)
library(ggmap)

lookups_dir <- path("/conf/linkage/output/lookups/Unicode")
shapefiles_dir <- path(lookups_dir, "Geography", "Shapefiles")

# 2. Read in locality shape files ----

shp_hscp <- read_sf(path(
  shapefiles_dir,
  "HSCP Locality (Datazone2011 Base)",
  "HSCP_Locality.shp"
)) |>
  st_transform(4326) |>
  select(hscp_local, HSCP_name, geometry) |>
  filter(HSCP_name == HSCP) |>
  mutate(
    hscp_locality = str_wrap(gsub("&", "and", hscp_local, fixed = TRUE), 24),
    hscp_local = str_wrap(hscp_local, 24),
    border_thickness = if_else(hscp_locality == LOCALITY, 0.8, 0.2)
  )

# 3. Map Code ----
# 3.1 Palettes ----

# Create colour palettes for different numbers of localities
# phs_colours_palette <- c(
#   phs_colours(c(
#     "phs-purple",
#     "phs-magenta",
#     "phs-blue",
#     "phs-green",
#     "phs-graphite",
#     "phs-teal",
#     "phs-liberty",
#     "phs-rust"
#   )),
#   "orchid3"
# )

phs_accessible_colours <- c(
  "#12436D",
  "#28A197",
  "#801650",
  "#F46A25",
  "#3F085C",
  "#3E8ECC",
  "#3D3D3D",
  "#A285D1"
)

colours_needed <- case_match(
  n_loc,
  1:4 ~ 4,
  5:6 ~ 6,
  7 ~ 7,
  8 ~ 8
)

col_palette <- phs_accessible_colours[1:colours_needed]

rm(colours_needed, n_loc, phs_accessible_colours)

# 3.2 Locality shapes ----
# Get latitude and longitude coordinates for each data locality, find min and max.
zones_coord <- shp_hscp |>
  st_coordinates() |>
  as_tibble()

# Get min and max longitude for locality, add a 0.01 extra to add a border to map.
min_long <- min(zones_coord$X) - 0.01
max_long <- max(zones_coord$X) + 0.01
min_lat <- min(zones_coord$Y) - 0.01
max_lat <- max(zones_coord$Y) + 0.01

rm(zones_coord)

# get data zones in HSCP
hscp_datazones <- read_csv(
  path(
    lookups_dir,
    "Geography",
    "HSCP Locality",
    "HSCP Localities_DZ11_Lookup_20240513.csv"
  )
) |>
  filter(hscp2019name == HSCP) |>
  pull(datazone2011)

# get place names of cities, towns and villages within locality
places <- read_csv(path(
  shapefiles_dir,
  "Scottish Places",
  "Places to Data Zone Lookup.csv"
)) |>
  rename(datazone2011 = DataZone) |>
  filter(datazone2011 %in% hscp_datazones) |>
  # extra filter to remove place names with coordinates outwith locality
  filter(
    between(Longitude, min_long, max_long),
    between(Latitude, min_lat, max_lat)
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
# Download map background from stadia maps, using API key
register_stadiamaps(
  key = read_lines(path(lp_path, "Services", "stadia_maps_api_key"))
)
# filter for max and min long/lat
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
      fill = hscp_local,
      linewidth = border_thickness
    ),
    colour = "black",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  scale_linewidth(range = c(0.2, 1), guide = "none") +
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
      fill = hscp_local,
      linewidth = border_thickness
    ),
    colour = "black",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  scale_linewidth(range = c(0.2, 1), guide = "none") +
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

all_markers <- bind_rows(
  "Minor Injury Unit" = markers_miu,
  "Care Home" = markers_care_home,
  "Emergency Department" = markers_emergency_dep,
  "GP Practice" = rename(markers_gp, name = gp_practice_name),
  .id = "type"
) |>
  select(name, latitude, longitude, type)

rm(markers_care_home, markers_emergency_dep, markers_gp, markers_miu)

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

# Housekeeping ----
rm(
  all_markers,
  blank_leg,
  col_palette,
  ext_year,
  hscp_loc,
  leg1,
  leg12,
  leg2,
  lookup2,
  lookups_dir,
  palette,
  places,
  service_map_1,
  service_map_2,
  service_map_background,
  shapefiles_dir,
  shp_hscp
)

gc()
