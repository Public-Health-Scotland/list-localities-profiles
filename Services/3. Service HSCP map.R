############################################################################################# .
#                                                                                           #
#                         LOCALITY PROFILES SERVICES MAP & TABLE CODE                       #
#                                                                                           #
############################################################################################# .

## Code for creating the HSCP services map for the locality profiles
# Sources in script "2. Services data manipulation and table.R"

## Written by C.Puech
## Created on 24/02/2020
## Latest update August 2022 - rewrote parts of code for smoother process

###### 1. Set up ######

## Load packages
library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(htmlwidgets)
library(knitr)
library(gridExtra)
library(grid)
library(data.table)
library(sf)
library(ggrepel)
library(ggmap)

## Select HCSP (for testing only)
#HSCP <- "Renfrewshire"

## Set file path

#lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Source in functions code (for testing only)
#source("Master RMarkdown Document & Render Code/Global Script.R")

## Select a locality based on the HSCP (for source code "2. Services Outputs" to run - it does not matter which one is chosen)
#LOCALITY <- as.character(filter(read_in_localities(), hscp2019name == HSCP)[1, 1])

# Source the data manipulation script for services
#source("Services/2. Services data manipulation & table.R")


###### 2. Read in locality shape files ######

shp <- sf::read_sf("/conf/linkage/output/lookups/Unicode/Geography/Shapefiles/HSCP Locality (Datazone2011 Base)/HSCP_Locality.shp")
shp <- sf::st_transform(shp, 4326) %>%
  select(hscp_local, HSCP_name, Shape_Leng, Shape_Area, geometry)

shp <- shp |>
  dplyr::mutate(hscp_locality = gsub("&", "and", hscp_local)) |>
  merge(lookup2, by = "hscp_locality")

shp_hscp <- shp |>
  filter(hscp2019name == HSCP)

###### 3. Formatting Code ######

# Create colour palettes for different numbers of localities

if (n_loc < 5) {
  col_palette <- c("#3F3685", "#9B4393", "#0078D4", "#83BB26")
} else if (n_loc %in% c(5, 6)) {
  col_palette <- c("#3F3685", "#9B4393", "#0078D4", "#83BB26", "#948DA3", "#1E7F84")
} else if (n_loc == 7) {
  col_palette <- c("#3F3685", "#9B4393", "#0078D4", "#83BB26", "#948DA3", "#1E7F84", "#6B5C85")
} else if (n_loc == 8) {
  col_palette <- c("#3F3685", "#9B4393", "#0078D4", "#83BB26", "#948DA3", "#1E7F84", "#6B5C85", "#C73918")
} else if (n_loc == 9) {
  col_palette <- c("#3F3685", "#9B4393", "#0078D4", "#83BB26", "#948DA3", "#1E7F84", "#6B5C85", "#C73918", "orchid3")
}

# Get latitude and longitude co-ordinates for each data locality, find min and max.
zones_coord <-
  shp_hscp %>%
  sf::st_coordinates() %>%
  as_tibble() %>%
  select("long" = X, "lat" = Y) %>%
  summarise(min_long = min(long),
            max_long = max(long),
            min_lat = min(lat),
            max_lat = max(lat))

# Get min and max longitude for locality, add a 0.01 extra to add a border to map.
min_long <- zones_coord$min_long -0.01
max_long <- zones_coord$max_long +0.01
min_lat <- zones_coord$min_lat -0.01
max_lat <- zones_coord$max_lat +0.01

#get data zones in HSCP
hscp_loc <- read.csv("/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/HSCP Localities_DZ11_Lookup_20240513.csv") %>%
  select(datazone2011, hscp2019name) %>%
  filter(hscp2019name == HSCP)

# get place names of cities, towns and villages within locality
places <- read_csv(paste0("/conf/linkage/output/lookups/Unicode/Geography/",
                          "Shapefiles/Scottish Places/Places to Data Zone Lookup.csv")) %>%
  rename(datazone2011 = DataZone) %>%
  filter(datazone2011 %in% hscp_loc$datazone2011) %>%
  #extra filter to remove place names with coordinates outwith locality
  filter(Longitude >= min_long & Longitude <= max_long &
           Latitude >= min_lat & Latitude <= max_lat) %>%
  group_by(name) %>%
  dplyr::summarise(Longitude = first(Longitude),
                   Latitude = first(Latitude),
                   type = first(type)) %>%
  st_as_sf(coords = c("Longitude","Latitude"), remove = FALSE, crs = 4326) %>%
  filter(!grepl('_', name)) %>% #filter incorrect name types and remove smaller places
  filter(type != "hamlet" & type != "village")

#3. Upload map for locality
locality_map_id <- read_csv("/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/Services/locality_map_id.csv")
MAP_ID <- locality_map_id$id
# upload map background from stadia maps, enter registration key, filter for max and min long/lat
ggmap::register_stadiamaps(MAP_ID)
service_map_background <- get_stadiamap(bbox = c(min_long, min_lat,
                                       max_long, max_lat),
                              maptype="stamen_terrain_background")

#preview map
#ggmap(service_map_background)

# check if services markers exist for locality
gp <- nrow(markers_gp)
ch <- nrow(markers_care_home)
ed <- nrow(markers_emergency_dep)
miu <- nrow(markers_miu)

# add locality polygons and service markers to map where services are located
service2 <- ggmap(service_map_background) +
  geom_sf(data = shp_hscp, mapping = aes(fill = hscp_local), colour = "black", alpha = 0.5, inherit.aes = FALSE) +
  labs(fill = 'Locality')

if (gp > 0) {service2 <- service2 + geom_point(data = markers_gp, aes(x = longitude, y = latitude, colour = "GP Practice"), size = 2,shape = 21, stroke = 0.5,
                                               fill = "red")}
if (ch > 0) {service2 <- service2 + geom_point(data = markers_care_home, aes(x = longitude, y = latitude, colour = "Care Home"), size = 2, shape = 22, stroke = 0.5,
                                               fill = "yellow")}
if (ed > 0) {service2 <- service2 + geom_point(data = markers_emergency_dep, aes(x = longitude, y = latitude, colour = "Emergency Department"), size = 2, shape = 23, stroke = 0.5,
                                               fill = "blue")}
if (miu > 0) {service2 <- service2 + geom_point(data = markers_miu, aes(x = longitude, y = latitude, colour = "Minor Injury Unit"), size = 2, shape = 24, stroke = 0.5,
                                                fill = "green")}

# preview HSCP map with service markers added and localities outlined
#plot(service2)

# create final service map
service_map <- service2 +
  scale_color_manual(values = c("GP Practice" = "black", 
                                "Care Home" = "black", 
                                "Emergency Department" = "black",
                                "Minor Injury Unit" = "black")) +
  theme(legend.title=element_blank()) +
  scale_fill_manual(values = col_palette) +
  geom_text_repel(data = places,
                  aes(x = Longitude, y = Latitude, label = name),
                  color = "black", size = 3.5,
                  max.overlaps = getOption("ggrepel.max.overlaps", default = 18)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank()) +
  labs(caption = "Source: Public Health Scotland")

# preview final service map
#plot(service_map)

# remove unnecessary objects
rm(
  data, hosp_postcodes, hosp_types, markers_care_home, markers_emergency_dep, markers_miu, markers_gp,
  postcode_lkp, shp, shp_hscp, Clacks_Royal, other_care_type
)


# detach(package:tidyverse, unload=TRUE)
# detach(package:janitor, unload=TRUE)
# detach(package:mapview, unload=TRUE)
# detach(package:data.table, unload=TRUE)
