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
library(leaflet)
library(dplyr)
library(htmlwidgets)
#library(mapview)
library(knitr)
library(gridExtra)
library(grid)
library(data.table)

## Select HCSP (for testing only)
# HSCP <- "Aberdeenshire"

## Set file path
lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Source in functions code (for testing only)
# source("Master RMarkdown Document & Render Code/Global Script.R")

## Select a locality based on the HSCP (for source code "2. Services Outputs" to run - it does not matter which one is chosen)
LOCALITY <- as.character(filter(read_in_localities(), hscp2019name == HSCP)[1, 1])

# Source the data manipulation script for services
source("Services/2. Services data manipulation & table.R")


###### 5. Read in locality shape files ######

shp <- sf::read_sf("/conf/linkage/output/lookups/Unicode/Geography/Shapefiles/HSCP Locality (Datazone2011 Base)/HSCP_Locality.shp")
shp <- sf::st_transform(shp,4326)

shp <- shp |> 
  dplyr::mutate(hscp_locality = gsub("&", "and", HSCP_Local)) |> 
  merge(lookup, by = "hscp_locality")

shp_hscp <- shp |> 
  filter(hscp2019name == HSCP)

###### 6. Mapping Code ######

# Create colour palettes for different numbers of localities

if (n_loc < 5) {
  col_palette <- c("dodgerblue2", "deeppink2", "purple", "navy")
} else if (n_loc %in% c(5, 6)) {
  col_palette <- c("dodgerblue2", "deeppink2", "purple", "navy", "forestgreen", "darksalmon")
} else if (n_loc == 7) {
  col_palette <- c("deeppink2", "navy", "forestgreen", "darksalmon", "dodgerblue2", "purple", "cadetblue")
} else if (n_loc == 8) {
  col_palette <- c("forestgreen", "dodgerblue2", "deeppink2", "cadetblue", "darksalmon", "purple", "olivedrab3", "navy")
} else if (n_loc == 9) {
  col_palette <- c("dodgerblue2", "deeppink2", "cadetblue", "darksalmon", "purple", "forestgreen", "olivedrab3", "navy", "orchid3")
}

loc.cols <- colorFactor(col_palette, domain = shp_hscp$hscp_locality)

## Create function for adding circle markers
addLegendCustom <- function(map, colors, labels, sizes, opacity = 1) {
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0(
    "<div style='display: inline-block;height: ",
    sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>",
    labels, "</div>"
  )
  
  return(addLegend(map,
                   colors = colorAdditions,
                   labels = labelAdditions, opacity = opacity
  ))
}


## Create Map
service_map <-
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  # Locality shapefiles
  addPolygons(
    data = shp_hscp,
    fillColor = ~ loc.cols(hscp_locality),
    fillOpacity = 0.2,
    color = "#2e2e30",
    stroke = T,
    weight = 2,
    label = ~hscp_locality,
    group = "Locality"
  ) %>%
  addLegend("bottomright", pal = loc.cols, values = shp_hscp$hscp_locality, title = "Locality", opacity = 0.7, group = "Locality") %>%
  # Markers
  addCircleMarkers(
    lng = markers_care_home$longitude, lat = markers_care_home$latitude, group = "Care Home",
    color = "black", radius = 3.5, weight = 1, opacity = 1, fillOpacity = 1, fillColor = "#c9c9c9"
  ) %>%
  addCircleMarkers(
    lng = markers_gp$longitude, lat = markers_gp$latitude, group = "GP practices",
    color = "black", radius = 3.5, weight = 1, opacity = 1, fillOpacity = 1, fillColor = "red"
  ) %>%
  addCircleMarkers(
    lng = markers_emergency_dep$longitude, lat = markers_emergency_dep$latitude, group = "Emergency departments",
    color = "black", radius = 3.5, weight = 1, opacity = 1, fillOpacity = 1, fillColor = "gold"
  ) %>%
  addCircleMarkers(
    lng = markers_miu$longitude, lat = markers_miu$latitude, group = "MIU",
    color = "black", radius = 3.5, weight = 1, opacity = 1, fillOpacity = 1, fillColor = "chartreuse"
  ) %>%
  # Custom markers legend
  addLegendCustom(colors = "red", labels = "GP practices", sizes = 10) %>%
  addLegendCustom(colors = "#c9c9c9", labels = "Care Home", sizes = 10) %>%
  addLegendCustom(colors = "gold", labels = "ED", sizes = 10) %>%
  addLegendCustom(colors = "chartreuse", labels = "MIU", sizes = 10)


## Screenshot the map
# It gets saved in the Services folder and inserted in the R Markdown document
# Every time the R Markdown is run, the previous map is overwritten.
#mapshot(service_map, file = paste0(lp_path, "/Services/map.png"))

htmlwidgets::saveWidget(service_map, "./Services/service_map.html")


# remove unnecessary objects
rm(
  data, hosp_postcodes, hosp_types, markers_care_home, markers_emergency_dep, markers_miu, markers_gp,
  postcode_lkp, shp, shp_hscp, Clacks_Royal, other_care_type
)


# detach(package:tidyverse, unload=TRUE)
# detach(package:janitor, unload=TRUE)
# detach(package:mapview, unload=TRUE)
# detach(package:data.table, unload=TRUE)
