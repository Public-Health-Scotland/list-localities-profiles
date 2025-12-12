######################### GLOBAL LOCALITY PROFILES CODE ########################

# Contains various settings and functions to be used in other locality profile scripts

# How to use this script:
# source("Master RMarkdown Document & Render Code/Global Script.R)

## Packages for functions ----
# (** note - should this contain all packages necessary for locality profiles?
# and automatically installing missing packages?)
library(data.table)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(purrr)
library(janitor)
library(glue)
library(fs)
library(arrow)
library(phsstyles)

# Prefer dplyr functions if there's a conflict
conflicted::conflict_prefer_all("dplyr", quiet = TRUE)

#### Colours & Formatting #### ----

## PHS colour palette from phsstyles
palette <- phsstyles::phs_colours(c(
  "phs-purple",
  "phs-magenta",
  "phs-blue",
  "phs-green",
  "phs-graphite",
  "phs-teal",
  "phs-liberty",
  "phs-rust"
))


## Function for formatting a value for R Markdown text
# First determines how many dps the value should have
# Then adds a comma for numbers over 1000 (becomes "1,000")

format_number_for_text <- function(x) {
  x <- ifelse(
    abs(x) < 1,
    round_half_up(x, 2), # if x < 1 then show 2dp
    ifelse(
      abs(x) < 100,
      round_half_up(x, 1), # if 1 =< x < 100 then 1dp
      round_half_up(x)
    )
  ) # if 10 =< x then no decimal places

  format(x, big.mark = ",")
}

# This will return the correct article depending on the (max 2-digit) number supplied
# e.g.
# 81.2 -> an
# 18 -> an
# 7.2 -> an
# To be used for "a xx increase" which could be "an xx increase"
get_article <- function(number) {
  # Cast as a character, so we are sure of the type
  number_chr <- as.character(number)

  if (identical(number_chr, character(0))) {
    # If the number wasn't calculated we still need to deal with it.
    return("-")
  }

  if (startsWith(number_chr, "8") || startsWith(number_chr, "18")) {
    return("an")
  } else {
    return("a")
  }
}

## Theme for charts ----
# This theme is similar to theme_phs() from phsstyles but adapted to locality profiles
# Differences include smaller text (to ensure names of areas always fit regardless of length)
# Code taken from phsstyles Github page
# https://github.com/Public-Health-Scotland/phsstyles/blob/master/R/theme_phs.R

theme_profiles <- function() {
  fontStyle <- "sans"
  gridLineColor <- grDevices::rgb(190 / 255, 190 / 255, 190 / 255)
  fontSize <- 11

  ggplot2::theme(
    # Text format:
    # This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(
      family = fontStyle,
      size = fontSize,
      face = "bold"
    ),

    # Legend format
    # This sets the position and alignment of the legend, removes a title and
    # background for it and sets the requirements for any text within the legend.
    # The legend may often need some more manual tweaking when it comes to its
    # exact position based on the plot coordinates.
    legend.position = "bottom",
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      family = fontStyle,
      size = fontSize,
      hjust = 0 # Replaces legend.text.align = 0
    ),

    # Axis format
    # This sets the text font, size and colour for the axis test, as well as
    # setting the margins and removes lines and ticks.
    # In some cases, axis lines and axis ticks are things we would want to have
    # in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_text(
      family = fontStyle,
      size = fontSize
    ),
    axis.text = ggplot2::element_text(
      family = fontStyle,
      size = fontSize
    ),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),

    # Grid Lines
    # This removes all minor gridlines and adds major vertical gridlines.
    # In many cases you will want to change this to remove vertical gridlines
    # and add horizontal gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(color = gridLineColor),
    panel.grid.major.y = ggplot2::element_blank(),

    # Blank Background
    # This sets the panel background as blank, removing the standard grey ggplot
    # background colour from the plot
    panel.background = ggplot2::element_blank()
  )
}

#### Lookup #### ----

## Import the latest locality lookup from cl-out ----
# Argument dz_level: Allows you to choose whether lookup contains all datazones in localities
# default is F - datazones are not imported, there is one line per locality (125 rows)
# if changed to dz_level = TRUE, this shows all the datazones in each locality (6976 rows)

read_in_localities <- function(dz_level = FALSE) {
  data <- fs::dir_ls(
    path = "/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality",
    regexp = "HSCP Localities_DZ11_Lookup_.+?\\.rds$"
  ) |>
    # Read in the most up to date lookup version
    max() |>
    readr::read_rds() |>
    dplyr::select(
      datazone2011,
      hscp_locality,
      hscp2019name,
      hscp2019,
      hb2019name,
      hb2019
    ) |>
    dplyr::mutate(hscp_locality = sub("&", "and", hscp_locality, fixed = TRUE))

  if (!dz_level) {
    data <- dplyr::distinct(
      data,
      hscp_locality,
      hscp2019name,
      hscp2019,
      hb2019name,
      hb2019
    )
  }

  return(data)
}

count_localities <- function(locality_lookup, hscp_name) {
  return(sum(locality_lookup[["hscp2019name"]] == hscp_name))
}

## Function to read in latest SPD file ----

# No arguments needed, just use read_in_latest_postcodes()
# The function pulls the latest "Scottish_Postcode_Directory_year_version.rds"

read_in_postcodes <- function() {
  data <- fs::dir_ls(
    path = "/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory",
    regexp = "\\.parquet$"
  ) |>
    # Read in the most up to date lookup version
    max() |>
    arrow::read_parquet(
      col_select = -c(hscp2019, hscp2019name, hb2019, hb2019name)
    )

  data <- dplyr::left_join(
    data,
    read_in_localities(dz_level = TRUE),
    by = dplyr::join_by(datazone2011),
    relationship = "many-to-one"
  )

  return(data)
}


## Function to read in the latest population file by DZ ----

# No arguments needed, just use read_in_latest_dz_pops()
# Function pulls the latest DZ populations DataZone2011_pop_est_2011_Xyear.rds
# Then joins this with the localities lookup to match hscp_locality

read_in_dz_pops <- function() {
  fs::dir_ls(
    glue(
      "/conf/linkage/output/lookups/Unicode/",
      "Populations/Estimates/"
    ),
    regexp = glue("DataZone2011_pop_est_2011_.+?\\.rds$")
  ) |>
    # Read in the most up-to-date lookup version
    max() |>
    read_rds() |>
    clean_names() |>
    select(
      -c(
        intzone2011,
        intzone2011name,
        ca2019,
        ca2019name,
        ca2018,
        ca2011,
        hscp2019,
        hscp2019name,
        hscp2018,
        hscp2016,
        hb2019,
        hb2019name,
        hb2018,
        hb2014
      )
    ) |>
    left_join(
      read_in_localities(dz_level = TRUE),
      by = join_by(datazone2011)
    ) |>
    mutate(year = as.integer(year))
}

read_in_dz_pops_proxy_year <- function() {
  read_in_dz_pops() |>
    filter(year == 2022L) |>
    select(-year) |>
    mutate(year = 2023L)
}

## Function to read in latest population projections ----

# No arguments needed, just use read_in_pop_proj()
# Function pulls the latest projections HSCP2019_pop_proj....rds
# Then joins this with the hscp lookup to match hscp names

read_in_pop_proj <- function() {
  proj <- fs::dir_ls(
    glue(
      "/conf/linkage/output/lookups/Unicode/",
      "Populations/Projections/"
    ),
    regexp = glue("HSCP2019_pop_proj_20.+?_.+?\\.rds$")
  ) |>
    # Read in the most up-to-date lookup version
    max() |>
    read_rds() |>
    clean_names() |>
    select(year, hscp2019, age, sex, sex_name, pop) |>
    mutate(year = as.integer(year))

  # join with lookup so all hscp2019 names are the same
  hscp_lkp <- read_in_localities() |>
    select(hscp2019, hscp2019name) |>
    distinct()

  left_join(proj, hscp_lkp, by = join_by(hscp2019))
}

#### Functions for ScotPHO data ####

## ScotPHO data cleaning function ----

# Renames localities so they can match onto lookup
# Removes unwanted areas like council area and IZ

clean_scotpho_dat <- function(data) {
  data |>
    filter(area_type != "Council area" & area_type != "Intermediate zone") |>
    mutate(area_name = gsub("&", "and", area_name, fixed = TRUE)) |>
    mutate(
      area_name = if_else(
        area_name == "Renfrewshire West",
        "West Renfrewshire",
        area_name
      )
    ) |>
    mutate(
      area_type = if_else(area_type == "HSC partnership", "HSCP", area_type),
      area_type = if_else(area_type == "HSC locality", "Locality", area_type)
    )
}

## Time trend function for ScotPHO data ----

# Creates a time trend for chosen locality, HSCP, HB and Scotland with confidence interval ribbons
# Data must first be cleaned using clean_scotpho_dat function
# Uses variable "period_short" which must be created - this is a shortened version of the "period" column in ScotPHO data
# ex: mutate(period_short = gsub("to", "-", substr(period, 1, 12)))
# Uses objects "LOCALITY", "HSCP" and "HB" to filter - these must be specified earlier in script

# Arguments:
# data: data to use for chart
# chart_title, xaxis_title, yaxis_title : titles for chart, x axis and y axis
# string_wrap: number of characters after which to "wrap" the x axis labels
# (ScotPHO data uses year aggregates which don't always fit on axis unless wrapped)
# rotate_xaxis: default F, if labels still don't fit even with wrapping (prev argument), labels can be rotated

scotpho_time_trend <- function(
  data,
  chart_title,
  xaxis_title,
  yaxis_title,
  string_wrap,
  rotate_xaxis = FALSE,
  trend_years = 10
) {
  # rotate axis criteria if T/F
  if (rotate_xaxis) {
    rotation <- element_text(angle = 45, hjust = 1)
  } else {
    rotation <- element_text(angle = 0)
  }

  # filter and reorder data
  data |>
    filter(
      (area_name == LOCALITY & area_type == "Locality") |
        (area_name == HSCP & area_type == "HSCP") |
        area_name == HB |
        area_name == "Scotland"
    ) |>
    filter(year >= max(year) - trend_years) |>
    mutate(
      area_type = factor(
        area_type,
        levels = c("Locality", "HSCP", "Health board", "Scotland")
      ),
      area_name = fct_reorder(
        as.factor(str_wrap(area_name, 23)),
        as.numeric(area_type)
      )
    ) |>
    # plot
    ggplot(aes(
      x = str_wrap(period_short, width = string_wrap),
      y = measure,
      group = area_name,
      fill = area_name,
      linetype = area_type
    )) +
    geom_line(aes(colour = area_name), linewidth = 1) +
    geom_point(aes(colour = area_name), size = 2) +
    geom_ribbon(
      aes(
        x = str_wrap(period_short, width = string_wrap),
        ymin = lower_confidence_interval,
        ymax = upper_confidence_interval
      ),
      alpha = 0.1
    ) +
    scale_fill_manual(values = palette) +
    scale_colour_manual(values = palette) +
    scale_y_continuous(labels = scales::comma) +
    theme_profiles() +
    expand_limits(y = 0) +
    labs(
      title = chart_title,
      x = xaxis_title,
      y = yaxis_title,
      caption = "Source: ScotPHO"
    ) +
    theme(axis.text.x = rotation) +
    guides(
      linetype = "none",
      shape = "none",
      fill = "none",
      colour = guide_legend(nrow = 1, byrow = TRUE)
    )
}


scotpho_time_trend_HSCP <- function(
  data,
  chart_title,
  xaxis_title,
  yaxis_title,
  string_wrap,
  rotate_xaxis = FALSE
) {
  # rotate axis criteria if T/F
  if (rotate_xaxis) {
    rotation <- element_text(angle = 45, hjust = 1)
  } else {
    rotation <- element_text(angle = 0)
  }

  # filter and reorder data
  data |>
    filter(
      (area_name == HSCP & area_type == "HSCP") |
        area_name == HB |
        area_name == "Scotland"
    ) |>
    filter(year >= max(year) - 10) |>
    mutate(
      area_type = factor(
        area_type,
        levels = c("HSCP", "Health board", "Scotland")
      ),
      area_name = fct_reorder(
        as.factor(str_wrap(area_name, 23)),
        as.numeric(area_type)
      )
    ) |>
    # plot
    ggplot(aes(
      x = str_wrap(period_short, width = string_wrap),
      y = measure,
      group = area_name,
      fill = area_name,
      linetype = area_type
    )) +
    geom_line(aes(colour = area_name), linewidth = 1) +
    geom_point(aes(colour = area_name), size = 2) +
    geom_ribbon(
      aes(
        x = str_wrap(period_short, width = string_wrap),
        ymin = lower_confidence_interval,
        ymax = upper_confidence_interval
      ),
      alpha = 0.1
    ) +
    scale_fill_manual(values = palette) +
    scale_colour_manual(values = palette) +
    theme_profiles() +
    expand_limits(y = 0) +
    labs(
      title = chart_title,
      x = xaxis_title,
      y = yaxis_title,
      caption = "Source: ScotPHO"
    ) +
    theme(axis.text.x = rotation) +
    guides(
      linetype = "none",
      shape = "none",
      fill = "none",
      colour = guide_legend(nrow = 1, byrow = TRUE)
    )
}

## Bar chart function for ScotPHO data ----

# Creates a horizontal bar chart comparing the last time period of data across
# all localities in a partnership, the HSCP, HB, and Scotland
# Data must first be cleaned using clean_scotpho_dat function
# Uses object "LOCALITY" and vector "other_locs" (other localities in HSCP) to filter
# these must be specified earlier in script

# Arguments:
# data: data to use for chart
# chart_title, xaxis_title : titles for chart and x axis

scotpho_bar_chart <- function(data, chart_title, xaxis_title) {
  data_for_plot <- data |>
    filter(year == max(year)) |>
    filter(
      (area_name %in%
        c(LOCALITY, other_locs$hscp_locality) &
        area_type == "Locality") |
        (area_name == HSCP & area_type == "HSCP") |
        area_name == HB |
        area_name == "Scotland"
    ) |>
    mutate(
      text_highlight = area_name == LOCALITY,
      area_type = factor(
        area_type,
        levels = c("Locality", "HSCP", "Health board", "Scotland")
      ),
      area_name = fct_reorder(as.factor(str_wrap(area_name, 28)), measure)
    ) |>
    arrange(area_name)

  ggplot(data_for_plot) +
    aes(y = area_name, fill = area_type, weight = measure) +
    geom_bar(colour = "white") +
    scale_fill_manual(values = palette) +
    theme_profiles() +
    theme(
      axis.text.y = element_text(
        colour = if_else(data_for_plot$text_highlight, "red", "black"),
        face = if_else(data_for_plot$text_highlight, "bold", "plain")
      )
    ) +
    labs(
      title = chart_title,
      x = xaxis_title,
      y = " ",
      fill = " ",
      caption = "Source: ScotPHO"
    ) +
    geom_errorbar(
      aes(xmin = lower_confidence_interval, xmax = upper_confidence_interval),
      width = 0.2,
      position = position_dodge(width = 1)
    )
}


scotpho_bar_chart_HSCP <- function(data, chart_title, xaxis_title) {
  data_for_plot <- data |>
    filter(year == max(year)) |>
    filter(
      (area_name %in% c(other_locs$hscp_locality) & area_type == "Locality") |
        (area_name == HSCP & area_type == "HSCP") |
        area_name == HB |
        area_name == "Scotland"
    ) |>
    mutate(
      text_highlight = area_name == HSCP,
      area_type = factor(
        area_type,
        levels = c("Locality", "HSCP", "Health board", "Scotland")
      ),
      area_name = fct_reorder(as.factor(str_wrap(area_name, 28)), measure)
    ) |>
    arrange(area_name)

  ggplot(data_for_plot) +
    aes(y = area_name, fill = area_type, weight = measure) +
    geom_bar(colour = "white") +
    scale_fill_manual(values = palette) +
    theme_profiles() +
    theme(
      axis.text.y = element_text(
        colour = if_else(data_for_plot$text_highlight, "red", "black")
      )
    ) +
    labs(
      title = chart_title,
      x = xaxis_title,
      y = " ",
      fill = " ",
      caption = "Source: ScotPHO"
    ) +
    geom_errorbar(
      aes(xmin = lower_confidence_interval, xmax = upper_confidence_interval),
      width = 0.2,
      position = position_dodge(width = 1)
    )
}

## Checking for missing data
check_missing_data_scotpho <- function(data) {
  data |>
    filter(area_type == "Locality") |>
    filter(year == max(year)) |>
    right_join(read_in_localities(), by = c("area_name" = "hscp_locality")) |>
    filter(is.na(indicator)) |>
    select(area_name, hscp2019name)
}


### Unscheduled care functions - can be used across other topics ### ----

# Reformat age groups to specific strings shown i.e. add spaces
age_group_1 <- function(age_group) {
  dplyr::case_match(
    age_group,
    c("<18", "0-17") ~ "0 - 17",
    c("18-24", "25-29", "30-34", "35-39", "40-44") ~ "18 - 44",
    c("45-49", "50-54", "55-59", "60-64") ~ "45 - 64",
    c("65-69", "70-74") ~ "65 - 74",
    c("75-79", "80-84", "85-89", "90-94", "95-99", "Over 100", "100+") ~ "75+",
    .default = "NA"
  )
}

# Bin ages in the required size for unscheduled care indicators
age_group_2 <- function(age) {
  dplyr::case_when(
    dplyr::between(age, 0, 17) ~ "0 - 17",
    dplyr::between(age, 18, 44) ~ "18 - 44",
    dplyr::between(age, 45, 64) ~ "45 - 64",
    dplyr::between(age, 65, 74) ~ "65 - 74",
    age >= 75 ~ "75+"
  )
}

# reformat partnership names # 1

ptsp <- function(partnership) {
  dplyr::case_match(
    partnership,
    "Borders" ~ "Scottish Borders",
    "Orkney" ~ "Orkney Islands",
    "Shetland" ~ "Shetland Islands",
    "Edinburgh City" ~ "Edinburgh",
    "City of Edinburgh" ~ "Edinburgh",
    "Perth & Kinross" ~ "Perth and Kinross",
    "Clackmannanshire" ~ "Clackmannanshire and Stirling",
    "Stirling" ~ "Clackmannanshire and Stirling",
    "Na h-Eileanan Siar" ~ "Western Isles",
    "Comhairle nan Eilean Siar" ~ "Western Isles",
    .default = partnership
  )
}

hbres <- function(hbres_currentdate) {
  dplyr::case_match(
    hbres_currentdate,
    "S08000015" ~ "NHS Ayrshire & Arran",
    "S08000016" ~ "NHS Borders",
    "S08000017" ~ "NHS Dumfries & Galloway",
    "S08000029" ~ "NHS Fife",
    "S08000019" ~ "NHS Forth Valley",
    "S08000020" ~ "NHS Grampian",
    "S08000031" ~ "NHS Greater Glasgow & Clyde",
    "S08000022" ~ "NHS Highland",
    "S08000032" ~ "NHS Lanarkshire",
    "S08000024" ~ "NHS Lothian",
    "S08000025" ~ "NHS Orkney",
    "S08000026" ~ "NHS Shetland",
    "S08000030" ~ "NHS Tayside",
    "S08000028" ~ "NHS Western Isles",
    .default = "Other"
  )
}

# Define a function to save multiple dataframes to an Excel workbook
save_dataframes_to_excel <- function(dataframes, sheet_names, file_path) {
  # Create a new workbook using openxlsx2
  wb <- openxlsx2::wb_workbook()

  # Loop over each dataframe and corresponding sheet name
  for (i in seq_along(dataframes)) {
    # Define the used columns
    cols <- seq_len(ncol(dataframes[[i]]))

    # Define the header range
    header_range <- openxlsx2::wb_dims(rows = 1, cols = cols)

    wb <- wb |>
      # Add a worksheet
      openxlsx2::wb_add_worksheet(sheet = sheet_names[[i]]) |>
      # Write data
      openxlsx2::wb_add_data(x = dataframes[[i]]) |>
      # Style the header bold
      openxlsx2::wb_add_font(dims = header_range, bold = TRUE) |>
      # Set column widths to auto
      openxlsx2::wb_set_col_widths(cols = cols, widths = "auto")
  }

  # Create the directories if they don't exist
  fs::dir_create(fs::path_dir(file_path), mode = "u=rwx,g=rwx,o=rx")

  # Save the workbook to a file
  openxlsx2::wb_save(wb, file = file_path, overwrite = TRUE)
}
