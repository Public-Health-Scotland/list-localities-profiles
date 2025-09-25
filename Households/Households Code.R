################################################################################ .
#                                                                              #
#                       LOCALITY PROFILES: HOUSEHOLDS                          #
#                                                                              #
################################################################################ .
###
### The purpose of this code is to produce graphs which will be used for locality
### profiles produced in RMarkdown.
###
### When rerunning for a new year of data, manually change dates at lines 39 and 41
###
### First Created: 05/09/2019
### UPDATES
### Oct 2021  Aidan Morrison
### Sep 2022  Diane Wilson  Updated to use functions within Global Script and updated palette
###
###
### Contents:
### Section 1 - Packages, working directory etc
### Section 2 - Households Data
### Section 3 - Council Tax Band Data
### Section 4 - Objects for Summary Table

##################### Section 1 - Packages, working directory etc ########################

# load in required packages
library(readxl)
library(reshape2)

# Update Data Year (this is the maximum year available for both housing data sets from NRS)
max_year_housing <- 2023
# Update Publication Year (the year marked on the Data folder)
ext_year <- 2024

# Set Directory.
# lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Read in Global Script for RMarkdown (For testing only)
# source("Master RMarkdown Document & Render Code/Global Script.R")

# Set locality (for testing only)
# LOCALITY <- "Whalsay and Skerries"
# LOCALITY <- "Ayr North and Former Coalfield Communities"

##################### Section 2 - Households Data #############################

## 2a) Data imports & cleaning ----

# get historic housing data, each year is on a separate sheet so map over it
house_raw_dat <- map(
  2014L:max_year_housing,
  \(year) {
    read_excel(
      path(
        lp_path,
        "Households",
        glue("Data {ext_year}"),
        "household_estimates.xlsx"
      ),
      col_types = c(rep("text", 4L), rep("numeric", 8L), rep("skip", 7L)),
      sheet = as.character(year),
      skip = 3L,
      .name_repair = make_clean_names # janitor::make_clean_names()
    ) |>
      mutate(year = year, .before = everything())
  }
) |>
  list_rbind()

# Global Script Function to read in Localities Lookup
lookup <- read_in_localities(dz_level = TRUE) %>%
  select(datazone2011, hscp_locality) %>%
  filter(hscp_locality == LOCALITY)


# filter housing data for locality of interest
house_dat <- filter(house_raw_dat, data_zone_code %in% lookup[["datazone2011"]])

# aggregate data
house_dat1 <- house_dat %>%
  group_by(year) %>%
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    occupied_dwellings = sum(occupied_dwellings),
    vacant_dwellings = sum(vacant_dwellings),
    second_homes = sum(second_homes),
    tax_exempt = sum(occupied_dwellings_exempt_from_paying_council_tax),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) %>%
  ungroup() %>%
  mutate(across(3:7, list(perc = ~ 100 * .x / total_dwellings)))


## 2b) Text objects ----

# numbers
n_houses <- format_number_for_text(
  filter(house_dat1, year == max(year))$total_dwellings
)
n_occupied <- format_number_for_text(
  filter(house_dat1, year == max(year))$occupied_dwellings
)
n_vacant <- format_number_for_text(
  filter(house_dat1, year == max(year))$vacant_dwellings
)
n_single_discount <- format_number_for_text(
  filter(house_dat1, year == max(year))$tax_discount
)
n_exempt <- format_number_for_text(
  filter(house_dat1, year == max(year))$tax_exempt
)
n_second_homes <- format_number_for_text(
  filter(house_dat1, year == max(year))$second_homes
)

# percentages
perc_occupied <- format_number_for_text(
  filter(house_dat1, year == max(year))$occupied_dwellings_perc
)
perc_vacant <- format_number_for_text(
  filter(house_dat1, year == max(year))$vacant_dwellings_perc
)
perc_single_discount <- format_number_for_text(
  filter(house_dat1, year == max(year))$tax_discount_perc
)
perc_exempt <- format_number_for_text(
  filter(house_dat1, year == max(year))$tax_exempt_perc
)
perc_second_homes <- format_number_for_text(
  filter(house_dat1, year == max(year))$second_homes_perc
)


## 2c) Plots and Tables ----

# Total dwellings over time
houses_ts <- ggplot(house_dat1, aes(x = year, y = total_dwellings, group = 1)) +
  geom_line(linewidth = 1, colour = "#3F3685") +
  theme_profiles() +
  geom_point(color = "#3F3685") +
  geom_text(
    aes(label = format(total_dwellings, big.mark = ",")),
    vjust = 2,
    color = "#4a4a4a",
    size = 3.5
  ) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, 1.1 * max(house_dat1$total_dwellings))
  ) +
  labs(
    x = "Year",
    y = "Number of Dwellings",
    title = paste0(
      "Number of Dwellings by Year in ",
      str_wrap(`LOCALITY`, 40),
      " ",
      max_year_housing
    ),
    caption = "Source: Council Tax billing system (via NRS)"
  ) +
  theme(plot.title = element_text(size = 12))


# Table
house_table <- house_dat1 %>%
  select(
    year,
    total_dwellings,
    occupied_dwellings,
    vacant_dwellings,
    tax_discount,
    tax_exempt,
    second_homes
  ) %>%
  mutate(across(2:7, ~ format(.x, big.mark = ",")))


######################## Section 3 - Council Tax Band Data ############################

## 3a) Data imports & cleaning ----

# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/small-area-statistics-on-households-and-dwellings

house_raw_dat2 <- read_excel(
  paste0(lp_path, "Households/", "Data ", ext_year, "/council_tax.xlsx"),
  sheet = as.character(max_year_housing),
  skip = 4
) %>%
  clean_names()

# Filter and aggregate
house_dat2 <- house_raw_dat2 %>%
  filter(data_zone_code %in% lookup$datazone2011) %>%
  select(5:14) %>%
  summarise(across(everything(), sum))


## 3b) Plots & tables ----

ctb <- house_dat2 %>%
  select(matches("council_tax_band_[a-h]")) %>%
  pivot_longer(cols = everything(), names_to = "variable")

variable <- ctb[["variable"]]

pal_ctb <- phsstyles::phs_colours(c(
  "phs-magenta",
  "phs-magenta-80",
  "phs-magenta-50",
  "phs-magenta-10",
  "phs-purple-30",
  "phs-purple-50",
  "phs-purple-80",
  "phs-purple"
))

ctb_plot <- ctb %>%
  ggplot(aes(
    x = value,
    y = 1,
    fill = factor(variable, levels = rev(variable))
  )) +
  geom_col(position = "fill", colour = "black", size = 0.5, orientation = "y") +
  theme_classic() +
  labs(
    x = "Proportion of Households",
    y = "",
    caption = "Source: Scottish Assessors’ Association (via NRS)"
  ) +
  scale_fill_manual(
    name = "Council Tax Band",
    labels = paste("Band", LETTERS[8:1]),
    values = pal_ctb,
    drop = FALSE,
    guide = guide_legend(reverse = TRUE)
  ) +
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black")
  )

ctb_table <- ctb %>%
  mutate(
    percent = paste0(format_number_for_text(100 * value / sum(value)), "%")
  ) %>%
  select(-value) %>%
  pivot_wider(names_from = variable, values_from = percent) %>%
  mutate(`Tax Band` = "Percent of households") %>%
  select(
    `Tax Band`,
    A = council_tax_band_a,
    B = council_tax_band_b,
    C = council_tax_band_c,
    D = council_tax_band_d,
    E = council_tax_band_e,
    `F` = council_tax_band_f,
    G = council_tax_band_g,
    H = council_tax_band_h
  )


## Objects for locality
perc_houses_AC <- format_number_for_text(
  sum(
    house_dat2$council_tax_band_a,
    house_dat2$council_tax_band_b,
    house_dat2$council_tax_band_c
  ) /
    house_dat2$total_number_of_dwellings *
    100
)

perc_houses_FH <- format_number_for_text(
  sum(
    house_dat2$council_tax_band_f,
    house_dat2$council_tax_band_g,
    house_dat2$council_tax_band_h
  ) /
    house_dat2$total_number_of_dwellings *
    100
)


########################## Section 4 - Objects for Summary Table ########################

## Relevant lookups for creating the table objects

# Global Script Function to read in Localities Lookup
lookup2 <- read_in_localities(dz_level = FALSE)

# Determine HSCP and HB based on Loc
HSCP <- as.character(filter(lookup2, hscp_locality == LOCALITY)$hscp2019name)

# Determine other localities based on LOCALITY object
other_locs <- lookup2 %>%
  select(hscp_locality, hscp2019name) %>%
  filter(hscp2019name == HSCP & hscp_locality != LOCALITY) %>%
  arrange(hscp_locality)

# Find number of locs per partnership
n_loc <- count_localities(lookup2, HSCP)

rm(lookup2)


# 1. Other localities

# Global Script Function to read in Localities Lookup
other_locs_dz <- read_in_localities(dz_level = TRUE) %>%
  arrange() %>%
  select(datazone2011, hscp_locality) %>%
  inner_join(other_locs, by = c("hscp_locality" = "hscp_locality"))

house_dat_otherlocs <- house_raw_dat %>%
  inner_join(other_locs_dz, by = c("data_zone_code" = "datazone2011")) %>%
  filter(year == max(year)) %>%
  group_by(hscp_locality) %>%
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) %>%
  ungroup() %>%
  mutate(
    tax_discount_perc = round_half_up(tax_discount / total_dwellings * 100, 1)
  )

other_locs_n_houses <- house_dat_otherlocs %>%
  mutate(
    tot_dwellings_chr = formatC(total_dwellings, format = "d", big.mark = ",")
  ) %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, tot_dwellings_chr) %>%
  pivot_wider(names_from = hscp_locality, values_from = tot_dwellings_chr)

other_locs_perc_discount <- house_dat_otherlocs %>%
  select(hscp_locality, tax_discount_perc) %>%
  arrange(hscp_locality) %>%
  pivot_wider(names_from = hscp_locality, values_from = tax_discount_perc)


house_dat2_otherlocs <- house_raw_dat2 %>%
  inner_join(other_locs_dz, by = c("data_zone_code" = "datazone2011")) %>%
  group_by(hscp_locality) %>%
  summarise(
    total_number_of_dwellings = sum(total_number_of_dwellings),
    band_a = sum(council_tax_band_a),
    band_b = sum(council_tax_band_b),
    band_c = sum(council_tax_band_c),
    band_f = sum(council_tax_band_f),
    band_g = sum(council_tax_band_g),
    band_h = sum(council_tax_band_h)
  ) %>%
  mutate(
    perc_houses_AC = round_half_up(
      (band_a + band_b + band_c) / total_number_of_dwellings * 100,
      1
    ),
    perc_houses_FH = round_half_up(
      (band_f + band_g + band_h) / total_number_of_dwellings * 100,
      1
    )
  )

other_locs_perc_housesAC <- house_dat2_otherlocs %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, perc_houses_AC) %>%
  pivot_wider(names_from = hscp_locality, values_from = perc_houses_AC)

other_locs_perc_housesFH <- house_dat2_otherlocs %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, perc_houses_FH) %>%
  pivot_wider(names_from = hscp_locality, values_from = perc_houses_FH)

rm(house_dat2_otherlocs, house_dat_otherlocs, other_locs_dz)


# 2. HSCP

# Global Script Function to read in Localities Lookup
hscp_dz <- read_in_localities(dz_level = TRUE) %>%
  select(datazone2011, hscp2019name) %>%
  filter(hscp2019name == HSCP)


house_dat_hscp <- house_raw_dat %>%
  inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) %>%
  filter(year == max(year)) %>%
  group_by(year) %>%
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    tax_discount = sum(dwellings_with_a_single_adult_council_tax_discount)
  ) %>%
  ungroup() %>%
  mutate(perc_discount = round_half_up(tax_discount / total_dwellings * 100, 1))

hscp_n_houses <- format_number_for_text(house_dat_hscp$total_dwellings)
hscp_perc_discount <- house_dat_hscp$perc_discount


house_dat2_hscp <- house_raw_dat2 %>%
  inner_join(hscp_dz, by = c("data_zone_code" = "datazone2011")) %>%
  group_by(hscp2019name) %>%
  summarise(
    total_dwellings = sum(total_number_of_dwellings),
    band_a = sum(council_tax_band_a),
    band_b = sum(council_tax_band_b),
    band_c = sum(council_tax_band_c),
    band_f = sum(council_tax_band_f),
    band_g = sum(council_tax_band_g),
    band_h = sum(council_tax_band_h)
  ) %>%
  ungroup() %>%
  mutate(
    perc_houses_AC = round_half_up(
      (band_a + band_b + band_c) / total_dwellings * 100,
      1
    ),
    perc_houses_FH = round_half_up(
      (band_f + band_g + band_h) / total_dwellings * 100,
      1
    )
  )

hscp_perc_housesAC <- house_dat2_hscp$perc_houses_AC
hscp_perc_housesFH <- house_dat2_hscp$perc_houses_FH

rm(hscp_dz, house_dat_hscp, house_dat2_hscp)


# 3. Scotland
scot_n_houses <- format_number_for_text(sum(
  filter(house_raw_dat, year == max(year))$total_number_of_dwellings,
  na.rm = TRUE
))
scot_perc_discount <- format_number_for_text(
  sum(
    filter(
      house_raw_dat,
      year == max(year)
    )$dwellings_with_a_single_adult_council_tax_discount,
    na.rm = TRUE
  ) /
    sum(
      filter(house_raw_dat, year == max(year))$total_number_of_dwellings,
      na.rm = TRUE
    ) *
    100
)

scot_perc_housesAC <- format_number_for_text(
  sum(
    house_raw_dat2$council_tax_band_a,
    house_raw_dat2$council_tax_band_b,
    house_raw_dat2$council_tax_band_c,
    na.rm = TRUE
  ) /
    sum(house_raw_dat2$total_number_of_dwellings, na.rm = TRUE) *
    100
)
scot_perc_housesFH <- format_number_for_text(
  sum(
    house_raw_dat2$council_tax_band_f,
    house_raw_dat2$council_tax_band_g,
    house_raw_dat2$council_tax_band_h,
    na.rm = TRUE
  ) /
    sum(house_raw_dat2$total_number_of_dwellings, na.rm = TRUE) *
    100
)

# Housekeeping ----
# These objects are left over after the script is run
# but don't appear to be used in any 'downstream' process:
# Main markdown, Summary Table, Excel data tables, SDC output.
# TODO: Investigate if these can be removed earlier or not created at all.
rm(
  ctb,
  ext_year,
  house_raw_dat,
  house_raw_dat2,
  i,
  n_occupied,
  n_vacant,
  pal_ctb,
  perc_vacant,
  variable
)
gc()
