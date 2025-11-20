############################################################################################# .
#                                                                                           #
#                       LOCALITY PROFILES GENERAL HEALTH OUTPUTS CODE                       #
#                                                                                           #
############################################################################################# .

## Code used to create infographics, charts, and figures for the General Health section of
#  the locality profiles.

############# 1) PACKAGES, DIRECTORY, LOOKUPS, DATA IMPORT + CLEANING #############

## load packages
library(cowplot)
library(png)
library(flextable)
library(officer)

# Determine locality (for testing only)
# LOCALITY <- "Eastwood"
# LOCALITY <- "Stirling City with the Eastern Villages Bridge of Allan and Dunblane"
# LOCALITY <- "Mid-Argyll, Kintyre and Islay"
# LOCALITY <- "City of Dunfermline"
# LOCALITY <- "Barra"

# Set year of data extracts for folder
ext_year <- 2024

# Source in functions code
# source("Master RMarkdown Document & Render Code/Global Script.R")

# Set file path
# lp_path <- path("/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles")

gen_health_data_dir <- path(lp_path, "General Health", glue("DATA {ext_year}"))

### Geographical lookups and objects ----

# Locality lookup
lookup <- read_in_localities()

# Determine HSCP and HB based on Locality
HSCP <- filter(lookup, hscp_locality == LOCALITY)[["hscp2019name"]]
HB <- filter(lookup, hscp_locality == LOCALITY)[["hb2019name"]]

# Determine other localities based on LOCALITY object
other_locs <- lookup %>%
  select(hscp_locality, hscp2019name) %>%
  filter(hscp2019name == HSCP & hscp_locality != LOCALITY) %>%
  arrange(hscp_locality)

# Find number of locs per partnership
n_loc <- count_localities(lookup, HSCP)

### Import + clean datasets ----

# Life expectancy

# Males
life_exp_male <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_life_exp_male.parquet"
)) %>%
  clean_scotpho_dat()
# Females
life_exp_fem <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_life_exp_fem.parquet"
)) %>%
  clean_scotpho_dat()

life_exp <- bind_rows(life_exp_male, life_exp_fem) %>%
  mutate(
    sex = case_match(
      indicator,
      "Life expectancy, males" ~ "Male",
      "Life expectancy, females" ~ "Female"
    )
  ) %>%
  mutate(
    period_short = str_replace(period, fixed(" to "), "-") |>
      str_sub(end = 9)
  )

rm(life_exp_fem, life_exp_male)

check_missing_data_scotpho(life_exp)

## Deaths aged 15-44
deaths_15_44 <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_deaths_15_44.parquet"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 12), fixed = TRUE))

check_missing_data_scotpho(deaths_15_44)

## Cancer registrations
cancer_reg <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_cancer_reg.parquet"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 12), fixed = TRUE))

check_missing_data_scotpho(cancer_reg)

## Early deaths cancer
early_deaths_cancer <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_early_deaths_cancer.parquet"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 12), fixed = TRUE))

check_missing_data_scotpho(early_deaths_cancer)


## Asthma hospitalisations
asthma_hosp <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_asthma_hosp.parquet"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

check_missing_data_scotpho(asthma_hosp)

## CHD hospitalisations
chd_hosp <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_chd_hosp.parquet"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

check_missing_data_scotpho(chd_hosp)

## COPD hospitalisations
copd_hosp <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_copd_hosp.parquet"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

check_missing_data_scotpho(copd_hosp)

## Anxiety/depression/psychosis prescriptions
adp_presc <- read_parquet(path(
  gen_health_data_dir,
  "scotpho_data_extract_adp_presc.parquet"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = substr(period, 1, 7))

check_missing_data_scotpho(adp_presc)


# Long-term conditions
ltc <- read_parquet(path(gen_health_data_dir, "LTC_from_SLF.parquet")) %>%
  rename(
    "Arthritis" = "arth",
    "Asthma" = "asthma",
    "Atrial fibrillation" = "atrialfib",
    "Cancer" = "cancer",
    "Cardiovascular disease" = "cvd",
    "Liver disease" = "liver",
    "COPD*" = "copd",
    "Dementia" = "dementia",
    "Diabetes" = "diabetes",
    "Epilepsy" = "epilepsy",
    "Coronary heart disease" = "chd",
    "Heart failure" = "hefailure",
    "Multiple sclerosis" = "ms",
    "Parkinsons" = "parkinsons",
    "Renal failure" = "refailure"
  ) %>%
  mutate(
    hscp_locality = gsub("&", "and", hscp_locality, fixed = TRUE),
    year = paste0("20", substr(year, 1, 2), "/", substr(year, 3, 4))
  )


############################### 2) SCOTPHO DATA ####################################

##### 2a Life expectancy #####

# Time objects

latest_year_life_exp_loc <- life_exp |>
  filter(area_type == "Locality") |>
  pull(year) |>
  max()
latest_year_life_exp_otherareas <- max(life_exp[["year"]])

latest_period_life_exp_loc <- life_exp |>
  filter(
    area_type == "Locality",
    year == latest_year_life_exp_loc
  ) |>
  pull(period_short) |>
  unique()
latest_period_life_exp_otherareas <- life_exp |>
  filter(
    area_type == "Scotland",
    year == latest_year_life_exp_otherareas
  ) |>
  pull(period_short) |>
  unique()


# Create time trend
life_exp_trend <- life_exp %>%
  filter(
    area_name == LOCALITY,
    area_type == "Locality",
    year >= max(year) - 10
  ) %>%
  mutate(
    period_short = period_short,
    measure = round_half_up(measure, 1)
  ) %>%
  ggplot(aes(
    x = period_short,
    y = measure,
    group = sex,
    linetype = sex,
    shape = sex,
    colour = sex
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(values = palette) +
  theme_profiles() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  expand_limits(y = 0) +
  labs(
    title = str_wrap(
      glue("Average Life Expectancy in {LOCALITY} locality"),
      width = 65
    ),
    x = "Year Groups (5-year aggregates)",
    y = str_wrap("Average Life Expectancy (in years)", width = 35),
    caption = "Source: ScotPHO"
  ) +
  theme(plot.margin = unit(c(0, 0, 0, 1), "cm")) +
  guides(
    linetype = "none",
    shape = "none",
    colour = guide_legend(
      override.aes = list(shape = c(21, 24), fill = palette[1:2])
    )
  )


# Make a table to compare with other areas

life_exp_table <- life_exp %>%
  filter(
    (year == latest_year_life_exp_loc &
      (area_name == LOCALITY & area_type == "Locality")) |
      year == latest_year_life_exp_otherareas &
        ((area_name == HSCP & area_type == "HSCP") |
          area_name == HB |
          area_name == "Scotland")
  ) %>%
  mutate(
    measure = round_half_up(measure, 1),
    area_type = ordered(
      area_type,
      levels = c("Locality", "HSCP", "Health board", "Scotland")
    )
  ) %>%
  arrange(area_type) %>%
  select("Sex" = sex, area_name, measure) %>%
  pivot_wider(names_from = area_name, values_from = measure)


## Numbers for text

if (LOCALITY %in% check_missing_data_scotpho(life_exp)$area_name) {
  avg_life_exp_latest_male <- NA_real_
  avg_life_exp_latest_fem <- NA_real_
} else {
  avg_life_exp_latest <- filter(
    life_exp,
    year == latest_year_life_exp_loc,
    area_name == LOCALITY,
    area_type == "Locality"
  )

  avg_life_exp_latest_male <- avg_life_exp_latest |>
    filter(sex == "Male") |>
    pull(measure) |>
    round_half_up(digits = 1)
  avg_life_exp_latest_fem <- avg_life_exp_latest |>
    filter(sex == "Female") |>
    pull(measure) |>
    round_half_up(digits = 1)
  rm(avg_life_exp_latest)
}


##### 2b Deaths aged 15-44 #####

## Create variables for latest year
latest_period_deaths_15_44 <- unique(
  filter(deaths_15_44, year == max(deaths_15_44$year))$period_short
)

## Time trend
deaths_15_44_time_trend <- scotpho_time_trend(
  data = deaths_15_44,
  chart_title = "Deaths Aged 15 to 44 Time Trend",
  xaxis_title = "Year Groups (3-year aggregates)",
  yaxis_title = "Deaths, aged 15 to 44\n(Standardised rates per 100,000)",
  string_wrap = 10
)


## Bar chart
deaths_15_44_bar <- scotpho_bar_chart(
  data = deaths_15_44,
  chart_title = paste0(
    "Deaths, Aged 15 to 44 by area, ",
    max(deaths_15_44[["period_short"]])
  ),
  xaxis_title = "Deaths (Standardised rates per 100,000)"
)


## Numbers for text
deaths_15_44_latest <- filter(
  deaths_15_44,
  year == max(deaths_15_44$year),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

scot_deaths_15_44 <- filter(
  deaths_15_44,
  year == max(deaths_15_44$year),
  area_name == "Scotland"
)$measure

deaths_15_44_diff_scot <- if_else(
  deaths_15_44_latest > scot_deaths_15_44,
  "higher",
  "lower"
)


##### 2c Cancer #####

### Cancer Registrations

## Time objects
latest_period_cancer_reg <- unique(
  filter(cancer_reg, year == max(cancer_reg$year))$period_short
)
prev_period_cancer_reg <- unique(
  filter(cancer_reg, year == max(cancer_reg$year) - 1)$period_short
)

## Time trend
cancer_reg_time_trend <- scotpho_time_trend(
  data = cancer_reg,
  chart_title = "Cancer Registrations Time Trend",
  xaxis_title = "Year Groups (3-year aggregates)",
  yaxis_title = "Cancer registrations \n(Standardised rates per 100,000)",
  string_wrap = 10
)


## Numbers for text
cancer_reg_rate_latest <- filter(
  cancer_reg,
  year == max(cancer_reg$year),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

cancer_reg_total_latest <- filter(
  cancer_reg,
  year == max(cancer_reg$year),
  area_name == LOCALITY,
  area_type == "Locality"
)$numerator


### Early deaths from cancer

latest_period_early_deaths_cancer <- unique(
  filter(
    early_deaths_cancer,
    year == max(early_deaths_cancer$year)
  )$period_short
)

early_deaths_cancer_rate_latest <- filter(
  early_deaths_cancer,
  year == max(early_deaths_cancer$year),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

## Time trend for cancer deaths
early_deaths_cancer_time_trend <- scotpho_time_trend(
  data = early_deaths_cancer,
  chart_title = "Early Deaths from Cancer Time Trend",
  xaxis_title = "Year Groups (3-year aggregates)",
  yaxis_title = "Early deaths from cancer\n(Standardised rates per 100,000)",
  string_wrap = 10
)


## Figures for text
early_deaths_cancer_rate_earliest <- filter(
  early_deaths_cancer,
  year == (max(early_deaths_cancer$year) - 10),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

cancer_deaths_perc_change <- abs(
  (early_deaths_cancer_rate_latest - early_deaths_cancer_rate_earliest) *
    100 /
    early_deaths_cancer_rate_earliest
)

cancer_deaths_changeword <- if_else(
  early_deaths_cancer_rate_latest > early_deaths_cancer_rate_earliest,
  "increase",
  "decrease"
)


##### 2d Hospitalisations from diseases #####

disease_hosp <- bind_rows(
  filter(asthma_hosp, year == max(year)),
  filter(chd_hosp, year == max(year)),
  filter(copd_hosp, year == max(year))
) %>%
  filter(
    (area_name == LOCALITY & area_type == "Locality") |
      (area_name == HSCP & area_type == "HSCP") |
      area_name == HB |
      area_name == "Scotland"
  ) %>%
  mutate(
    area_type = factor(
      area_type,
      levels = c("Locality", "HSCP", "Health board", "Scotland")
    ),
    area_name = fct_reorder(as.factor(area_name), as.numeric(area_type))
  ) %>%
  mutate(
    indicator = case_when(
      str_detect(indicator, fixed("Asthma")) ~ "Asthma",
      str_detect(indicator, fixed("CHD")) ~ "Coronary Heart Disease",
      str_detect(indicator, fixed("COPD")) ~ "COPD"
    )
  ) %>%
  mutate(measure = round_half_up(measure, 1))

highest_hosp_disease <- disease_hosp %>%
  filter(
    area_name == LOCALITY,
    area_type == "Locality"
  ) %>%
  filter(measure == max(measure))

disease_hosp_table <- disease_hosp |>
  mutate(
    area_order = case_when(
      area_name == LOCALITY ~ 1L,
      area_name == HSCP ~ 2L,
      str_starts(area_name, fixed("NHS")) ~ 4L,
      area_name == "Scotland" ~ 5L,
      .default = 2L
    )
  ) |>
  arrange(area_order) |>
  select(indicator, period_short, area_name, measure) |>
  pivot_wider(names_from = area_name, values_from = measure) |>
  rename(
    "Disease" = indicator,
    "Latest time period" = period_short
  )

table8_year_title <- max(disease_hosp_table[["Latest time period"]])

##### 2e Prescriptions for Anxiety, Depression and Psychosis #####

## Time objects
latest_period_adp_presc <- unique(
  filter(adp_presc, year == max(adp_presc$year))$period_short
)
prev_period_adp_presc <- unique(
  filter(adp_presc, year == max(adp_presc$year) - 10)$period_short
)

## Time trend
adp_presc_time_trend <- scotpho_time_trend(
  data = adp_presc,
  chart_title = "Anxiety, Depression and Psychosis Prescriptions Time Trend",
  xaxis_title = "Financial Year",
  yaxis_title = "Population prescribed\n medication (%)",
  string_wrap = 20,
  rotate_xaxis = TRUE
)


## Bar chart
adp_presc_bar <- adp_presc %>%
  scotpho_bar_chart(
    data = .,
    chart_title = paste0(
      "Anxiety, Depression and Psychosis Prescriptions, ",
      max(.$period_short)
    ),
    xaxis_title = "Population prescribed medication (%)"
  )


## Numbers for text

adp_presc_latest <- filter(
  adp_presc,
  year == max(adp_presc$year),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

adp_presc_earliest <- filter(
  adp_presc,
  year == (max(adp_presc$year) - 10),
  area_name == LOCALITY,
  area_type == "Locality"
)$measure

adp_presc_perc_change <- abs(
  (adp_presc_latest - adp_presc_earliest) * 100 / adp_presc_earliest
)
adp_presc_changeword <- if_else(
  adp_presc_latest > adp_presc_earliest,
  "increase",
  "decrease"
)

scot_adp_presc <- filter(
  adp_presc,
  year == max(adp_presc$year),
  area_name == "Scotland"
)$measure

adp_presc_diff_scot <- if_else(
  adp_presc_latest > scot_adp_presc,
  "larger",
  "smaller"
)


############################ 3) SLF DATA (LTCs) ####################################

# Extract SLF adjusted populations
slf_pops <- distinct(ltc, age_group, hscp_locality, hscp2019name, slf_adj_pop)

slf_pop_loc <- filter(slf_pops, hscp_locality == LOCALITY)

# Determine year
latest_year_ltc <- ltc[["year"]][1]

## Create Scotland totals
ltc_scot <- ltc %>%
  select(-year, -hscp2019name, -hscp_locality, -slf_adj_pop) %>%
  group_by(total_ltc, age_group) %>%
  summarise(across(everything(), sum)) %>%
  ungroup()


###### 3a Waffle Chart Infographic ######

# Load images
# under 65
ppl_bold_u65 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people bold under 65.png"
))
ppl_faint_u65 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people faint under 65.png"
))
# 65-74
ppl_bold_6574 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people bold 65-74.png"
))
ppl_faint_6574 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people faint 65-74.png"
))
# 75-84
ppl_bold_7584 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people bold 75-84.png"
))
ppl_faint_7584 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people faint 75-84.png"
))
# over 85
ppl_bold_o85 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people bold over 85.png"
))
ppl_faint_o85 <- readPNG(path(
  lp_path,
  "General Health",
  "infographics",
  "people faint over 85.png"
))

# LTC infographic waffle chart
create_infographic <- function(
  image1,
  image2,
  perc_ltc,
  col,
  age_label1,
  age_label2
) {
  ggplot() +
    scale_x_continuous(name = "x") +
    scale_y_continuous(name = "y") +
    geom_rect(
      data = data.frame(x1 = 0, x2 = 1, y1 = 0, y2 = 1.3),
      mapping = aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2),
      color = "white",
      fill = "white"
    ) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.box.background = element_rect(fill = "transparent", colour = NA)
    ) +
    annotation_raster(
      image1,
      ymin = 0.02,
      ymax = 0.99,
      xmin = 0.99 * perc_ltc,
      xmax = 0.99
    ) +
    annotation_raster(
      image2,
      ymin = 0.02,
      ymax = 0.99,
      xmin = 0.01,
      xmax = 0.99 * perc_ltc
    ) +
    coord_fixed(ratio = 0.3) +
    annotate(
      geom = "text",
      x = 0.5,
      y = 0.02,
      size = 3.8,
      label = paste0(
        round_half_up(10 * perc_ltc, 1),
        " in 10 people aged ",
        age_label1,
        " have at least 1 LTC"
      )
    ) +
    annotate(
      geom = "text",
      x = 0.5,
      y = 1.08,
      size = 4,
      colour = col,
      fontface = "bold",
      label = paste0(age_label2, " YEARS OLD")
    )
}

# Set percentage with LTC for infographic
ltc_infographic <- ltc %>%
  filter(hscp_locality == LOCALITY) %>%
  filter(total_ltc > 0) %>%
  group_by(hscp_locality, age_group) %>%
  summarise(people = sum(people)) %>%
  ungroup() %>%
  left_join(slf_pop_loc, by = join_by(hscp_locality, age_group)) %>%
  mutate(perc_with_ltc = round_half_up(people / slf_adj_pop, 2))

# objects for each percentage for text + cropping images
ltc.percent.u65 <- filter(
  ltc_infographic,
  age_group == "Under 65"
)$perc_with_ltc
ltc.percent.6574 <- filter(ltc_infographic, age_group == "65-74")$perc_with_ltc
ltc.percent.7584 <- filter(ltc_infographic, age_group == "75-84")$perc_with_ltc
ltc.percent.o85 <- filter(ltc_infographic, age_group == "85+")$perc_with_ltc

## Crop images

# under65
dm1 <- dim(ppl_bold_u65)
ppl_bold_u65 <- ppl_bold_u65[1:dm1[1], 1:floor(dm1[2] * ltc.percent.u65), ]
dm2 <- dim(ppl_faint_u65)
ppl_faint_u65 <- ppl_faint_u65[
  1:dm2[1],
  ceiling(dm2[2] * ltc.percent.u65):dm2[2],
]

# 65-74
dm1 <- dim(ppl_bold_6574)
ppl_bold_6574 <- ppl_bold_6574[1:dm1[1], 1:floor(dm1[2] * ltc.percent.6574), ]
dm2 <- dim(ppl_faint_6574)
ppl_faint_6574 <- ppl_faint_6574[
  1:dm2[1],
  ceiling(dm2[2] * ltc.percent.6574):dm2[2],
]

# 75-84
dm1 <- dim(ppl_bold_7584)
ppl_bold_7584 <- ppl_bold_7584[1:dm1[1], 1:floor(dm1[2] * ltc.percent.7584), ]
dm2 <- dim(ppl_faint_7584)
ppl_faint_7584 <- ppl_faint_7584[
  1:dm2[1],
  ceiling(dm2[2] * ltc.percent.7584):dm2[2],
]

# over65
dm1 <- dim(ppl_bold_o85)
ppl_bold_o85 <- ppl_bold_o85[1:dm1[1], 1:floor(dm1[2] * ltc.percent.o85), ]
dm2 <- dim(ppl_faint_o85)
ppl_faint_o85 <- ppl_faint_o85[
  1:dm2[1],
  ceiling(dm2[2] * ltc.percent.o85):dm2[2],
]


waffle.u65 <- create_infographic(
  image1 = ppl_faint_u65,
  image2 = ppl_bold_u65,
  perc_ltc = ltc.percent.u65,
  col = palette[1],
  age_label1 = "under 65",
  age_label2 = "UNDER 65"
)

waffle.6574 <- create_infographic(
  image1 = ppl_faint_6574,
  image2 = ppl_bold_6574,
  perc_ltc = ltc.percent.6574,
  col = palette[2],
  age_label1 = "65 to 74",
  age_label2 = "65 - 74"
)

waffle.7584 <- create_infographic(
  image1 = ppl_faint_7584,
  image2 = ppl_bold_7584,
  perc_ltc = ltc.percent.7584,
  col = palette[3],
  age_label1 = "75 to 84",
  age_label2 = "75 - 84"
)

waffle.o85 <- create_infographic(
  image1 = ppl_faint_o85,
  image2 = ppl_bold_o85,
  perc_ltc = ltc.percent.o85,
  col = palette[4],
  age_label1 = "over 85",
  age_label2 = "OVER 85"
)


## Combine images
ltc_waffles <- plot_grid(
  waffle.u65,
  waffle.6574,
  waffle.7584,
  waffle.o85,
  nrow = 2
)


## Numbers for text
ltc_percent_total_latest <- (sum(ltc_infographic$people) /
  sum(ltc_infographic$slf_adj_pop)) *
  100


# Remove unnecessary objects
rm(
  ppl_bold_u65,
  ppl_faint_u65,
  ppl_faint_o85,
  ppl_bold_o85,
  ppl_faint_7584,
  ppl_bold_7584,
  ppl_faint_6574,
  ppl_bold_6574,
  ltc.percent.u65,
  ltc.percent.6574,
  ltc.percent.7584,
  ltc.percent.o85,
  dm1,
  dm2,
  waffle.u65,
  waffle.6574,
  waffle.7584,
  waffle.o85
)


###### 3b Multi-morbidity LTC Table ######

## Create df with under 65 vs over 65 - will be used for rest of LTC work
ltc2 <- ltc %>%
  select(-year) %>%
  mutate(age_group = if_else(age_group == "Under 65", "Under 65", "65+")) %>%
  group_by(hscp2019name, hscp_locality, age_group, total_ltc) %>%
  summarise(across(everything(), sum)) %>%
  ungroup()

ltc_multimorbidity <- ltc2 %>%
  na.omit(ltc2) %>%
  filter(
    hscp_locality == LOCALITY,
    total_ltc != 0
  ) %>%
  mutate(
    total_ltc = case_when(
      total_ltc == 1 ~ "1 LTC",
      total_ltc == 2 ~ "2 LTCs",
      total_ltc == 3 ~ "3 LTCs",
      total_ltc >= 4 ~ "4 or more LTCs"
    )
  ) %>%
  mutate(
    total_ltc = factor(
      total_ltc,
      levels = c("1 LTC", "2 LTCs", "3 LTCs", "4 or more LTCs")
    )
  ) %>%
  group_by(age_group, total_ltc) %>%
  summarise(people = sum(people)) %>%
  ungroup() %>%
  mutate(
    ltc_pop = if_else(
      age_group == "Under 65",
      filter(slf_pop_loc, age_group == "Under 65")$slf_adj_pop,
      sum(filter(slf_pop_loc, age_group != "Under 65")$slf_adj_pop)
    )
  ) %>%
  group_by(age_group) %>%
  mutate(percent = round_half_up(people / ltc_pop * 100, 1)) %>%
  ungroup()


ltc_multimorbidity_table <- ltc_multimorbidity %>%
  select(age_group, total_ltc, percent) %>%
  pivot_wider(names_from = age_group, values_from = percent) %>%
  rename(
    " " = total_ltc,
    "Percentage under 65" = "Under 65",
    "Percentage over 65" = "65+"
  )


## Figures for text
ltc_multimorbidity_un65_perc <- sum(
  filter(
    ltc_multimorbidity,
    total_ltc != "1 LTC",
    age_group == "Under 65"
  )$percent
)

ltc_multimorbidity_ov65_perc <- sum(
  filter(
    ltc_multimorbidity,
    total_ltc != "1 LTC",
    age_group == "65+"
  )$percent
)


# ###### 3c Prevalence of LTC Types ######
ltc_types <- ltc2 %>%
  select(-hscp2019name, -total_ltc, -people) %>%
  filter(hscp_locality == LOCALITY) %>%
  group_by(hscp_locality, age_group) %>%
  summarise(across(everything(), sum)) %>%
  ungroup() |>
  pivot_longer(
    cols = "Arthritis":"Renal failure",
    names_to = "key",
    values_to = "value"
  )

# Create negative values for chart
ltc_types_temp <- ltc_types %>%
  filter(age_group == "Under 65") %>%
  mutate(
    percent = (value /
      (filter(slf_pop_loc, age_group == "Under 65")$slf_adj_pop) *
      -100)
  )

ltc_types <- ltc_types %>%
  filter(age_group == "65+") %>%
  mutate(
    percent = (value /
      sum(filter(slf_pop_loc, age_group != "Under 65")$slf_adj_pop) *
      100)
  ) %>%
  bind_rows(ltc_types_temp)

rm(ltc_types_temp)


#### lollipop with 3 separate plots put together

## create conditionals for expand limits
max_ltc_types_pct <- max(ltc_types$percent)

lims.un65 <- case_when(
  max_ltc_types_pct < 20 ~ -10,
  between(max_ltc_types_pct, 20, 24) ~ -12,
  max_ltc_types_pct > 24 ~ -15
)
lims.ov65 <- case_when(
  max_ltc_types_pct < 20 ~ 20,
  between(max_ltc_types_pct, 20, 24) ~ 24,
  max_ltc_types_pct > 24 ~ 30
)

rm(max_ltc_types_pct)

ltc_plot_left <- ltc_types %>%
  filter(age_group == "Under 65") %>%
  ggplot(aes(x = percent, y = key, label = round_half_up(percent, 1))) +
  geom_point(colour = palette[1], size = 3) +
  geom_segment(
    aes(x = 0, y = key, xend = percent, yend = key),
    linewidth = 0.4
  ) +
  labs(
    x = "People under 65 with\nthe condition (%)",
    y = "",
    title = "UNDER 65"
  ) +
  scale_x_continuous(breaks = seq(-100, 0, 2), labels = abs) +
  expand_limits(x = lims.un65) +
  theme_profiles() +
  theme(
    title = element_text(colour = palette[1]),
    plot.margin = unit(c(0.5, 0, 0, 0), "cm"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_y_discrete(limits = rev(levels(as.factor(ltc_types$key))))

ltc_axis <- ltc_types %>%
  filter(age_group == "Under 65") %>%
  ggplot(aes(x = 0, y = key, label = key)) +
  geom_text() +
  scale_y_discrete(limits = rev(levels(as.factor(ltc_types$key)))) +
  theme_void()

ltc_plot_right <- ltc_types %>%
  filter(age_group == "65+") %>%
  ggplot(aes(x = percent, y = key, label = round_half_up(percent, 1))) +
  geom_point(colour = palette[2], size = 3) +
  geom_segment(
    aes(x = 0, y = key, xend = percent, yend = key),
    linewidth = 0.4
  ) +
  labs(
    x = "People over 65 with\nthe condition (%)",
    y = "",
    title = "OVER 65"
  ) +
  scale_x_continuous(breaks = seq(0, 100, 2)) +
  expand_limits(x = lims.ov65) +
  theme_profiles() +
  theme(
    title = element_text(colour = palette[2]),
    plot.margin = unit(c(0.5, 0, 0, 0), "cm"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_y_discrete(limits = rev(levels(as.factor(ltc_types$key))))

title <- ggdraw() +
  draw_label(
    str_wrap(
      glue(
        "Prevalence estimates for {latest_year_ltc} of Physical Long-Term Conditions in the {LOCALITY} Locality"
      ),
      width = 65
    ),
    size = 11,
    fontface = "bold"
  )

caption <- ggdraw() +
  draw_label(
    "Source: SPARRA via the Source Linkage Files",
    size = 10,
    hjust = -0.5
  )

# Combine plots into 1
ltc_types_plot <- plot_grid(
  title,
  plot_grid(
    ltc_plot_left,
    ltc_axis,
    ltc_plot_right,
    ncol = 3,
    align = "h",
    rel_widths = c(0.5, 0.6, 1)
  ),
  caption,
  nrow = 3,
  rel_heights = c(3, 20, 1)
)


rm(
  ltc_plot_left,
  ltc_axis,
  ltc_plot_right,
  title,
  caption,
  lims.ov65,
  lims.un65
)


##### 3d Top LTCs Table #####

# Most common LTC all round
ltc_totals <- ltc2 %>%
  filter(total_ltc != 0) %>%
  select(-hscp2019name, -total_ltc, -age_group) %>%
  group_by(hscp_locality) %>%
  summarise(across(everything(), sum)) %>%
  ungroup()

ltc_totals <- left_join(
  ltc_totals,
  select(lookup, hscp_locality, hscp2019name),
  by = join_by(hscp_locality),
  relationship = "one-to-one"
)

# Extract population totals to make %
ltc_pops_total_loc <- sum(slf_pop_loc$slf_adj_pop)
ltc_pops_total_scot <- sum(slf_pops$slf_adj_pop)
ltc_pops_total_hscp <- sum(filter(slf_pops, hscp2019name == HSCP)$slf_adj_pop)

# Colour lookup for table
ltc_cols <- ltc_scot %>%
  select(!c(total_ltc, age_group, people)) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "topltc",
    values_to = "value"
  ) %>%
  arrange(desc(value)) %>%
  mutate(
    colours = c(
      palette,
      c(
        "navy",
        "lightsalmon4",
        "deeppink4",
        "forestgreen",
        "steelblue",
        "purple3",
        "red4"
      )
    )
  )

# Top 5 locality
top5ltc_loc <- ltc_totals %>%
  filter(hscp_locality == LOCALITY) %>%
  select(-hscp_locality, -hscp2019name, -people, -slf_adj_pop) %>%
  pivot_longer(
    cols = everything(),
    names_to = "topltc",
    values_to = "value"
  ) %>%
  slice_max(n = 5, order_by = value, with_ties = FALSE) %>%
  mutate(percent = round_half_up((value / ltc_pops_total_loc) * 100, 2)) %>%
  select(-value) %>%
  left_join(ltc_cols, by = join_by(topltc)) %>%
  unite("Prevalence", topltc, percent, sep = "\n") %>%
  mutate(Prevalence = paste(Prevalence, "%"))

# Top 5 HSCP
top5ltc_hscp <- ltc_totals %>%
  filter(hscp2019name == HSCP) %>%
  select(-hscp_locality, -hscp2019name, -people, -slf_adj_pop) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "topltc",
    values_to = "value"
  ) %>%
  slice_max(n = 5, order_by = value, with_ties = FALSE) %>%
  mutate(percent = round_half_up((value / ltc_pops_total_hscp) * 100, 2)) %>%
  select(-value) %>%
  left_join(ltc_cols, by = join_by(topltc)) %>%
  unite("Prevalence", topltc, percent, sep = "\n") %>%
  mutate(Prevalence = paste(Prevalence, "%"))

# Top 5 Scotland
top5ltc_scot <- ltc_totals %>%
  select(-hscp_locality, -hscp2019name, -people, -slf_adj_pop) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "topltc",
    values_to = "value"
  ) %>%
  slice_max(n = 5, order_by = value, with_ties = FALSE) %>%
  mutate(percent = round_half_up((value / ltc_pops_total_scot) * 100, 2)) %>%
  select(-value) %>%
  left_join(ltc_cols, by = join_by(topltc)) %>%
  unite("Prevalence", topltc, percent, sep = "\n") %>%
  mutate(Prevalence = paste(Prevalence, "%"))


## Create column headers

loc.ltc.table <- str_wrap(
  glue("{LOCALITY} Locality"),
  width = if_else(n_loc < 5, 30, 25)
)

hscp.ltc.table <- str_wrap(glue("{HSCP} HSCP"), width = 25)

# Top5 LTC table as a table (instead of an image)
top5_ltc_table <- bind_cols(
  select(top5ltc_loc, {{ loc.ltc.table }} := Prevalence),
  select(top5ltc_hscp, {{ hscp.ltc.table }} := Prevalence),
  select(top5ltc_scot, "Scotland" = Prevalence)
) |>
  flextable(cwidth = 2) |>
  add_header_lines(
    values = str_wrap(
      glue(
        "Top 5 most common Physical Long-Term Conditions in {LOCALITY} Locality by prevelance estimates for {latest_year_ltc}, compared to {HSCP} HSCP and Scotland estimates."
      ),
      width = 65
    )
  ) |>
  bg(j = 1, bg = top5ltc_loc$colours) |>
  bg(j = 2, bg = top5ltc_hscp$colours) |>
  bg(j = 3, bg = top5ltc_scot$colours) |>
  fontsize(size = 16, part = "header") |>
  fontsize(size = 12, part = "body") |>
  font(fontname = "Arial", part = "all") |>
  color(color = "white", part = "body") |>
  bold(part = "header") |>
  border(border = fp_border(color = "white", width = 5), part = "body")

rm(
  ltc_cols,
  ltc_pops_total_loc,
  loc.ltc.table,
  hscp.ltc.table,
  top5ltc_loc,
  top5ltc_hscp,
  top5ltc_scot
)

## Objects for text

ltc_perc_scot <- round_half_up(
  (sum(filter(ltc_scot, total_ltc > 0)$people) / ltc_pops_total_scot) * 100,
  1
)

ltc_diff_scot <- if_else(
  ltc_percent_total_latest > ltc_perc_scot,
  "higher",
  "lower"
)


############################### 4) CODE FOR SUMMARY TABLE ###############################

## Make GH objects table for hscp, scot AND other localities in the partnership

# Function to get latest data from scotpho

other_locs_summary_table <- function(data, latest_year) {
  data %>%
    filter(
      year == latest_year,
      area_type == "Locality"
    ) %>%
    rename("hscp_locality" = "area_name") %>%
    right_join(other_locs, by = join_by(hscp_locality)) %>%
    arrange(hscp_locality) %>%
    select(hscp_locality, measure) %>%
    mutate(measure = round_half_up(measure, 1)) %>%
    pivot_wider(names_from = hscp_locality, values_from = measure)
}

hscp_scot_summary_table <- function(data, latest_year, area) {
  type <- if_else(area == HSCP, "HSCP", "Scotland")
  temp <- filter(
    data,
    year == latest_year,
    area_name == area,
    area_type == type
  )

  round_half_up(temp[["measure"]], digits = 1)
}

# 1. Other localities

# male life expectancy
other_locs_life_exp_male <- other_locs_summary_table(
  data = filter(life_exp, sex == "Male"),
  latest_year = latest_year_life_exp_loc
)

# female life exp
other_locs_life_exp_fem <- other_locs_summary_table(
  data = filter(life_exp, sex == "Female"),
  latest_year = latest_year_life_exp_loc
)

## deaths 15-44
other_locs_deaths_15_44 <- other_locs_summary_table(
  deaths_15_44,
  latest_year = max(deaths_15_44$year)
)


## Cancer
other_locs_cancer <- other_locs_summary_table(
  cancer_reg,
  latest_year = max(cancer_reg$year)
)

## ADP
other_locs_adp <- other_locs_summary_table(
  adp_presc,
  latest_year = max(adp_presc$year)
)


## ltc
otherloc_ltc_pops <- slf_pops %>%
  inner_join(other_locs, by = "hscp_locality") %>%
  group_by(hscp_locality) %>%
  summarise(slf_adj_pop = sum(slf_adj_pop)) %>%
  ungroup()

other_locs_ltc <- ltc |>
  inner_join(other_locs, by = join_by(hscp2019name, hscp_locality)) %>%
  select(hscp_locality, total_ltc, people) %>%
  filter(total_ltc >= 1) %>%
  group_by(hscp_locality) %>%
  summarise(ltc_people = sum(people)) %>%
  ungroup() %>%
  left_join(otherloc_ltc_pops, by = "hscp_locality") %>%
  mutate(percent = round_half_up(ltc_people / slf_adj_pop * 100, 1)) %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, percent) %>%
  pivot_wider(names_from = hscp_locality, values_from = percent)


# 2. HSCP

if (HSCP == "Clackmannanshire and Stirling") {
  hscp_life_exp_male <- NA_real_
  hscp_life_exp_fem <- NA_real_
} else {
  hscp_life_exp_male <- hscp_scot_summary_table(
    data = filter(life_exp, sex == "Male"),
    latest_year = latest_year_life_exp_otherareas,
    area = HSCP
  )

  hscp_life_exp_fem <- hscp_scot_summary_table(
    data = filter(life_exp, sex == "Female"),
    latest_year = latest_year_life_exp_otherareas,
    area = HSCP
  )
}


hscp_deaths_15_44 <- hscp_scot_summary_table(
  deaths_15_44,
  latest_year = max(deaths_15_44$year),
  area = HSCP
)
hscp_cancer <- hscp_scot_summary_table(
  cancer_reg,
  latest_year = max(cancer_reg$year),
  area = HSCP
)
hscp_adp <- hscp_scot_summary_table(
  adp_presc,
  latest_year = max(adp_presc$year),
  area = HSCP
)

ltc_hscp <- sum(filter(ltc, hscp2019name == HSCP, total_ltc > 0)$people)
hscp_ltc <- round_half_up(ltc_hscp / ltc_pops_total_hscp * 100, 1)

# 3. Scotland

scot_life_exp_male <- hscp_scot_summary_table(
  data = filter(life_exp, sex == "Male"),
  latest_year = latest_year_life_exp_otherareas,
  area = "Scotland"
)

scot_life_exp_fem <- hscp_scot_summary_table(
  data = filter(life_exp, sex == "Female"),
  latest_year = latest_year_life_exp_otherareas,
  area = "Scotland"
)

scot_deaths_15_44 <- hscp_scot_summary_table(
  deaths_15_44,
  latest_year = max(deaths_15_44$year),
  area = "Scotland"
)
scot_cancer <- hscp_scot_summary_table(
  cancer_reg,
  latest_year = max(cancer_reg$year),
  area = "Scotland"
)
scot_cancer_deaths <- hscp_scot_summary_table(
  early_deaths_cancer,
  latest_year = max(early_deaths_cancer$year),
  area = "Scotland"
)
scot_adp_presc <- hscp_scot_summary_table(
  adp_presc,
  latest_year = max(adp_presc$year),
  area = "Scotland"
)

# Housekeeping ----
# These objects are left over after the script is run
# but don't appear to be used in any 'downstream' process:
# Main markdown, Summary Table, Excel data tables, SDC output.
# TODO: Investigate if these can be removed earlier or not created at all.
rm(
  create_infographic,
  disease_hosp,
  early_deaths_cancer_rate_earliest,
  gen_health_data_dir,
  hscp_scot_summary_table,
  latest_year_life_exp_loc,
  ltc_infographic,
  ltc_pops_total_hscp,
  ltc_pops_total_scot,
  ltc_hscp,
  ltc_scot,
  ltc_totals,
  ltc2,
  other_locs,
  other_locs_summary_table,
  otherloc_ltc_pops,
  prev_period_cancer_reg,
  slf_pop_loc,
  slf_pops,
  table8_year_title
)
gc()

## Stat disclosure control for LTC

# sdc1 <- ltc %>%
#   filter(total_ltc > 0) %>%
#   select(hscp2019name, hscp_locality, age_group, total_ltc, people) %>%
#   group_by(hscp2019name, hscp_locality, age_group) %>%
#   summarise(people = sum(people)) %>%
#   ungroup() %>%
#   left_join(slf_pops)
#
# sdc2 <- ltc2 %>%
#   na.omit(ltc2) %>%
#   filter(total_ltc != 0) %>%
#   mutate(total_ltc = case_when(total_ltc == 1 ~ "1 LTC",
#                                total_ltc == 2 ~ "2 LTCs",
#                                total_ltc == 3 ~ "3 LTCs",
#                                total_ltc >= 4 ~ "4 or more LTCs")) %>%
#   group_by(hscp2019name, hscp_locality, age_group, total_ltc, slf_adj_pop) %>%
#   summarise(people = sum(people)) %>%
#   ungroup()
#
# sdc3 <-  ltc2 %>%
#   select(-total_ltc, -people) %>%
#   group_by(hscp2019name, hscp_locality, age_group, slf_adj_pop) %>%
#   summarise_all(sum) %>%
#   gather(key ="key", value  ="value", c(`Arthritis`:`Renal failure`)) %>%
#   filter(value != 0)
#
#
# writexl::write_xlsx(x = list("Total Pop with LTC Age" = sdc1,
#                              "LTC Multimorbidity Age" = sdc2,
#                              "LTC Types Age" = sdc3),
#                     path = path(lp_path, "Publishing", "LTC Data.xlsx"))
