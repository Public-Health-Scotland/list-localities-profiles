#################### LOCALITY PROFILES UNSCHEDULED CARE: OUTPUTS ######################.

####################### SECTION 1: Packages, file paths, etc #########################

## Manually set year that the profiles are being run (year on data folder)
ext_year <- 2025

# Set locality profiles file path
#lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"
import_folder <- paste0(lp_path, "Unscheduled Care/DATA ", ext_year, "/")

### for testing run global script and locality placeholder below

## Packages
library(scales)

## Functions
#source("Master RMarkdown Document & Render Code/Global Script.R")

## Define locality

#LOCALITY <- "Inverness"

# Set date limit for financial year
# Unless we're in Q4 use the previous FY as the max
# max_fy <- ifelse(
#   lubridate::quarter(Sys.Date(), fiscal_start = 4) != 4,
#   phsmethods::extract_fin_year(Sys.Date() - years(1)),
#   phsmethods::extract_fin_year(Sys.Date())
# )

max_fy <- "2024/25" # TODO Change this to be dynamic and move to general!

max_year <- 2024

########################## SECTION 2: Lookups & Populations ###############################

aggregate_area_data <- function(data, measure) {
  data %>%

    # Group Up To Locality Level + Combine Sexes ----

    summarise(
      {{ measure }} := sum({{ measure }}),
      .by = c(
        "year",
        "financial_year",
        "hb2019name",
        "hscp2019name",
        "hscp_locality",
        "age_group"
      )
    ) %>%
    mutate(location = hscp_locality) %>%
    mutate(level = "Locality") %>%

    bind_rows(
      summarise(
        .,
        {{ measure }} := sum({{ measure }}),
        .by = c(
          "year",
          "financial_year",
          "hb2019name",
          "hscp2019name",
          "age_group"
        )
      ) %>%
        mutate(location = hscp2019name) %>%
        mutate(level = "HSCP")
    ) %>%

    bind_rows(
      summarise(
        .,
        {{ measure }} := sum({{ measure }}),
        .by = c("year", "financial_year", "hb2019name", "age_group")
      ) %>%
        mutate(location = hb2019name) %>%
        mutate(level = "HB")
    ) %>%

    bind_rows(
      summarise(
        .,
        {{ measure }} := sum({{ measure }}),
        .by = c("year", "financial_year", "age_group")
      ) %>%
        mutate(location = "Scotland") %>%
        mutate(level = "Scotland")
    ) %>%
    dplyr::select(
      year,
      financial_year,
      hb2019name,
      hscp2019name,
      hscp_locality,
      age_group,
      location,
      level,
      {{ measure }}
    ) %>%
    mutate(
      level = factor(
        level,
        levels = c("Locality", "HSCP", "HB", "Scotland")
      )
    )
}


## 1. Lookups ----

localities <- read_in_localities()

locality_info <- localities %>%
  filter(hscp_locality == LOCALITY)

HSCP <- locality_info %>%
  pull(hscp2019name)

HB <- locality_info %>%
  pull(hb2019name)


# Determine other localities based on LOCALITY object

other_locs <- localities %>%
  filter(hscp2019name == HSCP, hscp_locality != LOCALITY) %>%
  distinct(hscp_locality, hscp2019name)

# Find number of locs per partnership
n_loc <- count_localities(localities, HSCP)


## 2. Populations (for rates) ----

populations <- read_in_dz_pops()

#populations_proxy_year <- read_in_dz_pops_proxy_year() - Removed due to updated SAPE - keep as placeholder for future?

#populations <- rbind(populations, populations_proxy_year)

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


populations_filtered <- populations %>%
  mutate(financial_year = paste0(year, "/", substr(year + 1, 3, 4))) %>%
  dplyr::select(
    year,
    financial_year,
    hb2019name,
    hscp2019name,
    hscp_locality,
    datazone2011,
    datazone2011name,
    sex,
    Pop0_17,
    Pop18_44,
    Pop45_64,
    Pop65_74,
    Pop75Plus,
    Pop65Plus,
    total_pop
  ) %>%
  pivot_longer(
    cols = c(
      "Pop0_17",
      "Pop18_44",
      "Pop45_64",
      "Pop65_74",
      "Pop75Plus",
      "Pop65Plus",
      "total_pop"
    ),
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
  ) %>%
  aggregate_area_data(pop)

populations_filtered_area <- populations_filtered %>%
  filter(age_group == "Total") %>%
  dplyr::select(-age_group)

populations_filtered_age <- populations_filtered %>%
  filter(level == "Locality") %>%
  summarise(
    pop = sum(pop),
    .by = c(
      "year",
      "financial_year",
      "hb2019name",
      "hscp2019name",
      "hscp_locality",
      "age_group"
    )
  )


########################## SECTION 3: Functions ###############################

# Functions for creating time trends
age_group_trend_usc <- function(
  data_for_plot,
  measure,
  plot_title,
  yaxis_title,
  source
) {
  data_for_plot <- data_for_plot %>%
    rename(n = measure)

  data_for_plot %>%
    ggplot(aes(
      x = financial_year,
      y = n,
      group = age_group,
      color = age_group
    )) +
    geom_line(linewidth = 1) +
    geom_point() +
    scale_colour_manual(values = c(palette)) +
    scale_x_discrete(breaks = data_for_plot$financial_year) +
    scale_y_continuous(
      labels = comma,
      limits = c(0, 1.1 * max(data_for_plot$n))
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

area_trend_usc <- function(
  data_for_plot,
  measure,
  plot_title,
  yaxis_title,
  source
) {
  data_for_plot <- data_for_plot %>%
    rename(n = measure) %>%
    mutate(
      location = fct_reorder(
        as.factor(str_wrap(location, 23)),
        as.numeric(level)
      )
    )

  data_for_plot %>%
    ggplot() +
    aes(
      x = financial_year,
      y = n,
      group = location,
      fill = location,
      linetype = level
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
      limits = c(0, 1.1 * max(data_for_plot$n))
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

unscheduled_care_charts_and_text <- function(
  dataset,
  indicator_column,
  indicator_name,
  denominator_number,
  denominator_name,
  source,
  populations_area,
  populations_age,
  LOCALITY,
  other_locs,
  HSCP,
  HB,
  sdc_applied
) {
  min_fin_year <- dataset %>%
    filter(year == min(year)) %>%
    pull(financial_year) %>%
    unique()

  max_fin_year <- dataset %>%
    filter(year == max_year) %>%
    pull(financial_year) %>%
    unique()

  intro_paragraph_area <- paste0(
    "presents the ",
    str_to_lower(indicator_name),
    " rate per ",
    paste0(format(denominator_number, big.mark = ",", scientific = FALSE), " "),
    paste0(denominator_name, " "),
    "in the ",
    LOCALITY,
    " locality from ",
    min_fin_year,
    " to ",
    max_fin_year,
    "."
  )

  intro_paragraph_area <- gsub("a & e", "A & E", intro_paragraph_area)

  indicator_areas <- dataset %>%
    aggregate_area_data({{ indicator_column }}) %>%
    summarise(
      {{ indicator_column }} := sum({{ indicator_column }}),
      .by = c(
        "year",
        "financial_year",
        "hb2019name",
        "hscp2019name",
        "hscp_locality",
        "location",
        "level"
      )
    ) %>%
    left_join(
      populations_area,
      by = c(
        "year",
        "financial_year",
        "hb2019name",
        "hscp2019name",
        "hscp_locality",
        "location",
        "level"
      )
    ) %>%
    mutate(rate = round_half_up(100000 * ({{ indicator_column }} / pop))) %>%
    dplyr::select(
      financial_year,
      year,
      hb2019name,
      hscp2019name,
      hscp_locality,
      location,
      level,
      {{ indicator_column }},
      pop,
      rate
    )

  indicator_loc_ts <- indicator_areas %>%
    filter(
      level == "Locality" &
        location == LOCALITY |
        level == "HSCP" & location == HSCP |
        level == "HB" & location == HB |
        level == "Scotland"
    ) %>%
    area_trend_usc(
      measure = "rate",
      plot_title = paste(indicator_name, "per 100,000 over time by residence"),
      yaxis_title = paste(indicator_name, "rate\n per 100,000 population"),
      source = paste("Source:", source)
    )

  percentage_change_areas <- indicator_areas %>%
    filter(financial_year %in% c(min_fin_year, max_fin_year)) %>%
    dplyr::select(
      financial_year,
      hb2019name,
      hscp2019name,
      hscp_locality,
      location,
      level,
      rate
    ) %>%
    pivot_wider(names_from = financial_year, values_from = rate) %>%
    mutate(rate_change = !!sym(max_fin_year) - !!sym(min_fin_year)) %>%
    mutate(perc_change = 100 * (abs(rate_change) / !!sym(min_fin_year))) %>%
    mutate(perc_change = round_half_up(perc_change, digits = 1)) %>%
    filter(
      level == "Locality" &
        location %in% c(LOCALITY, other_locs) |
        level == "HSCP" & location == HSCP |
        level == "HB" & location == HB |
        level == "Scotland"
    ) %>%

    mutate(
      text = case_when(
        level == "Locality" ~ paste0(
          "the ",
          str_to_lower(indicator_name),
          " rate ",
          if_else(
            denominator_number != 100000 | denominator_name != "population",
            paste0(
              "per ",
              format(denominator_number, big.mark = ",", scientific = FALSE),
              " "
            ),
            ""
          ),
          if_else(
            denominator_number != 100000 | denominator_name != "population",
            paste0(denominator_name, " "),
            ""
          ),
          "in the ",
          location,
          " ",
          level,
          " for ",
          max_fin_year,
          " is ",
          format(!!sym(max_fin_year), big.mark = ","),
          ", ",
          get_article(perc_change),
          " ",
          perc_change,
          "% ",
          word_change_calc(!!sym(max_fin_year), !!sym(min_fin_year)),
          " since ",
          min_fin_year
        ),

        level %in% c("HSCP", "HB") ~ paste0(
          "The ",
          location,
          " ",
          level,
          " rate is ",
          format(!!sym(max_fin_year), big.mark = ","),
          ", ",
          get_article(perc_change),
          " ",
          perc_change,
          "% ",
          word_change_calc(!!sym(max_fin_year), !!sym(min_fin_year)),
          " since ",
          min_fin_year
        ),

        level == "Scotland" ~ paste0(
          "the ",
          location,
          " rate is ",
          format(!!sym(max_fin_year), big.mark = ","),
          ", ",
          get_article(perc_change),
          " ",
          perc_change,
          "% ",
          word_change_calc(!!sym(max_fin_year), !!sym(min_fin_year)),
          " since ",
          min_fin_year
        ),

        TRUE ~ NA
      )
    ) %>%
    filter(
      (level == "Locality" & location == LOCALITY) |
        level %in% c("HSCP", "HB", "Scotland")
    )

  indicator_paragraph_area <- paste0(
    filter(percentage_change_areas, level == "Locality")$text,
    ". ",
    filter(percentage_change_areas, level == "HSCP")$text,
    ". ",
    filter(percentage_change_areas, level == "HB")$text,
    " and ",
    filter(percentage_change_areas, level == "Scotland")$text,
    "."
  )

  indicator_paragraph_area <- gsub("a & e", "A & E", indicator_paragraph_area)

  if (indicator_name == "Potentially Preventable Admissions (PPA)") {
    indicator_paragraph_area <- paste0(
      toupper(substring(indicator_paragraph_area, 1, 1)),
      substring(indicator_paragraph_area, 2, nchar(indicator_paragraph_area))
    ) # Make first letter a capital

    indicator_paragraph_area <- gsub(
      "potentially preventable admissions \\(ppa\\)",
      "PPA",
      indicator_paragraph_area
    )
  }

  current_locality_rate_area <- indicator_areas %>%
    filter(financial_year == max_fin_year) %>%
    filter(level == "Locality" & hscp_locality == LOCALITY) %>%
    pull(rate)

  current_scotland_rate_area <- indicator_areas %>%
    filter(financial_year == max_fin_year) %>%
    filter(level == "Scotland") %>%
    pull(rate)

  # All Relevant Areas ----

  current_all_areas <- indicator_areas %>%
    filter(
      level == "Locality" &
        location == LOCALITY |
        level == "Locality" & location %in% other_locs |
        level == "HSCP" & location == HSCP |
        level == "HB" & location == HB |
        level == "Scotland"
    ) %>%
    filter(financial_year == max_fin_year)

  # Plotting by age

  intro_paragraph_age <- paste0(
    "presents the ",
    str_to_lower(indicator_name),
    " rate ",
    paste0(
      "per ",
      format(denominator_number, big.mark = ",", scientific = FALSE),
      " "
    ),
    paste0(denominator_name, " "),
    "in the ",
    LOCALITY,
    " locality from ",
    min_fin_year,
    " to ",
    max_fin_year,
    " by age group."
  )

  intro_paragraph_age <- gsub("a & e", "A & E", intro_paragraph_age)

  indicator_age <- dataset %>%
    filter(hscp_locality == LOCALITY & level == "Locality") %>%
    dplyr::select(-pop, -contains("rate")) %>%
    left_join(
      populations_age,
      by = c(
        "year",
        "financial_year",
        "hb2019name",
        "hscp2019name",
        "hscp_locality",
        "age_group"
      )
    ) %>%
    mutate(rate = round_half_up(100000 * ({{ indicator_column }} / pop))) %>%
    dplyr::select(
      financial_year,
      year,
      hb2019name,
      hscp2019name,
      hscp_locality,
      age_group,
      level,
      {{ indicator_column }},
      pop,
      rate
    )

  if (indicator_name == "Potentially Preventable Admissions (PPA)") {
    ppa_agebanded <- indicator_age %>%
      mutate(
        age_group_banded = case_when(
          age_group %in% c("0 - 17", "18 - 44", "45 - 64") ~ "Under 65",
          age_group %in% c("65 - 74", "75+") ~ "65+"
        )
      ) %>%
      summarise(
        {{ indicator_column }} := sum({{ indicator_column }}),
        pop = sum(pop),
        .by = c(
          "year",
          "financial_year",
          "hb2019name",
          "hscp2019name",
          "hscp_locality",
          "level",
          "age_group_banded"
        )
      ) %>%
      group_by(
        year,
        financial_year,
        hb2019name,
        hscp2019name,
        hscp_locality,
        level
      ) %>%
      mutate(
        perc = round_half_up(
          100 * ({{ indicator_column }} / sum({{ indicator_column }})),
          1
        )
      ) %>%
      ungroup()

    under_65_perc <- ppa_agebanded %>%
      filter(financial_year == max_fin_year) %>%
      filter(age_group_banded == "Under 65") %>%
      pull(perc)

    over_65_perc <- ppa_agebanded %>%
      filter(financial_year == max_fin_year) %>%
      filter(age_group_banded == "65+") %>%
      pull(perc)
  }

  indicator_age_ts <- indicator_age %>%
    age_group_trend_usc(
      measure = "rate",
      plot_title = paste(
        indicator_name,
        "per 100,000 over time by age group\n for",
        LOCALITY
      ),
      yaxis_title = paste(indicator_name, "rate\n per 100,000 population"),
      source = paste("Source:", source)
    )

  percentage_change_age <- indicator_age %>%
    filter(financial_year %in% c(min_fin_year, max_fin_year)) %>%
    dplyr::select(
      financial_year,
      hb2019name,
      hscp2019name,
      hscp_locality,
      level,
      age_group,
      rate
    ) %>%
    pivot_wider(names_from = financial_year, values_from = rate) %>%
    mutate(rate_change = !!sym(max_fin_year) - !!sym(min_fin_year)) %>%
    mutate(perc_change = 100 * (abs(rate_change) / !!sym(min_fin_year))) %>%
    mutate(perc_change = round_half_up(perc_change, digits = 1)) %>%

    mutate(
      rate_ranking = case_when(
        !!sym(max_fin_year) == max(!!sym(max_fin_year)) ~ "Highest",
        !!sym(max_fin_year) == min(!!sym(max_fin_year)) ~ "Lowest",
        TRUE ~ "Other"
      )
    ) %>%

    mutate(
      text = case_when(
        rate_ranking == "Highest" ~ paste0(
          "the highest ",
          str_to_lower(indicator_name),
          " rate for the ",
          hscp_locality,
          " locality in ",
          max_fin_year,
          " is ",
          format(!!sym(max_fin_year), big.mark = ","),
          " ",
          if_else(
            denominator_number != 100000 | denominator_name != "population",
            paste0(
              "per ",
              format(denominator_number, big.mark = ",", scientific = FALSE),
              " "
            ),
            ""
          ),
          if_else(
            denominator_number != 100000 | denominator_name != "population",
            paste0(denominator_name, " "),
            ""
          ),
          "for the ",
          age_group,
          " age group with ",
          get_article(perc_change),
          " percentage ",
          word_change_calc(!!sym(max_fin_year), !!sym(min_fin_year)),
          " of ",
          perc_change,
          "% since ",
          min_fin_year
        ),

        rate_ranking == "Lowest" ~ paste0(
          "The lowest ",
          str_to_lower(indicator_name),
          " rate for ",
          hscp_locality,
          " in ",
          max_fin_year,
          " is ",
          format(!!sym(max_fin_year), big.mark = ","),
          " per 100,000 population for the ",
          age_group,
          " age group with ",
          get_article(perc_change),
          " percentage ",
          word_change_calc(!!sym(max_fin_year), !!sym(min_fin_year)),
          " of ",
          perc_change,
          "% since ",
          min_fin_year
        ),

        TRUE ~ NA
      )
    )

  indicator_paragraph_age <- paste0(
    filter(percentage_change_age, rate_ranking == "Highest")$text,
    ". ",
    filter(percentage_change_age, rate_ranking == "Lowest")$text,
    "."
  )

  indicator_paragraph_age <- gsub("a & e", "A & E", indicator_paragraph_age)

  output_list <- list(
    min_fin_year = min_fin_year,
    max_fin_year = max_fin_year,

    intro_paragraph_area = intro_paragraph_area,
    area_data = indicator_areas,
    area_ts = indicator_loc_ts,
    area_text = indicator_paragraph_area,

    current_locality_rate_area = current_locality_rate_area,
    current_scotland_rate_area = current_scotland_rate_area,
    current_all_areas = current_all_areas,

    intro_paragraph_age = intro_paragraph_age,
    age_data = indicator_age,
    age_ts = indicator_age_ts,
    age_text = indicator_paragraph_age
  )

  if (indicator_name == "Potentially Preventable Admissions (PPA)") {
    output_list["under_65_perc"] <- under_65_perc

    output_list["over_65_perc"] <- over_65_perc
  }

  return(output_list)
}


scotpho_unscheduled_care_charts_and_text <- function(
  dataset,
  indicator_name,
  denominator_number,
  denominator_name,
  xaxis_title,
  yaxis_title,
  LOCALITY,
  other_locs,
  HSCP,
  HB,
  sdc_applied
) {
  min_period <- dataset %>%
    filter(year == max(min(year), max(year) - 10)) %>%
    pull(period) %>%
    unique()

  max_period <- dataset %>%
    filter(year == max(year)) %>%
    pull(period) %>%
    unique()

  min_period_for_text <- min_period %>%
    gsub(" financial years; 3-year aggregates", "", .) %>%
    gsub("to", "-", .)

  max_period_for_text <- max_period %>%
    gsub(" financial years; 3-year aggregates", "", .) %>%
    gsub("to", "-", .)

  aggregate_3_year_indicator <- grepl("3-year aggregates", max_period)

  indicator_intro <- paste0(
    "the ",
    str_to_lower(indicator_name),
    " ",
    case_when(
      aggregate_3_year_indicator ~ "3-year aggregate ",
      TRUE ~ ""
    ),
    "rate per ",
    paste0(format(denominator_number, big.mark = ",", scientific = FALSE), " "),
    paste0(denominator_name, " "),
    "in the ",
    LOCALITY,
    " locality from ",
    min_period_for_text,
    " to ",
    max_period_for_text,
    "."
  )

  ## Time trend
  indicator_time_trend <- dataset %>%
    scotpho_time_trend(
      data = .,
      chart_title = paste(indicator_name, "Time Trend"),
      xaxis_title = xaxis_title,
      yaxis_title = yaxis_title,
      string_wrap = 10,
      rotate_xaxis = TRUE
    )

  percentage_change_data <- dataset %>%
    filter(area_name %in% c("Scotland", HB, HSCP, LOCALITY)) %>%
    filter(period %in% c(min_period, max_period)) %>%
    dplyr::select(area_code, area_type, area_name, period, measure) %>%
    pivot_wider(names_from = "period", values_from = measure) %>%
    mutate(rate_change = !!sym(max_period) - !!sym(min_period)) %>%
    mutate(perc_change = 100 * (abs(rate_change) / !!sym(min_period))) %>%
    mutate(perc_change = round_half_up(perc_change, digits = 1)) %>%

    mutate(
      text = case_when(
        area_type == "Locality" ~ paste0(
          "the ",
          case_when(
            aggregate_3_year_indicator ~ "3-year aggregate ",
            TRUE ~ ""
          ),
          str_to_lower(indicator_name),
          " rate per ",
          paste0(
            format(denominator_number, big.mark = ",", scientific = FALSE),
            " "
          ),
          paste0(denominator_name, " "),
          "in the ",
          area_name,
          " ",
          area_type,
          " for ",
          max_period_for_text,
          " is ",
          format(!!sym(max_period), big.mark = ","),
          ", ",
          get_article(perc_change),
          " ",
          perc_change,
          "% ",
          word_change_calc(!!sym(max_period), !!sym(min_period)),
          " since ",
          min_period_for_text
        ),

        area_type %in% c("HSCP") ~ paste0(
          "the ",
          area_name,
          " ",
          area_type,
          " rate is ",
          format(!!sym(max_period), big.mark = ","),
          ", ",
          get_article(perc_change),
          " ",
          perc_change,
          "% ",
          word_change_calc(!!sym(max_period), !!sym(min_period)),
          " since ",
          min_period_for_text
        ),

        area_type %in% c("Health board") ~ paste0(
          "For the ",
          area_name,
          " ",
          area_type,
          " the 3-year aggregate rate for ",
          max_period_for_text,
          " is ",
          format(!!sym(max_period), big.mark = ","),
          ", ",
          get_article(perc_change),
          " ",
          perc_change,
          "% ",
          word_change_calc(!!sym(max_period), !!sym(min_period)),
          " since ",
          min_period_for_text
        ),

        area_type == "Scotland" ~ paste0(
          "the ",
          area_name,
          " ",
          case_when(
            aggregate_3_year_indicator ~ "3-year aggregate ",
            TRUE ~ ""
          ),
          "rate is ",
          format(!!sym(max_period), big.mark = ","),
          ", ",
          get_article(perc_change),
          " ",
          perc_change,
          "% ",
          word_change_calc(!!sym(max_period), !!sym(min_period)),
          " since ",
          min_period_for_text
        )
      )
    )

  current_locality_rate_area <- dataset %>%
    filter(period == max_period) %>%
    filter(area_type == "Locality" & area_name == LOCALITY) %>%
    pull(measure)

  current_scotland_rate_area <- dataset %>%
    filter(period == max_period) %>%
    filter(area_type == "Scotland") %>%
    pull(measure)

  current_all_areas <- dataset %>%
    filter(
      area_type == "Locality" &
        area_name == LOCALITY |
        area_type == "Locality" & area_name %in% other_locs |
        area_type == "HSCP" & area_name == HSCP |
        area_type == "Health board" & area_name == HB |
        area_type == "Scotland"
    ) %>%
    filter(period == max_period)

  indicator_paragraph <- paste0(
    filter(percentage_change_data, area_type == "Locality")$text,
    " and ",
    filter(percentage_change_data, area_type == "HSCP")$text,
    ". ",
    filter(percentage_change_data, area_type == "Health board")$text,
    " and ",
    filter(percentage_change_data, area_type == "Scotland")$text,
    "."
  )

  return(
    list(
      current_locality_rate_area = current_locality_rate_area,
      current_scotland_rate_area = current_scotland_rate_area,
      current_all_areas = current_all_areas,

      intro_paragraph = indicator_intro,
      indicator_ts = indicator_time_trend,
      indicator_paragraph = indicator_paragraph
    )
  )
}

# 1. Emergency Admissions ----
# _________________________________________________________________________

# 1. Load In Data ----

emergency_adm <- read_parquet(paste0(
  import_folder,
  "emergency_admissions_msg.parquet"
)) %>%
  mutate(level = "Locality") %>%
  filter(financial_year <= max_fy) %>%
  left_join(
    populations_filtered,
    by = c(
      "financial_year",
      "hscp2019name",
      "hscp_locality",
      "age_group",
      "level"
    )
  ) %>%
  mutate(rate = round_half_up(admissions / pop * 100000)) %>%
  dplyr::select(
    financial_year,
    year,
    hb2019name,
    hscp2019name,
    hscp_locality,
    age_group,
    adm = admissions,
    pop,
    rate,
    level
  )


emergency_adm_outputs <- unscheduled_care_charts_and_text(
  emergency_adm,
  adm,
  "Emergency Admissions",
  100000,
  "population",
  "PHS SMR01",
  populations_filtered_area,
  populations_filtered_age,
  LOCALITY,
  other_locs$hscp_locality,
  HSCP,
  HB
)


# 2a. Unscheduled bed days ----
# _________________________________________________________________________

bed_days <- read_parquet(paste0(import_folder, "bed_days_msg.parquet")) %>%
  filter(financial_year <= max_fy) %>%
  mutate(level = "Locality") %>%
  left_join(
    populations_filtered,
    by = c(
      "financial_year",
      "hscp2019name",
      "hscp_locality",
      "age_group",
      "level"
    )
  ) %>%
  mutate(rate = round_half_up(bed_days / pop * 100000)) %>%
  dplyr::select(
    financial_year,
    year,
    hb2019name,
    hscp2019name,
    hscp_locality,
    age_group,
    bd = bed_days,
    pop,
    rate,
    level
  )


bed_days_outputs <- unscheduled_care_charts_and_text(
  bed_days,
  bd,
  "Unscheduled Bed Days",
  100000,
  "population",
  "PHS SMR01",
  populations_filtered_area,
  populations_filtered_age,
  LOCALITY,
  other_locs$hscp_locality,
  HSCP,
  HB
)

# 2b. Unscheduled bed days - Mental Health ----
# _________________________________________________________________________

bed_days_mh <- read_parquet(paste0(
  import_folder,
  "bed_days_mh_msg.parquet"
)) %>%
  mutate(level = "Locality") %>%
  filter(financial_year <= max_fy) %>%
  left_join(
    populations_filtered,
    by = c(
      "financial_year",
      "hscp2019name",
      "hscp_locality",
      "age_group",
      "level"
    )
  ) %>%
  mutate(rate = round_half_up(bed_days / pop * 100000)) %>%
  dplyr::select(
    financial_year,
    year,
    hb2019name,
    hscp2019name,
    hscp_locality,
    age_group,
    bd = bed_days,
    pop,
    rate,
    level
  )


bed_days_mh_outputs <- unscheduled_care_charts_and_text(
  bed_days_mh,
  bd,
  "Unscheduled Mental Health Bed Days",
  100000,
  "population",
  "PHS SMR04",
  populations_filtered_area,
  populations_filtered_age,
  LOCALITY,
  other_locs$hscp_locality,
  HSCP,
  HB
)


# 3. A&E Attendances ----
# _________________________________________________________________________

ae_attendances <- read_parquet(paste0(
  import_folder,
  "ae_attendances_msg.parquet"
)) %>%
  filter(age_group != "NA") %>%
  mutate(level = "Locality") %>%
  filter(financial_year <= max_fy) %>%
  left_join(
    populations_filtered,
    by = c(
      "financial_year",
      "hscp2019name",
      "hscp_locality",
      "age_group",
      "level"
    )
  ) %>%
  mutate(rate = round_half_up(attendances / pop * 100000)) %>%
  dplyr::select(
    financial_year,
    year,
    hb2019name,
    hscp2019name,
    hscp_locality,
    age_group,
    att = attendances,
    pop,
    rate,
    level
  )

ae_attendances_outputs <- unscheduled_care_charts_and_text(
  ae_attendances,
  att,
  "A & E Attendances",
  100000,
  "population",
  "PHS A&E Datamart",
  populations_filtered_area,
  populations_filtered_age,
  LOCALITY,
  other_locs$hscp_locality,
  HSCP,
  HB
)


# 4. Delayed Discharges ----
# _________________________________________________________________________

delayed_disch <- read_parquet(paste0(
  import_folder,
  "delayed_discharges_msg.parquet"
)) %>%
  filter(financial_year <= max_fy) %>%
  filter(age_group %in% c("65 - 74", "75+")) %>%
  group_by(financial_year, hscp2019name, hscp_locality, age_group) %>%
  summarise(
    dd_people = sum(dd_people),
    dd_bed_days = sum(dd_bed_days)
  ) %>%
  ungroup() %>%
  mutate(level = "Locality") %>%
  left_join(
    populations_filtered,
    by = c(
      "financial_year",
      "hscp2019name",
      "hscp_locality",
      "age_group",
      "level"
    )
  ) %>%
  filter(!is.na(year)) %>%
  mutate(dd_bd_rate = round_half_up(dd_bed_days / pop * 100000)) %>%
  mutate(dd_ppl_rate = round_half_up(dd_people / pop * 100000)) %>%
  dplyr::select(
    financial_year,
    year,
    hb2019name,
    hscp2019name,
    hscp_locality,
    level,
    age_group,
    dd_ppl = dd_people,
    dd_bd = dd_bed_days,
    dd_bd_rate,
    dd_ppl_rate,
    pop
  )


delayed_discharges_outputs <- unscheduled_care_charts_and_text(
  delayed_disch,
  dd_bd,
  "Delayed Discharge Bed Days",
  100000,
  "population aged over 65+",
  "PHS Delayed Discharges",
  populations_filtered_area,
  populations_filtered_age,
  LOCALITY,
  other_locs$hscp_locality,
  HSCP,
  HB
)

# 5. Fall Admissions ----
# _________________________________________________________________________

falls <- read_parquet(paste0(import_folder, "falls_smr.parquet")) %>%
  filter(financial_year <= max_fy) %>%
  filter(age_group %in% c("65 - 74", "75+")) %>%
  mutate(level = "Locality") %>%
  left_join(
    populations_filtered,
    by = c(
      "financial_year",
      "hscp2019name",
      "hscp_locality",
      "age_group",
      "level"
    )
  ) %>%
  filter(!is.na(year)) %>%
  mutate(adm_rate = round_half_up(admissions / pop * 100000)) %>%
  dplyr::select(
    financial_year,
    year,
    hb2019name,
    hscp2019name,
    hscp_locality,
    level,
    age_group,
    adm = admissions,
    adm_rate,
    pop
  )


falls_outputs <- unscheduled_care_charts_and_text(
  falls,
  adm,
  "Emergency Admissions From Falls",
  100000,
  "population aged over 65+",
  "PHS SMR01",
  populations_filtered_area,
  populations_filtered_age,
  LOCALITY,
  other_locs$hscp_locality,
  HSCP,
  HB
)


# 6. Readmissions (28 days) ----
# _________________________________________________________________________

readmissions <- read_parquet(paste0(
  import_folder,
  "readmissions_smr.parquet"
)) %>%
  filter(financial_year <= max_fy) %>%
  mutate(level = "Locality") %>%
  left_join(
    populations_filtered,
    by = c(
      "financial_year",
      "hscp2019name",
      "hscp_locality",
      "age_group",
      "level"
    )
  ) %>%
  filter(!is.na(year)) %>%
  mutate(
    dd_rate = round_half_up(discharges / pop * 100000),
    read_28_rate = round_half_up(read_28 / pop * 100000)
  ) %>%
  dplyr::select(
    financial_year,
    year,
    hb2019name,
    hscp2019name,
    hscp_locality,
    level,
    age_group,
    dd = discharges,
    read_28,
    dd_rate,
    read_28_rate,
    pop
  )

readmissions_outputs <- unscheduled_care_charts_and_text(
  readmissions,
  dd,
  "Readmissions (28 days) ",
  1000,
  "discharges",
  "PHS SMR01",
  populations_filtered_area,
  populations_filtered_age,
  LOCALITY,
  other_locs$hscp_locality,
  HSCP,
  HB
)

# 7. Potentially Preventable Admissions ----
# _______________________________________________________________________________________________________

ppa <- read_parquet(paste0(import_folder, "ppa_smr.parquet")) %>%
  filter(financial_year <= max_fy) %>%
  mutate(level = "Locality") %>%
  left_join(
    populations_filtered,
    by = c(
      "financial_year",
      "hscp2019name",
      "hscp_locality",
      "age_group",
      "level"
    )
  ) %>%
  mutate(adm_rate = round_half_up(admissions / pop * 100000)) %>%
  dplyr::select(
    financial_year,
    year,
    hb2019name,
    hscp2019name,
    hscp_locality,
    level,
    age_group,
    adm = admissions,
    adm_rate,
    pop
  )

ppa_outputs <- unscheduled_care_charts_and_text(
  ppa,
  adm,
  "Potentially Preventable Admissions (PPA)",
  100000,
  "population",
  "PHS SMR01",
  populations_filtered_area,
  populations_filtered_age,
  LOCALITY,
  other_locs$hscp_locality,
  HSCP,
  HB
)


# 8. Psychiatric hospital admissions (ScotPHO) ----
# ___________________________________________________________________________

psych_hosp <- read_csv(paste0(
  import_folder,
  "scotpho_data_extract_psychiatric_admissions.csv"
)) %>%
  clean_scotpho_dat() %>%
  mutate(period_short = gsub("to", "-", substr(period, 1, 18), fixed = TRUE))

check_missing_data_scotpho(psych_hosp)


psych_hosp_outputs <- scotpho_unscheduled_care_charts_and_text(
  psych_hosp,
  "Psychiatric Patient Hospitalisations",
  100000,
  "population",
  "Financial Year Groups (3-year aggregates)",
  "Psychiatric Patient Hospitalisations\n(Standardised Rates Per 100,000)",
  LOCALITY,
  other_locs$hscp_locality,
  HSCP,
  HB,
  TRUE
)
