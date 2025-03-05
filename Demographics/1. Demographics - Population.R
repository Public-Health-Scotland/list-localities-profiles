##################### LOCALITY PROFILES DEMOGRAPHICS: POPULATION ######################.

### First Created: 08/08/2019
### Original Author: Aidan Morrison

### Written for: RStudio Server Pro, R Version 3.6.1

### Description: The purpose of this code is to produce outputs on population to be
###              used for LIST locality profiles produced in RMarkdown.

### Revised Oct/Nov 2022 by Craig Fraser and Luke Taylor for smoother process, ex:

# Incorporated lookup functions so less dependent on static files

####################### SECTION 1: Packages, file paths, etc #########################

## Libraries
library(scales)
library(reshape2)



## File path
#filepath <- paste0(lp_path, "RMarkdown/Locality Profiles/Demographics/")


## Final document will loop through a list of localities
# Create placeholder for for loop
# LOCALITY <- "Inverness"
# LOCALITY <- "Stirling City with the Eastern Villages Bridge of Allan and Dunblane"
# LOCALITY <- "Ayr North and Former Coalfield Communities"


########################## SECTION 2: Data Imports ###############################

## Locality/DZ lookup
# lookup <- read_in_localities() : defined in render code script

## Population data
pop_raw_data <- read_in_dz_pops()

## Population Projection Data
hscp_pop_proj <- read_in_pop_proj()

## Set year
pop_max_year <- max(pop_raw_data$year)
pop_min_year <- pop_max_year - 5



######################## SECTION 3: Gender and Age #############################

## Population data manipulation

# compute age bands
pop_raw_data$"Pop0_4" <- rowSums(subset(pop_raw_data, select = age0:age4))
pop_raw_data$"Pop5_17" <- rowSums(subset(pop_raw_data, select = age5:age17))
pop_raw_data$"Pop18_44" <- rowSums(subset(pop_raw_data, select = age18:age44))
pop_raw_data$"Pop45_64" <- rowSums(subset(pop_raw_data, select = age45:age64))
pop_raw_data$"Pop65_74" <- rowSums(subset(pop_raw_data, select = age65:age74))
pop_raw_data$"Pop75_84" <- rowSums(subset(pop_raw_data, select = age75:age84))
pop_raw_data$"Pop85Plus" <- rowSums(subset(pop_raw_data, select = age85:age90plus))
pop_raw_data$"Pop65Plus" <- rowSums(subset(pop_raw_data, select = age65:age90plus))

pops <- pop_raw_data %>%
  select(
    year, sex, hscp2019name, hscp_locality,
    Pop0_4, Pop5_17, Pop18_44, Pop45_64, Pop65_74,
    Pop75_84, Pop85Plus, Pop65Plus, total_pop
  )


# Aggregate and add partnership + Scotland totals
pops <- pops %>%
  group_by(year, sex, hscp2019name, hscp_locality) %>%
  summarise(across(everything(), sum)) %>%
  ungroup() %>%
  # Add a partnership total
  bind_rows(
    pops %>%
      select(-hscp_locality) %>%
      group_by(year, hscp2019name, sex) %>%
      summarise(across(everything(), sum)) %>%
      ungroup() %>%
      mutate(hscp_locality = "Partnership Total")
  ) %>%
  # Add a Scotland total
  bind_rows(
    pops %>%
      select(-hscp_locality, -hscp2019name) %>%
      group_by(year, sex) %>%
      summarise(across(everything(), sum)) %>%
      ungroup() %>%
      mutate(hscp_locality = "Scotland Total", hscp2019name = "Scotland")
  )



## Gender

gender_breakdown <- 
  map(locality_list, 
      ~pops %>% 
        filter(hscp_locality == .x, year == max(year)) %>% 
        mutate(hscp_locality = .x)
  ) %>%
  bind_rows() %>% 
  select(hscp_locality, sex, total_pop) %>%
  group_by(hscp_locality) %>% 
  mutate(
    total = sum(total_pop),
    perc = paste0(round_half_up(100 * total_pop / total, 1), "%")
  )

## Age & Gender

pop_breakdown <- 
  map(locality_list, 
      ~pops %>%
        filter(hscp_locality == .x, year == max(year)) %>%
        select(-year, -hscp_locality, -total_pop, -hscp2019name, -Pop65Plus) %>%
        mutate(locality = .x)
  ) %>% 
  bind_rows() %>% 
  pivot_longer(-c(sex, locality), names_to = "Age", values_to = "Population") %>% 
  mutate(Age = str_replace(Age, "_", "-"),
         Age = str_replace(Age, "Plus", "+"),
         Age = str_replace(Age, "Pop", "")) %>% 
  dplyr::rename(Gender = sex) %>%
  mutate(Gender = case_when(
    Gender == "M" ~ "Male",
    Gender == "F" ~ "Female")
  ) 


pop_pyramid <- list()

for(loc in locality_list){
  pop_pyramid[[loc]] <- 
    pop_breakdown %>% 
    filter(locality == loc) %>%
    ggplot(aes(y = factor(Age, levels = unique(pop_breakdown$Age)), fill = Gender)) +
    geom_col(
      data = subset(pop_breakdown, Gender == "Male"),
      aes(x = Population)
    ) +
    geom_col(
      data = subset(pop_breakdown, Gender == "Female"),
      aes(x = Population * (-1))
    ) +
    scale_x_continuous(
      labels = abs,
      limits = max(pop_breakdown$Population) * c(-1, 1)
    ) +
    scale_fill_manual(values = palette) +
    theme_profiles() +
    labs(
      x = "Population",
      y = "Age Group",
      title = paste0(str_wrap(`loc`, 50), " population pyramid ", pop_max_year)
    )
}

# Population Structure Changes

hist_pop_breakdown <- 
  pops %>% 
  filter(hscp_locality %in% locality_list, 
         year %in% c(max(year), max(year) - 5)) %>% 
  select(-c(total_pop, hscp2019name, Pop65Plus)) %>%
  pivot_longer(Pop0_4:Pop85Plus, names_to = "Age", values_to = "Population") %>% 
  mutate(Age = str_replace_all(Age, "_", "-"),
         Age = str_replace_all(Age, "Plus", "+"),
         Age = str_replace_all(Age, "Pop", "")) %>%
  dplyr::rename(Gender = sex) %>%
  dplyr::group_by(Gender, Age, hscp_locality) %>%
  arrange(year) %>%
  dplyr::summarise(change = (last(Population) - first(Population)) / first(Population)) %>%
  ungroup() %>%
  mutate(Gender = ifelse(Gender == "F", "Female", "Male"))

ord <- c("0-4", "5-17", "18-44", "45-64", "65-74", "75-84", "85+")

hist_pop_change <- 
  map(locality_list, 
      ~ggplot(hist_pop_breakdown %>% filter(hscp_locality == .x),  
              aes(x = factor(Age, levels = ord), y = change, fill = Gender)) +
        geom_col(position = position_dodge()) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
        scale_y_continuous(labels = scales::percent,
                           limits = c(min(hist_pop_breakdown$change),
                                      max(hist_pop_breakdown$change))) +
        scale_fill_manual(values = palette) +
        theme_profiles() +
        theme(plot.title = element_text(size = 12)) +
        labs(x = "Age Group", 
             y = "Percent Change",
             title = paste("Percent Change in Population \nfrom", pop_max_year - 5,
                           "to", pop_max_year, "by Age and Sex in\n", .x)#,
             #caption = "Source: National Records Scotland"
        )
  )


######################## SECTION 4: Population over time ############################

## 4a) Data wrangling ----

## Trend up to present year
locality_pop_trend <- 
  map(locality_list, ~
        pops %>%
        filter(hscp_locality == .x) %>%
        group_by(year, hscp_locality) %>%
        dplyr::summarise(pop = sum(total_pop)) %>%
        ungroup()
  ) %>% set_names(locality_list)

## Population projections by locality

# current locality populations data breakdown
loc_pops <- pops %>%
  select(-Pop65Plus, -total_pop) %>%
  filter(year == pop_max_year) %>%
  filter(!(hscp_locality %in% c("Partnership Total", "Scotland Total"))) %>%
  reshape2::melt(id.vars = c("year", "sex", "hscp2019name", "hscp_locality")) %>%
  dplyr::rename(age_group = variable) %>%
  as_tibble() %>%
  select(-year)

# hscp population projection data for localities in West Duns
hscp_pop_proj_weight <- hscp_pop_proj %>%
  mutate(age_group = case_when(
    age %in% 0:4 ~ "Pop0_4",
    age %in% 5:17 ~ "Pop5_17",
    age %in% 18:44 ~ "Pop18_44",
    age %in% 45:64 ~ "Pop45_64",
    age %in% 65:74 ~ "Pop65_74",
    age %in% 75:84 ~ "Pop75_84",
    age > 84 ~ "Pop85Plus"
  )) %>%
  # projection until 2028
  filter(year %in% pop_max_year:2028) %>%
  # aggregate to age groups
  group_by(year, hscp2019, hscp2019name, sex, age_group) %>%
  dplyr::summarise(pop = sum(pop)) %>%
  ungroup() %>%
  # change sex variable coding
  mutate(sex = ifelse(sex == 1, "M", "F")) %>%
  # calculate weights
  arrange(hscp2019, sex, age_group, year) %>%
  group_by(hscp2019, sex, age_group) %>%
  mutate(pop_change = if_else(year != pop_max_year, pop / first(pop), 1)) %>%
  ungroup()


## Apply weights to localities
locality_pop_proj <- hscp_pop_proj_weight %>%
  # merge with lookup file
  left_join(lookup, relationship = "many-to-many") %>%
  select(-hscp2019, -pop, -hb2019name, -hb2019) %>%
  # merge with locality populations data
  full_join(loc_pops, by = c("sex", "age_group", "hscp2019name", "hscp_locality")) %>%
  as_tibble() %>%
  ungroup() %>%
  # calculate population projections based on weights
  arrange(hscp2019name, hscp_locality, age_group, sex, year) %>%
  mutate(pop = round_half_up(pop_change * value, 0)) %>%
  select(-value, -pop_change)


pop_proj_dat <- 
  map(locality_list,
      ~locality_pop_proj %>%
        filter(hscp_locality == .x) %>%
        group_by(hscp_locality, year) %>%
        dplyr::summarise(pop = sum(pop)) %>%
        ungroup()
  ) %>% set_names(locality_list)


## 4b) Time trend plot ----

pop_plot_dat <- bind_rows(
  clean_names(mutate(locality_pop_trend %>% bind_rows(), data = "HISTORICAL")),
  clean_names(mutate(pop_proj_dat %>% bind_rows(), data = "PROJECTION"))
) %>%
  mutate(plot_lab = if_else(year %% 2 == 0, format(pop, big.mark = ","), ""))

pop_ts_plot <- 
  map(locality_list,
      ~ggplot(pop_plot_dat %>% filter(hscp_locality == .x), 
              aes(x = year, y = pop)) +
        geom_line(aes(color = data), linewidth = 1) +
        geom_point(color = "#0f243e") +
        geom_text(aes(label = plot_lab),
                  vjust = 2, color = "#4a4a4a", size = 3
        ) +
        scale_x_continuous(breaks = pop_plot_dat$year) +
        scale_y_continuous(labels = comma, limits = c(0, 1.1 * max(pop_plot_dat$pop))) +
        scale_colour_manual(values = palette) +
        theme_profiles() +
        guides(color = guide_legend(title = "")) +
        theme(
          legend.position = "none",
          plot.title = element_text(size = 12),
          axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.5)
        ) +
        labs(
          y = "Population", x = "Year",
          title = paste0("Population Over Time in ", str_wrap(.x, 45)),
          caption = "Source: National Records Scotland"
        )
  )

## 4c) Markdown text outputs ----

## Past trends
pop_graph_text <- list()
pop_proj_text <- list()

for (loc in locality_list){
  # run linear regression to approximate any linear trend in the data
  reg <- lm(data = locality_pop_trend[[loc]], pop ~ year) %>% summary()
  
  # get text to interpret graph
  pval <- reg$coefficients[, 4][2]
  coef <- reg$coefficients[, 1][2]
  
  pop_latest <- locality_pop_trend[[loc]][locality_pop_trend[[loc]]$year == max(locality_pop_trend[[loc]]$year), ]$pop
  pop_last <- locality_pop_trend[[loc]][locality_pop_trend[[loc]]$year == max(locality_pop_trend[[loc]]$year) - 1, ]$pop
  
  # if there is no linear trend, this calculates the year of the last change point
  change_point <- locality_pop_trend[[loc]] %>%
    mutate(
      change = ifelse(lag(pop) > pop, 1, 0),
      change_point = ifelse(lag(change) == change, NA, year - 1)
    ) %>%
    filter(!is.na(change_point)) %>%
    summarise(last(change_point)) %>%
    as.numeric()
  
  change_point <- ifelse(pop_latest == pop_last, "last year", change_point)
  change_point <- ifelse(change_point == max(pops$year) - 1, "last year", change_point)
  
  pop_change <- ifelse(pop_latest > pop_last, "been rising since",
                       ifelse(pop_latest == pop_last, "remained the same as",
                              "been falling since"
                       )
  )
  
  pop_graph_text[[loc]] <- ifelse(pval < 0.05,
                                  
                                  # if the pvalue is less than .05 then return:
                                  paste0(
                                    "For ", loc, ", the population has been ",
                                    
                                    # determine whether its rising or falling:
                                    ifelse(coef < 0, "falling", "rising"),
                                    
                                    # could have trend that has changed in recent years
                                    ifelse(coef < 0 & pop_latest > pop_last,
                                           " in general, however it has risen since last year.",
                                           ifelse(coef > 0 & pop_latest < pop_last,
                                                  " in general, however it has fallen since last year.",
                                                  "."
                                           )
                                    )
                                  ),
                                  
                                  # if the pvalue is not significant then return:
                                  paste0(
                                    paste(
                                      "There is no significant linear trend in population.",
                                      "However, it has", pop_change, change_point
                                    ), "."
                                  )
  ) %>% paste()
  
  ## Pop projection
  pop_proj_change <- 100 * abs(pop_proj_dat[[loc]][1, 2] - pop_proj_dat[[loc]][6, 2]) / pop_proj_dat[[loc]][1, 2]
  pop_proj_change <- round_half_up(pop_proj_change, 1) %>% as.character()
  
  pop_proj_text[[loc]] <- paste(
    "The population in", loc, "is estimated to",
    ifelse(pop_proj_dat[[loc]][1, 2] < pop_proj_dat[[loc]][6, 2],
           paste0("increase by ", pop_proj_change, "%"),
           ifelse(pop_proj_dat[[loc]][1, 2] == pop_proj_dat[[loc]][6, 2],
                  "remain the same",
                  paste0("decrease by ", pop_proj_change, "%")
           )
    ),
    "from ", pop_proj_dat[[loc]][1, 1], " to ", pop_proj_dat[[loc]][6, 1]
  )
}

rm(
  reg, pval, coef, pop_latest, pop_last, change_point, pop_change, pop_proj_change,
  locality_pop_trend, loc_pops, hscp_pop_proj_weight, locality_pop_proj
)


##################### SECTION 5: Objects for summary table #######################

## Relevant lookups for creating the table objects
#HSCP <- as.character(filter(lookup, hscp_locality == HSCP)$hscp2019name)

# Determine other localities based on LOCALITY object
other_locs <- lookup %>%
  select(hscp_locality, hscp2019name) %>%
  filter(hscp2019name == HSCP & hscp_locality != locality_list[1]) %>%
  arrange(hscp_locality)

# Find number of locs per partnership
n_loc <- count_localities(lookup, HSCP)

## Locality objects
total_population <- 
  gender_breakdown %>% 
  select(hscp_locality, total) %>%  
  distinct() %>% 
  group_by(hscp_locality) %>% 
  group_split() %>% 
  set_names(unique(gender_breakdown$hscp_locality)) %>%
  map(., ~select(.x, total) %>% format_number_for_text())

gender_ratio <- 
  map(locality_list,
      ~round_half_up(filter(gender_breakdown, sex == "F", hscp_locality == .x) %>% pull(total_pop) / 
                       filter(gender_breakdown, sex == "M", hscp_locality == .x) %>% pull(total_pop), 2)  
  ) %>% 
  set_names(locality_list)

# modified from PHS code into list
over65 <- map(locality_list, 
              ~round_half_up(sum(filter(pop_breakdown, locality == .x, Age %in% c("65-74", "75-84", "85+"))$Population) 
                             / filter(gender_breakdown, hscp_locality == .x)$total[1] * 100, 1)) %>% 
  set_names(locality_list)


## Other localities in HSCP objects

# total pop
other_locs_total_pop <- pops %>%
  filter(year == max(year)) %>%
  inner_join(other_locs, by = "hscp_locality") %>%
  group_by(hscp_locality) %>%
  summarise(total_pop = sum(total_pop)) %>%
  ungroup() %>%
  mutate(total_pop = format(total_pop, big.mark = ",")) %>%
  arrange(hscp_locality) %>%
  pivot_wider(names_from = hscp_locality, values_from = total_pop)

# gender ratio
other_locs_gender_ratio <- pops %>%
  filter(year == max(year)) %>%
  inner_join(other_locs, by = "hscp_locality") %>%
  select(hscp_locality, sex, total_pop) %>%
  pivot_wider(names_from = sex, values_from = total_pop) %>%
  mutate(ratio = round_half_up(`F` / `M`, 2)) %>%
  mutate(ratio = paste0("1:", ratio)) %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, ratio) %>%
  pivot_wider(names_from = hscp_locality, values_from = ratio)

# over 65 %
other_locs_over65 <- pops %>%
  filter(year == max(year)) %>%
  inner_join(other_locs, by = "hscp_locality") %>%
  group_by(hscp_locality) %>%
  summarise(over65 = sum(Pop65Plus), total_pop = sum(total_pop)) %>%
  mutate(over65_percent = round_half_up(over65 / total_pop * 100, 1)) %>%
  arrange(hscp_locality) %>%
  select(hscp_locality, over65_percent) %>%
  pivot_wider(names_from = hscp_locality, values_from = over65_percent)


## HSCP objects
pop_hscp <- filter(pops, hscp2019name == HSCP, hscp_locality == "Partnership Total", year == max(year))

hscp_total_pop <- sum(pop_hscp$total_pop) %>%
  formatC(format = "d", big.mark = ",")
hscp_gender_ratio <- paste0("1:", round_half_up(filter(pop_hscp, sex == "F")$total_pop / filter(pop_hscp, sex == "M")$total_pop, 2))
hscp_over65 <- pop_hscp %>%
  group_by(hscp2019name) %>%
  summarise(Pop65Plus = sum(Pop65Plus), total_pop = sum(total_pop)) %>%
  mutate(perc_over65 = round_half_up(Pop65Plus / total_pop * 100, 1)) %>%
  pull(perc_over65)


## Scotland objects
pop_scot <- filter(pops, hscp2019name == "Scotland", hscp_locality == "Scotland Total", year == max(year))

scot_total_pop <- sum(pop_scot$total_pop) %>%
  formatC(format = "d", big.mark = ",")
scot_gender_ratio <- paste0("1:", round_half_up(filter(pop_scot, sex == "F")$total_pop / filter(pop_scot, sex == "M")$total_pop, 2))
scot_over65 <- pop_scot %>%
  group_by(hscp2019name) %>%
  summarise(Pop65Plus = sum(Pop65Plus), total_pop = sum(total_pop)) %>%
  mutate(perc_over65 = round_half_up(Pop65Plus / total_pop * 100, 1)) %>%
  pull(perc_over65)

rm(pop_hscp, pop_scot)
