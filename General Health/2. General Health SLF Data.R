#############################################################################################
#                                                                                           #
#                     LOCALITY PROFILES GENERAL HEALTH SLF DATA CODE                        #
#                                                                                           #
#############################################################################################

## Code used to extract long-term conditions data from Source Linkage Files

## Created by C.Puech
## Original date 10/01/2020
## Latest update August 2022 - rewrote parts of code for smoother process

# Packages
library(fst)
library(tidyverse)
library(readxl)

# Set year for data extracts folder for saving
ext_year <- 2022

# Set financial year to use for SLFs (format ex: for FY 2021/2022 -> 202122)
# Recommended to use previous year's data for more up to date figures + pop
fy <- "202021"

# Set file path
lp_path <- "/conf/LIST_analytics/West Hub/02 - Scaled Up Work/RMarkdown/Locality Profiles/"

# Source in functions code
source("Master RMarkdown Document & Render Code/Global Script.R")


### Geographical lookups and objects ----

# Locality lookup
lookup <- read_in_localities(dz_level = T)

# Read in SLF individual level file
slf <- read.fst(paste0(
  "/conf/hscdiip/01-Source-linkage-files/",
  "source-individual-file-", fy, ".fst"
)) %>%
  select(-locality, -hscp2019, -hb2019)

# Compute total LTCs
slf$"total_ltc" <- rowSums(subset(slf, select = arth:refailure))

# compute age band
# remove -1 age
slf <- filter(slf, age >= 0)
slf$age_group <- case_when(
  slf$age < 65 ~ "Under 65",
  between(slf$age, 65, 74) ~ "65-74",
  between(slf$age, 75, 84) ~ "75-84",
  slf$age >= 85 ~ "85+"
)

# Aggregate to Locality level

ltc_agg <- slf %>%
  left_join(lookup, by = "datazone2011") %>%
  # drop_na(Locality) %>%
  group_by(year, hscp2019name, hscp_locality, age_group, total_ltc) %>%
  summarise(
    arth = sum(arth),
    asthma = sum(asthma),
    atrialfib = sum(atrialfib),
    cancer = sum(cancer),
    cvd = sum(cvd),
    liver = sum(liver),
    copd = sum(copd),
    dementia = sum(dementia),
    diabetes = sum(diabetes),
    epilepsy = sum(epilepsy),
    chd = sum(chd),
    hefailure = sum(hefailure),
    ms = sum(ms),
    parkinsons = sum(parkinsons),
    refailure = sum(refailure),
    # congen = sum(congen),
    # bloodbfo = sum(bloodbfo),
    # endomet = sum(endomet),
    # digestive = sum(digestive),
    people = n()
  ) %>%
  ungroup()


#### LTC POPULATIONS ####

## Using adjusted population from SLFs for closer estimates to true population

slf_pops <- slf %>%
  group_by(year, datazone2011, age_group) %>%
  summarise(population = sum(keep_population)) %>%
  ungroup()

slf_pops <- left_join(slf_pops, lookup) %>%
  group_by(year, hscp_locality, hscp2019name, age_group) %>%
  summarise(slf_adj_pop = sum(population)) %>%
  ungroup()


#### JOIN DATASETS ####

ltc_data <- left_join(ltc_agg, slf_pops)

#### SAVE DATA ####

saveRDS(ltc_data, file = paste0(
  lp_path, "General Health/DATA ",
  ext_year, "/LTC_from_SLF.RDS"
))
