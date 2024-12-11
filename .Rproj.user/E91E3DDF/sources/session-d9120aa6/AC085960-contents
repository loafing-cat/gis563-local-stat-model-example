rm(list = ls()); gc()

library(tidyverse)
library(rio)
library(here)
library(sf)
library(spdep)
library(tigris)
library(colorspace)
# library(knitr)
# library(kableExtra)
# library(stargazer)
# library(ggfortify)

options(tigris_use_cache = TRUE)

`%nin%` = Negate(`%in%`)

# import helper functions
source(here::here('scripts', 'fip_cd_check.R'))
source(here::here('scripts', 'fip_padder.R'))

foth_voting2020 <- import(here('input-data', 'processed-data', 'voting-2020-fotheringham.csv'))

# import internet access data
internet_access21 <- import(here('input-data', 'processed-data', 'processed-internet-access.csv'))

# import education data
education18_22 <- import(here('input-data', 'processed-data', 'processed-education.csv'))

# import urban data
urban20 <- import(here('input-data', 'processed-data', 'processed-urban-area.csv'))

# import poverty data
poverty21 <- import(here('input-data', 'processed-data', 'processed-poverty-estimates.csv'))

# import acs demographic data
acs17_21_dem <- import(here('input-data', 'processed-data', 'processed-acs17-21-dem-pop.csv'))

# import acs gini data
acs17_21_gini <- import(here('input-data', 'processed-data', 'processed-acs17-21-gini.csv'))

# process `fips_cd` columns by padding the left of the digits; proper fip codes are 
foth_voting2020 <- add_fips_padding(df = foth_voting2020)
internet_access21 <- add_fips_padding(df = internet_access21)
education18_22 <- add_fips_padding(df = education18_22)
urban20 <- add_fips_padding(df = urban20)
poverty21 <- add_fips_padding(df = poverty21)
acs17_21_dem <- add_fips_padding(df = acs17_21_dem)
acs17_21_gini <- add_fips_padding(df = acs17_21_gini)

detect_invalid_fips(df = foth_voting2020)
detect_invalid_fips(df = internet_access21)
detect_invalid_fips(df = education18_22)
detect_invalid_fips(df = urban20)
detect_invalid_fips(df = poverty21)
detect_invalid_fips(df = acs17_21_dem)
detect_invalid_fips(df = acs17_21_gini)

mgwr_data <- foth_voting2020 %>% 
  inner_join(internet_access21, by = 'fips_cd') %>% 
  inner_join(education18_22, by = 'fips_cd') %>% 
  inner_join(poverty21, by = 'fips_cd') %>%
  inner_join(urban20, by = 'fips_cd') %>% 
  inner_join(acs17_21_dem, by = 'fips_cd') %>% 
  inner_join(acs17_21_gini, by = 'fips_cd') %>% 
  select(
    fips_cd, county_name.x, state_cd.x, proj_X, proj_Y, med_income21, gini, ln_pop_den, #pct_internet_access21,
    pct_bach_higher_18_22, pct_pop_urban20, sex_ratio17_21,
    median_age17_21, pct_blck17_21, pct_hisplat17_21
  ) %>% 
  rename(county_name = county_name.x, state_cd = state_cd.x)

names(mgwr_data)

# standardize data
mgwr_data2 <- mgwr_data %>% 
  mutate(across(
    -c(fips_cd, county_name, state_cd, proj_X, proj_Y), # Exclude these columns
    ~ scale(.) %>% as.numeric()                         # Standardize
  ))

# export(mgwr_data, here('output-data', 'mgwr-data-raw.csv'))
# export(mgwr_data2, here('output-data', 'mgwr-data-standardized.csv'))