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