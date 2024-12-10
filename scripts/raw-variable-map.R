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

# import data
mgwr_data <- import(here('output-data', 'mgwr-data-raw.csv'))
mgwr_data <- add_fips_padding(df = mgwr_data)

us_counties <- counties(cb = TRUE, year = 2021, class = "sf")

us_states <- states(cb = TRUE, year = 2021, class = "sf") %>%
  st_transform(crs = 5070) %>% 
  filter(STUSPS %nin% c('AK', 'HI', 'DC', 'MP', 'VI', 'PR', 'GU', 'AS'))

# combine mgwr with geometry
df_merged <- mgwr_data %>% 
  left_join(us_counties, by = c("fips_cd" = "GEOID"))

df_merged <- st_as_sf(df_merged)

income_breaks <- c(0, 30000, 45000, 60000, 75000, 90000, Inf)
income_labels <- c("< $30k", "$30k - $45k", "$45k - $60k", "$60k - $75k", "$75k - $90k", "> $90k")

p1 <- ggplot() +
  geom_sf(data = df_merged, aes(fill = cut(med_income21, breaks = income_breaks, labels = income_labels)), color = 'black') +
  geom_sf(data = us_states, fill = NA, color = "white", size = 0.2) +
  scale_fill_viridis_d(option = "mako", na.value = "grey80", name = "Median Income", direction = -1) +
  labs(
    # title = "Median Household Income by U.S. County (2021)",
    # caption = "Data Source: U.S. Department of Agriculture: Economic Research Service"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(t = 10, b = 20)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 10)),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.position = c(0.90, 0.2),  # Move legend to bottom-right
    legend.justification = c("right", "bottom"),  # Anchor legend at its bottom-right corner
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


p1


ggsave(
  filename = paste0(here('images', 'median-income21.png')),
  plot = p1,
  dpi = 600,
  width = 16,
  height = 8,
  bg = 'white'
)


p2 <- ggplot() +
  geom_sf(data = df_merged, aes(fill = pct_bach_higher_18_22), color = 'black') +
  geom_sf(data = us_states, fill = NA, color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "mako", na.value = "grey80", 
                       name = "Bachelor's Degree (%)", 
                       direction = -1,
                       limits = c(0, 78.9),  # Based on summary
                       breaks = seq(0, 80, by = 10)) +
  labs(
    # title = "Bachelor's Degree or Higher by U.S. County (2021)",
    # caption = "Data Source: American Community Survey 5-year estimates (2017-2021)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(t = 10, b = 20)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 10)),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.position = c(0.90, 0.2),
    legend.justification = c("right", "bottom"),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

p2

ggsave(
  filename = paste0(here('images', 'bach-higher-18-22.png')),
  plot = p2,
  dpi = 600,
  width = 16,
  height = 8,
  bg = 'white'
)
