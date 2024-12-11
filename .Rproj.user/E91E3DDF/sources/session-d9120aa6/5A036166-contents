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
library(ggfortify)

options(tigris_use_cache = TRUE)

`%nin%` = Negate(`%in%`)

# import helper functions
source(here::here('scripts', 'fip_cd_check.R'))
source(here::here('scripts', 'fip_padder.R'))

# import mgwr results
mgwr_results <- import(here('mgwr-wo-monte-carlo', 'MGWR_session_results.csv'))

# preprocess with helper functions
mgwr_results <- add_fips_padding(df = mgwr_results)
mgwr_results <- mgwr_results %>% 
  mutate(fips_cd = as.character(fips_cd))


# import geometries
us_counties <- counties(cb = TRUE, year = 2021, class = "sf")
us_counties <- us_counties %>% 
  select(GEOID, NAMELSAD, STUSPS, geometry)

us_states <- states(cb = TRUE, year = 2021, class = "sf") %>%
  st_transform(crs = 5070) %>% 
  filter(STUSPS %nin% c('AK', 'HI', 'DC', 'MP', 'VI', 'PR', 'GU', 'AS'))

# combine mgwr results with geometry and cast to sf object
df_merged <- mgwr_results %>% 
  left_join(us_counties, by = c('fips_cd' = 'GEOID'))

df_merged <- st_as_sf(df_merged)

# make sure OLS and MGWR residual map use same range as OLS map
residual_range <- range(mgwr_results$ols_residual, na.rm = TRUE)

# plot ols residuals
p1 <- ggplot() +
  geom_sf(data = df_merged, aes(fill = ols_residual), color = 'black') +
  geom_sf(data = us_states, fill = NA, color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "mako", na.value = "grey80",
    name = "Residuals",
    direction = -1,
    limits = residual_range
  ) +
  labs(
    # title = "Global OLS Residuals",
    subtitle = "Moran's I:  0.3087 (p < 0.05)"
    # caption = "Data Source: American Community Survey 5-year estimates (2017-2021)"
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

# Display the plot
p1


# plot mgwr residuals
p2 <- ggplot() +
  geom_sf(data = df_merged, aes(fill = mgwr_residual), color = 'black') +
  geom_sf(data = us_states, fill = NA, color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "mako", na.value = "grey80",
    name = "Residuals",
    direction = -1,
    limits = residual_range
  ) +
  labs(
    # title = "MGWR Residuals",
    subtitle = "Moran's I:  0.0023 (p > 0.05)"
    # caption = "Data Source: American Community Survey 5-year estimates (2017-2021)"
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

# Display the plot
p2

p3 <- grid.arrange(
  p1,
  p2,
  ncol = 2
)

p3

ggsave(
  filename = paste0(here('images', 'ols-mgwr-residual-combined.png')),
  plot = p3,
  dpi = 600,
  width = 18,
  height = 10,
  bg = 'white'
)