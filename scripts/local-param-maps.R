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
df <- import(here('mgwr-wo-monte-carlo', 'MGWR_session_results.csv'))
df <- add_fips_padding(df = df)


# desired alpha
alpha <- 0.05

# dataframe of ENP_js to compute corrected alphas
variable_enps <- data.frame(
  Variable = c("Intercept", "gini", "ln_pop_den", "pct_internet_access21", 
               "pct_bach_higher_18_22", "pct_pop_urban20", "sex_ratio17_21", 
               "median_age17_21", "pct_blck17_21", "pct_hisplat17_21"),
  ENP_j = c(164.854, 76.129, 6.687, 149.197, 125.310, 1.920, 
            11.088, 48.350, 1.027, 8.884)
)

# calculate corrected alphas
variable_enps <- variable_enps %>% 
  mutate(alpha_corrected = round(alpha / ENP_j, 6))

# print(variable_enps)

# filter and create significant flags using updated thresholds
df_p_values <- df %>% 
  select(
    fips_cd,
    starts_with("beta_"),
    starts_with("p_"),
    starts_with("t_")
  ) %>% 
  mutate(
    sig_intercept = ifelse(p_Intercept < 0.000303, 'Y', 'N'),
    sig_gini = ifelse(p_gini < 0.000657, 'Y', 'N'),
    sig_ln_pop_den = ifelse(p_ln_pop_den < 0.007477, 'Y', 'N'),
    sig_pct_internet_access21 = ifelse(p_pct_internet_access21 < 0.000335, 'Y', 'N'),
    sig_p_pct_bach_higher_18_22 = ifelse(p_pct_bach_higher_18_22 < 0.000399, 'Y', 'N'),
    sig_p_pct_pop_urban20 = ifelse(p_pct_pop_urban20 < 0.026042, 'Y', 'N'),
    sig_p_sex_ratio17_21 = ifelse(p_sex_ratio17_21 < 0.004509, 'Y', 'N'),
    sig_p_median_age17_21 = ifelse(p_median_age17_21 < 0.001034, 'Y', 'N'),
    sig_p_pct_blck17_21 = ifelse(p_pct_blck17_21 < 0.048685, 'Y', 'N'),
    sig_p_pct_hisplat17_21 = ifelse(p_pct_hisplat17_21 < 0.005628, 'Y', 'N')
  )

# Retrieve county shapefiles for all states
us_counties <- counties(cb = TRUE, year = 2021, class = "sf")
us_states <- states(cb = TRUE, year = 2021, class = "sf") %>%
  st_transform(crs = 5070) %>% 
  filter(STUSPS %nin% c('AK', 'HI', 'DC', 'MP', 'VI', 'PR', 'GU', 'AS'))

# Merge shapefile data with p-value dataframe
df_p_value_merged <- left_join(df_p_values, us_counties, by = c('fips_cd' = 'GEOID'))

# Convert to spatial dataframe
df_p_value_merged <- st_as_sf(df_p_value_merged)

# create columns for significant betas based on updated significance flags
df_p_value_merged <- df_p_value_merged %>%
  mutate(
    beta_intercept_sig = ifelse(sig_intercept == 'Y', beta_Intercept, NA),
    beta_gini_sig = ifelse(sig_gini == 'Y', beta_gini, NA),
    beta_ln_pop_den_sig = ifelse(sig_ln_pop_den == 'Y', beta_ln_pop_den, NA),
    beta_pct_internet_access21_sig = ifelse(sig_pct_internet_access21 == 'Y', beta_pct_internet_access21, NA),
    beta_pct_bach_higher_18_22_sig = ifelse(sig_p_pct_bach_higher_18_22 == 'Y', beta_pct_bach_higher_18_22, NA),
    beta_pct_pop_urban20_sig = ifelse(sig_p_pct_pop_urban20 == 'Y', beta_pct_pop_urban20, NA),
    beta_sex_ratio17_21_sig = ifelse(sig_p_sex_ratio17_21 == 'Y', beta_sex_ratio17_21, NA),
    beta_median_age17_21_sig = ifelse(sig_p_median_age17_21 == 'Y', beta_median_age17_21, NA),
    beta_pct_blck17_21_sig = ifelse(sig_p_pct_blck17_21 == 'Y', beta_pct_blck17_21, NA),
    beta_pct_hisplat17_21_sig = ifelse(sig_p_pct_hisplat17_21 == 'Y', beta_pct_hisplat17_21, NA)
  )

### plots
# plot everything except for black and urban (8 plots)

# plot significant intercepts
p1 <- ggplot() +
  geom_sf(data = df_p_value_merged, aes(fill = beta_intercept_sig), color = 'black') +
  geom_sf(data = us_states, fill = NA, color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "mako",
    na.value = "grey80",
    name = "",
    direction = -1
    # Optionally, set limits and breaks if needed
  ) +
  # scale_fill_distiller(
  #   palette = 'RdBu',
  #   na.value = 'grey80',
  #   name = 'Intercept',
  #   direction = 1
  # ) +
  labs(
    # title = "Significant Intercept by U.S. County",
    subtitle = 'BW = 48 : [46, 52]'
    # caption = "Data Source: American Community Survey 5-year estimates (2017-2021)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(t = 10, b = 20)),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.position = c(0.85, 0.2),
    legend.justification = c("right", "bottom"),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

p1

ggsave(
  filename = paste0(here('images', 'local-param', 'sig-intercept.png')),
  plot = p1,
  dpi = 600,
  width = 16,
  height = 8,
  bg = 'white'
)

# Plot for Gini Index
p2 <- ggplot() +
  geom_sf(data = df_p_value_merged, aes(fill = beta_gini_sig), color = 'black') +
  geom_sf(data = us_states, fill = NA, color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "mako", 
    na.value = "grey80", 
    name = "",
    direction = -1,
  ) +
  labs(
    # title = "Significant Gini Index by U.S. County (2021)",
    subtitle = 'BW = 258 : [212, 383]'
    # caption = "Data Source: American Community Survey 5-year estimates (2017-2021)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(t = 10, b = 20)),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = c(0.85, 0.2),
    legend.justification = c("right", "bottom"),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Display the plot
p2

ggsave(
  filename = paste0(here('images', 'local-param', 'sig-gini.png')),
  plot = p2,
  dpi = 600,
  width = 16,
  height = 8,
  bg = 'white'
)

# Plot for Log Population Density
p3 <- ggplot() +
  geom_sf(data = df_p_value_merged, aes(fill = beta_ln_pop_den_sig), color = 'black') +
  geom_sf(data = us_states, fill = NA, color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "mako", 
    na.value = "grey80", 
    name = "",
    direction = -1
    # Optionally, set limits and breaks
  ) +
  labs(
    # title = "Significant Natural Logged Population Density by U.S. County",
    subtitle = 'BW = 1494 : [1210, 1932]'
    # caption = "Data Source: American Community Survey 5-year estimates (2017-2021)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(t = 10, b = 20)),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.position = c(0.85, 0.2),
    legend.justification = c("right", "bottom"),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Display the plot
p3

ggsave(
  filename = paste0(here('images', 'local-param', 'sig-ln-pop-den.png')),
  plot = p3,
  dpi = 600,
  width = 16,
  height = 8,
  bg = 'white'
)

# Plot for Percent Internet Access (2021)
p4 <- ggplot() +
  geom_sf(data = df_p_value_merged, aes(fill = beta_pct_internet_access21_sig), color = 'black') +
  geom_sf(data = us_states, fill = NA, color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "mako", 
    na.value = "grey80", 
    name = "",
    direction = -1
    # Optionally, set limits and breaks
  ) +
  labs(
    # title = "Significant Percent Internet Access (2021) by U.S. County",
    subtitle = 'BW = 3098 : [2654, 3098]'
    # caption = "Data Source: American Community Survey 5-year estimates (2017-2021)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(t = 10, b = 20)),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.position = c(0.85, 0.2),
    legend.justification = c("right", "bottom"),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Display the plot
p4

ggsave(
  filename = paste0(here('images', 'local-param', 'sig-internet-access.png')),
  plot = p4,
  dpi = 600,
  width = 16,
  height = 8,
  bg = 'white'
)

# sig_internet <- df_p_value_merged %>% 
#   select(beta_pct_internet_access21, sig_pct_internet_access21, p_pct_internet_access21, t_pct_internet_access21) %>% 
#   filter(sig_pct_internet_access21 == 'Y')


# Plot for Percent Bachelor Degree or Higher (18-22)
p5 <- ggplot() +
  geom_sf(data = df_p_value_merged, aes(fill = beta_pct_bach_higher_18_22_sig), color = 'black') +
  geom_sf(data = us_states, fill = NA, color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "mako",
    na.value = "grey80",
    name = "",
    direction = -1
    # Optionally, set limits and breaks
  ) +
  # scale_fill_continuous_diverging() +
  labs(
    # title = "Significant Percent Bachelor Degree or Higher (18-22) by U.S. County",
    subtitle = 'BW = 44 : [44, 48]'
    # caption = "Data Source: American Community Survey 5-year estimates (2017-2021)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(t = 10, b = 20)),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.position = c(0.85, 0.2),
    legend.justification = c("right", "bottom"),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Display the plot
p5

ggsave(
  filename = paste0(here('images', 'local-param', 'sig-pct-bach-higher.png')),
  plot = p5,
  dpi = 600,
  width = 16,
  height = 8,
  bg = 'white'
)

# Plot for Sex Ratio (2017-21)
p7 <- ggplot() +
  geom_sf(data = df_p_value_merged, aes(fill = beta_sex_ratio17_21_sig), color = 'black') +
  geom_sf(data = us_states, fill = NA, color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "mako", 
    na.value = "grey80", 
    name = "", 
    direction = -1
    # Optionally, set limits and breaks
  ) +
  labs(
    # title = "Significant Sex Ratio (2017-21) by U.S. County",
    subtitle = 'BW = 1361 : [764, 1656]'
    # caption = "Data Source: American Community Survey 5-year estimates (2017-2021)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(t = 10, b = 20)),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1),    legend.position = c(0.85, 0.2),
    legend.justification = c("right", "bottom"),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Display the plot
p7

ggsave(
  filename = paste0(here('images', 'local-param', 'sig-sex-ratio.png')),
  plot = p7,
  dpi = 600,
  width = 16,
  height = 8,
  bg = 'white'
)

# Plot for Median Age (2017-21)
p8 <- ggplot() +
  geom_sf(data = df_p_value_merged, aes(fill = beta_median_age17_21_sig), color = 'black') +
  geom_sf(data = us_states, fill = NA, color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "mako", 
    na.value = "grey80", 
    name = "", 
    direction = -1
    # Optionally, set limits and breaks
  ) +
  labs(
    # title = "Significant Median Age (2017-21) by U.S. County",
    subtitle = 'BW = 238 : [212, 278]'
    # caption = "Data Source: American Community Survey 5-year estimates (2017-2021)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(t = 10, b = 20)),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1),    legend.position = c(0.85, 0.2),
    legend.justification = c("right", "bottom"),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Display the plot
p8

ggsave(
  filename = paste0(here('images', 'local-param', 'sig-median-age.png')),
  plot = p8,
  dpi = 600,
  width = 16,
  height = 8,
  bg = 'white'
)

# Plot for Percent Black (2017-21)
p9 <- ggplot() +
  geom_sf(data = df_p_value_merged, aes(fill = beta_pct_hisplat17_21_sig), color = 'black') +
  geom_sf(data = us_states, fill = NA, color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "mako", 
    na.value = "grey80", 
    name = "", 
    direction = -1
    # Optionally, set limits and breaks
  ) +
  labs(
    # title = "Significant Percent Black (2017-21) by U.S. County",
    subtitle = 'BW = 3098.000 : [2378, 3098]'
    # caption = "Data Source: American Community Survey 5-year estimates (2017-2021)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, margin = margin(t = 10, b = 20)),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1),    legend.position = c(0.85, 0.2),
    legend.justification = c("right", "bottom"),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Display the plot
p9

ggsave(
  filename = paste0(here('images', 'local-param', 'sig-pct-hisplat.png')),
  plot = p9,
  dpi = 600,
  width = 16,
  height = 8,
  bg = 'white'
)