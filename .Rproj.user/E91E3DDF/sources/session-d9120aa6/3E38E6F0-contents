rm(list = ls()); gc()

library(tidyverse)
library(rio)
library(here)
# library(sf)
# library(spdep)
library(tigris)
library(colorspace)
# library(knitr)
# library(kableExtra)
# library(stargazer)
# library(ggfortify)
library(grid)
library(gridExtra)

options(tigris_use_cache = TRUE)

`%nin%` = Negate(`%in%`)

# import helper functions
source(here::here('scripts', 'fip_cd_check.R'))
source(here::here('scripts', 'fip_padder.R'))

# import mgwr results
df <- import(here('mgwr-wo-monte-carlo', 'MGWR_session_results.csv'))
df <- add_fips_padding(df = df)

# import modelled data
og_data <- import(here('output-data', 'mgwr-data-standardized.csv'))
og_data <- add_fips_padding(df = og_data)

# join modeling data and results data to plot betas vs. x_{i}
df <- left_join(df, og_data, by = 'fips_cd')

# variables to plot
variables <- c("gini", "ln_pop_den", "pct_internet_access21", "pct_bach_higher_18_22",
               "pct_pop_urban20", "sex_ratio17_21", "median_age17_21", "pct_blck17_21", "pct_hisplat17_21")

# create a list of plots for each of the above variables
plot_list <- lapply(variables, function(var) {
  df %>%
    ggplot(aes_string(x = var, y = paste0("beta_", var))) + # for every variable in the variable vector, plot the levels of that variable against its respective beta
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE) +
    geom_smooth(method = 'loess', se = FALSE, color = 'red') +
    ggtitle(paste(paste0("beta_", var))) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
})

# Now you can print each plot individually
print(plot_list[[1]])  # Print the first plot
print(plot_list[[2]])  # Print the first plot
print(plot_list[[3]])  # Print the first plot

# df %>% ggplot(aes(x = pct_internet_access21, y = beta_pct_internet_access21)) +
#   geom_point() +
#   geom_smooth(method = 'lm') +
#   geom_smooth(method = 'loess')

p2 <- grid.arrange(grobs = plot_list, ncol = 3)  # Arrange in a grid of 3 columns

ggsave(
  filename = paste0(here('images', 'nonlinearity', 'local-betas-vs-x.png')),
  plot = p2,
  dpi = 600,
  width = 24,
  height = 24,
  bg = 'white'
)