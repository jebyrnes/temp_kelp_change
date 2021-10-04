library(dplyr)
library(brms)
library(cmdstanr)
library(ggplot2)
library(bayesplot)

theme_set(theme_bw())
setwd(here::here())

source("scripts/05_load_data_for_analysis.R")

# Models with variation by ecoregion ####
mod_regional <- brm(bf(
  stdByECOREGION + 0.01 ~
    ECOREGION * (mean_temp_dev * mean_wave_energy_dev) +
    mean_wave_energy_site +
    mean_temp_site +
    mean_temp_ecoregion +
    mean_temp_province +
    Year +
    (1 | SiteName),
  sigma ~ focalUnit),
  family = gaussian(link = "log"),
  data = dat,
  iter = 4000,
  chains = 2,
  backend = "cmdstanr",
  threads = threading(4),
  save_model = "chain_output/fit_regional_model.Rdata"
)


#squared
mod_regional <- brm(bf(
  stdByECOREGION + 0.01 ~
    ECOREGION * (poly(mean_temp_dev,2) * mean_wave_energy_dev) +
    mean_wave_energy_site +
    mean_temp_site +
    mean_temp_ecoregion +
    mean_temp_province +
    Year +
    (1 |SiteName),
  sigma ~ focalUnit),
  family = gaussian(link = "log"),
  data = dat,
  iter = 4000,
  chains = 2,
  backend = "cmdstanr",
  threads = threading(4),
  save_model = "chain_output/fit_regional_model.Rdata"
)

## Models with no variation by ecoregion, but, canopy ####
#linear
mod_canopy_linear <- brm(bf( ln_stdByECOREGION + 0.01 ~
    (has_canopy * mean_temp_dev * mean_wave_energy_dev) +
    mean_wave_energy_site +
    mean_temp_site +
    mean_temp_ecoregion +
    mean_temp_province +
    Year +
    (mean_temp_dev * mean_wave_energy_dev|ECOREGION) +
    (1 | SiteName),
  sigma ~ focalUnit),
  data = dat,
  chains = 2,
  backend = "cmdstanr",
  threads = threading(4),
  save_model = "chain_output/fit_canopy_linear_model.Rdata"
)

#polynomial temperature effect
mod_canopy_quad <- brm(bf(
  ln_stdByECOREGION + 0.01 ~
    (has_canopy * poly(mean_temp_dev,2) * mean_wave_energy_dev) +
    mean_wave_energy_site +
    mean_temp_site +
    mean_temp_ecoregion +
    mean_temp_province +
    Year +
    (poly(mean_temp_dev,2) * mean_wave_energy_dev|ECOREGION) +
    (1 | SiteName),
  sigma ~ focalUnit),
  data = dat,
  chains = 2,
  backend = "cmdstanr",
  threads = threading(4),
  save_model = "chain_output/fit_canopy_linear_model.Rdata"
)
