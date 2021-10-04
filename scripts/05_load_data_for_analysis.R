library(dplyr)
setwd(here::here())


rd_for_analysis <-
  read.csv("derived_data/raw_data_with_temp_waves_canopy.csv")

#get some properties
dat <- rd_for_analysis %>%
  filter(!is.na(mean_wave_energy_dev)) %>%
  group_by(Study, Site) %>%
  mutate(n_per_site = n()) %>%
  ungroup() %>%
  filter(n_per_site > 2) %>%
  group_by(ECOREGION) %>%
  mutate(sites_per_ecoregion = n_distinct(Site)) %>%
  ungroup() %>%
  filter(sites_per_ecoregion > 5) %>%
  mutate(focalUnit = as.character(focalUnit)) #for modeling error
