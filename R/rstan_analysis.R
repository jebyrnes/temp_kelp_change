library(dplyr)
library(ggplot2)
library(rstanarm)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

kelp_data <- read.csv("../derived_data/raw_data_merged.csv", stringsAsFactors=FALSE) %>%
  dplyr::mutate(Group = ECOREGION) %>%
  #  dplyr::select(Group, Site, x, y, Study) %>%
  dplyr::mutate(Year_c = Year - mean(Year)) %>%
  filter(!is.na(max_waves)) %>%
  filter(!is.na(mean_tempC)) %>%
  filter(!is.na(mean_tempC)) %>%
  dplyr::mutate(Site = StudySite) %>%
  dplyr::mutate(y = stdByECOREGION+0.001) %>%
  dplyr::mutate(max_waves_c = max_waves-mean(max_waves)) %>%
  dplyr::mutate(mean_tempC_c = mean_tempC-mean(mean_tempC)) %>%
  dplyr::mutate(abs_lat = abs(Latitude))


kelp_rstan_mod <- stan_glmer(y ~ Year_c+
                               max_waves_c*max_tempC_anomoly*abs_lat +
                               (1+Year_c|ECOREGION) +
                               (1+Year_c|ECOREGION/Site),
                             data=kelp_data, family=gaussian(link="log"))

#summary(kelp_rstan_mod)

save(kelp_rstan_mod, file="../HLM_output/kelp_rstan_mod")

posterior_interval(kelp_rstan_mod, prob = 0.9, type = "central", 
                   pars = c("(Intercept)", "Year_c", "max_waves_c", "max_tempC_anomoly", "abs_lat",
                            "max_waves_c:max_tempC_anomoly", "max_waves_c:abs_lat", "max_tempC_anomoly:abs_lat",
                            "max_waves_c:max_tempC_anomoly:abs_lat"))