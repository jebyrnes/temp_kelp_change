library(rethinking)
library(dplyr)
library(ggplot2)

kelp_data <- read.csv("../derived_data/raw_data_merged.csv") %>%
  dplyr::rename(Group = ECOREGION) %>%
#  dplyr::select(Group, Site, x, y, Study) %>%
  dplyr::mutate(Year_c = Year - mean(Year)) %>%
  filter(!is.na(max_waves)) %>%
  filter(!is.na(mean_tempC)) %>%
  filter(!is.na(mean_tempC)) %>%
  dplyr::mutate(Site = StudySite) %>%
  dplyr::mutate(y = stdByECOREGION+0.001)

kelp_data_to_fit <- kelp_data %>%
  select(y, Year_c, Group, Site, max_waves, mean_tempC)

kelp_mod <- alist(
  #likelihood
  y ~ lognormal(y_loc, sd_loc), # likelihood
  
  #model
  y_loc <- beta_mu_int[Group] + beta_int[Site] + 
    (beta_slope_year[Site] + beta_mu_slope_year[Group])*Year_c +
    (beta_slope_temp[Site] + beta_mu_slope_temp[Group])*mean_tempC + 
    (beta_slope_waves[Site] + beta_mu_slope_waves[Group])*max_waves +
    (beta_slope_temp_waves[Site] + beta_mu_slope_temp_waves[Group])*mean_tempC*max_waves
    ,#expectation equation
  
  sd_loc <- sd_e[Site],
  
  #priors
  beta_mu_int[Group] ~ dnorm(0,10),
  beta_mu_slope_year[Group] ~ dnorm(0,10),
  beta_mu_slope_temp[Group] ~ dnorm(0,10),
  beta_mu_slope_temp[Group] ~ dnorm(0,10),
  beta_mu_slope_waves[Group] ~ dnorm(0,10),
  beta_mu_slope_temp_waves[Group] ~ dnorm(0,10),
  
  c(beta_int,beta_slope_year, beta_slope_temp, beta_slope_waves, beta_slope_temp_waves)[Site] ~ dmvnormNC( sigma_Site , Rho_Site ),
  
  sigma_Site ~ dcauchy(0,2.5), # vague priors for the group SD
  
  Rho_Site ~ dlkjcorr(2.0), # vague priors for the ranef corr matrix
  
  sd_e[Site] ~ dcauchy(0,1) #prior for study level errors
)


kelp_fit <- map2stan(kelp_mod, data=kelp_data_to_fit, chains=4, cores=4,
                     constraints=list(sd_e = "lower=0"), 
                     start=list(sd_e = rep(1,length(unique(kelp_data$Site)))))

save(kelp_fit, file="rethinking_kelp_fit.Rdata")

