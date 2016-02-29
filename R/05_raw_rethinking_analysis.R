library(rethinking)
library(dplyr)
library(ggplot2)

#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())

kelp_data <- read.csv("../derived_data/raw_data_merged.csv", stringsAsFactors=FALSE) %>%
  dplyr::rename(Group = ECOREGION) %>%
#  dplyr::select(Group, Site, x, y, Study) %>%
  dplyr::mutate(Year_c = Year - mean(Year)) %>%
  filter(!is.na(max_waves)) %>%
  filter(!is.na(mean_tempC)) %>%
  filter(!is.na(mean_tempC)) %>%
  dplyr::mutate(Site = StudySite) %>%
  dplyr::mutate(y = stdByECOREGION+0.001) %>%
  dplyr::mutate(max_waves_c = max_waves-mean(max_waves)) %>%
  dplyr::mutate(mean_tempC_c = mean_tempC-mean(mean_tempC))
  

kelp_data_to_fit <- kelp_data %>%
  select(y, Year_c, Group, Site, max_waves_c, mean_tempC_c)

kelp_mod <- alist(
  #likelihood
  y ~ lognormal(y_loc, sd_loc), # likelihood
  
  #model
  y_loc <- beta_mu_int[Group] + beta_int[Site] + 
    (beta_slope_year[Site] + beta_mu_slope_year[Group])*Year_c +
    (beta_slope_temp[Site] + beta_mu_slope_temp[Group])*mean_tempC_c + 
    (beta_slope_waves[Site] + beta_mu_slope_waves[Group])*max_waves_c +
    (beta_slope_temp_waves[Site] + beta_mu_slope_temp_waves[Group])*mean_tempC_c*max_waves_c
    ,#expectation equation
  
  sd_loc <- sd_e[Site],
  
  #priors
  beta_mu_int[Group] ~ dnorm(0,10),
  beta_mu_slope_year[Group] ~ dnorm(0,10),
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

save(kelp_fit, file="rethinking_kelp_fit_ecoregion.Rdata")

####Global model
kelp_fit_world <- map2stan(kelp_mod, data=kelp_data_to_fit %>% mutate(Group = "World"), 
                           chains=4, cores=4,
                           constraints=list(sd_e = "lower=0"), 
                           start=list(sd_e = rep(1,length(unique(kelp_data$Site)))))

save(kelp_fit_world, file="rethinking_kelp_fit_world.Rdata")


kelp_fit_realm <- map2stan(kelp_mod, data=kelp_data_to_fit %>% mutate(Group = kelp_data$REALM),
                              chains=4, cores=4,
                              constraints=list(sd_e = "lower=0"), 
                              start=list(sd_e = rep(1,length(unique(kelp_data$Site)))))

save(kelp_fit_realm, file="rethinking_kelp_fit_realm.Rdata")



kelp_fit_province <- map2stan(kelp_mod, data=kelp_data_to_fit %>% mutate(Group = kelp_data$PROVINCE),
                              chains=4, cores=4,
                              constraints=list(sd_e = "lower=0"), 
                              start=list(sd_e = rep(1,length(unique(kelp_data$Site)))))

save(kelp_fit_province, file="rethinking_kelp_fit_province.Rdata")



precis(kelp_fit_world, depth=2, pars=c("beta_mu_int", "beta_mu_slope_year",
                                       "beta_mu_slope_year", "beta_mu_slope_temp",
                                       "beta_mu_slope_waves", "beta_mu_slope_temp_waves"),
       prob=95)
