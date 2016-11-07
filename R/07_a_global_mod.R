library(rethinking)
library(dplyr)

#Load data
kelp_raw_data <- read.csv("../derived_data/raw_data_merged.csv", stringsAsFactors=FALSE)


#Filter it down
ksm_raw <- kelp_raw_data
ksm_raw <- ksm %>% filter(!is.na(max_waves)) %>%
  filter(!is.na(max_tempC_anomoly)) %>%
  mutate(Year_c = Year - mean(Year),
         max_waves_c = max_waves-mean(max_waves),
         Site = StudySite,
         y = stdByECOREGION+0.001,
         abs_lat = abs(Latitude))


kelp_mod <- alist(
  #likelihood
  y ~ lognormal(y_loc, sd_loc), # likelihood
  
  #model
  y_loc <- beta_mu_int + beta_int[Site] + 
    (beta_slope_year[Site] + beta_mu_slope_year)*Year_c +
    (beta_slope_temp[Site] + beta_mu_slope_temp)*max_tempC_anomoly + 
    (beta_slope_waves[Site] + beta_mu_slope_waves)*max_waves_c +
    (beta_slope_temp_waves[Site] + beta_mu_slope_temp_waves)*max_tempC_anomoly*max_waves_c
  ,#expectation equation
  
  sd_loc <- sd_e[Site],
  
  #priors
  beta_mu_int ~ dnorm(0,10),
  beta_mu_slope_year ~ dnorm(0,10),
  beta_mu_slope_temp ~ dnorm(0,10),
  beta_mu_slope_waves ~ dnorm(0,10),
  beta_mu_slope_temp_waves ~ dnorm(0,10),
  
  c(beta_int,beta_slope_year, beta_slope_temp, beta_slope_waves, beta_slope_temp_waves)[Site] ~ dmvnormNC( sigma_Site , Rho_Site ),
  
  sigma_Site ~ dcauchy(0,2.5), # vague priors for the group SD
  
  Rho_Site ~ dlkjcorr(2.0), # vague priors for the ranef corr matrix
  
  sd_e[Site] ~ dcauchy(0,1) #prior for study level errors
)


kelp_fit_world <- map2stan(kelp_mod, data=ksm %>% mutate(Group = "World"), 
                           chains=4, cores=4,
                           constraints=list(sd_e = "lower=0"), 
                           start=list(sd_e = rep(1,length(unique(kelp_data$Site)))))

save(kelp_fit_world, file="../HLM_output/rethinking_kelp_fit_world.Rdata")
