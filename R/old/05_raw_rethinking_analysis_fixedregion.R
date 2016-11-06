
kelp_mod_grand <- alist(
  #likelihood
  y ~ lognormal(y_loc, sd_loc), # likelihood
  
  #model
  y_loc <- beta_int_grand + beta_mu_int[Group] + beta_int[Site] + 
    (beta_slope_year_grand + beta_slope_year[Site] + beta_mu_slope_year[Group])*Year_c +
    (beta_slope_temp_grand + beta_slope_temp[Site] + beta_mu_slope_temp[Group])*max_tempC_anomoly + 
    (beta_slope_waves_grand + beta_slope_waves[Site] + beta_mu_slope_waves[Group])*max_waves_c +
    (beta_slope_temp_waves_grand + beta_slope_temp_waves[Site] + beta_mu_slope_temp_waves[Group])*max_tempC_anomoly*max_waves_c
  ,#expectation equation
  
  sd_loc <- sd_e[Study],
  
  #priors
  beta_int_grand ~ dnorm(0,10), 
  beta_slope_year_grand ~ dnorm(0,10),
  beta_slope_temp_grand ~ dnorm(0,10),
  beta_slope_waves_grand ~ dnorm(0,10),
  beta_slope_temp_waves_grand ~ dnorm(0,10),
  
  beta_mu_int[Group] ~ dnorm(0,10),
  beta_mu_slope_year[Group] ~ dnorm(0,10),
  beta_mu_slope_temp[Group] ~ dnorm(0,10),
  beta_mu_slope_waves[Group] ~ dnorm(0,10),
  beta_mu_slope_temp_waves[Group] ~ dnorm(0,10),
  
  c(beta_int,beta_slope_year, beta_slope_temp, beta_slope_waves, beta_slope_temp_waves)[Site] ~ dmvnormNC( sigma_Site , Rho_Site ),
  
  sigma_Site ~ dcauchy(0,2.5), # vague priors for the group SD
  
  Rho_Site ~ dlkjcorr(2.0), # vague priors for the ranef corr matrix
  
  #  sd_e[Study] ~ dcauchy(0,1) #prior for study level errors
  
  sd_e[Study] ~ dcauchy(0,A), #prior for study level errors
  A[focalUnit] ~ dunif(0.000001, 3) #hyperprior for method-based error
)



kelp_fit_grand <- map2stan(kelp_mod_grand, data=kelp_data_to_fit, chains=4, cores=4, 
                     constraints=list(sd_e = "lower=0.000001, upper= 5"), 
                     start=list(sd_e = rep(1,length(unique(kelp_data$Study))),
                                A = rep(1,4)))

save(kelp_fit_grand, file="../HLM_output/rethinking_kelp_fit_ecoregion.Rdata")


