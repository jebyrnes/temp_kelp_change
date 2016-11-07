library(dplyr)
library(readr)
library(rethinking)
library(ggplot2)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Load data
kelp_slopes_merged <- read_csv("../derived_data/kelp_slopes_with_temp_waves_canopy.csv") 

#Filter it down and do a little renaming
ksm <- kelp_slopes_merged %>% 
  filter(!is.na(max_wave_height_estimate)) %>%
  filter(!is.na(max_wave_height_std.error)) %>%
  filter(!is.na(max_temp_slope_estimate)) %>%
  dplyr::rename(slope = mean, slope_se = se,
                waves = max_wave_height_estimate,
                se_waves = max_wave_height_std.error,
                temp = max_temp_slope_estimate,
                se_temp = max_temp_slope_std.error) %>%
  mutate(abs_lat = abs(Latitude)) %>%
  #standardize to speed convergence
  mutate(abs_lat = as.numeric(scale(abs_lat)),
         Duration=as.numeric(scale(Duration)),
         se_waves_scale=se_waves/sd(waves),
         se_temp_scale=se_temp/sd(temp),
         waves_scale=as.numeric(scale(waves)),
         temp_scale=as.numeric(scale(temp)),
         nocanopy = as.numeric(factor(has_canopy)) - 1,
         studyIDX = as.numeric(factor(Study)))

#Make a clean version that won't cause STAN to barf
ksm_clean <- as.data.frame(ksm %>%
                             dplyr::select(slope, temp_scale, waves_scale, nocanopy, abs_lat,
                                           slope_se, se_temp_scale, se_waves_scale, studyIDX)) %>%
  #because we cant' do measurement errors of 0
  mutate(se_waves_scale = ifelse(se_waves_scale==0, 1e-20, se_waves_scale),
         se_temp_scale = ifelse(se_temp_scale==0,  1e-20, se_temp_scale)) 

#Model

kelp_slope_mod <- alist(
  
  
  #likelihoods
  slope_est ~ dnorm(slope_hat, true_se_slope),
  
  #link function
  slope_hat <- b0 + 
    b0_study[studyIDX] +
    bC * nocanopy + 
    bL * abs_lat +
    bW * waves_est[i] +
    bT * temp_est[i] +
    bCL * nocanopy * abs_lat +
    bCW * nocanopy * waves_est[i] +
    bCT * nocanopy * temp_est[i] +
    bLW * abs_lat * waves_est[i] +
    bLT * abs_lat * temp_est[i] +
    bWT * waves_est[i] * temp_est[i] +
    bCLW * nocanopy * abs_lat * waves_est[i]+
    bCLT * nocanopy * abs_lat * temp_est[i]+
    bCWT * nocanopy * waves_est[i] * temp_est[i] +
    bLWT * abs_lat * waves_est[i] * temp_est[i] +
    bCLWT * nocanopy * abs_lat * waves_est[i] * temp_est[i],
  
  #observation error
  slope ~ dnorm(slope_est, slope_se),
  waves_scale ~ dnorm(waves_est, se_waves_scale),
  temp_scale ~ dnorm(temp_est, se_temp_scale),
  
  #priors
  b0 ~ dnorm(0,10),
  bC ~ dnorm(0,10),
  bL ~ dnorm(0,10),
  bW ~ dnorm(0,10),
  bT ~ dnorm(0,10),
  bCL ~ dnorm(0,10),
  bCW ~ dnorm(0,10),
  bCT ~ dnorm(0,10),
  bLW ~ dnorm(0,10),
  bLT ~ dnorm(0,10),
  bWT ~ dnorm(0,10),
  bCLW ~ dnorm(0,10),
  bCLT ~ dnorm(0,10),
  bCWT ~ dnorm(0,10),
  bLWT ~ dnorm(0,10),
  bCLWT ~ dnorm(0,10),
  true_se_slope ~ dcauchy(0,2.5),
  b0_study[studyIDX] ~ dnorm(0, sigma_study),
  sigma_study ~ dcauchy(0, 2.5)
)


kelp_slope_fit <- 
  map2stan(kelp_slope_mod,
           data=ksm_clean,
           cores=4,
           chains=10,
           start=list(temp_est = ksm_clean$temp_scale,
                      waves_est = ksm_clean$waves_scale,
                      slope_est = ksm_clean$slope),
           # iter=10, warmup=10, #test
           iter=10000, #real
           warmup = 5000, #real
           control = list(adapt_delta=0.95))

precis(kelp_slope_fit)

save(kelp_slope_fit, file="../chain_output/kelp_slope_fit.Rdata")

#DIAGNOSTICS
#Validate MCMC
#plot(kelp_slope_fit,, pars=c("b0", "bC", "bL", "bW", "bT", "bCT"))
#evaluate Predicted v. Obs
#evaluate Residuals v. Fitted
#Histograms of residual sumulations
#Evaluate posterior

#DAT MEASUREMENT ERROR
#Show shrinkage in Slopes bases on measurement error
#Show shrinkage in Predictors bases on measurement error

### Model without latitude
kelp_slope_nolat_mod <-alist(
  
  #likelihoods
  slope_est ~ dnorm(slope_hat, true_se_slope),
  
  #link function
  slope_hat <- b0 + 
    b0_study[studyIDX] +
    bC * nocanopy + 
    bW * waves_est[i] +
    bT * temp_est[i] +
    bCW * nocanopy * waves_est[i] +
    bCT * nocanopy * temp_est[i] +
    bWT * waves_est[i] * temp_est[i] +
    bCWT * nocanopy * waves_est[i] * temp_est[i], 
  
  #observation error
  slope ~ dnorm(slope_est, slope_se),
  waves_scale ~ dnorm(waves_est, se_waves_scale),
  temp_scale ~ dnorm(temp_est, se_temp_scale),
  
  #priors
  b0 ~ dnorm(0,10),
  bC ~ dnorm(0,10),
  bW ~ dnorm(0,10),
  bT ~ dnorm(0,10),
  bCW ~ dnorm(0,10),
  bCT ~ dnorm(0,10),
  bWT ~ dnorm(0,10),
  bCWT ~ dnorm(0,10),
  true_se_slope ~ dcauchy(0,2.5),
  b0_study[studyIDX] ~ dnorm(0, sigma_study),
  sigma_study ~ dcauchy(0, 2.5)
)


kelp_slope_nolat_fit <- 
  map2stan(kelp_slope_nolat_mod,
           data=ksm_clean,
           cores=10,
           chains=4,
           start=list(temp_est = ksm_clean$maxTempChangeAnnualSample,
                      waves_est = ksm_clean$maxWaveChangeAnnualSample,
                      slope_est = ksm_clean$slope),
           # iter=10, warmup=10, #test
           iter=5000, #real
           warmup = 1000, #real
           control = list(adapt_delta=0.95))

save(kelp_slope_nolat_fit, file="../chain_output/kelp_slope_nolat_fit.Rdata")
