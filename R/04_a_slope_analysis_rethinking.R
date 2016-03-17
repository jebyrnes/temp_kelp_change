library(rethinking)
library(dplyr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#Load data
kelp_slopes_merged <- read.csv("../derived_data/kelp_slopes_merged.csv", stringsAsFactors=FALSE) %>%
  mutate(Duration = maxYear - minYear)

#Filter it down and do a little renaming
ksm <- kelp_slopes_merged
ksm <- ksm %>% filter(!is.na(maxWaveChangeAnnualSample)) %>%
  filter(!is.na(meanTempChangeAnnualSample)) %>%
  rename(slope = mean, slope_se = se) %>%
  mutate(abs_lat = abs(Latitude)) #%>%
  #standardize
  # mutate(abs_lat = as.numeric(scale(abs_lat)),
  #        Duration=as.numeric(scale(Duration)),
  #        se_maxWaveChangeAnnualSample=se_maxWaveChangeAnnualSample/sd(maxWaveChangeAnnualSample),
  #        se_maxTempChangeAnnualSample=se_maxTempChangeAnnualSample/sd(maxTempChangeAnnualSample),
  #        maxWaveChangeAnnualSample=as.numeric(scale(maxWaveChangeAnnualSample)),
  #        maxTempChangeAnnualSample=as.numeric(scale(maxTempChangeAnnualSample)))

#Model

kelp_slope_mod <- alist(
  
  
  #likelihoods
  slope_est ~ dnorm(slope_hat, true_se_slope),
  
  #link function
  slope_hat <- b0 + 
    b1 * Duration + 
    b2 * abs_lat +
    b3 * waves_est[i] +
    b4 * temp_est[i] +
    b5 * Duration * abs_lat +
    b6 * Duration * waves_est[i] +
    b7 * Duration * temp_est[i] +
    b8 * abs_lat * waves_est[i] +
    b9 * abs_lat * temp_est[i] +
    b10 * waves_est[i] * temp_est[i] +
    b11 * Duration * abs_lat * waves_est[i]+
    b12 * Duration * abs_lat * temp_est[i]+
    b13 * Duration * waves_est[i] * temp_est[i] +
    b14 * abs_lat * waves_est[i] * temp_est[i] +
    b15 * Duration * abs_lat * waves_est[i] * temp_est[i],
  
  #observation error
  slope ~ dnorm(slope_est, slope_se),
  maxWaveChangeAnnualSample ~ dnorm(waves_est, se_maxWaveChangeAnnualSample),
  maxTempChangeAnnualSample ~ dnorm(temp_est, se_maxTempChangeAnnualSample),

  #priors
  b0 ~ dnorm(0,10),
  b1 ~ dnorm(0,10),
  b2 ~ dnorm(0,10),
  b3 ~ dnorm(0,10),
  b4 ~ dnorm(0,10),
  b5 ~ dnorm(0,10),
  b6 ~ dnorm(0,10),
  b7 ~ dnorm(0,10),
  b8 ~ dnorm(0,10),
  b9 ~ dnorm(0,10),
  b10 ~ dnorm(0,10),
  b11 ~ dnorm(0,10),
  b12 ~ dnorm(0,10),
  b13 ~ dnorm(0,10),
  b14 ~ dnorm(0,10),
  b15 ~ dnorm(0,10),
  true_se_slope ~ dunif(0,20)
)


kelp_slope_fit <- 
  map2stan(kelp_slope_mod,
           data=ksm,
           ncores=4,
           chains=4,
           start=list(temp_est = ksm$maxTempChangeAnnualSample,
                      waves_est = ksm$maxWaveChangeAnnualSample,
                      slope_est = ksm$slope))

precis(kelp_slope_fit)

save(kelp_slope_fit, file="../chain_output/kelp_slope_fit.Rdata")

### Model without latitude
kelp_slope_nolat_mod <- alist(
  
  
  #likelihoods
  slope_est ~ dnorm(slope_hat, true_se_slope),
  
  #link function
  slope_hat <- b0 + 
    b1 * Duration + 
    b3 * waves_est[i] +
    b4 * temp_est[i] +
    b6 * Duration * waves_est[i] +
    b7 * Duration * temp_est[i] +
    b10 * waves_est[i] * temp_est[i] +
    b13 * Duration * waves_est[i] * temp_est[i],

  #observation error
  slope ~ dnorm(slope_est, slope_se),
  maxWaveChangeAnnualSample ~ dnorm(waves_est, se_maxWaveChangeAnnualSample),
  maxTempChangeAnnualSample ~ dnorm(temp_est, se_maxTempChangeAnnualSample),
  
  #priors
  b0 ~ dnorm(0,10),
  b1 ~ dnorm(0,10),
  b3 ~ dnorm(0,10),
  b4 ~ dnorm(0,10),
  b6 ~ dnorm(0,10),
  b7 ~ dnorm(0,10),
  b10 ~ dnorm(0,10),
  b13 ~ dnorm(0,10),
  
  true_se_slope ~ dunif(0,20)
)

kelp_slope_nolat_fit <- 
  map2stan(kelp_slope_nolat_mod,
           data=ksm,
           ncores=4,
           chains=4,
           start=list(temp_est = ksm$maxTempChangeAnnualSample,
                      waves_est = ksm$maxWaveChangeAnnualSample,
                      slope_est = ksm$slope))

save(kelp_slope_nolat_fit, file="../chain_output/kelp_slope_nolat_fit.Rdata")
