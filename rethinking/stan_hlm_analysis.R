######################################################
###  Script to sample model posteriors             ###
###  for the hierarchical linear model             ###
###  Author:  D.K. Okamoto                         ###
######################################################

library(rstan);library(dplyr)
library(gdata);library(ggplot2)
library(parallel);library(grid);library(scales);library(coda);library(rdrop2)
library(plyr);library(quantreg)

cat("Stan version:", stan_version(), "\n")
rstan_options(auto_write = TRUE)

### clear old data ###
rm(list=ls())


### list of parameters to save ###
params_for_stan <- c("beta","beta_mu",
           "sd_e","y_loc","y_loc_mu",
           "L_vcov_beta","L_omega_beta",
            "resid","log_lik")

### MCMC details
MCMC_details <- list(n.iter = 1500,
                     n.burnin = 500,
                     set.seed = 234,
                     n.chains = 2,
                     n.thin = 1)

### fetch the model ###
source("stan_model.R")

### fetch the necessary summary functions ###
source("stan_functions.R")

  
### Load Data ###
kelpdata <- read.csv("./test.csv", stringsAsFactors=FALSE) %>%
  dplyr::rename(SiteMethod = Unit)

  
### order data
#model_data <- model_data[order(model_data$Site),]
  

### wrapper using correct data, parameters and mcmc details to pass to mclapply for parallel sampling
stan_fit <- HLM_stan_fit(group="Ecoregion", data= kelpdata,params= params_for_stan, MCMC_details= MCMC_details)

save(stan_fit, file="./stan_fit.Rdata")
