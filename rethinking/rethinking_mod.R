library(rethinking)
library(dplyr)

kelpdata <- read.csv("./test.csv") %>%
  dplyr::rename(Group = EcoregionName, Site = SiteName, x = Year, Study = StudyName, focalUnit = Unit) %>%
  dplyr::select(Group, Site, x, y, Study, focalUnit) %>%
  dplyr::mutate(x = x - mean(x)) #Dan does this in the stan_functions.R file

kelp_mod <- alist(
  #likelihood
  y ~ lognormal(y_loc, sd_loc), # likelihood
  
  #model
  y_loc <- beta_mu_int[Group] + beta_int[Site] + 
    (beta_slope[Site] + beta_mu_slope[Group])*x,#expectation equation
  
  #,A[SiteMethod[i]]
  sd_loc <- sd_e[Study],
  
  #priors
  beta_mu_int[Group] ~ dnorm(0,10),
  beta_mu_slope[Group] ~ dnorm(0,10),
  
  c(beta_int,beta_slope)[Site] ~ dmvnormNC( sigma_Site , Rho_Site ),

  sigma_Site ~ dcauchy(0,2.5), # vague priors for the group SD
  
  Rho_Site ~ dlkjcorr(2.0), # vague priors for the ranef corr matrix
  
  sd_e[Study] ~ dcauchy(0,A), #prior for study level errors
  A[focalUnit] ~ dunif(0.000001, 3) #hyperprior for method-based error
)


kelp_fit <- map2stan(kelp_mod, data=kelpdata, chains=2, cores=2,
                     constraints=list(sd_e = "lower=0"), 
                     start=list(sd_e = rep(1,length(unique(kelpdata$Study))),
                                A = rep(1,4)))

save(kelp_fit, file="rethinking_kelp_fit.Rdata")
