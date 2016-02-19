library(dplyr)

kelpdata <- read.csv("./test.csv") %>%
  dplyr::rename(Group = EcoregionName, Site = SiteName, x = Year, Study = StudyName) %>%
  dplyr::select(Group, Site, x, y, Study) 

load("./rethinking_kelp_fit.Rdata")
load("./stan_fit.Rdata")


#Make a comparable table for the rethinking fit
precis(kelp_fit, depth=2, pars = c("beta_mu_slope", "beta_mu_int"), prob=0.95)

stan_fit$summary %>%
  filter(parameter %in% c("mean_intercept", "mean_slope")) %>%
  dplyr::select(group_name, parameter, mean, se, lower_0.95, upper_0.95) %>%
  arrange(parameter, group_name)


#Look at WAIC
WAIC(kelp_fit)
stan_fit$waic$total
