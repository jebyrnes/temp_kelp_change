source("./06_data_for_analysis_gen.R")
library(brms)
library(ggplot2)

#compare a model with versus without Duration
ksm_init <- make_ksm(max_wave_height_estimate, max_temp_slope_estimate)

all_factors_brms <- brm(slope | se(slope_se) ~ has_canopy*
                          waves_scale*temp_scale*abs_lat_scale*Duration_scale +
                          (1|Study), data=ksm_init)

no_duration_factors_brms <- brm(slope | se(slope_se) ~ has_canopy*
                                  waves_scale*temp_scale*abs_lat_scale +
                                  (1|Study), data=ksm_init %>% filter(!is.na(Duration_scale)))

WAIC(all_factors_brms, no_duration_factors_brms)



# Compare the different wave measures

fit_kelp_envt_mod <- function(...){
  ksm_tofit <- make_ksm(..., max_temp_slope_estimate)
  
  brm(slope | se(slope_se) ~ has_canopy*
        waves_scale*temp_scale*abs_lat_scale +
        (1|Study), data=ksm_tofit)
}

max_mean_wave_energy_mod <- fit_kelp_envt_mod(max_mean_wave_energy_estimate)
max_wave_height_mod <- fit_kelp_envt_mod(max_wave_height_estimate)
max_sum_wave_energy_mod <- fit_kelp_envt_mod(max_sum_wave_energy_estimate)
mean_wave_energy_mod<- fit_kelp_envt_mod(mean_wave_energy_estimate)
mean_wave_height_mod<- fit_kelp_envt_mod(mean_wave_height_estimate)
q95_wave_height_mod<- fit_kelp_envt_mod(q95_wave_height_estimate)
total_wave_energy_mod<- fit_kelp_envt_mod(total_wave_energy_estimate)

wTab <- WAIC(max_wave_height_mod,
     max_mean_wave_energy_mod,
     max_sum_wave_energy_mod,
     mean_wave_energy_mod,
     mean_wave_height_mod,
     q95_wave_height_mod,
     total_wave_energy_mod,
     compare=FALSE
     )

wt <- do.call(rbind, as.list(wTab))[,1:6] 
wtn <- rownames(wt)
wt <- unnest(as.data.frame(wt))
wt <- cbind(mod = wtn, wt) %>%
  mutate(delta = waic - min(waic))
wt %>%
  dplyr::arrange(waic)


#compare with v. without canopy
no_canopy_total_wave_energy_mod <- brm(slope | se(slope_se) ~ 
      waves_scale*temp_scale*abs_lat_scale +
      (1|Study), data=make_ksm(total_wave_energy_estimate, max_temp_slope_estimate))

WAIC(total_wave_energy_mod, no_canopy_total_wave_energy_mod)




#compare with v. without Latitude
no_lat_total_wave_energy_mod <- brm(slope | se(slope_se) ~ has_canopy*
                                         waves_scale*temp_scale +
                                         (1|Study), data=make_ksm(total_wave_energy_estimate, max_temp_slope_estimate))

WAIC(total_wave_energy_mod, no_lat_total_wave_energy_mod)


#Adding ecoregional random effect


#compare with v. without Latitude
ecoregion_total_wave_energy_mod <- brm(slope | se(slope_se) ~ has_canopy*
                                      waves_scale*temp_scale*abs_lat_scale +
                                      (1|Study) + (1|ecoregion), 
                                      data=make_ksm(total_wave_energy_estimate, max_temp_slope_estimate))

WAIC(ecoregion_total_wave_energy_mod)
WAIC(total_wave_energy_mod)

#Huh - go with ecoregion!

#last check on temp
ecoregion_total_wave_energy__meantemp_mod <- brm(slope | se(slope_se) ~ has_canopy*
                                         waves_scale*temp_scale*abs_lat_scale +
                                         (1|Study) + (1|ecoregion), 
                                       data=make_ksm(total_wave_energy_estimate, mean_temp_slope_estimate))

WAIC(ecoregion_total_wave_energy_mod, ecoregion_total_wave_energy__meantemp_mod)

#yeah, max temp dynamics

