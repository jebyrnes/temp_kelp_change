library(brms)

ksm_s <- ksm %>%
  dplyr::select(mean, se, has_canopy, max_wave_height_estimate, max_temp_slope_estimate, Latitude, Study, Duration) %>%
  mutate(waves = scale(max_wave_height_estimate), 
         temp = scale(max_temp_slope_estimate), 
         Latitude = scale(abs(Latitude)),
         Duration = scale(Duration))

all_factors_brms <- brm(mean | se(se) ~ has_canopy*
                          waves*temp*Latitude*Duration +
                          (1|Study), data=ksm_s)

all_factors_brms
#plot(marginal_effects(all_factors_brms), ask=TRUE)

a <- posterior_samples(all_factors_brms)
ctab_brms <- t(apply(a[,1:length(fixef(all_factors_brms))], 2, quantile, probs=c(0.05, 0.5, 0.95)))
ctab_brms <- as.data.frame(ctab_brms)
ctab_brms$coef_name <- factor(rownames(ctab_brms), levels=rev(rownames(ctab_brms)))

ggplot(data = ctab_brms) +
  geom_pointrange(mapping=aes(x=coef_name,
                              y = `50%`,
                              ymin=`5%`,
                              ymax = `95%`), size=1.2) +
  geom_hline(yintercept=0, lty=2) +
  coord_flip()  

library(shinystan)

launch_shiny(all_factors_brms)
