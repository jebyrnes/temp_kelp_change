# 
# ksm_init <- make_ksm(total_wave_energy_estimate, max_temp_slope_estimate)
# 
# duration_factors_brms <- brm(slope | se(slope_se) ~ Duration_scale*has_canopy*
#                                waves_scale*temp_scale*abs_lat_scale +
#                                (1|Study), data=ksm_init)
# 
# save(mod, file="../chain_output/fit_brms_duration_mod.Rdata")

load("../chain_output/fit_brms_duration_mod.Rdata")

make_mod_predictions <- function(adf_data){
  adf <- fitted(mod, summary=FALSE,
                re_formula = ~ -(1|Study) - (1|ecoregion),
                newdata = adf_data)
  
  adf_data$pred <- colMeans(adf)
  adf_data$ci_lb <- apply(adf, 2, quantile, prob=0.1)
  adf_data$ci_ub <- apply(adf, 2, quantile, prob=0.9)
  
  adf_data <- adf_data %>%
    mutate(temp = (temp_scale*sd(ksm_data$temp))+mean(ksm_data$temp),
           waves = (waves_scale*sd(ksm_data$waves))+mean(ksm_data$waves),
           abs_lat = (abs_lat_scale*sd(ksm_data$abs_lat))+mean(ksm_data$abs_lat),
           Duration = (Duration_scale*sd(ksm_data$Duration))+mean(ksm_data$Duration))
  
  adf_data
}


sdf <- 
  data.frame(has_canopy = c("no canopy", "canopy"), slope_se=0.04) %>% #sham se 
  crossing(Duration_scale =  quantile(ksm_data$temp_scale, probs=c(0.25, 0.75))) %>%
  crossing(temp_scale = quantile(ksm_data$temp_scale, probs=c(0.25, 0.75))) %>%
  crossing(waves_scale = seq(min(ksm_data$waves_scale), max(ksm_data$waves_scale), length.out=200)) %>%
  crossing(abs_lat_scale = quantile(ksm_data$abs_lat_scale, probs=c(0.25, 0.75))) %>%
  make_mod_predictions()

ggplot(sdf,
       mapping=aes(x=waves, y=pred, 
                   ymin=ci_lb, ymax=ci_ub, group=paste(Duration, has_canopy))) +
  geom_ribbon(alpha=0.3, mapping=aes(fill=has_canopy)) +
  geom_line(mapping=aes(color=has_canopy, lty=factor(Duration))) +
  facet_grid(temp~abs_lat) +
  geom_hline(yintercept=0, lty=2) 



sdf2 <- 
  data.frame(has_canopy = c("no canopy", "canopy"), slope_se=0.04) %>% #sham se 
  crossing(Duration_scale =  quantile(ksm_data$temp_scale, probs=c(0.25, 0.99))) %>%
  crossing(waves_scale = quantile(ksm_data$waves_scale, probs=c(0.25, 0.75))) %>%
  crossing(temp_scale = seq(min(ksm_data$temp_scale), max(ksm_data$temp_scale), length.out=200)) %>%
  crossing(abs_lat_scale = quantile(ksm_data$abs_lat_scale, probs=c(0.25, 0.75))) %>%
  make_mod_predictions()

ggplot(sdf2,
       mapping=aes(x=temp, y=pred, 
                   ymin=ci_lb, ymax=ci_ub, group=paste(Duration, has_canopy))) +
  geom_ribbon(alpha=0.3, mapping=aes(fill=has_canopy)) +
  geom_line(mapping=aes(color=has_canopy, lty=factor(Duration))) +
  facet_grid(waves~abs_lat) +
  geom_hline(yintercept=0, lty=2) 