source("./06_data_for_analysis_gen.R")
library(brms)
options(mc.cores = parallel::detectCores())
library(ggplot2)

########
#Total Wave Energy Model
########
ksm_data <- make_ksm(max_wave_height_estimate, max_temp_slope_estimate)
# 
# mod <- brm(slope | se(slope_se, sigma=TRUE) ~ has_canopy*
#                                   waves_scale*temp_scale*abs_lat_scale +
#                                   (1|Study) + (1|ecoregion), 
#                                   data=make_ksm(max_wave_height_estimate, max_temp_slope_estimate))
# 
# mod_me <- brm(slope | se(slope_se, sigma=FALSE) ~ has_canopy*
#                                   me(waves_scale, se_waves_scale)*
#                                   me(temp_scale, se_temp_scale)*
#                                   abs_lat_scale +
#                                   (1|Study) + (1|ecoregion),
#                                   data=make_ksm(max_wave_height_estimate, max_temp_slope_estimate) %>% filter(se_temp_scale>0) %>% filter(se_waves_scale>0) %>% mutate(has_canopy = gsub(" ", "_", has_canopy)), save_mevars=TRUE, chains=3, iter=1e6)
# 
# save(mod_me, file="../chain_output/fit_brms_mod_me.Rdata")
#
# mod_nocanopy <- brm(slope | se(slope_se, sigma=TRUE) ~ 
#              waves_scale*temp_scale*abs_lat_scale +
#              (1|Study) + (1|ecoregion), 
#            data=make_ksm(max_wave_height_estimate, max_temp_slope_estimate))
# 
# WAIC(mod, mod_nocanopy)
# LOO(mod, mod_nocanopy)

#save(mod, file="../chain_output/fit_brms_mod.Rdata")

load("../chain_output/fit_brms_mod.Rdata")

summary(mod)
chains <- as.data.frame(mod)
t(apply(chains[,1:10], 2, quantile, probs=c(0.1, 0.9)))

ksm_data <- make_ksm(max_wave_height_estimate, max_temp_slope_estimate)

res <- residuals(mod, summary=FALSE)
fit_vals <- predict(mod, summary=FALSE)

plot(colMeans(fit_vals), colMeans(res))
#plot(colMeans(res), ksm_data$slope)
#plot(colMeans(fit_vals), ksm_data$slope)
#plot(colMeans(res), colMeans(fit_vals))

########
#Coefficient Plot
########

ctab_brms <- t(apply(a[,1:length(fixef(mod))], 2, quantile, probs=c(0.05, 0.5, 0.95)))
ctab_brms <- as.data.frame(ctab_brms)
ctab_brms$coef_name <- factor(rownames(ctab_brms), levels=rownames(ctab_brms))

ctab_brms$coef_name <- gsub("b_", "", ctab_brms$coef_name)
ctab_brms$coef_name <- gsub("_", " ", ctab_brms$coef_name)
ctab_brms$coef_name <- gsub(" scale", "", ctab_brms$coef_name)
ctab_brms$coef_name <- gsub("temp", "Temperature", ctab_brms$coef_name)
ctab_brms$coef_name <- gsub("waves", "Waves", ctab_brms$coef_name)
ctab_brms$coef_name <- gsub("abs lat", "Latitude", ctab_brms$coef_name)
ctab_brms$coef_name <- gsub("has canopynocanopy", "No Canopy", ctab_brms$coef_name)
ctab_brms$coef_name <- factor(ctab_brms$coef_name, levels=rev(ctab_brms$coef_name))

ggplot(data = ctab_brms) +
  geom_pointrange(mapping=aes(x=coef_name,
                              y = `50%`,
                              ymin=`5%`,
                              ymax = `95%`), size=1.2) +
  geom_hline(yintercept=0, lty=2) +
  coord_flip() + 
  theme_bw(base_size=17) + xlab("") +
  ylab("Standardized Coefficient")

########
#Model Validation
########

plot(fitted(mod)[,1], residuals(mod)[,1])
plot(fitted(mod)[,1], na.omit(mod$data$slope))
qqnorm(residuals(mod)[,1])
qqline(residuals(mod)[,1]) #Hrm.............


pp_check(mod, type = "dens")
pp_check(mod, type = "dens_overlay")
pp_check(mod, type = "ecdf_overlay")
pp_check(mod, type = "error_scatter")
pp_check(mod, type = "error_scatter_avg")
pp_check(mod, type = "error_binned")
pp_check(mod, type = "scatter")


########
#Show the data
########
ksm_data$lat_breaks <- ifelse(ksm_data$abs_lat<40, "<40 Degrees Latitude", ">40 Degrees Latitude")
ksm_data$wave_breaks <- ifelse(ksm_data$waves<0, "Waves Decreasing", "Waves Increasing")

dataPlot <- ggplot(data = ksm_data,
       mapping=aes(x=temp, y=slope, color=wave_breaks)) +
  geom_point(size=2) +
  facet_grid(has_canopy~lat_breaks) +
  scale_color_manual(values=c("blue", "red"), guide=guide_legend(title="")) +
  theme_bw(base_size=17) +
  ylab("Proportional Change in Kelp per Year") +
  xlab("Change in Degrees C per Decade")

dataPlot

dataPlot+xlim(c(-0.5, 0.5))
########
#Counterfactual Plots
########


make_mod_predictions <- function(adf_data){
  adf <- fitted(mod, summary=FALSE,
                re_formula = NA,
                newdata = adf_data)
  
  adf_data$pred <- colMeans(adf)
  adf_data$ci_lb <- apply(adf, 2, quantile, prob=0.1)
  adf_data$ci_ub <- apply(adf, 2, quantile, prob=0.9)
  
  adf_data <- adf_data %>%
    mutate(temp = (temp_scale*sd(ksm_data$temp))+mean(ksm_data$temp),
           waves = (waves_scale*sd(ksm_data$waves))+mean(ksm_data$waves),
           abs_lat = (abs_lat_scale*sd(ksm_data$abs_lat))+mean(ksm_data$abs_lat))
  
  adf_data
}
#Plot effect of temperature at low latitudes, low and high wave energy

temp_low_lat_lh_waves_data <- 
  data.frame(has_canopy = c("no canopy", "canopy"), slope_se=0.04) %>% #sham se 
  crossing(waves_scale = c(-1,1)) %>%
  crossing(temp_scale = seq(min(ksm_data$temp_scale), max(ksm_data$temp_scale), length.out=200)) %>%
  crossing(abs_lat_scale = quantile(ksm_data$abs_lat_scale, probs=c(0.25, 0.75))) %>%
  make_mod_predictions() %>%
  mutate(waves = round(waves, 3), abs_lat = round(abs_lat,0)) %>%
  mutate(wave_label = ifelse(waves<0, paste0("Wave Decrease: ", waves, "m\nper decade"),
                             paste0("Wave Increase: ", waves, "m\nper decade")),
         lat_label = paste0(abs_lat, "° Latutide"))


ggplot(temp_low_lat_lh_waves_data %>% filter(has_canopy=="canopy"),
       mapping=aes(x=temp, y=pred, 
                   ymin=ci_lb, ymax=ci_ub)) +
  geom_ribbon(alpha=0.3, mapping=aes(fill=wave_label)) + 
  geom_line(mapping=aes(color=wave_label), size=2) +
  facet_wrap(~lat_label) +
  geom_hline(yintercept=0, lty=2) +
  scale_color_manual(values=c("blue", "red"), guide=guide_legend(title="")) +
  scale_fill_manual(values=c("blue", "red"), guide="none") +
  theme_bw(base_size=17) +
  ylab("Proportional Change in Kelp per Year") +
  xlab("Change in Degrees C per Decade")# + xlim(c(-1,1))


ggplot(temp_low_lat_lh_waves_data %>% filter(has_canopy!="canopy"),
       mapping=aes(x=temp, y=pred, 
                   ymin=ci_lb, ymax=ci_ub)) +
  geom_ribbon(alpha=0.3, mapping=aes(fill=wave_label)) + 
  geom_line(mapping=aes(color=wave_label), size=2) +
  facet_wrap(~lat_label) +
  geom_hline(yintercept=0, lty=2) +
  scale_color_manual(values=c("blue", "red"), guide=guide_legend(title="")) +
  scale_fill_manual(values=c("blue", "red"), guide="none") +
  theme_bw(base_size=17) +
  ylab("Proportional Change in Kelp per Year") +
  xlab("Change in Degrees C per Decade")# + xlim(c(-1,1))


#Now waves

waves_low_lat_lh_temp_data <- 
  data.frame(has_canopy = c("no canopy", "canopy"), slope_se=0.04) %>% #sham se 
  crossing(temp_scale = c(-1,1)) %>%
  crossing(waves_scale = seq(min(ksm_data$waves_scale), max(ksm_data$waves_scale), length.out=200)) %>%
  crossing(abs_lat_scale = quantile(ksm_data$abs_lat_scale, probs=c(0.25, 0.75))) %>%
  make_mod_predictions() %>%
  mutate(temp = round(temp, 3), abs_lat = round(abs_lat,0) )%>%
  mutate(temp_label = ifelse(temp<0, paste0("Cooling: ", temp, " deg. per decade"),
                             paste0("Warming: ", temp, " deg. per decade")),
         lat_label = paste0(abs_lat, "° Latutide"))



ggplot(waves_low_lat_lh_temp_data %>% filter(has_canopy == "canopy"),
       mapping=aes(x=waves, y=pred, 
                   ymin=ci_lb, ymax=ci_ub)) +
  geom_ribbon(alpha=0.3, mapping=aes(fill=temp_label)) +
  geom_line(mapping=aes(color=temp_label), size=2) +
  facet_grid(.~lat_label) +
  geom_hline(yintercept=0, lty=2) +
  scale_color_manual(values=c("blue", "red"), guide=guide_legend(title="")) +
  scale_fill_manual(values=c("blue", "red"), guide="none") +
  theme_bw(base_size=17) +
  ylab("Proportional Change in Kelp per Year") +
  xlab("Change in Max Wave Height (m) per Decade")


ggplot(waves_low_lat_lh_temp_data %>% filter(has_canopy == "no canopy"),
       mapping=aes(x=waves, y=pred, 
                   ymin=ci_lb, ymax=ci_ub)) +
  geom_ribbon(alpha=0.3, mapping=aes(fill=temp_label)) +
  geom_line(mapping=aes(color=temp_label), size=2) +
  facet_grid(.~lat_label) +
  geom_hline(yintercept=0, lty=2) +
  scale_color_manual(values=c("blue", "red"), guide=guide_legend(title="")) +
  scale_fill_manual(values=c("blue", "red"), guide="none") +
  theme_bw(base_size=17) +
  ylab("Proportional Change in Kelp per Year") +
  xlab("Change in Max Wave Height (m) per Decade")


##########
#Plots of wave and temp coefficients as each changes
##########
names(chains)[1:16] <- gsub("\\.", "\\:", names(chains)[1:16])
temp_effect <- 
  data.frame(has_canopy = c("no canopy", "canopy")) %>% 
  crossing(waves_scale = seq(min(ksm_data$waves_scale), max(ksm_data$waves_scale), length.out=30)) %>%
  crossing(abs_lat_scale = quantile(ksm_data$abs_lat_scale, probs=c(0.25, 0.75))) %>%
  crossing(chains[,1:16]) %>%
  mutate(nocanopy = ifelse(has_canopy=="no canopy",1,0)) %>%
  mutate(temp_effect = b_temp_scale + 
           `b_has_canopynocanopy:temp_scale`*nocanopy +
           `b_waves_scale:temp_scale`*waves_scale +
           `b_temp_scale:abs_lat_scale` * abs_lat_scale +
           `b_has_canopynocanopy:waves_scale:temp_scale` * nocanopy * waves_scale +
           `b_has_canopynocanopy:temp_scale:abs_lat_scale`*nocanopy*abs_lat_scale +
           `b_waves_scale:temp_scale:abs_lat_scale` * waves_scale * abs_lat_scale +
           `b_has_canopynocanopy:waves_scale:temp_scale:abs_lat_scale` * nocanopy*waves_scale*abs_lat_scale
           ) %>%
  group_by(has_canopy, waves_scale,abs_lat_scale) %>%
  dplyr::summarise(cl_ub = quantile(temp_effect, prob=.95),
                   cl_lb = quantile(temp_effect, prob=0.05),
                   temp_effect = mean(temp_effect)) %>%
  ungroup() %>%
  mutate(waves = (waves_scale*sd(ksm_data$waves))+mean(ksm_data$waves),
         abs_lat = (abs_lat_scale*sd(ksm_data$abs_lat))+mean(ksm_data$abs_lat),
         abs_lat_label = paste0("Latitude: ", round(abs_lat,0), "°"))

ggplot(data=temp_effect, 
       mapping = aes(x=waves,
                     y = temp_effect,
                     ymin=cl_lb, ymax=cl_ub, color=abs_lat_label)) +
  geom_pointrange() + 
 # facet_wrap(~abs_lat_label) +
  facet_wrap(~has_canopy, scale="free_y") +
  theme_bw(base_size=17) +
  geom_hline(yintercept=0, lty=2) +
  ylab("Standardized Effect of\nTemperature Change on Kelps") +
  xlab("Change in Max Wave Height (m) per Decade") +
  scale_color_manual(values=c("blue", "red"), guide=guide_legend(title=""))

#Plot how wave effect changes with temp
  
 wave_effect_data <- 
    data.frame(has_canopy = c("no canopy", "canopy")) %>% 
    crossing(temp_scale = seq(min(ksm_data$temp_scale), max(ksm_data$temp_scale), length.out=30)) %>%
    crossing(abs_lat_scale = quantile(ksm_data$abs_lat_scale, probs=c(0.25, 0.75))) %>%
    crossing(chains[,1:16]) %>%
    mutate(nocanopy = ifelse(has_canopy=="no canopy",1,0)) %>%
    mutate(wave_effect = b_waves_scale + 
             `b_has_canopynocanopy:waves_scale`*nocanopy +
             `b_waves_scale:temp_scale`*temp_scale +
             `b_waves_scale:abs_lat_scale` * abs_lat_scale +
             `b_has_canopynocanopy:waves_scale:temp_scale` * nocanopy * temp_scale +
             `b_has_canopynocanopy:waves_scale:abs_lat_scale`*nocanopy*abs_lat_scale +
             `b_waves_scale:temp_scale:abs_lat_scale` * temp_scale * abs_lat_scale +
             `b_has_canopynocanopy:waves_scale:temp_scale:abs_lat_scale` * nocanopy*temp_scale*abs_lat_scale
    ) %>%
    group_by(has_canopy, temp_scale,abs_lat_scale) %>%
    dplyr::summarise(cl_ub = quantile(wave_effect, prob=.95),
                     cl_lb = quantile(wave_effect, prob=0.05),
                     wave_effect = mean(wave_effect)) %>%
    ungroup() %>%
    mutate(temp = (temp_scale*sd(ksm_data$temp))+mean(ksm_data$temp),
           abs_lat = (abs_lat_scale*sd(ksm_data$abs_lat))+mean(ksm_data$abs_lat),
           abs_lat_label = paste0("Latitude: ", round(abs_lat,0), "°"))

 
 ggplot(data=wave_effect_data, 
        mapping = aes(x=temp,
                      y = wave_effect,
                      ymin=cl_lb, ymax=cl_ub, color=abs_lat_label)) +
   geom_pointrange() + 
   facet_wrap(~has_canopy, scale="free_y") +
   theme_bw(base_size=17) +
   geom_hline(yintercept=0, lty=2) +
   scale_color_discrete(guide=guide_legend(title="")) +
   ylab("Standardized Effect of\nWave Change on Kelps") +
   xlab("Change in Max Temp (C) per Decade") 
 
########
#Counterfactual Surfaces
########
library(modelr)
library(forcats)

newdata <- data_grid(ksm_data,
                     has_canopy = unique(has_canopy),
                     waves_scale = seq(-1,1,length.out=100),
                     temp_scale = seq(-3,3,length.out=100),
                     # waves_scale = seq_range(waves_scale,100),
                     # temp_scale = seq_range(temp_scale, 100),
                     abs_lat_scale = seq_range(abs_lat_scale, 5),
                     slope_se = median(slope_se))


fit <- fitted(mod, newdata = newdata, nsamples = 1000, re_formula=NA)
write.csv(cbind(newdata, fit), "../derived_data/mod_output.csv")


fit <- read_csv("../derived_data/mod_output.csv")

fit <- fit %>%
  mutate(Change = as.character(sign(Q2.5) + sign(Q97.5))) %>%
  mutate(waves = waves_scale * sd(ksm_data$waves) + mean(ksm_data$waves),
         temp = temp_scale * sd(ksm_data$temp) + mean(ksm_data$temp),
         abs_lat = round(abs_lat_scale * sd(ksm_data$abs_lat) + mean(ksm_data$abs_lat)),
         Change = case_when(Change<0 ~ "Loss", 
                            Change > 0 ~ "Gain",
                           TRUE ~ "No change"),
         Change = factor(Change, levels = c("Gain", "No change", "Loss")),
         where = factor(abs_lat),
         where = fct_recode(where, 
                             "Baja,\n Peru" = "21",
                             "Japan,\n W. Australia" = "32",
                             "Gulf of Maine,\n Tasmania" = "43",
                             "Northern Canada,\n Southern Chile" = "55",
                             "Norway,\n Northern Alaska" = "66"
                             ),
         where = fct_rev(where),
         has_canopy = case_when(has_canopy=="canopy" ~ "Canopy Kelps",
                                TRUE ~ "Non-Canopy Forming Kelps")
  )

ggplot(fit %>% filter(abs(temp)<=1) %>% filter(abs(waves) <=0.5), 
       aes(x=temp, y = waves, fill=Change, alpha=abs(Estimate))) +
  facet_grid(where ~ has_canopy) +
  geom_raster() +
  scale_fill_manual(values=c("blue", "grey", "red"),
                    guide = guide_legend("Trajectory of\nKelp Change")) +
  theme_bw(base_size=17) +
  theme(legend.background = element_blank(), 
        legend.key = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        strip.background = element_blank(), 
        plot.background = element_blank(),
        strip.text.y = element_text(angle = 0), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 10)) +
  xlab("Change in 90th Percentile Temperature (Degrees/yr)") +
  ylab("Change in 90th Percentile Wave Heights (m/yr)") +
  scale_alpha(guide = guide_legend("Absolute\nMagnitude\n(Std. Kelp/Year)")) 
  
