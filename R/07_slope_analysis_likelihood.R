library(visreg)
library(effects)
library(readr)
library(dplyr)
library(ggplot2)
library(metafor)

kelp_slopes_merged <- read_csv("../derived_data/kelp_slopes_with_temp_waves_canopy.csv")


####Show duration effect
jpeg("../figures/duration_slopes.jpg", height=600, width=800, type="quartz")
ggplot(data=kelp_slopes_merged, 
       mapping=aes(x=Duration, y=mean, 
                   ymin=lower_0.9,
                   ymax=upper_0.9)) +
  geom_pointrange(position=position_jitter(height=0, width=1),
                  alpha=0.7, size=1) +
  theme_bw(base_size=20) +
  geom_hline(color="red", lwd=0.7, lty=2, yintercept=0) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("Duration (Years)")
dev.off()

#show decline in variation
ggplot(data=kelp_slopes_merged, 
       mapping=aes(x=Duration, y=upper_0.9-lower_0.9)) +
  geom_point(position=position_jitter(height=0, width=1),
                  alpha=0.7) +
  theme_bw() +
  ylab("Width of 90% Confidence Limit\n") +
  xlab("Duration (Years)")

#Filter it down

ksm <- kelp_slopes_merged
ksm <- ksm %>% filter(!is.na(max_wave_height_estimate)) %>%
  filter(!is.na(mean_temp_slope_estimate))

#Functions and data filtering for plotting


pretty_cut <- function(...){
  int <- cut(...)
  
  levels(int) <- gsub(",", " - ", levels(int))
  levels(int) <- gsub("\\(", "", levels(int))
  levels(int) <- gsub("\\]", "", levels(int))
  levels(int) <- gsub("\\[", "", levels(int))
  
  int
  
  
}
ksm$DurationBreaks <- pretty_cut(ksm$Duration, breaks=c(1,20,40))
ksm$DurationBreaks <- paste0(ksm$DurationBreaks, " years")
ksm$LatBreaks <- pretty_cut(abs(ksm$Latitude), breaks=c(20, 40, 71))
ksm$LatBreaks <- paste0(ksm$LatBreaks, " N or S")
ksm$WaveBreaks <- pretty_cut(ksm$max_wave_height_estimate, 
                             breaks = c(min(ksm$max_wave_height_estimate),0,max(ksm$max_wave_height_estimate)))
ksm$TempBreaks <- pretty_cut(ksm$max_temp_slope_estimate, 
                             breaks = c(min(ksm$max_temp_slope_estimate),0,max(ksm$max_temp_slope_estimate)))

#ksm <- ksm %>% filter(abs(ksm$meanWaveChangeSample )< 0.3)


#Big plot!
qplot(max_temp_slope_estimate,mean, data=kelp_slopes_merged,  geom="jitter",
      color=abs(Latitude), size=max_wave_height_estimate) +
  scale_color_gradientn(colors=RColorBrewer::brewer.pal(7,"RdBu")) +
  theme_bw() +
  ylab("Percent Change in Standardized Kelp per Year") +
  xlab("Trend in Maximum Temperature During Sample Period")

###### REAL META-ANALYSIS

all_factors_mod <- rma.mv(mean, V=se^2, mods = ~ has_canopy*
                            max_wave_height_estimate*max_temp_slope_estimate*I(abs(Latitude))-1, 
             data=ksm, random =~ 1 |Study)

all_factors_mod

ctab <- coef(summary(all_factors_mod))
ctab$coef_name <- factor(rownames(ctab), levels=rev(rownames(ctab)))
ctab$sig <- ctab$pval <= 0.05

ggplot(ctab, mapping=aes(x=coef_name,
                         y = estimate,
                         ymin = ci.lb,
                         ymax = ci.ub,
                         color=sig)) +
  geom_pointrange() +
  geom_hline(yintercept=0, lty=2) +
  coord_flip()

### Plot results for temperature
ksm <- ksm %>% 
  mutate(Canopy_Lat_Break = paste(has_canopy, LatBreaks, sep="\n")) %>%
  filter(!is.na(WaveBreaks)) %>%
  filter(!is.na(TempBreaks))
  
qplot(max_temp_slope_estimate,
      mean, color=Study, group="1", 
      data=ksm ) +
  theme_bw(base_size=22) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in Maximum Temperature\nDuring Sample Period") +
  facet_grid( WaveBreaks~Canopy_Lat_Break, scale="free") +
  stat_smooth(method="lm", lwd=2, color="black") +
  scale_color_discrete(guide="none")

#waves
qplot(max_wave_height_estimate,
      mean, color=Study, group="1", 
      data=ksm ) +
  theme_bw(base_size=22) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in Maximum Wave Height\nDuring Sample Period") +
  facet_grid(TempBreaks ~ Canopy_Lat_Break, scale="free") +
  stat_smooth(method="lm", lwd=2, color="black") +
  scale_color_discrete(guide="none")


#long-term
qplot(mean_temp_slope_estimate,
      mean, color=Study, group="1", 
      data=ksm %>% filter(DurationBreaks!="1 - 6 years")) +
  theme_bw(base_size=22) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in Maximum Temperature\nDuring Sample Period") +
  facet_grid(DurationBreaks ~ LatBreaks) +
  stat_smooth(method="lm", lwd=2, color="black") +
  scale_color_discrete(guide="none")

#check something
ksm %>% filter(DurationBreaks!="1 - 6 years",
               LatBreaks == "40 - 71 N or S",
               maxTempChangeSample>0.05) 


### Plot results for waves
qplot(max_wave_height_estimate,
      mean, color=Study, group="1", 
       data = ksm %>% filter(DurationBreaks=="1 - 20 years")) +
  scale_color_gradientn(colors=RColorBrewer::brewer.pal(7,"RdBu")) +
  theme_bw(base_size=22) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in 90th Percentile\nWave Height (m) During Sample Period") +
  facet_grid(DurationBreaks ~ LatBreaks) +
  stat_smooth(method="lm", lwd=2, color="black")+
  scale_color_discrete(guide="none")


qplot(max_wave_height_estimate,
      mean, color=Study, group="1", 
      data=ksm %>% filter(DurationBreaks!="20 - 6 years")) +
  scale_color_gradientn(colors=RColorBrewer::brewer.pal(7,"RdBu")) +
  theme_bw(base_size=22) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in 90th Percentile\nWave Height (m) During Sample Period") +
  facet_grid(DurationBreaks ~ LatBreaks) +
  stat_smooth(method="lm", lwd=2, color="black")+
  scale_color_discrete(guide="none")


###### Temperature only
kst <- kelp_slopes_merged %>%
  filter(!is.na(meanTempChangeSample))


temp_mod <- rma.mv(mean, V=se^2, mods = ~ Duration*
                     meanTempChangeSample*I(abs(Latitude)), 
                          data=kst, random =~ 1 |Study)

temp_mod

###### Plot temperature results
kst$DurationBreaks <- pretty_cut(kst$Duration, breaks=c(1,20,40))
kst$DurationBreaks <- paste0(kst$DurationBreaks, " years")
kst$LatBreaks <- pretty_cut(abs(kst$Latitude), breaks=c(20, 40, 71))
kst$LatBreaks <- paste0(kst$LatBreaks, " N or S")

qplot(maxTempChangeSample,
      mean, color=Study, group="1", 
      data=kst %>% filter(DurationBreaks!="1 - 20 years")) +
  theme_bw(base_size=22) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in 90th Percentile Temperature\nDuring Sample Period") +
  facet_grid(DurationBreaks ~ LatBreaks, scale="free_x") +
  stat_smooth(method="lm", color="black", lwd=2) +
  facet_wrap(~Study)+
  scale_color_discrete(guide="none")


kst %>% filter(DurationBreaks=="1 - 20 years",
               LatBreaks!="20 - 40 N or S") %>%
  group_by(Study) %>% dplyr::slice(1L) %>% ungroup() %>%
  filter(maxTempChangeSample > 0.05) %>%
  select(Study)

qplot(maxTempChangeSample,
      mean, color=Study, group="1",
      data=kst %>% filter(DurationBreaks!="1 - 20 years")) +
  theme_bw(base_size=22) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in 90th Percentile Temperature\nDuring Sample Period") +
  facet_grid(DurationBreaks ~ LatBreaks, scale="free_x") +
  stat_smooth(method="lm", color="black", lwd=2) +
  scale_color_discrete(guide="none")

kst %>% filter(DurationBreaks!="1 - 20 years",
               LatBreaks=="20 - 40 N or S")