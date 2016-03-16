library(visreg)
library(effects)
library(readr)
library(dplyr)
library(ggplot2)
library(metafor)

kelp_slopes_merged <- read.csv("../derived_data/kelp_slopes_merged.csv", stringsAsFactors=FALSE) %>%
  mutate(Duration = maxYear - minYear)

####Show duration effect
ggplot(data=kelp_slopes_merged, 
       mapping=aes(x=Duration, y=mean, 
                   ymin=lower_0.9,
                   ymax=upper_0.9)) +
  geom_pointrange(position=position_jitter(height=0, width=1),
                  alpha=0.7) +
  theme_bw() +
  geom_hline(color="red", lwd=0.7, lty=2, yintercept=0) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("Duration (Years)")

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
ksm <- ksm %>% filter(!is.na(maxWaveChangeAnnualSample)) %>%
  filter(!is.na(meanTempChangeAnnualSample))

#Functions and data filtering for plotting


pretty_cut <- function(...){
  int <- cut(...)
  
  levels(int) <- gsub(",", " - ", levels(int))
  levels(int) <- gsub("\\(", "", levels(int))
  levels(int) <- gsub("\\]", "", levels(int))
  levels(int) <- gsub("\\[", "", levels(int))
  
  int
  
  
}
ksm$DurationBreaks <- pretty_cut(ksm$Duration, breaks=c(1,6,14))
ksm$DurationBreaks <- paste0(ksm$DurationBreaks, " years")
ksm$LatBreaks <- pretty_cut(abs(ksm$Latitude), breaks=c(20, 40, 71))
ksm$LatBreaks <- paste0(ksm$LatBreaks, " N or S")
ksm$WaveBreaks <- pretty_cut(ksm$maxWaveChangeAnnualSample, breaks=2)

#ksm <- ksm %>% filter(abs(ksm$meanWaveChangeAnnualSample )< 0.3)


#Big plot!
qplot(maxTempChangeAnnualSample,mean, data=kelp_slopes_merged,  geom="jitter",
      color=abs(Latitude), size=meanWaveChangeAnnualSample) +
  scale_color_gradientn(colors=RColorBrewer::brewer.pal(7,"RdBu")) +
  theme_bw() +
  ylab("Percent Change in Standardized Kelp per Year") +
  xlab("Trend in Maximum Temperature During Sample Period")

###### REAL META-ANALYSIS

all_factors_mod <- rma.mv(mean, V=se^2, mods = ~ Duration*
               maxWaveChangeAnnualSample*maxTempChangeAnnualSample*I(abs(Latitude)), 
             data=ksm, random =~ 1 |Study)

all_factors_mod


### Plot results for temperature
qplot(maxTempChangeAnnualSample,
      mean, color=Study, group="1", 
      data=ksm %>% filter(DurationBreaks=="1 - 6 years")) +
  theme_bw(base_size=17) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in Maximum Temperature During Sample Period") +
  facet_grid(DurationBreaks ~ LatBreaks) +
  stat_smooth(method="lm") +
  scale_color_discrete(guide="none")

#long-term
qplot(maxTempChangeAnnualSample,
      mean, color=Study, group="1", 
      data=ksm %>% filter(DurationBreaks!="1 - 6 years")) +
  theme_bw(base_size=17) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in Maximum Temperature During Sample Period") +
  facet_grid(DurationBreaks ~ LatBreaks) +
  stat_smooth(method="lm") +
  scale_color_discrete(guide="none")


### Plot results for waves
qplot(maxWaveChangeAnnualSample,
      mean, color=Study, group="1", 
       data=ksm) +
  scale_color_gradientn(colors=RColorBrewer::brewer.pal(7,"RdBu")) +
  theme_bw(base_size=17) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in 90th Percentile Wave Height (m) During Sample Period") +
  facet_grid(DurationBreaks ~ LatBreaks) +
  stat_smooth(method="lm")+
  scale_color_discrete(guide="none")


###### Temperature only
kst <- kelp_slopes_merged %>%
  filter(!is.na(maxTempChangeAnnualSample))


temp_mod <- rma.mv(mean, V=se^2, mods = ~ Duration*
                     maxTempChangeAnnualSample*I(abs(Latitude)), 
                          data=ksm, random =~ 1 |Study)

temp_mod

###### Plot temperature results
kst$DurationBreaks <- pretty_cut(kst$Duration, breaks=c(1,20,40))
kst$DurationBreaks <- paste0(kst$DurationBreaks, " years")
kst$LatBreaks <- pretty_cut(abs(kst$Latitude), breaks=c(20, 40, 71))
kst$LatBreaks <- paste0(kst$LatBreaks, " N or S")

qplot(maxTempChangeAnnualSample,
      mean, color=Study, group="1", 
      data=kst %>% filter(DurationBreaks=="1 - 20 years")) +
  theme_bw(base_size=17) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in Maximum Temperature During Sample Period") +
  facet_grid(DurationBreaks ~ LatBreaks, scale="free_x") +
  stat_smooth(method="lm") +
  scale_color_discrete(guide="none")

qplot(maxTempChangeAnnualSample,
      mean, color=Study, group="1",
      data=kst %>% filter(DurationBreaks!="1 - 20 years")) +
  theme_bw(base_size=17) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in Maximum Temperature During Sample Period") +
  facet_grid(DurationBreaks ~ LatBreaks, scale="free_x") +
  stat_smooth(method="lm") +
  scale_color_discrete(guide="none")

