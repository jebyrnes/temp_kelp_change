library(visreg)
library(effects)
library(readr)
library(dplyr)
library(ggplot2)
library(metafor)

kelp_slopes_merged <- read.csv("../derived_data/kelp_slopes_merged.csv", stringsAsFactors=FALSE) %>%
  mutate(Duration = maxYear - minYear)

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
               meanWaveChangeAnnualSample*maxTempChangeAnnualSample*I(abs(Latitude)), 
             data=ksm, random =~ 1 |Study)

all_factors_mod


### Plot results
qplot(maxTempChangeAnnualSample,
      mean, data=ksm) +
  theme_bw(base_size=17) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in Maximum Temperature During Sample Period") +
  facet_grid(DurationBreaks ~ LatBreaks,) +
  stat_smooth(method="lm")


qplot(meanWaveChangeAnnualSample,mean, 
       data=ksm) +
  scale_color_gradientn(colors=RColorBrewer::brewer.pal(7,"RdBu")) +
  theme_bw(base_size=17) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in Wave Height (m) During Sample Period") +
  facet_grid(DurationBreaks ~ LatBreaks,) +
  stat_smooth(method="lm")

