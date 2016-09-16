library(visreg)
library(effects)
library(readr)
library(dplyr)
library(ggplot2)

kelp_slopes_merged <- read.csv("../derived_data/kelp_slopes_merged.csv", stringsAsFactors=FALSE) %>%
  mutate(Duration = maxYear - minYear)

kelp_slopes_merged2 <- kelp_slopes_merged %>% filter(!is.na(maxWaveChangeAnnualSample))
kelp_slopes_merged2$waveBins <- cut_interval(kelp_slopes_merged2$maxWaveChangeAnnualSample,4)

qplot(meanTempChangeAnnualSample,mean, data=kelp_slopes_merged,  geom="jitter",
      color=abs(Latitude), size=maxWaveChangeAnnualSample) +
  scale_color_gradientn(colors=RColorBrewer::brewer.pal(7,"RdBu")) +
  theme_bw() +
  ylab("Percent Change in Standardized Kelp per Year") +
  xlab("Change in Mean Temperature During Sample Period")

a <- lm(mean ~ Duration*scale(meanTempChangeAnnualSample)*scale(I(abs(Latitude)), scale=F), data=kelp_slopes_merged)
b <- lm(mean ~ Duration*bigWaveYears*meanTempChangeAnnualSample*I(abs(Latitude)), data=kelp_slopes_merged)
d <- lm(mean ~ Duration*maxWaveChangeAnnualSample*meanTempChangeAnnualSample*I(abs(Latitude)), data=kelp_slopes_merged)

predict(a, newdata=data.frame(meanTempChangeAnnualSample=-0.01, Latitude=45))
predict(a, newdata=data.frame(meanTempChangeAnnualSample=0.04, Latitude=45))

predict(a, newdata=data.frame(meanTempChangeAnnualSample=-0.01, Latitude=70))
predict(a, newdata=data.frame(meanTempChangeAnnualSample=0.04, Latitude=70))

car::Anova(b)
summary(b)


predict(b, newdata=data.frame(meanTempChangeAnnualSample=-0.01, Latitude=45, bigWaveYears=0))
predict(b, newdata=data.frame(meanTempChangeAnnualSample=-0.01, Latitude=45, bigWaveYears=8))
predict(b, newdata=data.frame(meanTempChangeAnnualSample=0.04, Latitude=45, bigWaveYears=0))
predict(b, newdata=data.frame(meanTempChangeAnnualSample=0.04, Latitude=45, bigWaveYears=8))


predict(b, newdata=data.frame(meanTempChangeAnnualSample=-0.01, Latitude=70, bigWaveYears=0))
predict(b, newdata=data.frame(meanTempChangeAnnualSample=-0.01, Latitude=70, bigWaveYears=8))
predict(b, newdata=data.frame(meanTempChangeAnnualSample=0.04, Latitude=70, bigWaveYears=0))
predict(b, newdata=data.frame(meanTempChangeAnnualSample=0.04, Latitude=70, bigWaveYears=8))


car::Anova(d)
summary(d)

library(visreg)
visreg(a)
visreg2d(a, x="meanTempChangeAnnualSample", y="Latitude")
visreg(b, by="bigWaveYears", xvar="meanTempChangeAnnualSample", breaks=4)
visreg2d(b, x="meanTempChangeAnnualSample", y="bigWaveYears")
visreg2d(d, x="meanTempChangeAnnualSample", y="maxWaveChangeAnnualSample")

par(mfrow=c(1,2))
visreg2d(d, x="meanTempChangeAnnualSample", y="maxWaveChangeAnnualSample", 
         cond=list(Latitude=45), main="Slope at Low Latitudes")
visreg2d(d, x="meanTempChangeAnnualSample", y="maxWaveChangeAnnualSample", 
         cond=list(Latitude=70), main="Slope at High Latitudes")
par(mfrow=c(1,1))

visreg2d(d, x="maxWaveChangeAnnualSample", y="Latitude")
visreg2d(d, x="meanTempChangeAnnualSample", y="Latitude")
visreg(d, xvar="maxWaveChangeAnnualSample", by="Latitude")
visreg(d, xvar="meanTempChangeAnnualSample", by="Latitude")

plot(effect("maxWaveChangeAnnualSample*meanTempChangeAnnualSample*I(abs(Latitude))", d))

###### REAL META-ANALYSIS
library(metafor)

ksm <- kelp_slopes_merged
ksm <- ksm %>% filter(!is.na(maxWaveChangeAnnualSample)) %>%
  filter(!is.na(meanTempChangeAnnualSample))



pretty_cut <- function(...){
  int <- cut(...)
  
  levels(int) <- gsub(",", " - ", levels(int))
  levels(int) <- gsub("\\(", "", levels(int))
  levels(int) <- gsub("\\]", "", levels(int))
  levels(int) <- gsub("\\[", "", levels(int))
  
  int
  
  
}
ksm$DurationBreaks <- pretty_cut(ksm$Duration, breaks=c(1,4,14))
ksm$DurationBreaks <- paste0(ksm$DurationBreaks, " years")
ksm$LatBreaks <- pretty_cut(abs(ksm$Latitude), breaks=c(20, 40, 71))
ksm$LatBreaks <- paste0(ksm$LatBreaks, " N or S")
ksm$WaveBreaks <- pretty_cut(ksm$maxWaveChangeAnnualSample, breaks=2)

ksm <- ksm %>% filter(abs(ksm$meanWaveChangeAnnualSample )< 0.3)


dm <- rma.mv(mean, V=se^2, mods = ~ Duration*
               meanWaveChangeAnnualSample*maxTempChangeAnnualSample*I(abs(Latitude)), 
             data=ksm, random =~ 1 |Study)

qplot(maxTempChangeAnnualSample,mean, data=ksm) +
  theme_bw(base_size=17) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in Mean Temperature During Sample Period") +
  facet_grid(DurationBreaks ~ LatBreaks,) +
  stat_smooth(method="lm")


qplot(meanWaveChangeAnnualSample,mean, data=ksm) +
  scale_color_gradientn(colors=RColorBrewer::brewer.pal(7,"RdBu")) +
  theme_bw(base_size=17) +
  ylab("Percent Change in\nStandardized Kelp per Year") +
  xlab("\nSlope of Annual Change in Wave Height (m) During Sample Period") +
  facet_grid(DurationBreaks ~ LatBreaks,) +
  stat_smooth(method="lm")

