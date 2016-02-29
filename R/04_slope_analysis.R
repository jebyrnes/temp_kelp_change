library(visreg)
library(effects)
library(readr)
library(ggplot2)

kelp_slopes_merged <- read.csv("../derived_data/kelp_slopes_merged.csv", stringsAsFactors=FALSE)

kelp_slopes_merged2 <- kelp_slopes_merged %>% filter(!is.na(maxWaveChangeAnnualSample))
kelp_slopes_merged2$waveBins <- cut_interval(kelp_slopes_merged2$maxWaveChangeAnnualSample,4)

qplot(meanTempChangeAnnualSample,mean, data=kelp_slopes_merged,  geom="jitter",
      color=abs(Latitude), size=maxWaveChangeAnnualSample) +
  scale_color_gradientn(colors=RColorBrewer::brewer.pal(7,"RdBu")) +
  theme_bw() 

a <- lm(mean ~ meanTempChangeAnnualSample*scale(I(abs(Latitude)), scale=F), data=kelp_slopes_merged)
b <- lm(mean ~ bigWaveYears*meanTempChangeAnnualSample*I(abs(Latitude)), data=kelp_slopes_merged)
d <- lm(mean ~ maxWaveChangeAnnualSample*meanTempChangeAnnualSample*I(abs(Latitude)), data=kelp_slopes_merged)

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

######
library(metafor)

dm <- rma(mean, sei=se, mods = ~ maxWaveChangeAnnualSample*meanTempChangeAnnualSample*I(abs(Latitude)),, data=kelp_slopes_merged)

