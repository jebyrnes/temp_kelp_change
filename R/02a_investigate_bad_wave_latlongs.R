#build a data frame of bad latlongs
bad_latLongs <- mean_waves_data %>% filter(is.na(meanWaves))
nrow(bad_latLongs)
nrow(mean_waves_data)

bad_latLongs <- bad_latLongs %>% group_by(Latitude, Longitude) %>%
  summarize(len = n()) %>% ungroup()

nrow(bad_latLongs)


#turn them into spatial points
bll <- SpatialPoints(as.matrix(bad_latLongs[,2:1]))

#see roughly where they fall
plot(meanWaves_a[[1]])
plot(bll, add=T)

#define an extent - plot the hadsst or meanwaves data and grab it using
#dput(raster::drawExtent())
bad_extent <- raster::drawExtent()

par(mfrow=c(1,2))
plot(crop(hadsst[[1]], bad_extent))
plot(bll, add=T)

plot(crop(meanWaves_a[[1]], bad_extent))
plot(bll, add=T)
par(mfrow=c(1,1))
