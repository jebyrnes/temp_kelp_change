###### 0) Libraries and the like
library(dplyr)
library(raster)
library(ncdf4)
library(tidyr)
library(lubridate)
source("./00a_general_extract_fun.R")  

hadsst <- raster::brick("~/Dropbox/src/HADSST/HadISST_sst.nc")

fixWaveRaster <- function(obj){
  mwa2 <- obj
  mwa2 <- raster::rotate(mwa2)
#  mwa2 <- projectRaster(mwa2, hadsst, over=T)
  
  mwa2
}

###### 1) Load the slope data file and raw data file
raw_data <- read.csv("../../temporal_change/github_repo/05_HLM_analysis_code/formatted_data_3years.csv", stringsAsFactors=F) %>%
  arrange(StudySite) 


###### 2) Extract unique lat/long info from raw data
unique_lat_long <- raw_data %>%
  group_by(Latitude, Longitude) %>%
  dplyr::summarise(len=length(unique(trajectory_ID))) %>% ungroup()

###### 3) Load the wave data 
ww3Dir <- "~/Dropbox/wavewatch3/"
meanFiles <- dir(paste0(ww3Dir, "means"), pattern="grd")
stackDates <- lubridate::parse_date_time(gsub("\\.grd", "", meanFiles), orders="ymd")
 

#Starting at 2005-02-01.grd the extent changes
multiBreak <- grep("2005-02-01.grd", meanFiles)
meanWaves_a <- fixWaveRaster(stack(paste0(ww3Dir, "means/", meanFiles[1:multiBreak-1])))
meanWaves_b <- fixWaveRaster(stack(paste0(ww3Dir, "means/", meanFiles[multiBreak:length(meanFiles)])))
#spatial_sync_raster - did not work!


maxWaves_a <- fixWaveRaster(stack(paste0(ww3Dir, "maxes/", meanFiles[1:multiBreak-1])))
maxWaves_b <- fixWaveRaster(stack(paste0(ww3Dir, "maxes/", meanFiles[multiBreak:length(meanFiles)])))


meanWaves_a@z$Date <- stackDates[1:multiBreak-1]
meanWaves_b@z$Date <- stackDates[multiBreak:length(meanFiles)]
maxWaves_a@z$Date <- stackDates[1:multiBreak-1]
maxWaves_b@z$Date <- stackDates[multiBreak:length(meanFiles)]


###### 4a) Get the radius of a cell at each lat/long specified
# Since we've reprojected to match hadsst, we can use that
#area_raster <- raster::area(meanWaves_a) #gets area in sq km
#radii <- sqrt(extract(area_raster, cbind(unique_lat_long$Longitude, unique_lat_long$Latitude)))/2*1000

###### 4b) Extract data at only those lat/longs used
robustMean <- function(x){
  if(sum(is.na(x))==length(x)) return(NA)
  mean(x, na.rm=T)
  
}

mean_wave_vals_a <- extract_vals(unique_lat_long, meanWaves_a)
mean_wave_vals_b <- extract_vals(unique_lat_long, meanWaves_b)
max_wave_vals_a <- extract_vals(unique_lat_long, maxWaves_a)
max_wave_vals_b <- extract_vals(unique_lat_long, maxWaves_b)


mean_waves_data <- cbind(mean_wave_vals_a, mean_wave_vals_b[,-c(1:3)]) %>%
  gather(DateName, meanWaves, -Latitude, -Longitude) 

max_waves_data <- cbind(max_wave_vals_a, max_wave_vals_b[,-c(1:3)]) %>%
  gather(DateName, maxWaves, -Latitude, -Longitude) 

all_waves_data <- left_join(mean_waves_data, max_waves_data)


###### 5) Write out temp wave data as an intermediate step
write.csv(all_waves_data, "../derived_data/waves_at_latlongs.csv", row.names=F)

