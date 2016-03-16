library(dplyr)
library(readr)
library(tidyr)

###### 1) Load the slope data file and raw data file
kelp_slopes <- read.csv("../../temporal_change/github_repo/06_HLM_output/site_slopes_3_points.csv", stringsAsFactors=F)

raw_data <- read.csv("../../temporal_change/github_repo/05_HLM_analysis_code/formatted_data_3years.csv", stringsAsFactors=F) %>%
  arrange(StudySite) %>%
  dplyr::rename(Year=year)

hadsst_kelp <- read_csv("../derived_data/hadsst_at_latlongs.csv") %>%
  mutate(DateName = lubridate::parse_date_time(DateName, orders="ymd"))

all_waves_data <- read_csv("../derived_data/waves_at_latlongs.csv") %>%
  mutate(Year = lubridate::year(DateName), 
         DateName = lubridate::parse_date_time(DateName, orders="ymd"))

envt_data <- left_join(hadsst_kelp, all_waves_data)

###### 2) Filter kelp_slopes down to site-level slopes
kelp_slopes <- kelp_slopes %>% filter(parameter=="site_slope") %>%
  filter(grouping=="Ecoregion") %>%
  filter(Period == "1900-2015") %>%
  arrange(SiteName)

###### 3) Filter the environmenta; data to annual summaries
hadsst_kelp_annual <- hadsst_kelp %>%
  group_by(Latitude, Longitude, Year) %>%
  dplyr::summarise(mean_tempC = mean(tempC, na.rm=T), max_tempC = quantile(tempC, probs=0.9, na.rm=T))

waves_annual <- all_waves_data %>%
  group_by(Latitude, Longitude, Year) %>%
  dplyr::summarise(mean_waves = mean(meanWaves, na.rm=T), max_waves = max(maxWaves, na.rm=T))

envt_annual <- left_join(hadsst_kelp_annual, waves_annual)

#qplot(Year, mean_waves, color=paste(Latitude, Longitude), geom="line", 
#      data=envt_annual, alpha=I(0.3)) +
#  scale_color_discrete(guide="none") 


###### 4) For each unique lat/long/duration combo, extract
###### the linear trend in annual mean and annual max temperature
slopeSE <- function(x) sqrt(diag(vcov(x)))[2]

hadsst_trends_long <- envt_annual %>%
  filter(!is.na(mean_tempC)) %>%
  group_by(Latitude, Longitude) %>%
  nest() %>%
  mutate(maxTempMod = purrr::map(data, ~lm(max_tempC ~ Year, data=.)),
         meanTempMod = purrr::map(data, ~lm(mean_tempC ~ Year, data=.))) %>%
  
  #coefs and SEs
  mutate(maxTempChangeAnnual= purrr::map_dbl(maxTempMod, ~coef(.)[2]),
         se_maxTempChangeAnnual = purrr::map_dbl(maxTempMod, ~slopeSE(.)),
         meanTempChangeAnnual=purrr::map_dbl(meanTempMod, ~coef(.)[2]),
         se_meanTempChangeAnnual = purrr::map_dbl(meanTempMod, ~sqrt(diag(vcov(.)))[2])
  )  %>%
  dplyr::select(-data, -maxTempMod, -meanTempMod) %>%
  ungroup()

#qplot(Longitude, Latitude, color=meanTempChangeAnnual, data=hadsst_trends_long) +
#  borders("world") +
#  scale_color_gradientn(colors=rev(RColorBrewer::brewer.pal(7,"RdBu"))) +
#  theme_bw()

###### 4) Calculate trends for just the timespan of datasets
raw_data_spans <- group_by(raw_data, Latitude, Longitude, Study, StudySite, trajectory_ID) %>%
  dplyr::summarise(minYear = min(Year), maxYear = max(Year)) %>% 
  ungroup() %>%
  group_by(StudySite, Study, trajectory_ID) %>%
  dplyr::summarise(Latitude = Latitude[1], Longitude = Longitude[1],  #because some transects move b/t years
                   minYear = min(minYear), maxYear = max(maxYear))

#Take a line of the raw_data_spans dataset, and returns slopes for that span of time
getSlopeCoef <- function(adf){
  subdata <- envt_annual %>%
    filter( Year >= adf$minYear) %>%
    filter(Year <= adf$maxYear) %>%
    filter(Latitude == adf$Latitude) %>%
    filter(Longitude == adf$Longitude)
  

  #print(adf)
  #print(subdata)
  
  #ret <- data.frame(minYear = adf$minYear, maxYear=adf$maxYear,
  #  maxTempChangeAnnualSample = coef(lm(max_tempC ~ Year, data=subdata))[2],
  #           meanTempChangeAnnualSample=coef(lm(mean_tempC ~ Year, data=subdata))[2])
  if(sum(is.na(subdata$mean_tempC))!=nrow(subdata)){
    ret <- subdata %>% 
      nest() %>%
      mutate(maxTempChangeAnnualSample_lm =purrr::map(data, ~lm(max_tempC ~ Year, data=.))) %>%
      mutate(meanTempChangeAnnualSample_lm =purrr::map(data, ~lm(mean_tempC ~ Year, data=.))) %>%
    
      mutate(minYear =adf$minYear,
           maxYear = adf$maxYear,
           maxTempChangeAnnualSample= purrr::map_dbl(maxTempChangeAnnualSample_lm, ~coef(.)[2]),
           se_maxTempChangeAnnualSample = purrr::map_dbl(maxTempChangeAnnualSample_lm, ~slopeSE(.)),
           meanTempChangeAnnualSample=purrr::map_dbl(meanTempChangeAnnualSample_lm, ~coef(.)[2]),
           se_meanTempChangeAnnualSample = purrr::map_dbl(meanTempChangeAnnualSample_lm, ~sqrt(diag(vcov(.)))[2])
      ) %>% 
      dplyr::select(-data, -maxTempChangeAnnualSample_lm, 
                  -meanTempChangeAnnualSample_lm, -Latitude, -Longitude)
  }else{
    ret <- data.frame(minYear = adf$minYear, maxYear=adf$maxYear,
                      maxTempChangeAnnualSample=NA,
                      se_maxTempChangeAnnualSample=NA,
                      meanTempChangeAnnualSample=NA,
                      se_meanTempChangeAnnualSample=NA)
  }
  ###
  if(sum(!is.na(subdata$mean_waves))==adf$maxYear-adf$minYear+1){
    maxwavemod <- lm(max_waves ~ Year, data=subdata)
    meanwavemod <- lm(mean_waves ~ Year, data=subdata)
    wave_ret <- data.frame(maxWaveChangeAnnualSample = coef(maxwavemod)[2],
                           se_maxWaveChangeAnnualSample = slopeSE(maxwavemod),
                           meanWaveChangeAnnualSample = coef(meanwavemod)[2],
                           se_meanWaveChangeAnnualSample = slopeSE(meanwavemod),
                           bigWaveYears = sum(subdata$max_waves>8))
    
  }else{
    wave_ret <- data.frame(maxWaveChangeAnnualSample = NA, 
                           se_maxWaveChangeAnnualSample=NA,
                           meanWaveChangeAnnualSample = NA,
                           se_maxWaveChangeAnnualSample = NA,
                           bigWaveYears=NA)
  }
  
  cbind(ret, wave_ret)
}

#Now make a data set using the above function, with a column to merge on
sample_data_slopes <- raw_data_spans %>%
  group_by( Latitude, Longitude, Study, StudySite, trajectory_ID) %>%
  do(getSlopeCoef(.)) %>%
  mutate(SiteName = paste(StudySite, Study, sep=":"))

###### 5) Merge both temp data sets with slopes & raw data
raw_data_merged <- left_join(raw_data, envt_annual)
kelp_slopes_merged <- left_join(kelp_slopes, 
                                right_join(hadsst_trends_long, sample_data_slopes))

###### 6) Calculate temperature anomolies for raw data
unique_region_temp <- read.csv("../derived_data/hadsst_regions.csv", stringsAsFactors=F) %>%
  group_by(ECOREGION, Year) %>%
  dplyr::summarise(mean_tempC = mean(tempC),
                   max_tempC = quantile(tempC, 0.9)) %>%
  ungroup() %>%
  group_by(ECOREGION) %>%
  dplyr::summarise(mean_tempC_region = mean(mean_tempC),
                   max_tempC_region = mean(max_tempC))

raw_data_merged <- left_join(raw_data_merged, unique_region_temp) %>%
  mutate(mean_tempC_anomoly = mean_tempC - mean_tempC_region,
         max_tempC_anomoly = max_tempC - max_tempC_region) %>%
  dplyr::select(-mean_tempC_region, -max_tempC_region)

###### 7) Write out merged slope and raw data sets

write_csv(raw_data_merged, "../derived_data/raw_data_merged.csv")
write_csv(kelp_slopes_merged, "../derived_data/kelp_slopes_merged.csv")
