library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(readxl)

###### 1) Load the slope data file and raw data file
kelp_slopes <- read.csv("../../temporal_change/github_repo/06_HLM_output/site_slopes_3_points.csv", stringsAsFactors=F)

raw_data <- read.csv("../../temporal_change/github_repo/05_HLM_analysis_code/formatted_data_3years.csv", stringsAsFactors=F) %>%
  arrange(StudySite) %>%
  dplyr::rename(Year=year)

hadsst_kelp <- read_csv("../derived_data/hadsst_at_latlongs.csv") %>%
  mutate(DateName = lubridate::parse_date_time(DateName, orders="ymd"))


hadice_kelp <- read_csv("../derived_data/hadice_at_latlongs.csv") %>%
  mutate(DateName = lubridate::parse_date_time(DateName, orders="ymd")) %>%
  mutate(ice_conc = ifelse(is.na(ice_conc), 0, ice_conc))


####bring in and reshape wave data from Borja
wave_we <- read_excel("../wave_data_reguero/wave_data_kelp.xls", sheet="TimeSeries_WE") %>%
  gather(Year, wave_energy, -Latitute, -Longitude)

wave_hs <- read_excel("../wave_data_reguero/wave_data_kelp.xls", sheet="TimeSeries_Hs") %>%
  gather(Year, wave_height, -Latitute, -Longitude)

all_wave_data <- full_join(wave_we, wave_hs) %>%
  rename(Latitude = Latitute) %>%
  mutate(Year = as.integer(Year))

###### 2) Filter kelp_slopes down to site-level slopes
kelp_slopes <- kelp_slopes %>% filter(parameter=="site_slope") %>%
  filter(grouping=="Ecoregion") %>%
  filter(Period == "1900-2015") %>%
  arrange(SiteName)

###### 3) Filter the environmenta; data to annual summaries
#Note the shenanigans with lat/long
#making them doubles led to rounding errors
#that cascaded through the joins in a bad way
#the rounding and as.character fixes the issue
hadsst_kelp_annual <- hadsst_kelp %>%
  group_by(Latitude, Longitude, Year) %>%
  dplyr::summarise(mean_tempC = mean(tempC, na.rm=T), 
                   max_tempC = quantile(tempC, probs=0.9, na.rm=T)) %>%
   ungroup() #%>%
  # mutate(Latitude = as.character(round(Latitude,5)),
  #        Longitude = as.character(round(Longitude,5)))

hadice_kelp_annual <- hadice_kelp %>%
  group_by(Latitude, Longitude, Year) %>%
  dplyr::summarise(mean_ice_conc = mean(ice_conc, na.rm=T), 
                   iceFreeMonths = 12-sum(ice_conc>0, na.rm=T)) %>%
  ungroup() %>%
  mutate(Latitude = as.character(round(Latitude,5)),
         Longitude = as.character(round(Longitude,5)))

full_join(hadice_kelp_annual[4,], hadsst_kelp_annual[5,]) -> a
a[1,] - a[2,]

had_annual <- left_join(hadice_kelp_annual, hadsst_kelp_annual)

envt_annual <- left_join(hadsst_kelp_annual, all_wave_data)
# 
# qplot(Year, iceFreeMonths, color=paste(Latitude, Longitude), geom="line", 
#       data=envt_annual, alpha=I(0.3)) +
#   scale_color_discrete(guide="none") 


###### 4) Merge with raw data to get range of environmental years

raw_data_merged <- left_join(raw_data, envt_annual)

envt_sample_years <- raw_data_merged %>%
  dplyr::select(Latitude, Longitude, Year, 
         mean_tempC, max_tempC,  wave_energy, wave_height) %>%
  group_by(Latitude, Longitude, Year) %>%
  slice(1L) %>%
  ungroup()

###### 5) Get the trend for each piece

slopeSE <- function(x) sqrt(diag(vcov(x)))[2]

#Note: This has to be done separately for waves due to measurements only going to 2008
had_sample_years <- envt_sample_years %>%
  dplyr::select(Latitude, Longitude, Year, 
                mean_tempC, max_tempC)  %>%
  group_by(Latitude, Longitude) %>%
  mutate(isBad = sum(is.na(mean_tempC))>length(mean_tempC)-3) %>%
  ungroup() %>%
  filter(!isBad) %>%
  group_by(Latitude, Longitude) %>%
  nest() %>%
  mutate(maxTempMod = purrr::map(data, ~lm(max_tempC ~ Year, data=.)),
         meanTempMod = purrr::map(data, ~lm(mean_tempC ~ Year, data=.))
  ) %>%
  
  #coefs and SEs
  mutate(maxTempChangeSample= purrr::map_dbl(maxTempMod, ~coef(.)[2]),
         se_maxTempChangeSample = purrr::map_dbl(maxTempMod, ~slopeSE(.)),
         meanTempChangeSample=purrr::map_dbl(meanTempMod, ~coef(.)[2]),
         se_meanTempChangeSample = purrr::map_dbl(meanTempMod, ~sqrt(diag(vcov(.)))[2])
  )  %>%
  dplyr::select(-data, -maxTempMod, -meanTempMod) %>%
  ungroup()


wave_sample_years <- envt_sample_years %>%
  dplyr::select(Latitude, Longitude, Year, 
                wave_energy, wave_height) %>%
  group_by(Latitude, Longitude) %>%
  mutate(isBad = sum(is.na(wave_energy))>length(wave_energy)-3) %>%
  ungroup() %>%
  filter(!isBad) %>%
  group_by(Latitude, Longitude) %>%
  nest() %>%
  mutate(wave_heightMod = purrr::map(data, ~lm(wave_height ~ Year, data=.)),
         wave_energyMod = purrr::map(data, ~lm(wave_energy ~ Year, data=.))
  ) %>%
  
  #coefs and SEs
  mutate(waveHeightChangeSample= purrr::map_dbl(wave_heightMod, ~coef(.)[2]),
         se_WaveHeightChangeSample = purrr::map_dbl(wave_heightMod, ~slopeSE(.)),
         waveEnergyChangeSample= purrr::map_dbl(wave_energyMod, ~coef(.)[2]),
         se_waveEnergyChangeSample = purrr::map_dbl(wave_energyMod, ~slopeSE(.))
  )  %>%
  dplyr::select(-data, -wave_heightMod, -wave_energyMod) %>%
  ungroup()

###### 6) Get thelong-term temperature trends

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


###### 5) Merge both temp data sets with slopes & raw data
siteTab <- raw_data %>%
  group_by(Latitude, Longitude, Study, StudySite, trajectory_ID) %>%
  slice(1L) %>%
  ungroup() %>%
  dplyr::select(Latitude, Longitude, Study, StudySite, trajectory_ID) %>%
  mutate(SiteName = paste0(StudySite, ":", Study))

kelp_slopes_merged <- left_join(kelp_slopes, 
                                right_join(hadsst_trends_long, 
                                           right_join(wave_sample_years, 
                                                      right_join(siteTab, had_sample_years))))



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
