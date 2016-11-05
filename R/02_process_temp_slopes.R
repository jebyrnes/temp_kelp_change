library(dplyr)
library(readr)
library(tidyr)
library(broom)

###### 1) Load the slope data file and raw data file
#the raw data with a new column that can join on the kelp_slopes
raw_data <- read_csv("../raw_data/raw_data.csv") 


hadsst_kelp <- read_csv("../derived_data/hadsst_at_latlongs.csv") %>%
  mutate(DateName = lubridate::parse_date_time(DateName, orders="ymd"))

#Sift the temp data down to annual mean and max, and within year SD
hadsst_kelp_annual <- hadsst_kelp %>%
  group_by(Latitude, Longitude, cell, Year) %>%
  summarize(mean_tempC = mean(tempC, na.rm=T),
            max_tempC = max(tempC, na.rm=T), 
            sd_tempC = sd(tempC, na.rm=T))

#Left Join on the raw data to get sample appropriate temp data
merged_rd_temp <- left_join(raw_data, hadsst_kelp_annual)

#Add a column which is year range of the data to minimize refitting
merged_rd_temp <- merged_rd_temp %>%
  group_by(SiteName) %>%
  mutate(YearRange = paste(min(Year), max(Year), sep="-")) %>%
  ungroup()

#For each study year range/cell combo, get the slope and SE of mean and max
#temp change, as well as the average temporal SD of temperature
#http://omaymas.github.io/Climate_Change_ExpAnalysis/
#https://blog.rstudio.org/2016/02/02/tidyr-0-4-0/
sample_temp_slopes <- merged_rd_temp %>%
  group_by(SiteName) %>%
  nest() %>%
  mutate(maxTempMod = purrr::map(data, ~lm(max_tempC ~ Year, data=.)),
         meanTempMod = purrr::map(data, ~lm(mean_tempC ~ Year, data=.)),
         max_temp_slope = maxTempMod %>% purrr::map(broom::tidy),
         mean_temp_slope = meanTempMod %>% purrr::map(broom::tidy)) %>%
  unnest(max_temp_slope, mean_temp_slope, .sep="_") %>%
  ungroup() %>%
  filter(max_temp_slope_term != "(Intercept)") %>%
  dplyr::select(-max_temp_slope_term, -mean_temp_slope_term)

#For each study year range/cell combo, average w/in year temporal SD of temperature
sd_temp_slopes <- merged_rd_temp %>%
  group_by(SiteName)%>%
  summarize(avg_sd_tempC = mean(sd_tempC, na.rm=T)) %>%
  ungroup()

#Merge slope info with the slopes of relationships from Krumhansl paper
#kelp slopes filtered down to site-level slopes
kelp_slopes <- read_csv("../../temporal_change/github_repo/06_HLM_output/site_slopes_3_points.csv") %>% 
  filter(parameter=="site_slope") %>%
  filter(grouping=="Ecoregion") %>%
  filter(Period == "1900-2015") %>%
  arrange(SiteName)


#Merge All of the Info with Slopes
merged_slopes <- left_join(kelp_slopes, sample_temp_slopes)
merged_slopes <- left_join(merged_slopes, sd_temp_slopes)


write_csv(merged_slopes,  "../derived_data/kelp_slopes_with_temp.csv")
