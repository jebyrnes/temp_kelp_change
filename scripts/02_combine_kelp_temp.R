library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
setwd(here::here())

#' ------------------------------------
#' 1) Load the raw data, the HADSST data for sites
#' and the temp data for realm, province, and ecoregion
#' ------------------------------------

dat_year_filter <- . %>%
  filter(Year >= 1952 & Year <= 2008) #first data point in kelp, last year of waves

raw_data <- read.csv("raw_data/raw_data.csv") %>% as_tibble()%>%
  dat_year_filter


hadsst_kelp <- read_csv("derived_data/hadsst_at_latlongs.csv") %>%
  mutate(DateName = lubridate::parse_date_time(DateName, orders="ymd")) %>%
  dat_year_filter

#' ------------------------------------
#' 2) Calculate the spring and summer means for realm, province, ecoregion
#' and individual sites as well as days > 17C
#' ------------------------------------


get_temp_mean <- . %>%
  dat_year_filter %>%
  mutate(month = ymd(DateName) %>% month()) %>%
  summarize(mean_temp = mean(tempC, na.rm=TRUE),
            march_may_temp = mean(tempC[month %in% 3:5]),
            june_aug_temp = mean(tempC[month %in% 6:8])) %>%
  ungroup()

realms <- read_csv("derived_data/hadsst_realms.csv") %>%
  group_by(REALM) %>%
  get_temp_mean %>%
  rename_with(~paste0(.x, "_realm"),
              contains("temp"))

provinces <- read_csv("derived_data/hadsst_provinces.csv")%>%
  group_by(PROVINCE) %>%
  get_temp_mean %>%
  rename_with(~paste0(.x, "_province"),
              contains("temp"))

regions <- read_csv("derived_data/hadsst_regions.csv")%>%
  group_by(ECOREGION) %>%
  get_temp_mean %>%
  rename_with(~paste0(.x, "_ecoregion"),
              contains("temp"))

sites <- hadsst_kelp %>%
  group_by(Latitude, Longitude, cell) %>%
  get_temp_mean %>%
  rename_with(~paste0(.x, "_site"),
              contains("temp"))

hadsst_kelp_annual <- hadsst_kelp %>%
  mutate(month = month(DateName)) %>%
  group_by(Latitude, Longitude, cell, Year) %>%
  summarize(mean_temp = mean(tempC, na.rm=TRUE),
            stress_days = sum(tempC >= 17, na.rm=TRUE),
            
            mean_temp_march_may = mean(tempC[month %in% 3:5], na.rm=TRUE),
            stress_days_march_may = sum(tempC[month %in% 3:5] >= 17, na.rm=TRUE),
            
            mean_temp_june_august = mean(tempC[month %in% 6:8], na.rm=TRUE),
            stress_days_june_august = sum(tempC[month %in% 6:8] >= 17, na.rm=TRUE)) %>%
  ungroup()

#' ------------------------------------
#' 3) Merge all the data
#' ------------------------------------
data_merged <- list(raw_data, 
                    hadsst_kelp_annual, 
                    sites, 
                    regions, 
                    provinces, 
                    realms) %>%
  purrr::reduce(left_join)

#' ------------------------------------
#' 3) For each data point, calculate the deviation from site for
#' all temp metrics
#' ------------------------------------
data_merged <- data_merged %>%
  mutate(mean_temp_dev = mean_temp - mean_temp_site)


write_csv(data_merged,  "derived_data/raw_data_with_temp.csv")
