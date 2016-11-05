###############
# Let's make sure the wave data coordinates
# match those from our data
###############

library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(ggplot2)
world <- map_data("world")

#### 1) Load the different pieces of the wave timeseries
unique_lat_long_tab <- read_excel("../wave_data_reguero/timeseries_v2.xls", sheet=2) #updated nov 2nd
unique_lat_long_tab_new_points <- read_excel("../wave_data_reguero/timeseries_new_points.xls", sheet=2)
unique_lat_long_tab_1 <- read_excel("../wave_data_reguero/timeseries.xls", sheet=2)

unique_lat_long_tab$file <- "timeseries_v2.xls"
unique_lat_long_tab_new_points$file <- "timeseries_new_points.xls"
unique_lat_long_tab_1$file <- "timeseries.xls"

ull <- rbind(unique_lat_long_tab, unique_lat_long_tab_new_points, unique_lat_long_tab_1) %>%
  rename(Latitude = `orig Lat`, Longitude = `orig Lon`)

#Plot it
ggplot(ull, mapping=aes(x=Longitude, y=Latitude, color=file)) +
  geom_point(alpha=0.4) +
  facet_wrap(~file) +
  geom_map(data=world, map=world,
             aes(x=long, y=lat, map_id=region),
             color="white", fill="#7f7f7f", size=0.05, alpha=1/4)

#### 2) Load the raw data
raw_data_ull <- read_csv("../raw_data/raw_data.csv") %>%
  group_by(Latitude, Longitude, SiteName, Study) %>%
  slice(1L) %>%
  ungroup()

raw_data_ull_3 <- rbind(rbind(raw_data_ull, raw_data_ull), raw_data_ull)
raw_data_ull_3$file = c(rep( "timeseries_v2.xls", nrow(raw_data_ull)), 
                        rep( "timeseries.xls", nrow(raw_data_ull)),
                        rep( "timeseries_new_points.xls", nrow(raw_data_ull)))

### 3) See What's missing
missed <- anti_join(raw_data_ull_3, ull) %>%
  group_by(Study, Site, Latitude, Longitude) %>%
  slice(1L) %>%
  ungroup()

#unique_lat_long_tab
ggplot(missed, mapping=aes(x=Longitude, y=Latitude, color=file)) +
  geom_point(alpha=0.4) +
  facet_wrap(~file) +
  geom_map(data=world, map=world,
           aes(x=long, y=lat, map_id=region),
           color="white", fill="#7f7f7f", size=0.05, alpha=1/4)
