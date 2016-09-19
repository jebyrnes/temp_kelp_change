###### 0) Libraries and the like
library(dplyr)
library(purrr)
library(raster)
library(tidyr)
library(lubridate)

###### 1) Load the slope data file and raw data file
raw_data <- read.csv("../../temporal_change/github_repo/05_HLM_analysis_code/formatted_data_3points.csv", stringsAsFactors=F) %>%
  arrange(StudySite) 

###### 2) Extract unique lat/long info from raw data
unique_lat_long <- raw_data %>%
  group_by(Latitude, Longitude) %>%
  summarise(len=length(unique(trajectory_ID)))

ull_points <- SpatialPoints(cbind(unique_lat_long$Longitude, unique_lat_long$Latitude),
                            proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

###### 3) Load and rasterize HADSST data set from 1950 - 2013
hadsst <- raster::brick("~/Dropbox/src/HADSST/HadISST_sst.nc")
#hadsst <- raster::brick("~/Dropbox/src/HADSST/HadISST_ice.nc")
raster::NAvalue(hadsst) <- -1000 #make sure we don't have any super small NAs
yearIDx <- which(chron::years(hadsst@z$Date) %in% 1950:2013)
#hadsst_subset <- stack(hadsst)
hadsst_subset <- raster::subset(hadsst, names(hadsst)[yearIDx]) 

###### 4) Get the unique non-NA cells
#Based on answer from
# from http://stackoverflow.com/questions/27562076/if-raster-value-na-search-and-extract-the-nearest-non-na-pixel/39539718#39539718
r <- hadsst_subset@layers[[length(hadsst_subset@layers)]]
r <- hadsst_subset@layers[[1]]
r <- readAll(r)

non_na_cells <- 
  simplify2array(apply(as.data.frame(ull_points), 1, function(xy)
    which.min(replace(distanceFromPoints(r, xy), is.na(r), NA))[1]))

unique_lat_long$cell <- cellFromXY(hadsst_subset, ull_points)
unique_lat_long$non_na_cell <- non_na_cells

###### 4) Extract data at only those lat/longs used

unique_cells <- unique(non_na_cells)

hadsst_kelp_vals <- raster::extract(hadsst_subset, 
                                    unique_cells) 

unique_cell_frame <- data.frame(non_na_cell = unique_cells, hadsst_kelp_vals)

###### 5) Join data to unique cells

ht <- left_join(unique_lat_long, unique_cell_frame) %>%
  dplyr::select(-len)

hadsst_kelp <- ht %>%
  gather(DateName, tempC, -Latitude, -Longitude, -cell, -non_na_cell) %>%
  mutate(DateName = gsub("X", "", as.character(DateName))) %>%
  mutate(Year = year(parse_date_time(DateName, orders="ymd"))) %>%
  arrange(Latitude, Longitude)

#Make sure that no lat/longs have NAs all through the record
#A few is fine, as the nearest cell with a value might be missing
#for some dates - should not be a large impact on results
as.data.frame(hadsst_kelp[which(is.na(hadsst_kelp$tempC)),])

#make sure there are no stupidly high values
as.data.frame(hadsst_kelp[which(hadsst_kelp$tempC>100),])

######FOR ICE
#hadsst_kelp <- hadsst_kelp %>%
#  rename(ice_conc = tempC)
#write.csv(hadsst_kelp, "../derived_data/hadice_at_latlongs.csv", row.names=F)

###### 6) Write out temp kelp data 

hadsst_kelp_clean <- hadsst_kelp %>% ungroup() %>%
  dplyr::select(-non_na_cell)

write.csv(hadsst_kelp_clean, "../derived_data/hadsst_at_latlongs.csv", row.names=F)



