###### 0) Libraries and the like
library(dplyr)
library(purrr)
library(raster)
library(tidyr)
library(lubridate)

###### 1) Load the slope data file and raw data file
raw_data <- read.csv("../../temporal_change/github_repo/05_HLM_analysis_code/formatted_data_3years.csv", stringsAsFactors=F) %>%
  arrange(StudySite) 

###### 2) Extract unique lat/long info from raw data
unique_lat_long <- raw_data %>%
  group_by(Latitude, Longitude) %>%
  summarise(len=length(unique(trajectory_ID)))

ull_points <- SpatialPoints(cbind(unique_lat_long$Longitude, unique_lat_long$Latitude),
                            proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


###### 3) Load and rasterize HADSST data set from 1950 - 2013
hadsst <- raster::brick("~/Dropbox/src/HADSST/HadISST_sst.nc")
raster::NAvalue(hadsst) <- -1000 #make sure we don't have any super small NAs
yearIDx <- which(chron::years(hadsst@z$Date) %in% 1950:2013)
hadsst_subset <- raster::subset(hadsst, names(hadsst)[yearIDx]) 

###### 4) Extract data at only those lat/longs used
#area_raster <- raster::area(hadsst) #gets area in sq km
#radii <- sqrt(raster::extract(area_raster, cbind(unique_lat_long$Longitude, unique_lat_long$Latitude)))/2*1000
all_cells <- cellFromXY(hadsst, ull_points)
unique_lat_long$cell <- all_cells

unique_cells <- unique(all_cells)

hadsst_kelp_vals <- raster::extract(hadsst_subset, 
                                    unique_cells) 

unique_cell_frame <- data.frame(cell = unique_cells, hadsst_kelp_vals)

#deal with NAs

###### Deal with NAs
naCells_idx <- which(is.na(unique_cell_frame[,2]))

if(length(naCells_idx)>0){
  
  adj_cells <- data.frame(adjacent(x = hadsst, 
                                   cells = unique_cells[naCells_idx], 
                                   directions = 8)) %>%
    mutate(cell_num = match(to, unique_cells))
  
  unknown_cells <- unique(adj_cells$to[which(is.na(adj_cells$cell_num))])
  
  print("Extracting NA cells")
  #presuming there aren't many
  if(length(unknown_cells)>0){
    unk_vals <- cbind(cell = unknown_cells, raster::extract(hadsst_subset, 
                                unknown_cells))
    
    unique_cell_frame <- rbind(unique_cell_frame,  unk_vals)
  }
  
  print("Finished extracting NA cells")
  
  #Now merge adj_cells with hadsst_extracted on to column
  adj_cells <- left_join(adj_cells, unique_cell_frame, by=c("to" = "cell"))
  
  #take averages on from column
  adj_cells <- adj_cells %>% 
    dplyr::select(-to, -cell_num) %>%
    split(.$from) %>%
    map_df(.f=function(x) data.frame(t(colMeans(x, na.rm=T)))) %>%
    ungroup() %>%
    dplyr::rename(cell = from)
  
  #fill in NA rows in hadsst_extracted
  htIDX <- match(adj_cells$cell, unique_cell_frame$cell)
  unique_cell_frame[htIDX,] <- adj_cells
  
  #cleanup hadsst_extracted
  if(length(unknown_cells)>0)
    unique_cell_frame <- unique_cell_frame[-c(length((unique_cells)+1):nrow(unique_cell_frame)),]
  
}

ht <- left_join(unique_lat_long, unique_cell_frame) %>%
  dplyr::select(-cell, -len)

hadsst_kelp <- ht %>%
  gather(DateName, tempC, -Latitude, -Longitude) %>%
  mutate(DateName = gsub("X", "", as.character(DateName))) %>%
  mutate(Year = year(parse_date_time(DateName, orders="ymd")))# %>%
#  mutate(tempC=ifelse(tempC == -1000, NA, tempC))
  

###### 5) Write out temp kelp data as an intermediate step
write.csv(hadsst_kelp, "../derived_data/hadsst_at_latlongs.csv", row.names=F)



