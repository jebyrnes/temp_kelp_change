library(dplyr)
library(readr)
library(tidyr)
library(broom)
library(readxl)

filename <- "../wave_data_reguero/timeseries_v2.xls"

#### 1) Load the different pieces of the wave timeseries
unique_lat_long_tab <- read_excel(filename, sheet=2) #updated nov 2nd
#unique_lat_long_tab <- read_excel("../wave_data_reguero/timeseries_new_points.xls", sheet=2)
#unique_lat_long_tab <- read_excel("../wave_data_reguero/timeseries.xls", sheet=2)
#unique_lat_long_tab2 <- read_excel("../wave_data_reguero/timeseries.xls", sheet=2)


#unique_lat_long_tab
#lat_long <- unique(paste("X", unique_lat_long_tab$LonGOW, unique_lat_long_tab$LatGOW, sep="_"))

#Unique coordinates in the data
gow_coords <- read_excel(filename, sheet=3)
gow_coords_names <- paste("X", gow_coords$GOWLon, gow_coords$GOWLat, sep="_")



## Most of the data uses the same format, so, here's a function to parse it
parse_wave_data <- function(sheet, month=FALSE){
  wave_col_name <- c("Year", "Month", gow_coords_names)
  if(!month) wave_col_name <- wave_col_name[-2]
  
  wd <- read_excel(filename, sheet=sheet, 
             col_names=wave_col_name) 
  
  if(month){
    
    #forgive this hack - but something odd was happening with tidyr
    #hopefully it will be unnecessary once my issue on github
    #is resolved
    nameFrame <- tibble(gow_coords = names(wd)[-c(1:2)], 
                        newName = paste("X", 1:(ncol(wd)-2), sep="_"))
    
    names(wd) <- c("Year", "Month", nameFrame$newName)
    wd <- wd %>% gather(newName, measurement, -Year, -Month) %>%
      left_join(nameFrame) %>%
      dplyr::select(-newName)
    
   # wd <- wd %>% gather(gow_coords, measurement, -Year, -Month)
  }else{
    
    nameFrame <- tibble(gow_coords = names(wd)[-1], 
                        newName = paste("X", 1:(ncol(wd)-1), sep="_"))
    
    names(wd) <- c("Year", nameFrame$newName)
    wd <- gather(wd, newName, measurement, -Year) %>%
      left_join(nameFrame) %>%
      dplyr::select(-newName)
    
#    wd <- wd %>% gather(gow_coords, measurement, -Year)
  }
  
  wd %>%
    mutate(gow_coords = gsub("X_", "", gow_coords)) %>%
    separate(gow_coords, c("GOWLon", "GOWLat"), "_") %>%
    mutate(GOWLon = as.numeric(GOWLon), GOWLat = as.numeric(GOWLat))
  
}

annual_mean_wave_energy <- parse_wave_data(sheet = 4) %>%
  dplyr::rename(mean_wave_energy = measurement)

annual_mean_wave_height <- parse_wave_data(sheet = 5) %>%
  dplyr::rename(mean_wave_height = measurement)

annual_q95_wave_height <- parse_wave_data(sheet = 6) %>%
  dplyr::rename(q95_wave_height = measurement)


monthly_mean_wave_energy <- parse_wave_data(sheet = 7, month=TRUE) 

monthly_sum_wave_energy <- parse_wave_data(sheet = 8, month=TRUE) 

annual_max_wave_energy <- monthly_mean_wave_energy %>%
  group_by(Year, GOWLon, GOWLat) %>%
  dplyr::summarise(max_mean_wave_energy = max(measurement, na.rm=T),
            sd_mean_wave_energy = sd(measurement, na.rm=T))


sum_wave_energy <- monthly_sum_wave_energy %>%
  group_by(Year, GOWLon, GOWLat) %>%
  dplyr::summarise(max_sum_wave_energy = max(measurement, na.rm=T),
            total_wave_energy = sum(measurement, na.rm=T),
            sd_sum_wave_energy = sd(measurement, na.rm=T))

monthly_max_wave_height <- parse_wave_data(sheet = 9, month=TRUE) 

annual_max_wave_height <- monthly_max_wave_height %>%
  group_by(Year, GOWLon, GOWLat) %>%
  dplyr::summarise(max_wave_height = max(measurement, na.rm=T),
            sd_max_wave_height = sd(measurement, na.rm=T))


#
#  qplot_wave_trends <- function(adf, preprocess=F, preprocessFun = max){
#  if(preprocess) {
#    adf <- adf %>% group_by(Year, GOWLon, GOWLat) %>%
#      summarize(measurement = preprocessFun(measurement, na.rm=T)) %>%
#      ungroup()
#  }
# 
# ggplot(adf %>% group_by(GOWLon, GOWLat) %>%
#          mutate(measurement = measurement-mean(measurement, na.rm=T)) %>%
#          ungroup(),
#        aes(x=Year, y = measurement,
#            color=abs(as.numeric(GOWLat)), group=factor(paste(GOWLon, GOWLat)))) +
#   geom_line(alpha=0.1) +
#   scale_color_continuous(low="red", high="blue", guide=guide_colorbar(title="GOWLat")) +
#   stat_smooth(method="lm", fill=NA, color="grey", alpha=0.5) +
#   stat_smooth(method="lm", fill=NA, color="black", alpha=0.5, mapping=aes(group=1)) +
#   theme_bw(base_size=17) +
#   ylab("anomoly")
# }

##### 2) Merge into one mega-frame
wave_data <- left_join(
  left_join(
    left_join(annual_mean_wave_energy, annual_mean_wave_height),
    left_join(annual_max_wave_energy, sum_wave_energy)),
  left_join(annual_max_wave_height, annual_q95_wave_height)) %>%
  dplyr::rename(LonGOW = GOWLon, LatGOW = GOWLat)

wave_data_full <- left_join(unique_lat_long_tab, wave_data) %>%
  dplyr::rename(Latitude = `orig Lat`, Longitude = `orig Lon`)
# Expand with original lat/long pairs

##### 3) Load the unique lat/longs from kelp and merge with wave data
raw_data <- read_csv("../raw_data/raw_data.csv") 

rd_wave <- left_join(raw_data, wave_data_full)
#test <- anti_join(wave_data_full, raw_data) %>% group_by(id, Longitude, Latitude) %>% slice(1L)

##### 4) Calculate the sample slopes in each quantity

rd_wave_slopes <- rd_wave %>% 
  #Gather up the data in such a way that each variable
  #can be split by row
  gather(variable, value,
         mean_wave_energy:q95_wave_height) %>%
  filter(!is.na(value)) %>% #FOR NOW - UGH
  #Group it by variable and fit a lm to extract coefficients
  group_by(SiteName, Latitude, Longitude, variable) %>%
  nest() %>% #
  mutate(mod = purrr::map(data, ~lm(value ~ Year, data=.)),
         out = mod %>% purrr::map(broom::tidy)) %>%
  unnest(out) %>%
  ungroup() %>%
  filter(term != "(Intercept)") %>%
  dplyr::select(-statistic) %>%
  
  #OK, we now have to reshape the data to a wide format where
  #each variable gets a set of columns - we start by further gathering
  gather(new_col, value, -SiteName, -Latitude, -Longitude, -variable, -term) %>%
  unite(new_col, variable, new_col, sep="_") %>%
  dplyr::select(-term) %>%
  spread(new_col, value)


##### 5) Load the kelp slopes with temperature and merge with wave data
merged_slopes_temp <- read_csv("../derived_data/kelp_slopes_with_temp.csv")

merged_slopes <- left_join(merged_slopes_temp, rd_wave_slopes)

anti_join(merged_slopes_temp, rd_wave_slopes)$SiteName

merged_slopes[which(is.na(merged_slopes$max_mean_wave_energy_estimate)),]$SiteName
merged_slopes[which(is.na(merged_slopes$max_mean_wave_energy_estimate)),]$Latitude
merged_slopes[which(is.na(merged_slopes$max_mean_wave_energy_estimate)),]$Longitude
#rd_wave_slopes$SiteName[which(!(rd_wave_slopes$SiteName %in% merged_slopes_temp$SiteName))]

##### 6) Write it all out
write_csv(merged_slopes,  "../derived_data/kelp_slopes_with_temp_waves.csv")
