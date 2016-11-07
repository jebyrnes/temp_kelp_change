library(dplyr)
library(tidyr)
library(readr)

#Load data
kelp_slopes_merged <- read_csv("../derived_data/kelp_slopes_with_temp_waves_canopy.csv") 

kelp_slopes_merged_complete <- kelp_slopes_merged %>% 
  filter(!is.na(max_wave_height_estimate)) %>%
  filter(!is.na(max_wave_height_std.error)) %>%
  filter(!is.na(max_temp_slope_estimate))

make_ksm <- function(wavecol, tempcol, cleanForRethinking=FALSE){
 # print(deparse(substitute(wavecol)))
#Filter it down and do a little renaming
  ksm <- kelp_slopes_merged_complete %>%
    dplyr::rename(slope = mean, slope_se = se) %>%
    mutate(abs_lat = abs(Latitude))
  
  #yeah, it's wonky. Deal with it.
                  # waves = max_wave_height_estimate,
                  #  se_waves = max_wave_height_std.error,
                  # temp = max_temp_slope_estimate,
                  # se_temp = max_temp_slope_std.error) 
  
  ksm$waves = ksm[[deparse(substitute(wavecol))]]
  ksm$se_waves = ksm[[gsub("estimate", "std\\.error", deparse(substitute(wavecol)))]]
  ksm$temp = ksm[[deparse(substitute(tempcol))]]
  ksm$se_temp = ksm[[gsub("estimate", "std\\.error", deparse(substitute(tempcol)))]]


  ksm <- ksm %>%
  #standardize to speed convergence
  mutate(abs_lat_scale = as.numeric(scale(abs_lat)),
         Duration=as.numeric(Duration),
         Duration_scale=as.numeric(scale(Duration)),
         se_waves_scale=se_waves/sd(waves),
         se_temp_scale=se_temp/sd(temp),
         waves_scale=as.numeric(scale(waves)),
         temp_scale=as.numeric(scale(temp)),
         nocanopy = as.numeric(factor(has_canopy)) - 1,
         studyIDX = as.numeric(factor(Study)))

  if(!cleanForRethinking) return(ksm)
  
  #Make a clean version that won't cause STAN to barf
  ksm_clean <- as.data.frame(ksm %>%
                             dplyr::select(slope, temp_scale, waves_scale, nocanopy, abs_lat,
                                           slope_se, se_temp_scale, se_waves_scale, studyIDX)) %>%
  #because we cant' do measurement errors of 0
  filter(se_waves_scale != 0 ) %>%
    filter(se_temp_scale !=0)
  
  #Used to just make them small, but I worry it's due to their being low n
  #ifelse(se_waves_scale==0, 1e-20, se_waves_scale),
   #      se_temp_scale = ifelse(se_temp_scale==0,  1e-20, se_temp_scale)) 
  
  return(ksm_clean)
}
