library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

####### Explore SE Relationship
kelp_slopes_merged <- read_csv("../derived_data/kelp_slopes_with_temp_waves_canopy.csv") 


se_frame <- kelp_slopes_merged %>% select(dplyr::contains("std.error"), matches("se"))

se_frame_melt <- gather(se_frame, variable, stderr, -se)

qplot(se, stderr, data=se_frame_melt) +facet_wrap(~variable, scale="free") + geom_smooth(method="lm")
