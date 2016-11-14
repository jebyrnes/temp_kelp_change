source("./06_data_for_analysis_gen.R")
library(ggplot2)
library(colorspace)

world <- map_data("world")


####### Basics
#How much data for we have
nrow(kelp_slopes_merged)
nrow(kelp_slopes_merged_complete)


#Plot the distribution of temperature change on a map
ggplot(kelp_slopes_merged_complete,
       mapping=aes(x=Longitude, y=Latitude, color=max_temp_slope_estimate,
                   size=1/max_temp_slope_std.error)) +
  geom_point(alpha=0.3) +
  scale_color_gradient2(low = "blue", mid = "goldenrod", high = "red", midpoint=0, 
                         guide = guide_colorbar(title="Change in Max\nTemperature\n(Degree C per Year)")) +
  theme_bw(base_size=17) +
  geom_map(data=world, map=world,
           aes(x=long, y=lat, map_id=region),
           color="white", fill="#7f7f7f", size=0.05, alpha=1/4) +
  scale_size_continuous(guide = guide_legend(title="Precision"))

#Histogram of temp change
ggplot(kelp_slopes_merged_complete,
       mapping=aes(x=max_temp_slope_estimate)) +
  geom_histogram(bins=50) +
  theme_bw(base_size=17) +
  xlab("Change in Wave Energy (Degree C per Year)")

#Plot the distribution of temperature change on a map
ggplot(kelp_slopes_merged_complete,
       mapping=aes(x=Longitude, y=Latitude, color=max_wave_height_estimate,
                   size=1/total_wave_energy_std.error)) +
  geom_point(alpha=0.3) +
#  scale_colour_gradientn(colours = rev(diverge_hsv(4, power=0.4)),
  scale_color_gradient2(low = "blue", mid = "goldenrod", high = "red", midpoint=0,
                         guide = guide_colorbar(title="Change in Max\nWave Height (m) \nper Year")) +
  theme_bw(base_size=17) +
  geom_map(data=world, map=world,
           aes(x=long, y=lat, map_id=region),
           color="white", fill="#7f7f7f", size=0.05, alpha=1/4) +
  scale_size_continuous(guide = guide_legend(title="Precision")) +
  ylab("Count")

ggplot(kelp_slopes_merged_complete,
       mapping=aes(x=total_wave_energy_estimate)) +
  geom_histogram(bins=50) +
  theme_bw(base_size=17) +
  xlab("Change in Total Wave Energy Per Year") 

### The data

#Plot the distribution of temperature change on a map
ggplot(kelp_slopes_merged_complete,
       mapping=aes(x=Longitude, y=Latitude, color=mean,
                   size=1/se)) +
  geom_point(alpha=0.9) +
  #  scale_colour_gradientn(colours = rev(diverge_hsv(4, power=0.4)),
  scale_color_gradient2(low = "darkred", mid = "white", high = "darkblue", midpoint=0,
                        guide = guide_colorbar(title="Porportional Change in Kelp per year")) +
  theme_bw(base_size=17) +
  geom_map(data=world, map=world,
           aes(x=long, y=lat, map_id=region),
           color="white", fill="#7f7f7f", size=0.05, alpha=1/4) +
  scale_size_continuous(guide = guide_legend(title="Precision")) 

