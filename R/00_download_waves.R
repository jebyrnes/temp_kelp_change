library(RCurl)
library(raster)

archiveURL <- "ftp://polar.ncep.noaa.gov/pub/history/waves/"

fileStrings <- c("nww3.hs", "multi_1.glo_30m.hs")

#fsu
#uf
#lsu
#mississippi/alabama



waveFiles <- strsplit(getURL(archiveURL,verbose=TRUE,ftp.use.epsv=TRUE,dirlistonly = TRUE), "\n")[[1]]
waveFiles <- waveFiles[-grep("MD5", waveFiles)]

nww3 <- grep(fileStrings[1], waveFiles)
multi <- grep(fileStrings[2], waveFiles)

#Download the files and 
setwd("~/Dropbox/wavewatch3")

for (i in c(nww3, multi)){
  afile <- waveFiles[i]
  print(afile)
  
  system(paste0("wget ", archiveURL,waveFiles[i]))
  
  
  
  isMulti <- grepl("multi", afile)
  
  if(isMulti){
    aDate <- gsub("(multi_1\\.glo_30m\\.hs\\.)([0-9]{4})([0-9]{2})(\\.grb2)", "\\2-\\3-01", afile)
    
    system(paste0("wgrib2 ", afile," -netcdf multi.nc"))
    Waves <- brick("multi.nc")

  }else{
    aDate <- gsub("(nww3\\.hs\\.)([0-9]{4})([0-9]{2})(\\.grb)", "\\2-\\3-01", afile)
    Waves <- brick(afile)
    
  }
  aDate <- lubridate::parse_date_time(aDate, orders="ymd")
  
  #get the grib file and turn it into a raster brick
  
  #make the new rasters
  WavesMean <- calc(Waves, mean)
  WavesMax <- calc(Waves, max)
  
  #set their dates
  WavesMean@z$Date <- aDate
  WavesMax@z$Date <- aDate
  
  #write them out
  writeRaster(WavesMean, paste0("./means/", aDate, ".grd"), format="raster", overwrite=TRUE)
  writeRaster(WavesMax, paste0("./maxes/", aDate, ".grd"), format="raster", overwrite=TRUE)
  
  #clean up
  system(paste0("rm ",waveFiles[i]))
  if(isMulti)  system("rm multi.nc")

}  

print("DONE!!!")

