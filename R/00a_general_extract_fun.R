extract_vals <- function(unique_lat_long, a_raster){
  #get the cells
  ull_points <- SpatialPoints(cbind(unique_lat_long$Longitude, unique_lat_long$Latitude),
                              proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  
  all_cells <- cellFromXY(a_raster, ull_points)
  unique_lat_long$cell <- all_cells
  #get unique cells
  unique_cells <- unique(all_cells)
  
  
  #get the first round of extractions
  a_raster_kelp_vals <- raster::extract(a_raster, 
                                      unique_cells) #max to avoid -1000 error
  
  unique_cell_frame <- data.frame(cell = unique_cells, a_raster_kelp_vals)
  
  ###### Deal with NAs
  naCells_idx <- which(is.na(unique_cell_frame[,2]))
  
  if(length(naCells_idx)>0){
    
    adj_cells <- data.frame(adjacent(x = a_raster, 
                                     cells = unique_cells[naCells_idx], 
                                     directions = 8)) %>%
      mutate(cell_num = match(to, unique_cells))
    
    unknown_cells <- unique(adj_cells$to[which(is.na(adj_cells$cell_num))])
    
    print("Extracting NA cells")
    #presuming there aren't many
    if(length(unknown_cells)>0){
      unk_vals <- cbind(cell = unknown_cells, raster::extract(a_raster, 
                                                              unknown_cells))
      
      unique_cell_frame <- rbind(unique_cell_frame,  unk_vals)
    }
    
    print("Finished extracting NA cells")
    
    #Now merge adj_cells with a_raster_extracted on to column
    adj_cells <- left_join(adj_cells, unique_cell_frame, by=c("to" = "cell"))
    
    #take averages on from column
    adj_cells <- adj_cells %>% 
      dplyr::select(-to, -cell_num) %>%
      split(.$from) %>%
      map_df(.f=function(x) data.frame(t(colMeans(x, na.rm=T)))) %>%
      ungroup() %>%
      dplyr::rename(cell = from)
    
    #fill in NA rows in a_raster_extracted
    htIDX <- match(adj_cells$cell, unique_cell_frame$cell)
    unique_cell_frame[htIDX,] <- adj_cells
    
    #cleanup a_raster_extracted
    if(length(unknown_cells)>0)
      unique_cell_frame <- unique_cell_frame[-c(length((unique_cells)+1):nrow(unique_cell_frame)),]
    
  }
  
  names(unique_cell_frame)[-1] <- as.character(a_raster@z$Date)
  
  #return joined data frame
  left_join(unique_lat_long, unique_cell_frame) %>%
    dplyr::select(-cell, -len)
}