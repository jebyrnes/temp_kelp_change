DATA DESCRIPTION : 

coordinates - points you sent me and I gave data previously. I added the long, lat of the global ocean waves data. It includes and index for the next tab identifying which unique GOW point to use. 

unique_coordinates- this list identifies the single GOW points that are associated with multiple kelp locations. The rest of the tabs only include data for this unique list of coordinates 

The rest of tabs: WE = Mean Wave Energy in the period; Hs = mean sign wave height, HsMax = monthly maxima, WESum = total wave energy in that period 

They are year x month x id according to unique_coordinates, so: 
col 1 - years 
col 2 - months (january 1948 does not exist, in blank) 
col 3:3+127 - each column is one GOW point, according to unique_coordinates 

X_years refers to annual time series, and X_months to monthly data (in case you want to use them in a more straight forward way)

