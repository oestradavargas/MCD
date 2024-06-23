####################
## Load libraries ##
####################
require(pacman)
pacman::p_load(raster, ncdf4, lubridate)

setwd("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\PET_PENMAN-MONTEITH")

rastlist.PET = list.dirs(getwd(), full.names = FALSE, recursive = FALSE)

nyears = length(rastlist.PET)

## Filtering year and month  ##
for (i in 1:nyears){
  #i=1
    ## Generating range of dates ##
    date = seq(as.Date(paste0(1980+i,'-01-01')), as.Date(paste0(1980+i,'-12-31')), by = "day")
    
    #Get the year corresponding to each date
    year = lubridate::year(date)
    
    #Get the month corresponding to each date
    month = lubridate::month(date)
  
    ##-- Import raster files for each year --##
    rastlist = list.files(path = paste(getwd(),"/",1980+i,sep =""), pattern="tif$", full.names = TRUE, recursive = FALSE)
    allrasters = lapply(rastlist, raster)
    allrasters = stack(allrasters)
    
  for (j in 1:12) {
    #j=1
    
    month_start= j
    month_end= j
    
    ini = which(year == 1980+i & month == month_start)[1]#Get the index of the first date corresponding to the year 'i' and the starting month
    last = which(year == 1980+i & month == month_end)[length(which(year == 1980+i & month == month_end))] 
    
    ## Stacking rasters for the i-th month ##
    PET = stack(allrasters[[ini:last]])
    
    #Calculate the total PET
    Total.PET = sum(PET)
    
    month_str = sprintf("%02d", j)
    
    #Rename raster files
    names(Total.PET)= paste0("PET_total_",1980+i, month_str) 
    
    writeRaster(Total.PET, paste0("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\PET_PENMAN-MONTEITH\\PET_total_", 1980+i, month_str, ".tif"))
    
  }
}
