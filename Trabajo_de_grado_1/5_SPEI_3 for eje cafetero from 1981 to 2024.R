#########################################################################################
## This Script calculates the Standardized Precipitation Evapotranspiration Index      ##                              ##
## International Center for Tropical Agriculture (CIAT)                                ##
#########################################################################################

######################
## Upload libraries ##
######################
require(pacman)
pacman::p_load(sp, raster, ncdf4, terra, tibble, plyr, SPEI)

setwd("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\PET_PENMAN-MONTEITH")

rastlist.PET = list.files(path = getwd(), pattern="tif$", full.names = TRUE, recursive = FALSE)

##-- Stack PET files --##
allrasters.PET = lapply(rastlist.PET, raster)
allrasters.PET = stack(allrasters.PET)

setwd("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\CROPPED_DOWNSCALING\\PREC")

##-- Import the all Precipitation raster files --##
rastlist.Pre = list.files(path = getwd(), pattern="tif$", full.names = TRUE, recursive = FALSE)

##-- Stack Precipitation files --##
allrasters.Pre = lapply(rastlist.Pre, raster)
allrasters.Pre = stack(allrasters.Pre)

##-- Create a new raster stack with water balance maps --##
Wat.Bal = allrasters.Pre - allrasters.PET

##-- Rename Water Balance maps --##
for (i in 1: nlayers(Wat.Bal)){
  
  name = names(allrasters.Pre[[i]])
  date = substr(name, nchar(name) - 5, nchar(name))
  names (Wat.Bal[[i]]) = paste("Wat.Balance_",date, sep="")
  
}

##-- Convert the stack with water balance maps to a table --##
Wat.Bal.Table = as.data.frame(Wat.Bal, xy = TRUE)

##-- Get the position of pixels with values different from NA --##
pixels.no.Na = which(!is.na(Wat.Bal.Table[,3]))

##-- Create a table to be filled with SPEI values --##
SPEI.Table = Wat.Bal.Table

for (i in 1:length(pixels.no.Na)){
  #i=1
  ##-- Extract historical water balance values for each pixel --##
  pixel.location = pixels.no.Na[i]
  Wat.Bal.Pixel = as.numeric (Wat.Bal.Table[pixel.location,3:ncol(Wat.Bal.Table)])
  
  ##-- Calculate the SPEI-3 months (For Nov-Jan) values for each pixel --##
  spei = spei(Wat.Bal.Pixel, 3)
  spei.numeric = as.numeric(spei$fitted)
  
  ##-- Replace SPEI values in SPEI.Table --## 
  SPEI.Table[pixel.location,3:ncol(SPEI.Table)] = spei.numeric
  
}

##-- Create a list to store the SPEI maps for each month from 1981 to 2024 --##
SPEI_Maps = list()

reference_map= Wat.Bal[[1]]

for(j in 1:nlayers(Wat.Bal)){
  #j=1
  New_map = reference_map
  New_map[]=SPEI.Table[,2+j]
  
  name = names(allrasters.Pre[[j]])
  date = substr(name, nchar(name) - 5, nchar(name))
  names (New_map) = paste("SPEI_3_",date, sep="")
  
  SPEI_Maps[[j]] = New_map
  
}

##-- Stack all SPEI maps created previously --##
SPEI_Maps.stack = stack(SPEI_Maps)

##-- Export the SPEI maps of each year --##

for (i in 1:nlayers(SPEI_Maps.stack)){
  
  name = names(SPEI_Maps.stack[[i]])
  writeRaster(SPEI_Maps.stack[[i]], paste("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\SPEI\\SPEI_3_months\\",name,".tif",sep =""), overwrite=TRUE)
  
}
