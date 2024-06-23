######################
## Upload libraries ##
######################
require(pacman)
pacman::p_load(raster, stringr, terra, tidyverse, ncdf4, sp, sf)

# Cargar el DEM
DEM = raster("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\DEM_COLOMBIA\\DEM_5km_eje.tif") # Digital Elevation Model at 5km
plot(DEM)

# Asignar constantes
P <- 101.3 * ((293 - 0.0065 * DEM) / 293)^5.26 # presion atmosferica: ec 7, pag 211
Cp <- 0.001013 # constantes pag 31
epsilon <- 0.622
Lambda <- 2.45
gamma <- (Cp * P) / (epsilon * Lambda) # const. psicrometrica: ec 8, pag 211

# Ajustar velocidad del viento
uz <- (4.87/log((67.8*10)-5.42)) # factor ajuste vel. viento a 10m: ec 47, pag 220
G <- 0 # el flujo de calor en el suelo se ignora para la PET diaria, se elimina de la ecuacion: pag 66

## Lesctura de listas de archivos
Tmin = list.files(paste("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\CROPPED_DOWNSCALING\\TMIN"), pattern="tif$", full.names = TRUE, recursive = FALSE)
Tmax = list.files(paste("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\CROPPED_DOWNSCALING\\TMAX"), pattern="tif$", full.names = TRUE, recursive = FALSE)
Rhum = list.files(paste("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\CROPPED_DOWNSCALING\\RHUM"), pattern="tif$", full.names = TRUE, recursive = FALSE)
Srad = list.files(paste("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\CROPPED_DOWNSCALING\\SRAD"), pattern="tif$", full.names = TRUE, recursive = FALSE)
Wvel = list.files(paste("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\CROPPED_DOWNSCALING\\WVEL"), pattern="tif$", full.names = TRUE, recursive = FALSE)

for (i in 5:44){
  
  dir.create(paste("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\PET_PENMAN-MONTEITH\\",1980+i,sep ="")) 
  
  anual.Tmin <- raster::stack(Tmin[i])
  anual.Tmax <- raster::stack(Tmax[i])
  anual.Rhum <- raster::stack(Rhum[i])
  anual.Srad <- raster::stack(Srad[i])
  anual.Wvel <- raster::stack(Wvel[i])


  for (j in 1:nlayers(anual.Tmin)){
  #for (j in 1:1){
    
    daily.Tmin <- anual.Tmin[[j]]
    daily.Tmin <- daily.Tmin-273.15
    
    daily.Tmax <- anual.Tmax[[j]]
    daily.Tmax <- daily.Tmax-273.15
    
    daily.Tmean <- (daily.Tmin + daily.Tmax)/2
    
    daily.Rhum <- anual.Rhum[[j]]
    
    daily.Srad <- anual.Srad[[j]]
    daily.Srad <- daily.Srad/1000000 # conversion de J m2/d a MJ m2/d
    
    daily.Wvel <- anual.Wvel[[j]]
    daily.Wvel <- daily.Wvel * uz # ajuste de la velocidad del viento a 2m
    
    delta <- 4098 * (0.6108 * exp(17.27 * daily.Tmean / (daily.Tmean + 237.3))) / (daily.Tmean + 237.3)^2
    
    e_temp_max <- 0.6108 * exp(17.27 * daily.Tmax / (daily.Tmax + 237.3)) # presion sat. vapor: ec 11, pag 213
    e_temp_min <- 0.6108 * exp(17.27 * daily.Tmin / (daily.Tmin + 237.3))
    e_s <- (e_temp_max + e_temp_min)/2
    e_a <- e_s*(daily.Rhum/100)
    
    PET <- ((0.408*delta*daily.Srad) + (gamma*(900/(daily.Tmean+273))*daily.Wvel*(e_s-e_a))) / (delta + (gamma*(1+0.34*daily.Wvel)))
    
    ####################
    ## Export PET map ##
    ####################
    
    fecha <- as.Date(paste(1980+i, j, sep = "-"), format = "%Y-%j")# + (j - 1)    
    writeRaster(PET,paste("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\PET_PENMAN-MONTEITH\\",1980+i,"\\","PET_",fecha,".tif",sep =""))
  }
}