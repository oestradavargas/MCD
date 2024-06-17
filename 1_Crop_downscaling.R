######################
## Cargar librerias ##
######################
require(pacman)
pacman::p_load(raster, stringr, terra, tidyverse, ncdf4, sp, sf)

####################################################
## Cargar archivos DEM, shapefile y de referencia ##
## para realizar los recortes del area de interes ##
####################################################

# Cargar el modelo digital de elevacion
DEM = raster("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\DEM_COLOMBIA\\DEM_5km_Colombia.tif") # Digital Elevation Model at 5km
plot(DEM)

# Cargar shapefile con los departamentos de Colombia
ruta_shapefile <- "D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\SHAPEFILES_COLOMBIA\\depto.shp"
Datos_mapa <- raster::shapefile(ruta_shapefile)

# Ver proyeccion del shapefile
st_crs(Datos_mapa)

# Filtrar el shapefile para los departamentos específicos, generar 2 mascaras
shp_big <- Datos_mapa[Datos_mapa$NOMBRE_DPT %in% c('QUINDIO', 'RISARALDA', 'CALDAS', 'CHOCO', 'ANTIOQUIA', 'TOLIMA', 'VALLE DEL CAUCA', 'CUNDINAMARCA', 'BOYACA'), ]
shp <- Datos_mapa[Datos_mapa$NOMBRE_DPT %in% c('QUINDIO', 'RISARALDA', 'CALDAS'), ]
plot(shp_big)

# Cargar archivo de referencia con resolucion a 5 km
ruta_referencia_5km <- "D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\RASTER_RESAMPLING_REF\\chirps-v2.0.1981.01.01.tif"
reference_5km <- raster(ruta_referencia_5km)

# Ver proyeccion
crs(reference_5km)

# Función para reproyectar el shapefile
reproyectar_shapefile <- function(shp, crs_raster) {
  if (!identical(crs(shp), crs_raster)) {
    shp <- spTransform(shp, crs_raster)
  }
  return(shp)
}

# Reproyectar el shapefile a la proyección del raster de referencia
shp <- reproyectar_shapefile(shp, crs(reference_5km))
shp_big <- reproyectar_shapefile(shp_big, crs(reference_5km))


##############################################################################
## Leer archivos raster, agrupar en stack, hacer recorte previo,            ##
## hacer downsclaing a 5 Km, realizar el recorte final y guardar los stacks ##
## de TMIN, TMAX, SRAD, WVEL y RHUM                                         ##
##############################################################################

# Ejemplo para humedad relativa
dir.create("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\CROPPED_DOWNSCALING\\RELHUM\\") 

for (i in 1:44){ # de 1981 a 2024
  
  # Leer la lista de archivos del servidor
  Rhum = list.files(paste("\\\\ALLIANCEDFS.ALLIANCE.CGIAR.ORG\\data_cluster17\\observed\\gridded_products\\AgERA5_V1.1\\Relative Humidity\\",1980+i,sep=""), pattern="nc$", full.names=TRUE, recursive=TRUE)
  allrasters.Rhum = lapply(Rhum, raster)
  allrasters.Rhum = stack(allrasters.Rhum)
  
  # Recortar el stack raster usando el shapefile
  rst_crp <- crop(allrasters.Rhum, extent(shp_big))
  
  # Enmascarar el stack raster con el shapefile
  rst_msk <- mask(rst_crp, shp_big)
  
  # Reescalar el stack raster a la resolución de 5 km
  rst_5km <- resample(rst_msk, reference_5km, method = 'bilinear')
  
  # Recortar el stack raster reescalado usando el shapefile de eje cafetero
  rst_crp <- crop(rst_5km, extent(shp))
  
  # Enmascarar el stack raster recortado con el shapefile de eje cafetero
  rst_msk <- mask(rst_crp, shp)
  
  writeRaster(rst_msk,paste("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\CROPPED_DOWNSCALING\\RELHUM\\","humedad_relativa_stack_",1980+i,".tif",sep =""), format = "GTiff", overwrite = TRUE)
} 


##################################################
## Leer archivos raster de precipitacion, hacer ##
## el recorte y guardar los raster recortados   ##
##################################################

for (i in 1:44){

  dir.create(paste("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\CROPPED_DOWNSCALING\\PRECIP\\",1980+i,"\\",sep ="")) 
  
  Rain.list = list.files(paste("\\\\ALLIANCEDFS.ALLIANCE.CGIAR.ORG\\data_cluster17\\observed\\gridded_products\\chirps\\Daily\\p05\\",1980+i,sep=""), pattern="tif$", full.names=TRUE, recursive=FALSE)
  
  for (rain in Rain.list){
    rain.raster <- raster(rain)
    
    # Recortar el raster reescalado usando el shapefile
    rst_crp <- crop(rain.raster, extent(shp))
    
    # Enmascarar el stack raster recortado con el shapefile
    rst_msk <- mask(rst_crp, shp)
    
    fecha <- substr(rain, nchar(rain) - 10)
    
    writeRaster(rst_msk,paste("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\CROPPED_DOWNSCALING\\PRECIP\\",1980+i,"\\","Rain_",fecha,".tif",sep =""), overwrite = TRUE)
  }
} 


######################################
## Recortar el DEM con el shapefile ##
######################################

# Recortar el raster
rst_crp <- crop(DEM, extent(shp))
  
# Enmascarar el raster con el shapefile
rst_msk <- mask(rst_crp, shp)
plot(rst_msk)

writeRaster(rst_msk,"D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\DEM_COLOMBIA\\DEM_5km_eje.tif")