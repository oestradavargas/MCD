######################
## Cargar librerias ##
######################

require(pacman)
pacman::p_load(raster, lubridate, terra, ncdf4, sp, sf, zoo)

setwd("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\DATA\\SPEI\\SPEI_3_months\\")

##-- Importar rasters SPEI para cada año y mes --##

rastlist.SPEI = list.files(path = getwd(), pattern="tif$", full.names = TRUE, recursive = FALSE)

##-- hacer stack con los rasters --##
allrasters.SPEI = lapply(rastlist.SPEI, raster)
allrasters.SPEI = stack(allrasters.SPEI)

plot(allrasters.SPEI[[517]])

date=seq(as.Date("1981-03-01"),as.Date("2024-03-31") ,by="month")
year_date  = lubridate::year(date)
month_date = lubridate::month(date)

############################
## Crear archivo para CPT ##
############################

coor_min=apply(coordinates(allrasters.SPEI),2,min)
coor_max=apply(coordinates(allrasters.SPEI),2,max) 
coor_all=cbind(coor_min,coor_max)

res = 0.05  # Resolucion de los rasters

lat=sort(seq(coor_all[2,1]-(res/2),coor_all[2,2]-(res/2),res),decreasing = T)
lon=sort(seq(coor_all[1,1]-(res/2),coor_all[1,2]-(res/2),res))

## Reemplazar NA por -999
val = values(allrasters.SPEI)
val[which(is.na(val),arr.ind = T)]= -999

## Los raster quedan en listas de vectores
val_l = split( val, col(val) )

## Generar matrices con latitud en las filas y longitud en las columnas 
val_matrix=lapply(val_l,function(x)matrix(x,length(lat),length(lon),byrow=TRUE,dimnames=list(lat,lon)))

p="xmlns:cpt=http://iri.columbia.edu/CPT/v10/"
p1="cpt:nfields=1"

year_month = seq(as.Date("1981-03-01"),as.Date("2024-03-31") ,by="month")
year_only = format(year_month, "%Y-%m")
year_p=paste0("cpt:T=",year_only)

## Crear el encabezado de año y mes para el primer raster de datos
p2=paste0("cpt:field=ssta, ",year_p[1],", cpt:nrow=",length(lat),", cpt:ncol=",length(lon),", cpt:row=Y, cpt:col=X, cpt:units=Celsius_scale, cpt:missing=-999")

setwd("D:\\OneDrive - CGIAR\\CIAT\\[] MAESTRIA\\TRABAJO DE GRADO_1\\CPT\\")
getwd()

name_file="EJE_CAF_CPT_SPEI3.txt"
sink(name_file)
cat(p)
cat("\n")
cat(p1) 
cat("\n")
cat(p2) 
cat("\n")
u=Map(function(x,y){write.table(t(c(" ",lon)),sep="\t",col.names=F,row.names=F,quote = F);write.table(x,sep="\t",col.names=F,row.names=T,quote = F);cat(y);cat("\n")},val_matrix,c(year_p[-1],""))
sink()