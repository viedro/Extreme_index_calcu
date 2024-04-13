#################################################################
## TÍTULO : Cálculo de índices extremos climáticos mediante archivos netcdf
## AUTOR : Victor Eduardo Diaz Romero
## CORREO : 20180177@lamolina.edu.pe
## Código también disponible en: https://github.com/viedro/Extreme_index_calcu/blob/main/C_index.R
#################################################################


##Antes que nada instalar Rtools : https://cran.r-project.org/bin/windows/Rtools/
## Instalar dependiendo de la version de R que tengas, puedes poner R.version en la consola para verificarlo

#Paquete para rasterizar datos netcdf
#install.packages("raster")
library(raster)

#Directorio de trabajo
setwd("C:/Users/ASUS/Desktop/prof_Ibañez")

#Paquete para el manejo de datos netcdf
#install.packages("ncdf4")
library(ncdf4)

#Lectura de variables
pp <- nc_open("SENAMHI_pp_d12k_MPI-ESM-LR_hist_scal.nc")

#Corrección de variables y dimensiones (reparar irregularidades entre datos)
#Manejo de arrays
pr <- ncvar_get(pp,"pr")
str(pr) ; class(pr)     ####Dimensiones y tipo de objeto
lat <- ncvar_get(pp,"lat")
lon <- ncvar_get(pp,"lon")
lat <- rev(lat)
pr <- pr[,ncol(pr):1 ,]

#Extracción de tiempo
time <- ncvar_get(pp,"time")
time

#cerrando netcdf para ahorrar memoria
nc_close(pp)

#Añadir tiempos ( numero de días) al periodo base
time2 <- as.Date("1981-01-01")+time
time2

t_max <- max(as.numeric(format(time2,"%Y")))
t_min <- min(as.numeric(format(time2,"%Y")))

######## Recortando rectangulo de ubicación de cuencas
######## En este caso las cuencas a estudiar se encuetran entre  -85°W -77°W, -8.5°S -2.5°S
######## Esto se hace para no tomar toda la región disponible y acelerar el proceso de calculo
max_lon <- -77
min_lon <- -85
max_lat <- -2.5
min_lat <- -8.5

########Correccion final de arrays
pr <- pr[which(lon<= max_lon & lon>= min_lon ),
         which(lat<=max_lat & lat>=min_lat),]

########Corrección final de dimensiones
lon <- lon[which(lon<= max_lon & lon>= min_lon )]
lat <- lat[which(lat<= max_lat & lat>= min_lat)]

######## "easyNCDF" paquete para la creación simple de archivos netcdf
# Por defecto la creación de un netcdf con todos sus metadatos es muy engorrosa, este paquete lo hace en pocas lineas
#install.packages("easyNCDF")
library(easyNCDF)
#install.packages("oceanmap")
library(oceanmap)

#Crearemos nuevos archivos netcdf con el area recortada y con metadatos modificados para que el paquete climdex.pcic.ncdf que instalaremos mas adelante
#pueda funcionar correctamente

################### Creación de archivos netcdf 
metadata <- list(pr = list(units = 'kg m-2 d-1'))
attr(pr, 'variables') <- metadata
names(dim(pr)) <- c('lon', 'lat', 'time')
metadata <- list(lon = list(units = 'degrees_east'))
attr(lon, 'variables') <- metadata
names(dim(lon)) <- 'lon'
metadata <- list(time = list(units = 'days since 1981-01-01'))
attr(time, 'variables') <- metadata

dim(lat) <- length(lat)
metadata <- list(lat = list(units = 'degrees_north'))
attr(lat, 'variables') <- metadata
names(dim(lat)) <- 'lat'
names(dim(time)) <- 'time'

##Array a netcdf, el nombre del archivo no modificarlo ya que es un artificio para el funcionamiento de climdex.pcic.ncdf
##Solo modificar las fechas 
ArrayToNc(list(pr, lon, lat,time), paste0("pr_NAM44_CanRCM4_ERAINT_r1i1p1_",t_min,"-",t_max,".nc"))

##############

#install.packages("devtools")
library(devtools)

#instalación de climdex.pcic desde github
#devtools::install_github("pacificclimate/climdex.pcic")
library(climdex.pcic)

#instalación de climdex.pcic.ncdf desde github
#devtools::install_github('pacificclimate/climdex.pcic.ncdf', ref='release')
library(climdex.pcic.ncdf)

#Creando ficheros de entrada 
input.files <- paste0("pr_NAM44_CanRCM4_ERAINT_r1i1p1_",t_min,"-",t_max,".nc")
#Parte del artificio
author.data <- list(institution="Looney Bin", institution_id="LBC")

#Aqui seleccionar 2 nucleos menos que la cantidad de nucleos de tu pc, para un trabajo en paralelo
#Por ejemplo yo tengo 8 nucleos, le dare el trabajo a 5 o 6 nucleos
#como seguramente las pcs del lab sean de 4 nucleos, darle el trabajo a 2 nucleos
n_nucleos <- 6

#Creando directorio donde guardar los resultados
name_dir <- "hist_Accessssssss"
dir.create(name_dir)

#Con esto calcularemos todos los indices
create.indices.from.files(input.files,paste0(name_dir,"/"),input.files[1], author.data,
                          base.range=c(t_min, t_max), parallel= n_nucleos,climdex.time.resolution ="annual")


####################################################################################
############# Parte 2 calculo de acumulados, generacion de archivos raster y graficado
#install.packages("rgdal")
library(rgdal)
#install.packages("sp")
library(sp)
#install.packages("sf")
library(sf)
#install.packages("maptools")
library(maptools)

#Shapefile de cuencas
shp2 <- readOGR(dsn = "norte_cuencas.shp")

#Modificando proyeccion del shapefile
crs_destino <- CRS("+proj=longlat +datum=WGS84")

# Realizar la transformación de coordenadas
shp <- spTransform(shp2, crs_destino)

# Extrayendo departamneteos y provincias
mapaSHP <- readShapeLines('DEPARTAMENTOS_inei_geogpsperu_suyopomalia.shp')
mapaSHP <- mapaSHP[mapaSHP$NOMBDEP == "PIURA" | mapaSHP$NOMBDEP == "TUMBES"|mapaSHP$NOMBDEP == "LAMBAYEQUE"|mapaSHP$NOMBDEP == "CAJAMARCA"|mapaSHP$NOMBDEP == "AMAZONAS",]

provinSHP <- readShapeLines("PROVINCIAS_inei_geogpsperu_suyopomalia.shp")
provinSHP <- provinSHP[provinSHP$NOMBDEP %in% c("PIURA", "TUMBES", "LAMBAYEQUE", "CAJAMARCA", "AMAZONAS"), ]

landuse <- readShapePoly("PROVINCIAS_inei_geogpsperu_suyopomalia.shp") 
landuse<- landuse[landuse$NOMBDEP %in% c("PIURA", "TUMBES", "LAMBAYEQUE", "CAJAMARCA", "AMAZONAS"), ]

#install.packages("rgeos")
library(rgeos)

#Obtencion de centroides
centr <- gCentroid(landuse, byid = TRUE)
centr <- SpatialPointsDataFrame(centr, data= landuse@data)

#install.packages("rasterVis")
library(rasterVis)
#install.packages("paleteer")
library(paletteer)
#install.packages("rnaturalearth")
library(rnaturalearth)
#install.packages("dplyr")
library(dplyr)
#install.packages("grid")
library(grid)
library(latticeExtra)
#install.packages("lattice")
library(lattice)


# Obtener una lista de todos los archivos en el directorio
archivos <- list.files(name_dir)

# Filtrar los archivos que contienen la palabra "ETCCDI"
archivos_con_ETCCDI <- archivos[grep("ETCCDI", archivos)]
archivos_con_ETCCDI <-  archivos_con_ETCCDI[c(1,6:31)]

# Años a trabajar:
year <- t_min:t_max
year

# Determinar periodo, en el caso del historico puedes tomar todo, pero para las proyecciones
# Lo ideal seria dividir por periodos como 2030:2050 y 2051:2065
# En este caso seleccionare todo el periodo
period <- which(year>=t_min & year<=t_max)


##########Bucle 

funciones <- c("altcdd","csdi","cwd","fd","gsl","id","prcptot","r10mm","r1mm","r20mm","r95p","r99p","rx1day","rx5day","sdii","su","tn10p","tn90p","tr","tx10p","tx90p","wsdi")

##Bucle para lectura de netcdf como raster,el interpolado, recorte , graficado y generqacion de raster de cada indice.
for (i in 1:length(funciones)){
  
  tryCatch({
    archivos2 <- archivos_con_ETCCDI[-4]
    data <- archivos2[grep(funciones[i], archivos2)]
    
    data2 <- brick(paste0(name_dir,"/",data))
    
    rs <- calc(data2[[c(period)]],sum)
    
    u_f <- disaggregate(rs,fact= 10,method="bilinear")
    
    u_f2<- raster::crop(u_f,shp) %>%
      raster::mask(shp)
    
  },error=function(e){})
  
  png(paste0(name_dir,"/",funciones[i], "_ACCES1_hist.png"), width = 12, height = 11, res = 700, units = "in")
  tryCatch({
    image <- levelplot(u_f2, margin = FALSE, xlab = "Longitud", cuts = 100, ylab = "Latitud", 
                       col.regions = rev(paletteer_c("grDevices::Spectral", 101)), maxpixels = 1e20, 
                       main = paste0("Índice ", toupper(funciones[i]), " Histórico Modelo Acces1-0 \n Costa, Sierra Norte 1981-2005")) +
      layer(sp.lines(mapaSHP, lwd = 1.8, col = 'black')) +
      layer(sp.lines(provinSHP, lwd = 1, col = 'gray30')) +
      layer(sp.points(centr, pch = 20, cex = 1, col = "black")) +
      layer(sp.text(coordinates(centr) + 0.07, txt = landuse$NOMBPROV, col = "black", cex = 0.6))
  },error=function(e){})
  
  tryCatch({
    print(image)
  },error=function(e){})
  dev.off() # Cerrar el dispositivo gráfico
  
  
  tryCatch({
    writeRaster(u_f2, filename = paste0(name_dir,"/",funciones[i], "_ACCES1_hist.tif"), format = "GTiff", overwrite = TRUE)
  },error=function(e){})
  
  if (i == length(funciones)) {
    cat("Índices calculados y exportados , revisar directorio creado\n")
    cat("   _______________\n")
    cat(" /                          \\\n")
    cat("|    When you finally |\n")
    cat("|    fix that bug     |\n")
    cat("|    and it runs      |\n")
    cat(" \\_______/\n")
    cat("         |\n")
    cat("         O\n")
    cat("        /|\\\n")
    cat("        / \\\n")
  }
}





