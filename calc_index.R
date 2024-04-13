#################################################################
## TÍTULO : Cálculo de índices extremos climáticos mediante archivos netcdf
## AUTOR : Victor Eduardo Diaz Romero
## CORREO : 20180177@lamolina.edu.pe
## Código también disponible en: https://github.com/viedro/Extreme_index_calcu/edit/main/calc_index.R
#################################################################


##Antes que nada instalar Rtools : https://cran.r-project.org/bin/windows/Rtools/
## Instalar dependiendo de la version de R que tengas, puedes poner R.version en la consola para verificarlo

#Paquete para rasterizar datos netcdf
#install.packages("raster")
library(raster)

#Directorio de trabajo
setwd("C:/Users/ASUS/Desktop/Extreme_index")

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
pr <- pr[  ,ncol(pr):1,]

#Extracción de tiempo
time <- ncvar_get(pp,"time")
time

#cerrando netcdf para ahorrar memoria
nc_close(pp)

#Añadir tiempos ( numero de días) al periodo base
time2 <- as.Date("1981-01-01")+time
time2

class(time2)
t_max <- max(as.numeric(format(time2,"%Y")))
t_min <- min(as.numeric(format(time2,"%Y")))

######## Recortando rectangulo de ubicación de cuencas
######## En este caso las cuencas a estudiar se encuetran entre  -85°W -77°W, -8.5°S -2.5°S
######## Esto se hace para no tomar toda la región disponible y acelerar el proceso de calculo
max_lon <- -74
min_lon <- -78
max_lat <- -9
min_lat <- -14

########Correccion final de arrays
pr <- pr[which(lon<= max_lon & lon>= min_lon ),
         which(lat<=max_lat & lat>=min_lat),]
str(pr)
########Corrección final de dimensiones
lon <- lon[which(lon<= max_lon & lon>= min_lon )]
lat <- lat[which(lat<= max_lat & lat>= min_lat)]

######## "easyNCDF" paquete para la creación simple de archivos netcdf
# Por defecto la creación de un netcdf con todos sus metadatos es muy engorrosa, este paquete lo hace en pocas lineas
#install.packages("easyNCDF")
library(easyNCDF)


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

#install.packages("devtools")   ####nstalar paquetes desde github
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

n_nucleos <- 6

#Creando directorio donde guardar los resultados
name_dir <- "hist_MPI"
dir.create(name_dir)

#Con esto calcularemos todos los indices
create.indices.from.files(input.files,paste0(name_dir,"/"),input.files[1], author.data,
                          base.range=c(t_min, t_max), parallel= n_nucleos,climdex.time.resolution ="annual")


library(rasterVis)
library(paletteer)
####################################################################################
############# Parte 2 calculo de acumulados, generacion de archivos raster y graficado

# Obtener una lista de todos los archivos en el directorio
archivos <- list.files(name_dir)

# Filtrar los archivos que contienen la palabra "ETCCDI"
archivos_con_ETCCDI <- archivos[grep("ETCCDI", archivos)]

# Años a trabajar:
year <- t_min:t_max
year

# Determinar periodo, en el caso del historico puedes tomar todo, pero para las proyecciones
# Lo ideal seria dividir por periodos como 2030:2050 y 2051:2065
# En este caso seleccionare todo el periodo
period <- which(year>=t_min & year<=t_max)

shp <- shapefile('DEPARTAMENTOS_inei_geogpsperu_suyopomalia.shp')
shp <- shp[shp$NOMBDEP == "LIMA",]

library(sf)
shape <- st_read("DEPARTAMENTOS_inei_geogpsperu_suyopomalia.shp")
shape <- shape[shape$NOMBDEP == "LIMA", ]

shape2 <- st_read("PROVINCIAS_inei_geogpsperu_suyopomalia.shp")
shape2 <- shape2[shape2$NOMBDEP == "LIMA", ]

#Filtro de nombre de indices
indices <- sub("ETCCDI.*", "", archivos_con_ETCCDI)

#install.packages("ggplot2")
library(ggplot2)
library(metR)
library(ggspatial)
##########Bucle 
##Bucle para lectura de netcdf como raster,el interpolado, recorte , graficado y generqacion de raster de cada indice.
for (i in 1:length(archivos_con_ETCCDI)){
  tryCatch({
    data <- archivos_con_ETCCDI[i]
    
    data2 <- brick(paste0(name_dir,"/",data))
   
    rs <- calc(data2[[c(period)]],sum)
    
    rs <- disaggregate(rs,fact= 10,method="bilinear")
    
    u_f2<- raster::crop(rs,shp) %>%
      raster::mask(shp)
    
  },error=function(e){})
  
  
  u_f3 <- as.data.frame(u_f2,xy=T)
  u_f3 <- na.omit(u_f3)
  


  tryCatch({
    ggplot(data=shape)+
      geom_contour_fill(data= u_f3,aes(x=x,y=y,z=layer),bins=30)+
      geom_sf(data=shape2,size=1,fill="transparent",col="black")+
      geom_sf(fill="transparent",size=1,col="black") +
      theme(plot.title=element_text(hjust=0.5),legend.key.height = unit(2,"cm"))+ theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5))+
      scale_fill_gradientn(colours=rev(paletteer_c("grDevices::Spectral",30)))+theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
      theme_bw()+
      xlab("Longitud")+
      ylab("Latitud")+
      theme(
        panel.grid.major = element_line(color="gray60", linetype="dashed", size=0.25),legend.key.height = unit(1.5,"cm"),
        panel.ontop = TRUE,
        panel.background = element_rect(color=NA, fill=NA),  # Change the background to white
        legend.key = element_rect(color="black"),  # Make the text bold
        plot.title = element_text(hjust=0.5, face="bold"),
        strip.background = element_rect(fill="white",color=NA),
        strip.text = element_text(color="black", face="bold",size=15)
      ) +
      annotation_scale(location = "bl", width_hint = 0.4) +
      annotation_north_arrow(location = "tr", which_north = "true", 
                             pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                             style = north_arrow_fancy_orienteering)+ggtitle(paste0("Índice ", toupper(indices[i]), " Histórico Modelo MPI \n Lima 1981-2005"))+ labs(fill="")
    
    ggsave(paste0(name_dir,"/",indices[i], "_MPI_hist.png"), width = 7, height = 8,units = 'in',dpi=700)

  },error=function(e){})
  

  
  
  tryCatch({
    writeRaster(u_f2, filename = paste0(name_dir,"/",indices[i], "_MP1_hist.tif"), format = "GTiff", overwrite = TRUE)
  },error=function(e){})
  
  if (i == length(archivos_con_ETCCDI)) {
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




