#####################
## Lectura de datos##
#####################

ap <- read.delim("clipboard",header = TRUE,dec=".")

#####################
## Analisis exploratorio
#####################

#ver el objeto
View(ap) 
#ver la estructura del objeto
str(ap)  

#ver datos de la columna seleccionada
ap$pH 
ap[,5]

#Estadisticas basica
mean(ap$pH)
sd(ap$pH)
sd(ap$pH) * 100 / abs(mean(ap$pH))

summary(ap$pH)

#####################
## Analisis espacial
#####################
library(raster)
library(sp)

#Asignar coordenadas
ap.sp <- ap
coordinates(ap.sp) <- ~X+Y

#Graficar objeto espacial de puntos
plot(ap.sp)

#Crear un objeto raster a partir de los puntos
gridded(ap.sp) <- TRUE
ap.rast <- raster(ap.sp)
plot(ap.rast)

ap.rast2 <- raster(ncol=57, nrow=15, xmn=0, xmx=57, ymn=0, ymx=15)
ap.rast2
values(ap.rast2) <- 0 #agregar valores de 0 a cada celda
plot(ap.rast2)

#Cambiar la resolucion del objeto raster
ap.rast <- aggregate(ap.rast, fact=2) #engrosar
ap.rast

res(ap.rast) <- 1 #especificar resolucion
