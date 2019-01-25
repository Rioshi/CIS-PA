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
## Analisis exploratorio espacial
#####################
library(raster)
library(sp)

#Asignar coordenadas
ap.sp <- ap
coordinates(ap.sp) <- ~X+Y
crs(ap.sp) <- "+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#Graficar objeto espacial de puntos
plot(ap.sp)

#Crear un objeto raster a partir de los puntos
gridded(ap.sp) <- TRUE
ap.rast <- raster(ap.sp)
plot(ap.rast)

ap.rast2 <- raster(ncol=57, nrow=15, xmn=-3, xmx=60, ymn=-3, ymx=18)
ap.rast2
values(ap.rast2) <- 1 #agregar valores de 0 a cada celda
crs(ap.rast2) <- "+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #asignar sistema de coordenadas
plot(ap.rast2)

#Cambiar la resolucion del objeto raster
ap.rast <- aggregate(ap.rast, fact=2) #engrosar
ap.rast

res(ap.rast) <- 1 #especificar resolucion

#####################
## Interpolacion geograficas
#####################

# Vecinos mas proximos "Nearest neighbour interpolation"
library(gstat)
gs <- gstat(formula=pH~1, locations=ap.sp, nmax=5, set=list(idp = 0)) 
#idp = ponderacion 0 (todos los vecinos tienen el mismo peso)
# nmax = 5 maximo numero de puntos es 5
nn <- interpolate(ap.rast2, gs) 
plot(nn)
#gstat(id = "pH", formula = pH~1, locations = ~X+Y, data=ap)



#Distancia inversa ponderada "Inverse distance weighted"
library(gstat)
gs <- gstat(formula=pH~1, locations=ap.sp)
idw <- interpolate(ap.rast2, gs)
plot(idw,main="pH del suelo")

gs <- gstat(formula=pH~1, locations=ap.sp, nmax=5, set=list(idp=1))
idw <- interpolate(ap.rast2, gs)
plot(idw)

gs <- gstat(formula=pH~1, locations=ap.sp, nmax=5, set=list(idp=2))
idw <- interpolate(ap.rast2, gs)
plot(idw)

gs <- gstat(formula=pH~1, locations=ap.sp, nmax=5, set=list(idp=5))
idw <- interpolate(ap.rast2, gs)
plot(idw)

gs <- gstat(formula=pH~1, locations=ap.sp, nmax=5, set=list(idp=10))
idw <- interpolate(ap.rast2, gs)
plot(idw)

#####################
## Interpolacion estadistica
#####################

#Variograma
library(gstat)
variog <- variogram(CEc~1, ap.sp)
plot(variog)

#cutoff = distancia máxima a la que queremos sacar la semivarianza
#width = intervalo entre las distancias
variog <- variogram(pH~1, ap.sp, width=6,cutoff=40)
plot(variog)

#variograma ajustado
variog.ajust <- fit.variogram(variog, model=vgm(psill=var(ap.sp$CEc), 
                                                model="Gau", range=15, nugget=0.10),fit.ranges = FALSE, fit.method = 7)
#psill: Semivarianza maxima (donde ya no hay autocorrelacion) = varianza
#range: Distancia a la cual la semivarianza llega al maximo
#nugget: Distancia donde comienza a existir autocorrelacion espacial
plot(variog, variog.ajust)
ap.rast2 <- raster(ncol=24, nrow=12, xmn=-2, xmx=46, ymn=-2, ymx=22)
ap.rast2
values(ap.rast2) <- 1 #agregar valores de 0 a cada celda
crs(ap.rast2) <- "+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #asignar sistema de coordenadas
plot(ap.rast2)

#Kriging
nl <- as(ap.rast2, 'SpatialGridDataFrame')
pH.krige <- krige(formula=pH~1,locations = ap.sp,newdata=nl,model=variog.ajust) 
#newdata debe ser un Grid (no puede ser raster)
pH.plo <- raster(pH.krige["var1.pred"])
plot(pH.plo,main="Conductividad eléctrica del suelo (1:1)")

spplot(pH.krige["var1.pred"], main="Residuos krigeados (kriging ordinario)")
spplot(ap.sp["pH"])
#####################
## COMPARACION GRAFICA
#####################
library(rasterVis)
todo <- stack(nn,idw,pH.plo)
names(todo) <- c("NN","IDW","Kriging")
levelplot(todo, contour=TRUE,par.settings = viridisTheme)

