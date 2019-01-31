library(googlesheets)
library(raster)
library(rasterVis)
library(spdep)
library(dplyr)
library(maptools)
library(agricolae)
library(gstat)
##################################
#Lectura de datos con googlesheets
##################################

gs_auth()
my_sheets <- gs_ls()
gap <- gs_title("Agriculture")
ap <- gap %>%
  gs_read(ws = "Results")

ap2 <- gap %>%
  gs_read(ws = "Results 2")

#####################
## Analisis exploratorio
#####################
summary(ap)
summary(ap2)

#####################
## Preparacion para analisis espacial
#####################

##Asignar coordenadas 

#hortalizas
ap.sp <- ap
coordinates(ap.sp) <- ~X+Y
crs(ap.sp) <- "+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#Frutales
ap2.sp <- ap2
coordinates(ap2.sp) <- ~X+Y
crs(ap2.sp) <- "+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"


#Crear grilla de trabajo

#hortalizas
ap.rast <- raster(ncol=21, nrow=63, xmn=-3, xmx=60, ymn=-3, ymx=18)
ap.rast
values(ap.rast) <- 0 #agregar valores de 0 a cada celda
crs(ap.rast) <- "+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #asignar sistema de coordenadas
plot(ap.rast)

#Frutales
ap.rast2 <- raster(ncol=48, nrow=24, xmn=-2, xmx=46, ymn=-2, ymx=22)
ap.rast2
values(ap.rast2) <- 0 #agregar valores de 0 a cada celda
crs(ap.rast2) <- "+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #asignar sistema de coordenadas
plot(ap.rast2)

#####################
## Interpolaciones Geograficas
#####################

#hortalizas
ap.ph <- gstat(formula=pH~1, locations=ap.sp)
idw1 <- interpolate(ap.rast, ap.ph)
plot(idw,main="Hortalizas: pH del suelo")

ap.ce <- gstat(formula=CEc~1, locations=ap.sp)
idw2 <- interpolate(ap.rast, ap.ce)
plot(idw,main="Hortalizas: Conductividad Eléctrica del suelo")

#Frutales
ap.ph2 <- gstat(formula=pH~1, locations=ap2.sp)
idw3 <- interpolate(ap.rast2, ap.ph2)
plot(idw,main="Frutales: pH del suelo")

ap.ce2 <- gstat(formula=CEc~1, locations=ap2.sp)
idw4 <- interpolate(ap.rast2, ap.ce2)
plot(idw,main="Frutales: Conductividad Eléctrica del suelo")

#####################
## Modelado Geoestadistico
#####################

#Hortalizas
variog.ap.ph <- variogram(pH~1, ap.sp)
plot(variog.ap.ph)

variog.ajust.ap.ph <- fit.variogram(variog.ap.ph, model=vgm(psill=var(ap$pH), 
                                                model=c("Exp", "Mat", "Sph","Gau"), range=12, nugget=0.04),fit.ranges = FALSE)
plot(variog.ap.ph, variog.ajust.ap.ph,main="Hortalizas: pH del suelo",xlab="Distancia",ylab="Semivarianza",
     sub="Modelo Esférico | Rango = 6 m | Sill = 0.06",pch=16,lty=1,lwd=2)


variog.ap.ce <- variogram(CEc~1, ap.sp)
plot(variog.ap.ce)

variog.ajust.ap.ce <- fit.variogram(variog.ap.ce, model=vgm(psill=var(ap$CEc), 
                                                            model=c("Exp", "Mat", "Sph","Gau"), range=12, nugget=0.10),fit.ranges = FALSE)
plot(variog.ap.ce, variog.ajust.ap.ce,main="Hortalizas: Conductividad Eléctrica del suelo",xlab="Distancia",ylab="Semivarianza",
      sub="Modelo Esférico | Rango = 6 m | Sill = 0.19",pch=16,lty=1,lwd=2)

#Frutales
variog.ap2.ph <- variogram(pH~1, ap2.sp)
plot(variog.ap2.ph)

variog.ajust.ap2.ph <- fit.variogram(variog.ap2.ph, model=vgm(psill=var(ap2$pH), 
                                                            model=c("Exp", "Mat", "Sph","Gau"), range=10, nugget=0.08),fit.ranges = FALSE)
plot(variog.ap2.ph, variog.ajust.ap2.ph,main="Frutales: pH del suelo",xlab="Distancia",ylab="Semivarianza",
     sub="Modelo Esférico | Rango = 5 m | Sill = 0.11",pch=16,lty=1,lwd=2,ylim=c(0,0.15))

variog.ap2.ce <- variogram(CEc~1, ap2.sp)
plot(variog.ap2.ce)

variog.ajust.ap2.ce <- fit.variogram(variog.ap2.ce, model=vgm(psill=var(ap2$CEc), 
                                                              model=c("Exp", "Mat", "Sph","Gau"), range=10, nugget=1.5),fit.ranges = FALSE)
plot(variog.ap2.ce, variog.ajust.ap2.ce,main="Frutales: Conductividad Eléctrica del suelo",xlab="Distancia",ylab="Semivarianza",
     sub="Modelo Esférico | Rango = 5 m | Sill = 2.36",pch=16,lty=1,lwd=2,ylim=c(0,2.8))

#####################
## Interpolacion Geoestadistica
#####################

#Hortalizas
nl <- as(ap.rast, 'SpatialGridDataFrame')
pH.krige.ap <- krige(formula=pH~1,locations = ap.sp,newdata=nl,model=variog.ajust.ap.ph) 
pH.plo.ap <- raster(pH.krige.ap["var1.pred"])
plot(pH.plo.ap,main="Hortalizas: pH del suelo")

ce.krige.ap <- krige(formula=CEc~1,locations = ap.sp,newdata=nl,model=variog.ajust.ap.ce) 
ce.plo.ap <- raster(ce.krige.ap["var1.pred"])
plot(ce.plo.ap,main="Hortalizas: Conductividad Eléctrica del suelo")

#Frutales
nl <- as(ap.rast2, 'SpatialGridDataFrame')
pH.krige.ap2 <- krige(formula=pH~1,locations = ap2.sp,newdata=nl,model=variog.ajust.ap2.ph) 
pH.plo.ap2 <- raster(pH.krige.ap2["var1.pred"])
plot(pH.plo.ap2,main="Frutales: pH del suelo")

ce.krige.ap2 <- krige(formula=CEc~1,locations = ap2.sp,newdata=nl,model=variog.ajust.ap2.ce) 
ce.plo.ap2 <- raster(ce.krige.ap2["var1.pred"])
plot(ce.plo.ap2,main="Frutales: Conductividad Eléctrica del suelo")

#####################
## Comparacion grafica
#####################
library(rasterVis)
ht.ph <- stack(idw1,pH.plo.ap)
ht.ce <- stack(idw2,ce.plo.ap)
ft.ph <- stack(idw3,pH.plo.ap2)
ft.ce <- stack(idw4,ce.plo.ap2)

names(ht.ph) <- c("Distancia Inversa Ponderada","Kriging Ordinario")
levelplot(ht.ph, contour=TRUE,par.settings = viridisTheme,main="pH del suelo - Campo de Hortalizas")

names(ht.ce) <- c("Distancia Inversa Ponderada","Kriging Ordinario")
levelplot(ht.ce, contour=TRUE,par.settings = viridisTheme,main="CE del suelo - Campo de Hortalizas")
#
names(ft.ph) <- c("Distancia Inversa Ponderada","Kriging Ordinario")
levelplot(ft.ph, contour=TRUE,par.settings = viridisTheme,main="pH del suelo - Campo de Frutales")

names(ft.ce) <- c("Distancia Inversa Ponderada","Kriging Ordinario")
levelplot(ft.ce, contour=TRUE,par.settings = viridisTheme,main="CE del suelo - Campo de Frutales")
