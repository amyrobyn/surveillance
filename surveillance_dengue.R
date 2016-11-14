##################amy krystosik, surveillance package, 11/13/16##################

setwd ("C:/Users/amykr/Google Drive/Kent/james/dissertation/chkv and dengue/arcgis analysis/gwr models/output/surveillance")
install.packages("surveillance")
library(surveillance)
help(surveillance)
help(sp)
install.packages("zoo")
install.packages("xts")
install.packages("sp")
install.packages("ggplot2")  
install.packages("rgdal") 
install.packages("rgeos") 
install.packages("tmap") 
install.packages("ggmap") 
install.packages("leaflet") 
install.packages("spatstat") 
install.packages("gstat") 
install.packages("spdep")
install.packages("rgdal")
require("ISOweek")
library("rgdal")
library(xts)
install.packages("maptools")
library(maptools)
###################################################create spatialpolygon from barrios shapefile###################################################
crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
barrios<-readShapePoly("C:/Users/amykr/Google Drive/Kent/james/dissertation/chkv and dengue/arcgis analysis/gwr models/output/surveillance/barrios.shp",proj4string=crswgs84,verbose=TRUE)
class(barrios)
barrios$ID_BARRIO<- as.numeric(as.character(barrios$ID_BARRIO))
str(barrios@data)
plot(barrios)
barrios$barrios_order <- nbOrder(poly2adjmat(barrios), maxlag=10)

#sucess!

###################put sptialpolygon into dataframe: ##################
polygons <- readOGR('C:/Users/amykr/Google Drive/Kent/james/dissertation/chkv and dengue/arcgis analysis/gwr models/output/surveillance/barrios.shp', layer = 'barrios')
class(polygons)
poly_df <- as.data.frame(polygons)
# do some staff with "poly_df" that doesn't support SpatialPolygonsDataFrame
# then convert it to SPDF back again
s_poly <- SpatialPolygonsDataFrame(polygons, poly_df)
# add new column to SPDF:
s_poly$new_column <- "codigo_barrio" 
polygons <- SpatialPolygonsDataFrame(polygons, counts_numeric)

###################import numeric data- counts of dengue##################
counts_numeric<-read.csv("C:\\Users\\amykr\\Google Drive\\Kent\\james\\dissertation\\chkv and dengue\\arcgis analysis\\gwr models\\output\\counts.csv")
counts_numeric$idx<-as.Date(counts_numeric$date_formated)
ts(counts_numeric[2:52], start = c(2014, 10), frequency = 12)
structure(counts_numeric)


counts_numeric$codigo_barrio<- as.numeric(as.character(counts_numeric$codigo_barrio))
counts_numeric$dengue<- as.numeric(as.character(counts_numeric$dengue))
counts_numeric$date_numeric<- as.numeric(as.character(counts_numeric$date_numeric))
str(counts_numeric)


denguex<- xts(counts_numeric$dengue, order.by = idx)
codigo_barriox<- xts(counts_numeric$codigo_barrio, order.by = idx)

xts(cbind(denguex,codigo_barriox),idx)

ts_dengue<-ts(denguex, start=c(2014, 10), end=c(2016, 4), frequency=12)
structure(ts_dengue)
ts_barrio<-ts(codigo_barriox, start=c(2014, 10), end=c(2016, 4), frequency=12)
structure(ts_barrio)
ts_idx<-ts(idx, start=c(2014, 10), end=c(2016, 4), frequency=12)
structure(ts_idx)
ts_barriosorderx<-ts(barrios_orderx, start=c(2014, 10), end=c(2016, 4), frequency=12)
structure(ts_barrios_orderx)

plot(ts_dengue)
dim(ts_dengue)
dim(aggregate(ts_dengue, by="codigo_barriox"))
dim(aggregate(counts_numericsts, by="idx"))
head(ts_dengue, n=4)

#colnames(ts) <- c("ID_BARRIO") 
counts_numericsts<-sts(epoch=as.numeric(ts_idx),observed=matrix(ts_dengue),epochAsDate=TRUE, freq =12, start=c(2014, 10), map = barrios)


#, map=barrios, neighbourhood = barrios_order
str(counts_numericsts)
class(counts_numericsts)
plot(counts_numericsts, type=observed~time)
#sucess!




#plot the data by time and by region
plot(counts_numericsts, type=observed~time)
plot(counts_numericsts[,1:8], type=observed~time|codigo_barrio)
fix(counts_numeric)


######################example code###################################################
## convert ts/mts object to sts
z <- ts(matrix(rpois(300,10), 100, 10), start = c(2014, 1), frequency = 12)
z.sts <- as(z, "sts")
plot(z.sts)

## conversion to the quasi-standard "xts" class is also possible
## -< enables interactive time series plots using package "dygraphs"
if (require("xts")) {
    z.xts <- as.xts(z.sts)
    plot(z.xts)
}

## reconstruct 'measlesWeserEms' from its components
data("measlesWeserEms")
measlesWeserEms
counts <- observed(measlesWeserEms)
map <- measlesWeserEms@map
populationFrac <- measlesWeserEms@populationFrac
weserems_nbOrder <- if(requireNamespace("spdep")) {
    # determine adjacency orders from the map
    nbOrder(poly2adjmat(map), maxlag = 10)
} else {
    # use the stored neighbourhood structure
    neighbourhood(measlesWeserEms)
}
mymeasles <- sts(observed = counts, start = c(2001, 1), frequency = 52,
    neighbourhood = weserems_nbOrder, map = map, population = populationFrac)
stopifnot(identical(mymeasles, measlesWeserEms))

