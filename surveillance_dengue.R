##################amy krystosik, surveillance package, 11/13/16##################
#video example of importing date: https://vimeo.com/140669369

setwd ("C:/Users/amykr/Google Drive/Kent/james/dissertation/chkv and dengue/arcgis analysis/gwr models/output/surveillance")

pkg = c("sp", "xts", "zoo", "spacetime", "trajectories",
"maptools", "maps", "mapview", "leaflet", "rglwidget",
"rgl", "RColorBrewer", "ggmap", "ggplot2", "dplyr",
"rmarkdown", "units", "rgdal", "rgeos", "tmap", "leaflet", "spatstat", "gstat", "spdep",
"rgdal")
install.packages(pkg)

install.packages("surveillance")
library(surveillance)
help(surveillance)
help(sp)

require("ISOweek")
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
#create ts data class
ts(counts_numeric[2:52], start = c(2014, 10), frequency = 12)
structure(counts_numeric)
#create sts data class
counts_numericsts<-sts(epoch=as.numeric(counts_numeric$idx),observed=matrix(counts_numeric$dengue),epochAsDate=TRUE, freq =12, start=c(2014, 10), map=barrios, neighbourhood = barrios_order)
####el errror sale aqui!###









str(counts_numericsts)
class(counts_numericsts)
plot(counts_numericsts, type=observed~time)
#sucess!

#plot the data by time and by region
plot(counts_numericsts, type=observed~time)
plot(counts_numericsts[,1:8], type=observed~time|counts_numericsts$codigo_barrio)
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

