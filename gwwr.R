library(maptools)

setwd ("C:/Users/amykr/Google Drive/Kent/james/dissertation/chkv and dengue/arcgis analysis/gwr models/output/gwr4nov16/dengue")
crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
barrios<-readShapePoly("barrios.shp",proj4string=crswgs84,verbose=TRUE)
class(barrios)
barrios$ID_BARRIO<- as.numeric(as.character(barrios$ID_BARRIO))
barrios$barrios_order <- nbOrder(poly2adjmat(barrios), maxlag=10)

bw <- 3986.321
#bw <- ggwr.sel(dengue_c_4 ~ I(dengue_c_5 + dengue_c_6 + dengue_c_7 + dengue_c_8 + dengue_c_9 + dengue__10 + dengue__11 + dengue__12), data=barrios,
#               family=quasipoisson(), longlat=TRUE)
## End(Not run)


gridded(x) tells whether x derives from SpatialPixels, or when used in
assignment, coerces a SpatialPoints object into a SpatialPixels object.


crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
barrios2<-readShapePoly("C:/Users/amykr/Google Drive/Kent/james/dissertation/chkv and dengue/arcgis analysis/gwr models/output/surveillance/zika/barrios.shp",proj4string=crswgs84,verbose=TRUE)
class(barrios)

#gGrid <- sample.Polygons(slot(gSRouter, "polygons")[[1]], 5000,
gGrid <- spsample(slot(gSRouter, "polygons")[[1]], 338, type="regular")
gridded(gGrid) <- TRUE

gw.adapt(dp =cbind(barrios$dengue_c_2, barrios$dengue_c_3), fp = gGrid,  quant = .75, longlat=TRUE)

gw.cov(barrios$dengue_c_4, barrios$dengue_c_5 + barrios$dengue_c_6 + barrios$dengue_c_7 + barrios$dengue_c_8 + barrios$dengue_c_9 + barrios$dengue__10 + barrios$dengue__11 + barrios$dengue__12, adapt = NULL, bw = bw, gweight = gwr.bisquare,
       cor = TRUE, var.term = FALSE, longlat = TRUE)


cali <- ggwr(dengue_c_4 ~ I(dengue_c_5 + dengue_c_6 + dengue_c_7 ), data=barrios,
             family=quasipoisson(), longlat=TRUE, bandwidth=bw)

cali
plot(cali)
