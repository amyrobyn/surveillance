setwd ("C:/Users/amykr/Google Drive/Kent/james/dissertation/chkv and dengue/arcgis analysis/gwr models/output/surveillance")

install.packages("surveillance")
library(surveillance)
help(surveillance)
help(sp)

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

##example dataset following prof's code from https://vimeo.com/140669369
data("salmNewport")
salmNewport

ts[,1]<-ISOweek2date(toupper(paste0(ts[,1],"-1")))
sNewportAll<-sts(epoch=as.numeric(ts[,1],observed=matrix(ts[,2]),epochAsDate=True)
class(sNewportAll)

#view the data by space and time
dim(salmNewport)
dim(aggregate(salmNewport, by="unit"))
dim(aggregate(salmNewport, by="time"))
head(observed(salmNewport), n=1)

#plot the data by time and by region
plot(salmNewport, type=observed~time)
plot(salmNewport[,1:8], type=observed~time|unit)


######################################
vignette(package = "surveillance")
demo(package = "surveillance")

data(abattoir)
plot(abattoir)
population(abattoir)


##################################################
#import numeric data- counts of dengue
counts_numeric<-read.csv("C:\\Users\\amykr\\Google Drive\\Kent\\james\\dissertation\\chkv and dengue\\arcgis analysis\\gwr models\\output\\counts_numeric.csv")
head(counts_numeric, n=1)
counts_numericsts<-sts(epoch=as.numeric(counts_numeric[,29]),observed=matrix(counts_numeric[,6]),epochAsDate=TRUE)
class(counts_numericsts)

plot(counts_numericsts)


setclass("sts", representation(epoch= as.numeric("date"),
					freq ="12",
					start ="numeric",
					observed = "matrix",
					state ="matrix",
					map ="SpatialPolygonDataFrame",
					epochAsDate ="TRUE",
					))



fix(counts_numeric)
sNewportAll<-sts(epoch=as.numeric(ts[,29],observed=matrix(ts[,6]),epochAsDate=True)
dim(sNewportAll) 

total_pop<- counts_numeric$total_pop

dengue<- counts_numeric$dengue

#succesfully import the data as ts data
total_pop_v <- as.data.frame(t(total_pop))
dengue_v <- as.data.frame(t(dengue))
ts<-ts(counts_numeric$dengue$total_pop_v, start=c(2014, 10, 1), end=c(2016, 4, 1), frequency=6177)
fix(ts2)

#create spatialpolygon from barrios shapefile
crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
barrios=readShapePoly("C:/Users/amykr/Google Drive/Kent/james/dissertation/chkv and dengue/arcgis analysis/gwr models/output/surveillance/barrios.shp",proj4string=crswgs84,verbose=TRUE)
class(barrios)
str(barrios@data)
plot(barrios)
barrios_order <- nbOrder(poly2adjmat(barrios), maxlag=10)



#create spatial temporal data set
countsdata <- sts(observed=dengue, start = c(2014, 10), frequency = 12, neighbourhood = barrios_order, map = barrios, population = total_pop)

exists("total_pop")


# Read SHAPEFILE.shp from the current working directory (".")
shape <- readOGR(dsn = ".", layer = "barrios")
as.SpatialPolygons.PolygonsList(Srl, proj4string=CRS(as.character(NA))

library(maptools)
barrios.shp <- read.shape(system.file("shapes/sids.shp", package="maptools")[1])


#import counts of dengue data
ts<-read.csv("C:\\Users\\amykr\\Google Drive\\Kent\\james\\dissertation\\chkv and dengue\\arcgis analysis\\gwr models\\output\\counts_numeric.csv", fileEncoding="UCS-2LE",sep=",")
head(ts,n=2)
require(ISOweek)

#attach data
attach(counts_long)	

#set data as time series?
ts<- as.ts(counts_long)

#view data in editor
#fix(counts_long)

#summarize data
summary(counts_long)

#format dates into mdy
format(date, format=" %m %d  %y")
dates <- as.Date(fecha_s, "%m/%d/%y")

#set data as spatial time series??
sts<-sts(epoch=as.numeric(ts[,1], observed=matrix(ts[,2], epochAsDate=TRUE)

#try to set data to spatial time series by aggregation??
sts<-linelist2sts(counts_long, dateCol="date", aggregate.by="1 month")






#############example code###############

if (require("maptools")) {
    # load disProg-object "counts_long" and convert to S4-class "sts"
    data("counts_long")
    shp <- system.file("shapes/berlin.shp",package="surveillance")
    counts_long.sts <- disProg2sts(ha, map=maptools::readShapePoly(shp,IDvar="SNAME"))
  } else {
    # load pre-built sts-object
    data("counts_long.sts")
    # the only difference is that here German umlauts have been removed from
    # 'ha.sts@map@data$BEZIRK' for compatibility reasons
  }
  
  plot(ha.sts, type=observed ~ 1 | unit)


## convert ts/mts object to sts
z <- ts(matrix(rpois(300,10), 100, 3), start = c(1961, 1), frequency = 12)
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
