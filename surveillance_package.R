#https://vimeo.com/140669369

install.packages("surveillance")
library(surveillance)
help(surveillance)

##example dataset
data("salmNewport")
salmNewport

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
format(date, format=" %m %d  %y")
dates <- as.Date(fecha_s, "%m/%d/%y")
sts<-sts(epoch=as.numeric(ts[,1], observed=matrix(ts[,2], epochAsDate=TRUE)

sts<-linelist2sts(counts_long, dateCol="date", aggregate.by="1 month")



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


counts_numeric<-read.csv("C:\\Users\\amykr\\Google Drive\\Kent\\james\\dissertation\\chkv and dengue\\arcgis analysis\\gwr models\\output\\counts_numeric.csv")

ts2<-ts(counts_numeric$dengue, start=c(2014, 10, 1), end=c(2016, 4, 1), frequency=6176)

fix(ts2)
plot(counts_numeric)






#############example code###############


if (requireNamespace("maptools")) {
    # load disProg-object "ha" and convert to S4-class "sts"
    data("ha")
    shpfile <- system.file("shapes/berlin.shp",package="surveillance")
    ha.sts <- disProg2sts(ha, map=maptools::readShapePoly(shpfile,IDvar="SNAME"))
} else {
    data("ha.sts")
    # is almost identical to the above except that German umlauts
    # have been replaced in 'ha.sts@map@data$BEZIRK' for compatibility reasons
}

ha.sts
plot(ha.sts, type = observed ~ 1 | unit)

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
