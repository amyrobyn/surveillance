setwd ("C:/Users/amykr/Google Drive/Kent/james/dissertation/chkv and dengue/arcgis analysis/gwr models/output/surveillance")

install.packages("surveillance")
library(xts)
library(maptools)
library(surveillance)
library(dplyr)
library(tidyr)
install.packages("animation")
library(animation)
install.packages("gridExtra")

crswgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
barrios<-readShapePoly("barrios.shp",proj4string=crswgs84,verbose=TRUE)
class(barrios)
barrios$ID_BARRIO<- as.numeric(as.character(barrios$ID_BARRIO))
str(barrios@data)
plot(barrios)
barrios$barrios_order <- nbOrder(poly2adjmat(barrios), maxlag=10)
counts_numeric<-read.csv("./counts1.csv") %>% filter(codigo_barrio!=2300)
counts_numeric$idx<-as.Date(counts_numeric$date.1)
ident<-data.frame(cbind(ID=0:337,ID_Barrio=barrios$ID_BARRIO))
counts_numeric2<-inner_join(counts_numeric,ident,by=c("codigo_barrio"="ID_Barrio"))
counts1<-counts_numeric2 %>% select(c(54,2,4)) %>% spread(key=ID,value=dengue)
counts_numericsts<-sts(epoch=counts1$date_numeric,observed=counts1[,2:ncol(counts1)],epochAsDate=FALSE, freq =12, start=c(2014, 10) ,map=barrios, neighbourhood = barrios$barrios_order)

###basic plots###
plot(counts_numericsts, type = observed~time)
plot(counts_numericsts, type = observed~unit)
#, labels=list(font=2), colorkey = list(space ="right"), sp.layout=layout.scalebar(counts_numericsts@map, corner = c(0.05, 0.05), scale = 50, labels = c("0", "50 km"), height = 0.03)
#population=counts_numericsts@maps$POPULATION/100000, 
stsplot_space(counts_numericsts)
par(mar=c(1,1,1,1))
plot(counts_numericsts, units = which(colSums(observed(counts_numericsts))>0))
##animation###
#animation::saveHTML(animate(counts_numericsts, tps=1:19, total.args = list()), title ="Evolution of dengue outbreak in Cali, Colombia, 2014-2016", ani.width = 500, ani.height = 600)

##basic models##
counts_numericsts_basicmodel<-list(end = list(f=addSeason2formula(~1+t, period = counts_numericsts@freq)), ar = list(f=~1), ne =list(f=~1, weights = neighbourhood(counts_numericsts)==1), family = "NegBin1") 

counts_numericsts_fit_basic<-hhh4(stsObj= counts_numericsts, control =counts_numericsts_basicmodel)
summary(counts_numericsts_basicmodel, idx2Exp = TRUE, amplitudeShift = TRUE, maxEV=+TRUE)
hhh4(stsObj= counts_numericsts, control = counts_numericsts_basicmodel)

plot(counts_numericsts_fit_basic, type = "season", components = "end", main = "")
confint(counts_numericsts_fit_basic, parm = "overdisp")
AIC(counts_numericsts_fit_basic, update(counts_numericsts_fit_basic, family = "Poisson"))

districts2plot<-which(colSums(observed(counts_numericsts))>20)
plot(counts_numericsts_fit_basic, type = "fitted", units = districts2plot, hide0s = TRUE)

Spropo <-matrix(1 ~counts_numericsts@map@data$xxx.2014, nrow = nrow(counts_numericsts), ncol =ncol(counts_numericsts), byrow = TRUE)
