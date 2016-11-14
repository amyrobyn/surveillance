
#eg.
data("measles.weser")
measles <- disProg2sts(measles.weser)

setwd ("C:/Users/amykr/Google Drive/Kent/james/dissertation/chkv and dengue/arcgis analysis/gwr models/output/surveillance")

#install.packages("animation")
#install.packages("surveillance")
#install.packages("gridExtra")
library(xts)
library(maptools)
library(surveillance)
library(dplyr)
library(tidyr)
library(animation)


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

population<-counts_numeric2 %>% select(c(54,2,10)) %>% spread(key=ID,value=total_pop)
population<-data.matrix(population, rownames.force = NA)

summary(population)
counts_sts<-sts(epoch=counts1$date_numeric,observed= counts1[,2:ncol(counts1)],epochAsDate=FALSE, freq =12, start=c(2014, 10), map=barrios, neighbourhood = barrios$barrios_order, population=population[,2:ncol(population)])


###basic plots###
jpeg("dengue_time.jpg")
plot(counts_sts, type = observed~time)
dev.off()

jpeg("dengue_time_map.jpg")
plot(counts_sts, type = observed~unit)
dev.off()
     #, population=as.Numeric(population[,2:ncol(population)]), labels=list(font=2), colorkey = list(space ="right"), sp.layout=layout.scalebar(counts_numericsts@map, corner = c(0.05, 0.05), scale = 50, labels = c("0", "50 km"), height = 0.03))
 

#margins are too small....
op <- par(mfrow = c(2, 2), # 2 x 2 pictures on one plot
          pty = "s")       # square plotting region,
# independent of device size
jpeg("dengue_units.jpg")
?par
plot(counts_sts, units = which(colSums(observed(counts_sts))>0))
dev.off()

##animation###
#animation::saveHTML(animate(counts_numericsts, tps=1:19, total.args = list()), title ="Evolution of dengue outbreak in Cali, Colombia, 2014-2016", ani.width = 500, ani.height = 600)

##basic models##
counts_sts_basicmodel<-list(end = list(f=addSeason2formula(~1+t, period = counts_sts@freq)), ar = list(f=~1), ne =list(f=~1, weights = neighbourhood(counts_sts)==1), family = "NegBin1") 
counts_sts_fit_basic<-hhh4(stsObj= counts_sts, control =counts_sts_basicmodel)
summary(counts_sts_basicmodel, idx2Exp = TRUE, amplitudeShift = TRUE, maxEV=+TRUE)
hhh4(stsObj= counts_numericsts, control = counts_sts_basicmodel)
plot(counts_sts_fit_basic, type = "season", components = "end", main = "")
confint(counts_sts_fit_basic, parm = "overdisp")
AIC(counts_sts_fit_basic, update(counts_sts_fit_basic, family = "Poisson"))
districts2plot<-which(colSums(observed(counts_sts))>20)
plot(counts_sts_fit_basic, type = "fitted", units = districts2plot, hide0s = TRUE)

#multivariate modoel
#e.g: Sprop <-matrix(1~measlesWeserE<S@map@data$vacc1.2004), nrow = nrow(measlesWeserEMS), ncol(measlesWeserEMS), byrow=TRUE)
#time varying
rain<--matrix(as.numeric(counts_numeric2$services_index),  nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)
temp<--matrix(as.numeric(counts_numeric2$services_index),  nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)
#fixed
services<--matrix(as.numeric(counts_numeric2$services_index),  nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)
educ<--matrix(as.numeric(counts_numeric2$assist_educ_P),  nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)
afro<--matrix(as.numeric(counts_numeric2$negro__a___mulato__afrop),  nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)
area<--matrix(as.numeric(counts_numeric2$arean3210),  nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)
estrato<--matrix(as.numeric(counts_numeric2$estrato_mon3210),  nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)
limit<--matrix(as.numeric(counts_numeric2$alguna_limit_p),  nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)
pop<--matrix(as.numeric(counts_numeric2$total_pop),  nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)
literate<--matrix(as.numeric(counts_numeric2$literate_p),  nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)
empty<--matrix(as.numeric(counts_numeric2$home_empty_p),  nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)
single<--matrix(as.numeric(counts_numeric2$single_p), nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)
male<--matrix(as.numeric(counts_numeric2$male_p), nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)
unemployed<--matrix(as.numeric(counts_numeric2$unem_p), nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)
home<--matrix(as.numeric(counts_numeric2$home_p), nrow=nrow(counts_numeric2), ncol(counts_numeric2), byrow =TRUE)

counts_sts_multivariate <-