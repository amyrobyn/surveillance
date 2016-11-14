setwd ("C:/Users/amykr/Google Drive/Kent/james/dissertation/chkv and dengue/arcgis analysis/gwr models/output/surveillance")

#install.packages("animation")
install.packages("surveillance")
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
hhh4(stsObj= counts_sts, control = counts_sts_basicmodel)
plot(counts_sts_fit_basic, type = "season", components = "end", main = "")
confint(counts_sts_fit_basic, parm = "overdisp")
AIC(counts_sts_fit_basic, update(counts_sts_fit_basic, family = "Poisson"))
districts2plot<-which(colSums(observed(counts_sts))>20)
plot(counts_sts_fit_basic, type = "fitted", units = districts2plot, hide0s = TRUE)

#multivariate modoel
#e.g: Sprop <-matrix(1~measlesWeserE<S@map@data$vacc1.2004), nrow = nrow(measlesWeserEMS), ncol(measlesWeserEMS), byrow=TRUE)
#time and spacy varying
Sprop<-counts_numeric2 %>% select(c(54,2,23)) %>% spread(key=ID,value=Avg_rain)
rain<-counts_numeric2 %>% select(c(54,2,23)) %>% spread(key=ID,value=Avg_rain)
rainlag1<-counts_numeric2 %>% select(c(54,2,51)) %>% spread(key=ID,value=rainlag1)

#time varying
temp<-counts_numeric2 %>% select(c(54,2,22)) %>% spread(key=ID,value=temp_anom_median_c)
templag1<-counts_numeric2 %>% select(c(54,2,52)) %>% spread(key=ID,value=templag1)

#fixed
services<-counts_numeric2 %>% select(c(54,2,15)) %>% spread(key=ID,value=services_index)
educ<-counts_numeric2 %>% select(c(54,2,34)) %>% spread(key=ID,value=assist_educ_P)
afro<-counts_numeric2 %>% select(c(54,2,26)) %>% spread(key=ID,value=negro__a___mulato__afrop)
area<-counts_numeric2 %>% select(c(54,2,6)) %>% spread(key=ID,value=arean3210)
limit<-counts_numeric2 %>% select(c(54,2,12)) %>% spread(key=ID,value=alguna_limit_p)
pop<-counts_numeric2 %>% select(c(54,2,10)) %>% spread(key=ID,value=total_pop)
literate<-counts_numeric2 %>% select(c(54,2,13)) %>% spread(key=ID,value=literate_p)
empty<-counts_numeric2 %>% select(c(54,2,18)) %>% spread(key=ID,value=home_empty_p)
single<-counts_numeric2 %>% select(c(54,2,29)) %>% spread(key=ID,value=single_p)
male<-counts_numeric2 %>% select(c(54,2,19)) %>% spread(key=ID,value=male_p)
unemployed<-counts_numeric2 %>% select(c(54,2,27)) %>% spread(key=ID,value=unem_p)
home<-counts_numeric2 %>% select(c(54,2,28)) %>% spread(key=ID,value=home_p)


Soptions <- c("unchanged", "Soffset", "Scover")
SmodelGrid <- expand.grid(end=Soptions, ar = Soptions)
row.names(SmodelGrid) <- do.call("paste", c(SmodelGrid, list(sep ="|")))

counts_sts_multivariate <-apply(X=SmodelGrid, MARGIN = 1, FUN=function(options){ 
  updatecomp <-function(comp, option) switch(option, 
  "unchanged" = list(),
  "Soffset"=list(offset = comp$offset * Sprop), 
"Scovar"=list(f=update(comp$f,~.+log(Sprop))))
update(counts_sts_fit_basic, 
       end = updatecomp(counts_sts_fit_basic$control$end, options[1]),
       ar = updatecomp(counts_sts_fit_basic$control$ar, options[2]),
       data = list(Sprop = Sprop))
  })