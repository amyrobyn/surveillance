data(measlesWeserEms)

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
population<-population/2369829
#population cali = 2369829 in 2015 http://www.cali.gov.co/publicaciones/poblacion_de_cali_aumenta_anualmente_en_habitantes_pub
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
dengueFit_basic<-hhh4(stsObj= counts_sts, control =counts_sts_basicmodel)
summary(counts_sts_basicmodel, idx2Exp = TRUE, amplitudeShift = TRUE, maxEV=+TRUE)
hhh4(stsObj= counts_sts, control = counts_sts_basicmodel)
plot(counts_sts_fit_basic, type = "season", components = "end", main = "")
confint(dengueFit_basic, parm = "overdisp")
AIC(dengueFit_basic, update(dengueFit_basic, family = "Poisson"))
districts2plot<-which(colSums(observed(counts_sts))>20)
plot(dengueFit_basic, type = "fitted", units = districts2plot, hide0s = TRUE)

#multivariate modoel

#other covariates I want to import
#time and spacy varying
rain<-as.matrix(counts_numeric2 %>% select(c(54,2,23)) %>% spread(key=ID,value=Avg_rain))[,-1]
rainlag1<-as.matrix(counts_numeric2 %>% select(c(54,2,51)) %>% spread(key=ID,value=rainlag1))[,-1]

#time varying
temp<-as.matrix(counts_numeric2 %>% select(c(54,2,22)) %>% spread(key=ID,value=temp_anom_median_c))[,-1]
templag1<-as.matrix(counts_numeric2 %>% select(c(54,2,52)) %>% spread(key=ID,value=templag1))[,-1]


#fixed
services<-as.matrix(counts_numeric2 %>% select(c(54,2,15)) %>% spread(key=ID,value=services_index))[,-1]
educ<-as.matrix(counts_numeric2 %>% select(c(54,2,11)) %>% spread(key=ID,value=assist_educ_P))[,-1]
afro<-as.matrix(counts_numeric2 %>% select(c(54,2,26)) %>% spread(key=ID,value=negro__a___mulato__afrop))[,-1]
area<-as.matrix(counts_numeric2 %>% select(c(54,2,6)) %>% spread(key=ID,value=arean3210))[,-1]
limit<-as.matrix(counts_numeric2 %>% select(c(54,2,12)) %>% spread(key=ID,value=alguna_limit_p))[,-1]
pop<-as.matrix(counts_numeric2 %>% select(c(54,2,10)) %>% spread(key=ID,value=total_pop))[,-1]
literate<-as.matrix(counts_numeric2 %>% select(c(54,2,13)) %>% spread(key=ID,value=literate_p))[,-1]
empty<-as.matrix(counts_numeric2 %>% select(c(54,2,18)) %>% spread(key=ID,value=home_empty_p))[,-1]
single<-as.matrix(counts_numeric2 %>% select(c(54,2,29)) %>% spread(key=ID,value=single_p))[,-1]
male<-as.matrix(counts_numeric2 %>% select(c(54,2,19)) %>% spread(key=ID,value=male_p))[,-1]
unemployed<-as.matrix(counts_numeric2 %>% select(c(54,2,27)) %>% spread(key=ID,value=unem_p))[,-1]
home<-as.matrix(counts_numeric2 %>% select(c(54,2,28)) %>% spread(key=ID,value=home_p))[,-1]


#e.g: Sprop <-matrix(1~measlesWeserE<S@map@data$vacc1.2004), nrow = nrow(measlesWeserEMS), ncol(measlesWeserEMS), byrow=TRUE)
#Sprop<-counts_numeric2 %>% select(c(54,2,23)) %>% spread(key=ID,value=Avg_rain)
Sprop<-as.matrix(counts_numeric2 %>% select(c(54,2,23)) %>% spread(key=ID,value=Avg_rain))[,-1]

Soptions <- c("unchanged", "Soffset", "Scovar")
SmodelGrid <- expand.grid(end=Soptions, ar = Soptions)
row.names(SmodelGrid) <- do.call("paste", c(SmodelGrid, list(sep ="|")))

#update the basic model with an offset- here they used teh vaccinated population. For us, we could use incidence as this woudl estimate those susceptible...
#they try combinations of offset and covariate here
dengueFit_offset <-apply(X=SmodelGrid, MARGIN = 1, FUN=function(options){ 
  updatecomp <-function(comp, option) switch(option, 
  "unchanged" = list(),
  "Soffset"=list(offset = comp$offset * Sprop), 
  "Scovar"=list(f=update(comp$f,~.+log(Sprop))))
update(dengueFit_basic, 
       end = updatecomp(dengueFit_basic$control$end, options[1]),
       ar = updatecomp(dengueFit_basic$control$ar, options[2]),
       data = list(Sprop = Sprop))
  })
#compare the AIC from each optpion for offscet/covariate
aics<-do.call(AIC, lapply(names(counts_sts_multivariate), as.name), envir = as.environment(counts_sts_multivariate))
dengueFit_offset<-dengueFit_offset[["Scovar|unchanged"]]
coef(dengueFit_offset, se = TRUE)["end.log(Sprop)",]

#add spatial intercation weighted according to population fraction
dengueFit_nepop <- update(dengueFit_offset, ne = list(f= ~log(pop)), data=list(pop=population(counts_sts)))
#error happens here! 
#Error in nlminb(start, negll, gradient = negsc, hessian = fi, ..., scale = scale,  : 
#NA/NaN gradient evaluation

#weighted To account for long-range transmission of cases
dengueFit_powerlaw <- update(dengueFit_nepop, ne = list(weights = W_powerlaw(maxlag = 5)))
#another type of weight To account for long-range transmission of cases (a second-order model)
dengueFit_np2 <- update(dengueFit_nepop, ne = list(weights = W_np(maxlag = 2)))
#plot different weight types
library("lattice")
plot(dengueFit_powerlaw, type = "neweights", plotter = stripplot, panel = function (...) {panel.stripplot(...); panel.average(...)}, jitter.data = TRUE, xlab = expression(o[ji]), ylab = expression(w[ji]))
#compare aic for different measure types
AIC(dengueFit_nepop, dengueFit_powerlaw, dengueFit_np2)

#Random intercepts models
dengueFit_ri <- update(dengueFit_powerlaw, end = list(f = update(formula(dengueFit_powerlaw)$end, ~. + ri(type = "car") - 1)), ar = list(f = update(formula(dengueFit_powerlaw)$ar, ~. + ri(type = "car")- 1)), ne = list(f = update(formula(dengueFit_powerlaw)$ne, ~. + ri(type = "car") - 1))) 

summary(dengueFit_ri, amplitudeShift = TRUE, maxEV = TRUE)
hhh4(stsObj = object$stsObj, control = control)

#district specific intercepts
head(ranef(dengueFit_ri, tomatrix = TRUE), n = 3)


#map the random intercepts
for (comp in c("ar", "ne", "end")) {print(plot(dengueFit_ri, type = "ri", component = comp, col.regions = rev(cm.colors(100)), labels = list(cex = 0.6), at = seq(-1.6, 1.6, length.out = 15)))}

plot(dengueFit_ri, type = "fitted", units = districts2plot, hide0s = TRUE)


#predictive model assessment
tp <- c(65, 77)
models2compare <- paste0("dengueFit_", c("basic", "powerlaw", "ri"))
denguePreds1 <- lapply(mget(models2compare), oneStepAhead, tp = tp, type = "final")
