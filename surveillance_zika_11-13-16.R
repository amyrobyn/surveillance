setwd ("C:/Users/amykr/Google Drive/Kent/james/dissertation/chkv and dengue/arcgis analysis/gwr models/output/surveillance/zika")

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
barrios$barrios_order <- nbOrder(poly2adjmat(barrios), maxlag=10)

counts_numeric<-read.csv("./counts1.csv") %>% filter(codigo_barrio!=2300)
counts_numeric$idx<-as.Date(counts_numeric$date.1)
jpeg("counts_numeric$zika.jpg")
plot(counts_numeric$zika)
dev.off()

ident<-data.frame(cbind(ID=0:337,ID_Barrio=barrios$ID_BARRIO))
counts_numeric2<-inner_join(counts_numeric,ident,by=c("codigo_barrio"="ID_Barrio"))
counts1<-counts_numeric2 %>% select(c(54,2,30)) %>% spread(key=ID,value=zika)

population<-counts_numeric2 %>% select(c(54,2,10)) %>% spread(key=ID,value=total_pop)
population<-data.matrix(population, rownames.force = NA)
population<-population/2369829
#population cali = 2369829 in 2015 http://www.cali.gov.co/publicaciones/poblacion_de_cali_aumenta_anualmente_en_habitantes_pub
summary(population)
zikacounts_sts<-sts(epoch=counts1$date_numeric,observed= counts1[,2:ncol(counts1)],epochAsDate=FALSE, freq =12, start=c(2014, 10), map=barrios, neighbourhood = barrios$barrios_order, population=population[,2:ncol(population)])


###basic plots###
jpeg("zika_time.jpg")
plot(zikacounts_sts, type = observed~time)
dev.off()

jpeg("zika_time_map.jpg")
plot(zikacounts_sts, type = observed~unit)
dev.off()
     #, population=as.Numeric(population[,2:ncol(population)]), labels=list(font=2), colorkey = list(space ="right"), sp.layout=layout.scalebar(counts_numericsts@map, corner = c(0.05, 0.05), scale = 50, labels = c("0", "50 km"), height = 0.03))
 

jpeg("zika_units.jpg")
plot(zikacounts_sts, units = which(colSums(observed(zikacounts_sts))>300))
dev.off()

##animation###
animation::saveHTML(animate(zikacounts_sts, tps=1:19, total.args = list()), title ="Evolution of zika outbreak in Cali, Colombia, 2014-2016", ani.width = 500, ani.height = 600)

#format covariates
#time and spacy varying
rain<-as.matrix(counts_numeric2 %>% select(c(54,2,23)) %>% spread(key=ID,value=Avg_rain))[,-1]
rainlag1<-as.matrix(counts_numeric2 %>% select(c(54,2,51)) %>% spread(key=ID,value=rainlag1))[,-1]
rain_sts<-sts(epoch=counts1$date_numeric,observed= rain[,2:ncol(rain)],epochAsDate=FALSE, freq =12, start=c(2014, 10), map=barrios)
stsplot_space(rain_sts)
#animation::saveHTML(animate(rain_sts, tps=1:19, total.args = list()), title ="Rainfall in Cali, Colombia, 2014-2016", ani.width = 500, ani.height = 600)

#time varying
temp<-as.matrix(counts_numeric2 %>% select(c(54,2,22)) %>% spread(key=ID,value=temp_anom_median_c))[,-1]
templag1<-as.matrix(counts_numeric2 %>% select(c(54,2,52)) %>% spread(key=ID,value=templag1))[,-1]
temp_sts<-sts(epoch=counts1$date_numeric,observed= temp[,2:ncol(temp)],epochAsDate=FALSE, freq =12, start=c(2014, 10), map=barrios)
stsplot_time(temp_sts)

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

##basic models##
zika_basicmodel<-list(end = list(f=addSeason2formula(~1+t, period = zikacounts_sts@freq)), ar = list(f=~1), ne =list(f=~1, weights = neighbourhood(zikacounts_sts)==1), family = "NegBin1",  data = list(rain = rain, temp=temp, afro=afro, educ=educ, empty=empty, home=home, limit=limit, literate=literate, male=male, area=area, single=single, unemployed=unemployed) ) 
zikaFit_basic<-hhh4(stsObj= zikacounts_sts, control =zika_basicmodel)
summary(zika_basicmodel, idx2Exp = TRUE, amplitudeShift = TRUE, maxEV=+TRUE)
hhh4(stsObj= zikacounts_sts, control = zika_basicmodel)

jpeg("zika_seasonaleffect.jpg")
plot(zikaFit_basic, type = "season", components = "end", main = "")
dev.off()

confint(zikaFit_basic, parm = "overdisp")
AIC(zikaFit_basic, update(zikaFit_basic, family = "Poisson"))
districts2plot<-which(colSums(observed(zikacounts_sts))>290)

jpeg("zikaFit_basic_fitted.jpg")
plot(zikaFit_basic, type = "fitted", units = districts2plot, hide0s = TRUE)
dev.off()

jpeg("zikaFit_basic_mean.jpg")
plotHHH4_maps(zikaFit_basic, which =c("mean"), prop=FALSE)
dev.off()

jpeg("zikaFit_basic.jpg")
plotHHH4_maps(zikaFit_basic_endemic, which =c("endemic"), prop=FALSE)
dev.off()

jpeg("zikaFit_basic.jpg")
plotHHH4_maps(zikaFit_basic_epiown, which =c("epi.own"), prop=FALSE)
dev.off()

jpeg("zikaFit_basic.jpg")
plotHHH4_maps(zikaFit_basic_neighbors, which =c("epi.neighbours"), prop=FALSE)
dev.off()

#multivariate modoel


#e.g: Sprop <-matrix(1~measlesWeserE<S@map@data$vacc1.2004), nrow = nrow(measlesWeserEMS), ncol(measlesWeserEMS), byrow=TRUE)
#Sprop<-counts_numeric2 %>% select(c(54,2,23)) %>% spread(key=ID,value=Avg_rain)
Sprop<-as.matrix(counts_numeric2 %>% select(c(54,2,23)) %>% spread(key=ID,value=Avg_rain))[,-1]

Soptions <- c("unchanged", "Soffset", "Scovar")
SmodelGrid <- expand.grid(end=Soptions, ar = Soptions)
row.names(SmodelGrid) <- do.call("paste", c(SmodelGrid, list(sep ="|")))

#update the basic model with an offset- here they used teh vaccinated population. For us, we could use incidence as this woudl estimate those susceptible...
#they try combinations of offset and covariate here
zikaFit_offset <-apply(X=SmodelGrid, MARGIN = 1, FUN=function(options){ 
  updatecomp <-function(comp, option) switch(option, 
  "unchanged" = list(),
  "Soffset"=list(offset = comp$offset * home), 
  "Scovar"=list(f=update(comp$f,~.+ rain + temp + afro + educ + empty + home + limit + literate + male + area  + single + unemployed)))
update(zikaFit_basic, 
       end = updatecomp(zikaFit_basic$control$end, options[1]),
       ar = updatecomp(zikaFit_basic$control$ar, options[2]),
       data = list(Sprop = Sprop))
  })
#compare the AIC from each optpion for offscet/covariate
aics<-do.call(AIC, lapply(names(zikaFit_offset), as.name), envir = as.environment(zikaFit_offset))
zikaFit_offset<-zikaFit_offset[["Scovar|unchanged"]]
coef(zikaFit_offset, se = TRUE)["end.log(Sprop)",]

summary(zikaFit_offset)

#add spatial intercation weighted according to population fraction
zikaFit_nepop <- update(zikaFit_offset, ne = list(f= ~(pop)), data=list(pop=population(zikacounts_sts)))
#error happens here! 
#Error in nlminb(start, negll, gradient = negsc, hessian = fi, ..., scale = scale,  : 
#NA/NaN gradient evaluation

#weighted To account for long-range transmission of cases
zikaFit_powerlaw <- update(zikaFit_nepop, ne = list(weights = W_powerlaw(maxlag = 5)))
#another type of weight To account for long-range transmission of cases (a second-order model)
zikaFit_np2 <- update(zikaFit_nepop, ne = list(weights = W_np(maxlag = 2)))
#plot different weight types
library("lattice")

jpeg("zikaFit_powerlaw_newweights")
plot(zikaFit_powerlaw, type = "neweights", plotter = stripplot, panel = function (...) {panel.stripplot(...); panel.average(...)}, jitter.data = TRUE, xlab = expression(o[ji]), ylab = expression(w[ji]))
dev.off()
#compare aic for different measure types
AIC(zikaFit_nepop, zikaFit_powerlaw, zikaFit_np2)

#Random intercepts models
zikaFit_ri <- update(zikaFit_powerlaw, end = list(f = update(formula(zikaFit_powerlaw)$end, ~. + ri() - 1)), ar = list(f = update(formula(zikaFit_powerlaw)$ar, ~. + ri()- 1)), ne = list(f = update(formula(zikaFit_powerlaw)$ne, ~. + ri() - 1))) 

summary(zikaFit_ri, amplitudeShift = TRUE, maxEV = TRUE)
hhh4(stsObj = object$stsObj, control = control)

#district specific intercepts
head(ranef(zikaFit_ri, tomatrix = TRUE), n = 3)


#map the random intercepts
jpeg("zikaFit_ri")
for (comp in c("ar", "ne", "end")) {print(plot(zikaFit_ri, type = "ri", component = comp, col.regions = rev(cm.colors(100)), labels = list(cex = 0.6), at = seq(-1.6, 1.6, length.out = 15)))}
dev.off()

jpeg("zikaFit_ri_fitted")
plot(zikaFit_ri, type = "fitted", units = districts2plot, hide0s = TRUE)
dev.off()

#predictive model assessment
tp <- c(65, 77)
models2compare <- paste0("zikaFit_", c("basic", "powerlaw", "ri"))
zikaPreds1 <- lapply(mget(models2compare), oneStepAhead, tp = tp, type = "final")

