library(xts)
library(maptools)
library(surveillance)
library(dplyr)
library(tidyr)
library(animation)
library(spgwr)
library(ggplot2)
library(maptools)
library(spgwr)

setwd ("C:/Users/amykr/Google Drive/Kent/james/dissertation/chkv and dengue/arcgis analysis/gwr models/output/surveillance/dengue")
counts_numeric<-read.csv("./dengue.csv") %>% filter(codigo_barrio!=2300)
attach(counts_numeric)

#local fixed "serv_cov_index l1ztemp Avg_rain rainlag1 estrato_mon3210 home_p male_p zafro"

#model1 <- glm(counts_numeric$dengue ~  counts_numeric$Avg_rain +counts_numeric$home_empty_p*counts_numeric$codigo_barrio, family = quasipoisson)
model1 <- glm(counts_numeric$dengue ~  counts_numeric$anm_ed_index_sum + counts_numeric$rainlag1 + counts_numeric$Avg_rain + counts_numeric$negro__a___mulato__afrop + counts_numeric$arean3210 + counts_numeric$serv_cov_index + counts_numeric$anm_cobertura_energi + counts_numeric$male_p + counts_numeric$single_p + counts_numeric$anm_cobertura_alcant , family = quasipoisson)

summary(model1)
plot(model1, which =3)



resids<-residuals(model1)
colours <- c("dark blue", "blue", "red", "dark red") 
#here it is assumed that your eastings and northings coordinates are stored in columns called x and y in your dataframe
map.resids <- SpatialPointsDataFrame(data=data.frame(resids), coords=cbind(POINT_X, POINT_Y)) 
#for speed we are just going to use the quick sp plot function, but you could alternatively store your residuals back in your LondonWards dataframe and plot using geom_point in ggplot2
spplot(map.resids, cuts=quantile(resids), col.regions=colours, cex=1) 

GWRbandwidth <- gwr.sel(counts_numeric$dengue ~  counts_numeric$Avg_rain +counts_numeric$home_empty_p*counts_numeric$codigo_barrio, data=counts_numeric, coords=cbind(POINT_X, POINT_Y),adapt=T) 

#run the gwr model
#gwr.model = gwr(dengue ~ anm_ed_index_sum+rainlag1gweight , data=counts_numeric, coords=cbind(POINT_X, POINT_Y), adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
??gwr.model
gwr.model = gwr(dengue ~ anm_ed_index_sum, data=counts_numeric, adapt=bw)

gwr.model = gwr(dengue ~ anm_ed_index_sum+rainlag1gweight+ Avg_rain + negro__a___mulato__afrop + arean3210 + serv_cov_index + anm_cobertura_energi + male_p + single_p + anm_cobertura_alcant, data=counts_numeric, family = "quasipoisson", gweight = gwr.Gauss, verbose = TRUE, coords=cbind(POINT_X, POINT_Y), adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
#print the results of the model
gwr.model

results<-as.data.frame(gwr.model$SDF)
head(results)
Call: gwr.model = gwr(dengue ~  anm_ed_index_sum + rainlag1 + Avg_rain + negro__a___mulato__afrop + arean3210 + serv_cov_index + anm_cobertura_energi + male_p + single_p + anm_cobertura_alcant, data=counts_numeric, family = "quasipoisson", gweight = gwr.Gauss, verbose = TRUE, coords=cbind(POINT_X, POINT_Y), adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 


#attach coefficients to original dataframe
LondonWards$coefUnauthAbsenceSchools11<-results$UnauthAbsenceSchools11
LondonWards$coefPctWithNoQual11<-results$PctWithNoQual11
LondonWards$coefCarsPerHH2011<-results$CarsPerHH2011


#read in the shapefile using the maptools function readShapePoly
boroughs <- readShapePoly("london_boroughs.shp")
#fortify for use in ggpplot2
boroughoutline <- fortify(boroughs, region="name")


#now plot the various GWR coefficients                       
gwr.point1<-ggplot(LondonWards, aes(x=x,y=y))+geom_point(aes(colour=LondonWards$coefUnauthAbsenceSchools11))+scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, space = "rgb", na.value = "grey50", guide = "colourbar", guide_legend(title="Coefs"))
gwr.point1+geom_path(data=boroughoutline,aes(long, lat, group=id), colour="grey")+coord_equal()

