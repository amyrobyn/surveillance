counts_long<-read.csv("C:\\Users\\amykr\\Google Drive\\Kent\\james\\dissertation\\chkv and dengue\\arcgis analysis\\gwr models\\output\\counts.csv")
attach(counts_long)	
ts<- as.ts(counts_long)
summary(counts_long)
str(counts_long)

library(AER)
library(pscl)
library(MASS)

Y <- cbind(dengue)
Y1<-cbind(chikv)
Y2<-cbind(zika)
x<- cbind(anm_ed_index_sum, rainlag1, Avg_rain, negro__a___mulato__afrop, arean3210, serv_cov_index, male_p)
x1<- cbind(anm_ed_index_sum, rainlag1)

# sum stats
summary(Y)
summary(Y1)
summary(Y2)
summary(x)

#poisson model
poisson<-glm(Y1~x, family = "poisson")

summary(poisson)

#check for dispersion
dispersiontest(poisson)

#negative binomial model
negbin<- glm.nb(Y1~x)
summary(negbin)

#poisson hurdle model
hpoisson<-hurdle(Y1 ~ x | x1, link = "logit", dist = "poisson")
summary(hpoisson)

#negative binomial hurdle model
hnegbin <- hurdle(Y1 ~ x | x1, link = "logit", dist = "negbin")
summary(hnegbin)
plot(residuals(hnegbin) ~ fitted(hnegbin))

coef(hnegbin)
logLik(hnegbin)

#zero inflated poisson model
zip<- zeroinfl(Y1~x|x1, link="logit", dist="poisson")
summary(zip)

#zero inflated negative binomial model
zinb<-zeroinfl(Y1~x|x1, link="logit", dist="negbin")
summary(zinb)
plot(residuals(zinb) ~ fitted(zinb))


install.packages("foreign")
install.packages("ggplot2")
install.packages("MASS")

 
# negative binomial distribution regression
id <- factor(codigo_barrio)

summary(m1 <- glm.nb(Y1~ x))
summary(m2 <- glm.nb(Y1~ x1))

anova(m1, m2)

m3 <- glm(Y1~ x, family = "poisson")
X2 <- 2 * (logLik(m1) - logLik(m3))
X2
pchisq(X2, df = 1, lower.tail=FALSE)

(est <- cbind(Estimate = coef(m1), confint(m1)))
exp(est)


# Main version
install.packages("Matrix")

install.packages("lme4")

library(lme4) 

model.glm <- glmer(Y1 ~ 1 + monthtime + x + (monthtime | codigo_barrio), data = POISSON, family = "poisson" (link = "log"))
summary(model.glmer)

plot(Y1~Avg_rain)
model1=lm(Y1~x)
plot(model1)


model2=glm.nb(Y1~x)

plot(model2)
outlierTest(model2)


install.packages("mboost")
library(mboost)
install.packages("gamboostLSS")
library(gamboostLSS)
install.packages("stabs")
library(stabs)
glmboost=glmboost(Y1~x, center = T)

plot(glmboost)
summary(glmboost)

glmboostLSS<- glmboostLSS( Y1 ~ x, NBinomialLSS(mu = NULL, sigma = NULL, stabilization = c("none", "MAD"))) 
plot(glmboostLSS)

install.packages("surveillance")
library(surveillance)
help(surveillance)




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


ts<-read.csv("C:\\Users\\amykr\\Google Drive\\Kent\\james\\dissertation\\chkv and dengue\\arcgis analysis\\gwr models\\output\\counts_numeric.csv", fileEncoding="UCS-2LE",sep=",")
head(ts,n=2)

require(ISOweek)

attach(counts_long)	

ts<- as.ts(counts_long)

fix(counts_long)
summary(counts_long)
format(date, format=" %m %d  %y")
dates <- as.Date(fecha_s, "%m/%d/%y")

sts<-sts(epoch=as.numeric(ts[,1], observed=matrix(ts[,2], epochAsDate=TRUE)


family[, 1] <-as.numeric(as.character( family[, 1] ))
family[, 3] <- as.numeric(as.character( family[, 3] ))
sts<-linelist2sts(counts_long, dateCol="date", aggregate.by="1 month")


data("salmNewport")
salmNewport



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