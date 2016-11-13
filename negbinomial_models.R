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


