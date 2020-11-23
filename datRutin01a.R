
# Ch-1 --------------------------------------------------------------------
## dataset 
datTotal <- read.csv("dat/denTot.csv", na.string="#N/A")
head(datTotal)
str(datTotal)

par(mfrow=c(3,1))

## line plot
plot(datTotal$time, datTotal$denguel0, type="l", ylab="Cases", xlab="t")
plot(datTotal$time, datTotal$templ0, type="l", ylab="Temperature", xlab="t")
plot(datTotal$time, datTotal$rainl0, type="l", ylab="Rainfall", xlab="t")

## box plot
boxplot(denguel0~month, data=datTotal, main="Cases", xlab="m", ylab="")
boxplot(templ0~month, data=datTotal, main="Temperature", xlab="m", ylab="")
boxplot(rainl0~month, data=datTotal, main="Rainfall", xlab="m", ylab="")


# Ch-2 --------------------------------------------------------------------
## library
#install.packages("zoo")
library(zoo)
#install.packages("ggplot2")
library(ggplot2)

source("multiplot.R")

dat <- datTotal

dat$YM <- paste(dat$month, dat$year, sep="-")
dat$YM

dat$YM <- as.yearmon(dat$YM, format="%m-%Y")
dat$YM

dat$YM <- as.Date(dat$YM)
dat$YM

p1 <- qplot(YM, denguel0, data=dat, geom=c("point", "line"),
            xlab = "t", 
            ylab = "jumlah kasus") + 
  theme_bw()
p1

p2 <- qplot(YM, templ0, data=dat, geom=c("point", "line"),
            xlab = "t", 
            ylab = "suhu") + 
  theme_bw()
p2

p3 <- qplot(YM, rainl0, data=dat, geom=c("point", "line"),
            xlab = "t", 
            ylab = "curah hujan") + 
  theme_bw()
p3

multiplot(p1, p2, p3, cols=1)


## train-test
datTrain <- subset(dat, year<2011)

# 
lag <- as.integer(c(0:3))
par(mfrow=c(3,1))

# dengue 
short_den <- data.frame(denL0=datTrain$denguel0, 
                        denL1=datTrain$denguel1, 
                        denL2=datTrain$denguel2, 
                        denL3=datTrain$denguel3)
cor(datTrain$denguel0, short_den, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(datTrain$denguel0, short_den , method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of monthly dengue cases")          
title(main="Dengue cases vs. Dengue cases")

# temp
short_temp <- data.frame(tempL0=datTrain$templ0, 
                         tempL1=datTrain$templ1, 
                         tempL2=datTrain$templ2, 
                         tempL3=datTrain$templ3)
cor(datTrain$denguel0, short_temp, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(datTrain$denguel0, short_temp, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of monthly temperature")          
title(main="Dengue cases vs. Temperature")

# rain
short_rain <- data.frame(rainL0=datTrain$rainl0,
                         rainL1=datTrain$rainl1, 
                         rainL2=datTrain$rainl2, 
                         rainL3=datTrain$rainl3)
cor(datTrain$denguel0, short_rain, method="spearman", use="pairwise.complete.obs")
plot(lag, as.numeric(cor(datTrain$denguel0, short_rain, method="spearman", use="pairwise.complete.obs")), ylab="Correlation coefficient", xlab="Lag of monthly rainfall")          
title(main="Dengue cases vs. Rainfall")


## model
library(mgcv)

# association models
# meteorology only model - all variables all lags
mod.fmet <- gam(denguel0 ~ 
                  s(templ0, k=4) + s(templ1, k=4) + s(templ2, k=4) + s(templ3, k=4) 
                + s(rainl0, k=4) + s(rainl1, k=4) + s(rainl2, k=4) + s(rainl3, k=4), 
                data=datTrain,
                family=quasipoisson, 
                na.action=na.exclude)
summary(mod.fmet)

par(mfrow=c(2,4))
plot.gam(mod.fmet, ylim=c(-1.2,1.2), ylab="log(RR)")

# meteorology optimal model
mod.omet <- gam(denguel0 ~ 
                  s(templ3,k=4) + s(rainl2,k=4) + s(rainl3,k=4), 
                data=datTrain,
                family=quasipoisson, 
                na.action=na.exclude)
summary(mod.omet)

par(mfrow=c(1,1))
p <- fitted.values(mod.omet)
plot(datTrain$denguel0, type="l", ylab="Cases", axes=F, xlab="Year")
points(predict(mod.omet, type="response"), type="l", col="red")
axis(1, at=c(6,18,30,42,54,66,78,90,102,114),labels=c(2001:2010))
axis(2, at=c(0,50,100,150,200,250))
#title(main="Model A")

# sqrt(mean((datTrain$denguel0-p)^2,na.rm=T))
# sqrt(mean((datTrain$denguel0-p)^2,na.rm=T))/sqrt(mean((datTrain$denguel0)^2))

# AR lag 2 model
mod.l2 <- gam(denguel0 ~ s(denguel2, k=4), 
              data=datTrain,
              family=quasipoisson, 
              na.action=na.exclude)
summary(mod.l2)

par(mfrow=c(1,1))
p <- fitted.values(mod.l2)
plot(datTrain$denguel0, type="l", ylab="Cases", axes=F, xlab="Year")
points(predict(mod.l2, type="response"), type="l", col="red")
axis(1, at=c(6,18,30,42,54,66,78,90,102,114),labels=c(2001:2010))
axis(2, at=c(0,50,100,150,200,250))

# sqrt(mean((datTrain$denguel0-p)^2,na.rm=T))
# sqrt(mean((datTrain$denguel0-p)^2,na.rm=T))/sqrt(mean((datTrain$denguel0)^2))


# model validation

form <- as.formula("denguel0 ~ s(templ3,k=4) + 
                   s(rainl2,k=4) + s(rainl3,k=4) +
                   s(denguel2,k=4) + s(denguel24,k=4)")

mod.train <- gam(form, family=quasipoisson, na.action=na.exclude, data=datTrain)
summary(mod.train)

p <- fitted.values(mod.train)
# sqrt(mean((datTrain$denguel0-p)^2,na.rm=T))
# sqrt(mean((datTrain$denguel0-p)^2,na.rm=T))/sqrt(mean((datTrain$denguel0)^2))

preddata <- data.frame(templ3=dat$templ3,
                       rainl2=dat$rainl2,
                       rainl3=dat$rainl3,
                       denguel2=dat$denguel2,
                       denguel24=dat$denguel24)
dat$predict <- predict(mod.train, type="response", newdata=as.data.frame(preddata))

p <- dat$predict
train <- subset(dat, year<2011)
pred <- subset(dat, year>2010)

par(mfrow=c(1,1))
plot(dat$time, dat$denguel0, type="l")
points(train$p, type="l", col="red")
points(pred$time, pred$p, type="l", col="blue")
#abline(h=60, col = "gray60")

#for training data
# sqrt(mean((train$denguel0-train$p)^2,na.rm=T))/sqrt(mean((train$denguel0)^2))
#for validation data 2011-2013
# sqrt(mean((pred$denguel0-pred$p)^2,na.rm=T))/sqrt(mean((pred$denguel0)^2))


# residual analysis

resid <- residuals(mod.train)

par(mfrow=c(2,2))
hist(resid, xlab="Residuals", main=" ")
pacf(resid, na.action = na.pass, main=" ")      
#plot.ts(resid, xlab="Month", ylab="Residuals") 
qq.gam(mod.train, main="")
plot(datTrain$denguel0, fitted(mod.train), ylab="Predicted Cases", xlab="Reported Cases", main=" ")
