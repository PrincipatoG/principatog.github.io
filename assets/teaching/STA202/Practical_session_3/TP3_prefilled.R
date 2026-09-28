rm(list=objects())

# Packages
library(zoo)
library(magrittr)
library(xts)
library(forecast)
library(mgcv)
library(tidyverse)

##############################################################################
# EXERCICE 1 — Lissage sur séries simulées
##############################################################################

# 1. Simulation des séries
set.seed(10)
n <- 100
t <- c(1:n)
s <- 
eps <-
X1 <- 
X2 <- 
X3 <- 

par(mfrow=c(1,1))
plot(X1,type='l',ylim=range(X1,X2,X3))
lines(X2,lty=2)
lines(X3,lty=1, col='red')

acf(X2, lag.max=50)
acf(X3, lag.max=50)

# 2. Implémentation des lissages
# 2.a) Lissage simple
expSmooth <- function(x,alpha){

}
alpha <- 0.05
X1.smooth <- expSmooth(X1,alpha)
plot(X1,type='l')
lines(X1.smooth,col='red', lwd=2)

alpha <- 0.01
X1.smooth <- expSmooth(X1,alpha)

mean((tail(X1,n-1)-head(X1.smooth,n-1))^2)
lines(head(X1.smooth,n-1),col='red', lwd=2)

alpha <- 0.99
X1.smooth <- expSmooth(X1,alpha)

mean((tail(X1,n-1)-head(X1.smooth,n-1))^2)
lines(head(X1.smooth,n-1),col='blue', lwd=2)

alpha <- seq(10^-5,0.95,length=100)
forecast <- lapply(alpha,expSmooth,x=X1)

erreur <- unlist(
  lapply(forecast,
         function(x){mean((tail(X1,n-1)-head(x,n-1))^2)})
)

plot(alpha,erreur,type='l')

X1.smooth <- expSmooth(X1,alpha[which.min(erreur)])

plot(X1,type='l')
lines(X1.smooth,col='red')

alpha <- seq(0.05,0.95,length=100)
forecast   <- lapply(alpha,expSmooth,x=X2)
erreur     <- unlist(lapply(forecast,function(x){mean((tail(X2,n-1)-head(x,n-1))^2)}))
erreur_app <- unlist(lapply(forecast,function(x){mean(X2-x)^2}))

plot(alpha,erreur,type='l', ylim=range(erreur,erreur_app))
lines(alpha, erreur_app, col='red')

X2.smooth <- expSmooth(X2,alpha=0.9)

plot(X2,type='l')
lines(X2.smooth,col='red')

X2.smooth <- expSmooth(X2,alpha[which.min(erreur)])
plot(X2,type='l')
lines(X2.smooth,col='red')

alpha <- seq(0.05,0.95,length=100)
forecast <- lapply(alpha,expSmooth,x=X3)
erreur <- unlist(lapply(forecast,function(x){mean((tail(X3,n-1)-head(x,n-1))^2)}))

plot(alpha,erreur,type='l')

X3.smooth <- expSmooth(X3,alpha[which.min(erreur)])

plot(X3,type='l')
lines(X3.smooth,col='red')

alpha <- seq(0.05,0.95,length=100)
forecast <- lapply(alpha,expSmooth,x=X3)

alpha.opt <- NULL
for(h in c(1:30)){
  erreur <- unlist(lapply(forecast,function(x){mean((tail(X3,n-h)-head(x,n-h))^2)}))
  alpha.opt <- c(alpha.opt, alpha[which.min(erreur)])
}
plot(alpha.opt, type='l')

erreur <- unlist(lapply(forecast,function(x){mean((tail(X3,n-h)-head(x,n-h))^2)}))
plot(alpha,erreur,type='l')

# 2.a) Lissage double
DoubleExpSmooth <- function(x,alpha){

}

alpha <- seq(10^-5,0.95,length=100)
forecast <- lapply(alpha,DoubleExpSmooth,x=X1)
erreur <- unlist(
  lapply(forecast,
         function(x){mean((tail(X1,n-1)-head(x$smooth,n-1))^2)}))

plot(alpha,erreur,type='l')

X1.smooth <- DoubleExpSmooth(X1,alpha[which.min(erreur)])

plot(X1,type='l')
lines(X1.smooth$smooth,col='red')

plot(X1.smooth$l,type='l',ylim=range(X1.smooth$l,X1.smooth$b),col='blue')
lines(X1.smooth$b,col='red')

alpha <- seq(10^-5,0.95,length=100)
forecast <- lapply(alpha,DoubleExpSmooth,x=X2)
erreur <- unlist(
  lapply(forecast,
         function(x){mean((tail(X2,n-1)-head(x$smooth,n-1))^2)}))
plot(alpha,erreur,type='l')

X2.smooth <- DoubleExpSmooth(X2,alpha[which.min(erreur)])
X2.smooth <- DoubleExpSmooth(X2,alpha=0.2)

plot(X2,type='l')
lines(X2.smooth$smooth,col='red')

plot(X2.smooth$l,type='l',ylim=range(X2.smooth$l,X2.smooth$b),col='blue')
lines(X2.smooth$b,col='red', type='l')
abline(h=0.2)

X2.smooth01 <- DoubleExpSmooth(X2,alpha=0.1)
X2.smooth02 <- DoubleExpSmooth(X2,alpha=0.2)

plot(X2.smooth01$b,col='red', type='l')
lines(X2.smooth02$b, col='purple')
abline(h=0.2)

# 3. Prévision à horizon h
predict.expSmooth <- function(Xsmooth,inst,horizon,smooth.type="double"){
  if(smooth.type=="simple")
  {

  }
  
  if(smooth.type=="double")
  {

  }
  return(prev)
}

alpha <- 0.8
X.d.exp.mooHW <- DoubleExpSmooth(X2,alpha)
prevHW <- predict.expSmooth(X.d.exp.mooHW,
                            inst=50,horizon=30, smooth.type="double")

plot(X2,pch=20,ylim=range(X2,prevHW))
lines(prevHW,col='red',lwd=2)
abline(v=50,lty='dashed')

X.d.exp.mooHW$b[50]

# Seasonal double HW
SeasonalDoubleExpSmooth=function(x,alpha,beta,delta,T){
  
}

X2.smooth <- DoubleExpSmooth(X2,alpha[which.min(erreur)])

alpha <- seq(10^-5,0.95,length=100)
forecast <- lapply(alpha,DoubleExpSmooth,x=X3)
erreur <- unlist(
  lapply(forecast,
         function(x){mean((tail(X3,n-1)-head(x$smooth,n-1))^2)}))

plot(alpha,erreur,type='l')

X3.smooth <- DoubleExpSmooth(X3,alpha[which.min(erreur)])

plot(X3.smooth$smooth)

plot(X3-X3.smooth$smooth, type='l')

par(mfrow=c(1,2))
acf(X3)
acf(X3-X3.smooth$smooth)

alpha <- 0.2
beta  <- 0.4
delta <- 0.6
T <- 10
X.seas.exp.mooHW <- SeasonalDoubleExpSmooth(X3,alpha,beta,delta,T)

par(mfrow=c(1,1))
plot(X3, type='l')
lines(X.seas.exp.mooHW$smooth, col='red')

names(X.seas.exp.mooHW)

par(mfrow=c(3,1))
plot(X.seas.exp.mooHW$l, type='l')
plot(X.seas.exp.mooHW$b, type='l')
plot(X.seas.exp.mooHW$s-mean(X.seas.exp.mooHW$s), type='l')
lines(s, col='red')

# 4. Avec le package forecast
n <- 100
t <- c(1:n)
s <- cos(2*pi*t/10)
eps <- rnorm(n,0,1)
X1  <- eps
X2  <- t/5+eps
X3  <- t/5+s+eps

?ets
ets1 <- ets(y=X1, model='ANN')
ets1$initstate
plot(ets1)

ets1 <- ets(y=X1, model='ZZZ')
forecast(ets1, h=10)

par(mfrow=c(1,1))
plot(X1, type='l')
lines(ets1$fitted, col='red')

ets2 <- ets(y=X2, model='AAN')
ets2 <- ets(y=X2, model='ZZZ')
ets2

par(mfrow=c(1,1))
plot(X2, type='l')
lines(ets2$fitted, col='red')

plot(ets2)
par(mfrow=c(2,1))
plot(X2.smooth02$l, type='l')
plot(X2.smooth02$b, type='l')

X3 <- ts(X3, frequency=10)
ets3 <-  ets(y=X3, model='AAA')
ets3 <-  ets(y=X3, model='ZZZ')

plot(ets3)

plot(X3, type='l')
lines(ets3$fitted, col='red')
lines(X.seas.exp.mooHW$smooth, col='orange')

##############################################################################
# Exercice 2 — Sur des données réelles
##############################################################################

# 1. Charger les données
data("EuStockMarkets")
class(EuStockMarkets)
dim(EuStockMarkets)
summary(EuStockMarkets)
EuStockMarkets[1:3,]
?EuStockMarkets

plot(EuStockMarkets)
plot(EuStockMarkets[,3])

summary(time(EuStockMarkets))
frequency(EuStockMarkets)

plot(window(EuStockMarkets,1998))
plot(window(EuStockMarkets,1991,1997))

length(as.numeric(EuStockMarkets[,3]))
which(time(EuStockMarkets)>=1998)

# 1bis. Décomposition en train/test
cac_0 <- as.numeric(EuStockMarkets[1:1691,3])

cac_1 <- as.numeric(EuStockMarkets[1692:nrow(EuStockMarkets),3])

time  <- c(1:nrow(EuStockMarkets))

acf(cac_0)
acf(diff(cac_0))
pacf(diff(cac_0))

par(mfrow=c(1,1))
n0<-length(cac_0)
n1<-length(cac_1)

plot(time,c(cac_0,cac_1),type='l')
lines(time[1:n0],cac_0,type='l')
lines(time[(n0+1):(n0+n1)],cac_1,col='red')

# 2. Prévision par lissage exponentiel
# 2.a) Lissage simple
ets1 <- ets(cac_0, model="ANN")
ets2 <- ets(cac_0, model="AAN")
plot(ets2)

ets1.forecast <- forecast(ets1, h=length(cac_1))
ets2.forecast <- forecast(ets2, h=length(cac_1))

plot(time,c(cac_0,cac_1),type='l')
lines(time[1:n0],cac_0,type='l')
lines(time[(n0+1):(n0+n1)],cac_1,col='red')
lines(time[(n0+1):(n0+n1)],  ets1.forecast$mean, col='blue')
lines(time[(n0+1):(n0+n1)],  ets2.forecast$mean, col='purple')

# 2.b) Holt winters
hw1 <- HoltWinters(cac_0, gamma=F)
hw1.forecast <- predict(hw1, n.ahead = length(cac_1))

plot(time,c(cac_0,cac_1),type='l')
lines(time[1:n0],cac_0,type='l')
lines(time[(n0+1):(n0+n1)],cac_1,col='red')
lines(time[(n0+1):(n0+n1)],  hw1.forecast, col='blue')
lines(time[(n0+1):(n0+n1)],  ets2.forecast$mean, col='purple')

# 2.c) Code maison de Yannig
time <- c(1:nrow(EuStockMarkets))
par(mfrow=c(1,1))
n0 <- length(cac_0)
n1 <- length(cac_1)

plot(time,c(cac_0,cac_1),type='l')
lines(time[1:n0],cac_0,type='l')
lines(time[(n0+1):(n0+n1)],cac_1,col='red')

alpha <- seq(0.05,0.95,length=100)
forecast <- lapply(alpha,expSmooth,x=cac_0)
erreur   <- unlist(
  lapply(forecast,
         function(x){mean((tail(cac_0,n-1)-head(x,n-1))^2)})
)

plot(alpha,erreur,type='l')

cac.smooth.simple<-expSmooth(cac_0,alpha=0.95)

alpha <- seq(0.05,0.95,length=100)
forecast <- lapply(alpha,DoubleExpSmooth,x=cac_0)
erreur <- unlist(
  lapply(forecast,
         function(x){mean((tail(cac_0,n-1)-head(x$smooth,n-1))^2)})
)

par(mfrow=c(1,1))
plot(alpha,erreur,type='l')
cac.smooth.double<-DoubleExpSmooth(cac_0,alpha=0.75)

time <- c(1:nrow(EuStockMarkets))
n0 <- length(cac_0)
n1 <- length(cac_1)

plot(time,c(cac_0,cac_1),type='l')
lines(time[1:n0],cac_0,type='l')
lines(time[(n0+1):(n0+n1)],cac_1,col='red')

cac.smooth.simple.forecast <- predict.expSmooth(cac.smooth.simple,n0,n1,smooth.type="simple")
cac.smooth.double.forecast <- predict.expSmooth(cac.smooth.double,n0,n1,smooth.type="double")

lines(cac.smooth.simple.forecast,col='blue')
lines(cac.smooth.double.forecast,col='purple')

plot(cac.smooth.double$b, type='l')

# 3. Méthodes du cours 2 (GAM)
Data0 <- data.frame(cac=cac_0, time=time[1:n0])
Data1 <- data.frame(cac=cac_1, time=time[(n0+1):(n0+n1)])

g <- gam(cac ~ s(time, k=5), data=Data0)
summary(g)
plot(g)

g.forecast <- predict(g, newdata=Data1)

time <- c(1:nrow(EuStockMarkets))
n0 <- length(cac_0)
n1 <- length(cac_1)

plot(time,c(cac_0,cac_1),type='l')
lines(time[1:n0],cac_0,type='l')
lines(time[(n0+1):(n0+n1)],cac_1,col='red')

cac.smooth.simple.forecast<-predict.expSmooth(cac.smooth.simple,n0,n1,smooth.type="simple")
cac.smooth.double.forecast<-predict.expSmooth(cac.smooth.double,n0,n1,smooth.type="double")

lines(cac.smooth.simple.forecast,col='blue')
lines(cac.smooth.double.forecast,col='purple')

Data <- Data0
Nblock <- 5
borne_block <- seq(1, nrow(Data), length=Nblock+1)%>%floor
block_list  <- list()
l <- length(borne_block)
for(i in c(2:(l-1))){
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]] <- c(borne_block[l-1]:(borne_block[l]))

block_res<-function(equation, block){
  g <- gam(as.formula(equation), data=Data[-block,])
  forecast<-predict(g, newdata=Data[block,])
  return(Data[block,]$cac-forecast)
} 

equation = "cac ~ s(time, k=3)"
Block_residuals<-lapply(block_list, block_res, equation=equation) %>% unlist
mean(Block_residuals^2) %>% sqrt

equation = "cac ~ s(time, k=5)"
Block_residuals<-lapply(block_list, block_res, equation=equation) %>% unlist
mean(Block_residuals^2) %>% sqrt

equation = "cac ~ s(time, k=10)"
Block_residuals<-lapply(block_list, block_res, equation=equation)%>%unlist
mean(Block_residuals^2)%>%sqrt

equation = "cac ~ s(time, k=20)"
Block_residuals<-lapply(block_list, block_res, equation=equation)%>%unlist
mean(Block_residuals^2)%>%sqrt

plot(Data0$time, Data0$cac, type='l', xlim=range(Data0$time, Data1$time), ylim=range(Data0$cac, Data1$cac))
lines(Data0$time[block_list[[5]]], Data0$cac[block_list[[5]]], col='pink')
lines(Data1$time, Data1$cac, col='red')

equation = "cac ~ s(time, k=3)"
res <- block_res(equation, block_list[[5]])
mean(res^2) %>% sqrt

equation = "cac ~ s(time, k=5)"
res <- block_res(equation, block_list[[5]])
mean(res^2) %>% sqrt

equation = "cac ~ s(time, k=10)"
res <- block_res(equation, block_list[[5]])
mean(res^2) %>% sqrt

equation = "cac ~ s(time, k=15)"
res <- block_res(equation, block_list[[5]])
mean(res^2) %>% sqrt
