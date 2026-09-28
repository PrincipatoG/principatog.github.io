rm(list=objects())

# Packages
library(zoo)
library(magrittr)
library(xts)
library(forecast)
library(mgcv)
library(tidyverse)

##############################################################################
# EXERCICE 2 — AR(1)
##############################################################################

# 1. Simulation de la série
n <- 100
eps <- rnorm(n,0,1)
plot(cumsum(eps), type='l')
acf(eps)
acf(cumsum(eps))

a <- 0.9
puiss <- function(a,k){c(a^c(k:1),rep(0,n-k))}
puiss(a,k=4)
plot(puiss(a,k=4))
M <- lapply(c(1:n),puiss,a=a) %>% unlist %>% matrix(nrow=n,ncol=n,byrow=T)

head(M)
y0 <- M%*%eps

plot(y0, type='l')
plot(eps, type='l')

acf(y0)
acf(eps)

# 2. Test différentes valeurs
a <- 0.1
puiss <- function(a,k){c(a^c(k:1),rep(0,n-k))}
puiss(a,k=4)
plot(puiss(a,k=4))
M <- lapply(c(1:n),puiss,a=a) %>% unlist %>% matrix(nrow=n,ncol=n,byrow=T)

head(M)
y1 <- M%*%eps

plot(y1, type='l')
plot(eps, type='l')

acf(y1)
acf(eps)

a <- 0.7
puiss <- function(a,k){c(a^c(k:1),rep(0,n-k))}
puiss(a,k=4)
plot(puiss(a,k=4))
M <- lapply(c(1:n),puiss,a=a) %>% unlist %>% matrix(nrow=n,ncol=n,byrow=T)

head(M)
y2 <- M%*%eps

plot(y2, type='l')
plot(eps, type='l')

acf(y2)
acf(eps)

a <- -0.7
puiss <- function(a,k){c(a^c(k:1),rep(0,n-k))}
puiss(a,k=4)
plot(puiss(a,k=4))
M <- lapply(c(1:n),puiss,a=a) %>% unlist %>% matrix(nrow=n,ncol=n,byrow=T)

head(M)
y3 <- M%*%eps

plot(y3, type='l')
plot(eps, type='l')

acf(y0)
acf(eps)

par(mfrow=c(3,1))
plot(y1,type='l')
plot(y2,type='l')
plot(y3,type='l')

par(mfrow=c(3,1))
acf(y1)
acf(y2)
acf(y3)

# 3. Acf et Pacf
n <- 10000
eps <- rnorm(n=n,0,1)
acf(eps)
a <- 0.9
puiss <- function(a,k){c(a^c(k:1),rep(0,n-k))}
M <- lapply(c(1:n),puiss,a=a)%>%unlist%>%
  matrix(nrow=n,ncol=n,byrow=T)
y <- M %*% eps

autoCorr<-function(x,h)
{
  x.lag<-c(x[1:h],head(x,length(x)-h))
  return(cor(x.lag,x))
}

a1 <- c(1,sapply(c(1:30),autoCorr,x=y))
par(mfrow=c(1,1))
plot(c(0:30), a1,type='h',ylim=c(0,1))
lines(c(0:30),a^c(0:30),col='red')

par(mfrow=c(1,1))
acf(y,lag.max=30)
lines(c(0:30),a^c(0:30),col='red')
lines(c(0:30),a1,col='blue')

lag <- function(x, h){
  x.lag <- c(x[1:h],head(x,length(x)-h))
  return(x.lag)
}

PartialAutoCorr<-function(x,h){
  x.lag <- lapply(c(1:h),lag, x=x)
  x.lag <- matrix(unlist(x.lag),ncol=length(x.lag))
  reg   <- lm(x~x.lag-1)
  return(tail(reg$coef,1))
}

PartialAutoCorr(y,h=2)

pa1<-sapply(c(1:50),PartialAutoCorr,x=y)
plot(pa1, type='h')

# 4. Rebelote avec MA(q)
n <- 200
eps <- rnorm(n,0,1)
b <- rep(2, 4)
b <- b/sum(b)
y <- stats::filter(eps, filter=b, method = c("convolution"),
          sides = 1, circular = T)

par(mfrow=c(1,1))
plot(eps,type='l',col='grey', ylim=range(y, eps))
lines(y)

par(mfrow=c(1,1))
acf(y,na.action = na.omit)
pacf(y,na.action = na.omit)

y <- y07
h <- 1
y.lag1 <- c(y[1:h],head(y,length(y)-h))
h <- 2
y.lag2 <- c(y[1:h],head(y,length(y)-h))

lm(y~y.lag1+y.lag2)