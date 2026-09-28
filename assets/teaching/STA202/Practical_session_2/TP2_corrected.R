rm(list=objects())

# Packages
library(zoo)
library(timeDate)
library(xts)
library(forecast)
library(mgcv)
library(tidyverse)

##############################################################################
# EXERCICE 1 — Série simulée
##############################################################################

# 1. Construction de Date
date_start <- strptime("01/01/1900", "%m/%d/%Y")
date_end   <- strptime("01/01/2000", "%m/%d/%Y")
Date <- seq.POSIXt(date_start, date_end, by = "year")

# 2. Simulation de la série
n <- length(Date)
t <- 1:n

Tt  <- t / 20 + 1
w   <- 2 * pi / 50
St  <- cos(w * t) + sin(w * t)
eps <- rnorm(n)


X  <- xts(Tt + St + eps, order.by = Date)
Tt <- xts(Tt, order.by = Date)
St <- xts(St, order.by = Date)


plot(X, main = "Série simulée")
lines(Tt, col = "red")
lines(Tt + St, col = "blue")

str(X)
Acf(X, lag.max = 2*50)

# 3. Estimation de la tendance
# 3.a) Regression Linéaire
reg <- lm(X~t) # reg <- lm(X~t+I(t**2))

summary(reg)
ychap.lm<-xts(as.numeric(reg$fitted), order.by=Date)

plot(X, type='l')
lines(ychap.lm, col='red')
lines(Tt, col='blue')

# 3.b) Moyenne mobile
l <- 20 # jouer sur ce paramètre
mb<-stats::filter(reg$residuals, filter=array(1/l, dim=l),
                  method = c("convolution"),
                  sides = 2, circular = TRUE)
mb<-xts(mb,order.by=Date)

plot(reg$residuals,type='l')
lines(mb,col='red',lwd=2)

Acf(mb, lag.max=60)

# 3.c) Noyau Gaussien
h <- 100
x <- seq(1,max(t), length=n)

noyau <- function(x){
  dnorm(x-t, 0, sd=sqrt(h/2) )/sum(dnorm(x-t, 0, sd=sqrt(h/2) ) )
  }

W <- matrix(unlist(lapply(x, noyau) ), ncol=n, nrow=n, byrow=F)

plot(W[,70], type='l')

ychap.kernel <- colSums(as.numeric(X)*W)
ychap.kernel <- xts(ychap.kernel, order.by=Date)

plot(X, type='l')
lines(ychap.kernel, col='red')

# 3.d) Polynômes locaux
lo <- loess(X~t, degree=1, span=0.1) # jouer sur la fenêtre (span)
ychap.lo <- xts(lo$fitted,order.by=Date)

plot(X, type='l')
lines(ychap.lo, col='red')

# 3.d) Regression sur base de spline (GAM)
g <- gam(X~s(t, k=3)) # jouer sur le nombre de morceaux k
?gam()
summary(g)
ychap.gam <- xts(g$fitted,order.by=Date)

plot(X, type='l')
lines(ychap.gam, col='red')

# 4. Estimation de la saisonnalité
# 4. Preliminaire : retirer la tendance (avec le modèle de votre choix)
X.detrend <- X-ychap.lm

plot(X.detrend)

acf(X, lag.max=100)
acf(X.detrend, lag.max=100)

# 4.a) Regression linéaire sur série de Fourier
w <- 2*pi/50
K <- 30

fourier <- cbind(cos(w*t), sin(w*t))
for(i in c(2:K)){
  fourier <- cbind(fourier, cos(i*w*t), sin(i*w*t))
}

matplot(fourier[,1:10], type='l')
dim(fourier)

r2<- NULL # Le r2 est une métrique de performance : \approx pourcentage de variance expliquée
for(i in seq(2, ncol(fourier), by=2)){
  reg <- lm(X.detrend~fourier[,1:i]-1)
  s   <- summary(reg)
  r2  <- c(r2, s$r.squared)
}

plot(r2, type='b', pch=20)

reg <- lm(X.detrend~fourier[,1:2]-1)
summary(reg)
reg <- lm(X.detrend~fourier[,1:4]-1)
summary(reg)
reg <- lm(X.detrend~fourier[,1:6]-1)
summary(reg)

# On garde ce modèle car il est simple et efficace (on est pas en prévision ici)
reg <- lm(X.detrend~fourier[,1:2]-1)
ychap.lm.season <- xts(as.numeric(reg$fitted), order.by=Date)

plot(X.detrend, type='l')
lines(ychap.lm.season, col='red', lwd=2)
lines(xts(St,,order.by=Date), col='blue', lwd=2)

# 4.b) Moyenne mobile
K <- 25
mb.season <- stats::filter(X.detrend, filter=array(1/K,dim=K), method = c("convolution"), 
                         sides = 2, circular = TRUE)
mb.season <- xts(mb.season, order.by=Date)

plot(X.detrend, type='l')
lines(mb.season, col='red')
lines(xts(St,, order.by=Date), col='blue')

# 4.c) Noyau Gaussien
h <- 50
x <- seq(1,max(t),length=n)
W <- matrix(unlist(lapply(x, function(x){dnorm(x-t, 0, sd=sqrt(h/2))/sum(dnorm(x-t, 0, sd=sqrt(h/2)))})), ncol=n, nrow=n, byrow=F)
plot(W[,10], type = "l")

ychap.kernel.season <- colSums(as.numeric(X.detrend)*W)
ychap.kernel.season <- xts(ychap.kernel.season, order.by=Date)

plot(X.detrend,type='l')
lines(ychap.kernel.season,col='red')
lines(xts(St,, order.by=Date), col='blue')

# 4.d) Polynômes locaux
lo <- loess(X.detrend~t, degree=1,span=0.25)
ychap.lo.season <- xts(lo$fitted,order.by=Date)

plot(X.detrend,type='l')
lines(ychap.lo.season,col='red')
lines(St,col='blue')

# 4.e) Régression sur base de spline cyclique (GAM)
cycle <- c(rep(c(1:50), 2), 1)
plot(cycle)

plot(cycle, X.detrend, pch=20)

g <- gam(X.detrend~s(cycle, k=5, bs='cc'))
summary(g)

ychap.gam.season <- xts(g$fitted, order.by=Date)
plot(X.detrend,type='l')
lines(ychap.gam.season,col='red')
lines(xts(St,,order.by=Date),col='blue')

plot(g)

# Question Bonus : Modéliser simultanément tendance avec un modèle gam
g2 <- gam(X~s(t, k=3)+s(cycle, k=5, bs='cc'))
summary(g2)

ychap.g2 <- xts(g2$fitted, order.by=Date)

plot(X,type='l')
lines(ychap.g2,col='red') 

# Regarder effet par effet
M <- predict(g2, type='terms')

plot(as.numeric(X), type='l')
lines(g2$coefficients[1]+M[,1], col='red')
lines(g2$coefficients[1]+ M[,1]+M[,2], col='blue')

# 3. Comparaison des méthodes
plot(X-eps, type='l', ylim=range(X))
lines(X-eps, lwd=2)
lines(X, col='grey')
lines(ychap.lm+ychap.lm.season, col='purple')
lines(ychap.lm+ychap.kernel.season, col='red')
lines(ychap.lm+ychap.lo.season, col='blue')
lines(ychap.lm+ychap.gam.season, col='turquoise2')
lines(ychap.lm+mb.season, col='violetred1')

# MSE
epschap <- X-(ychap.lm+ychap.lm.season)
mean(epschap**2)

epschap <- X-(ychap.lm+ychap.kernel.season)
mean(epschap**2)

# ACF
acf(X-(ychap.lm+ychap.lm.season))

acf(X-(ychap.lm+ychap.kernel.season))

acf(X-(ychap.lm+ychap.lo.season))

acf(X-(ychap.lm+ychap.gam.season))

acf(X-(ychap.lm+mb.season))

# Validation Croisé (et discussion sur la prévision)

# Generalized Cross Validation score
g <- gam(X~s(t, k=3)+s(cycle, k=5, bs='cc'))
g$gcv.ubre
g <- gam(X~s(cycle, k=5, bs='cc'))
g$gcv.ubre

# Block Cross Validation 
Data   <- data.frame(X, t, cycle)
Nblock <- 10

borne_block <- seq(1, nrow(Data), length=Nblock+1) %>% floor
block_list  <- list()
l <- length(borne_block)
for(i in c(2:(l-1))){
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]] <- c(borne_block[l-1]:(borne_block[l]))

block_res <- function(equation, block){
  g <- gam(as.formula(equation), data=Data[-block,])
  forecast <- predict(g, newdata=Data[block,])
  return(Data[block,]$X-forecast)
} 

equation <- "X~s(t, k=3)+s(cycle,k=4, bs='cc')"
Block_residuals <- lapply(block_list, block_res, equation=equation) %>% unlist
mean(Block_residuals**2)

equation = "X~s(t, k=3)+s(cycle,k=5, bs='cc')"
Block_residuals<-lapply(block_list, block_res, equation=equation) %>% unlist
mean(Block_residuals**2)

equation = "X~s(cycle,k=10, bs='cc')"
Block_residuals<-lapply(block_list, block_res, equation=equation) %>% unlist
mean(Block_residuals**2)

equation = "X~s(t, k=20)"
Block_residuals<-lapply(block_list, block_res, equation=equation) %>% unlist
mean(Block_residuals**2)

g <- gam(as.formula(equation), data=Data)
plot(g)

plot(Data$X, type='l')


##############################################################################
# Exercice 2 — Faire la même chose avec les données du TP1
##############################################################################

# Dossier de travail (à adapter)
data_path <- "TP1_data"

# Importation des données
beer_raw <- read.csv(file.path(data_path, "beer2.csv"), 
                     header = TRUE, skip = 1)
# Création de la variable Date
date_start <- as.POSIXct("1991-01-01")
date_end   <- as.POSIXct("1995-08-01")
Date <- seq(from = date_start, to = date_end, by = "month")

stopifnot(length(Date) == nrow(beer_raw))

beer <- data.frame(
  Date = Date,
  BeerProd = beer_raw$BeerProd
)

Time <- c(1:length(Date))
beer <- data.frame(Date,beer$BeerProd,Time)
names(beer) <- c("Date","BeerProd","Time")

plot(beer$Date,beer$BeerProd,type='l')

# Régression sur base de splines
Month <- as.numeric(format(Date,"%m"))
beer  <- data.frame(beer, Month)
plot(beer$Month)

g <- gam(BeerProd~s(Time, k=3)+s(Month, k=10,bs='cc'), data=beer)
summary(g)
plot(g)
g$gcv.ubre

g <- gam(BeerProd~Time+s(Month, k=10,bs='cc'),data=beer)
summary(g)
plot(g)
g$gcv.ubre

g <- gam(BeerProd~s(Month, k=4,bs='cc'),data=beer)
summary(g)
plot(g)
g$gcv.ubre

Acf(g$residuals)

g <- gam(BeerProd~s(Month, k=10,bs='cc'),data=beer)
summary(g)
plot(g)
g$gcv.ubre

# Block Cross Validation
Data   <- beer
Nblock <- 5
borne_block <- seq(1, nrow(Data), length=Nblock+1) %>% floor
block_list  <- list()
l <- length(borne_block)
for(i in c(2:(l-1))){
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]] <- c(borne_block[l-1]:(borne_block[l]))

block_res <- function(equation, block){
  g <- gam(as.formula(equation), data=Data[-block,])
  forecast <- predict(g, newdata=Data[block,])
  return(Data[block,]$BeerProd-forecast)
} 

equation = "BeerProd~s(Month, k=10,bs='cc')"
Block_residuals <- lapply(block_list, block_res, equation=equation) %>% unlist
mean(Block_residuals**2)


equation = "BeerProd~Time+s(Month, k=10,bs='cc')"
Block_residuals <- lapply(block_list, block_res, equation=equation) %>% unlist
mean(Block_residuals**2)



ychap.gam<-g$fitted

plot(beer$Date,beer$BeerProd,type='l')
lines(beer$Date,ychap.gam,col='red')

plot(g)
terms <- predict(g, newdata=beer, type="terms")

plot(beer$Date,beer$BeerProd-mean(beer$BeerProd),type='l')
lines(beer$Date,terms[,1],col='blue')

# Extrapolation
sel   <- which(lubridate::year(beer$Date)>=1994)
Data0 <- beer[-sel,]
Data1 <- beer[sel,]

g <- gam(BeerProd~Time+s(Month, k=10,bs='cc'), data=Data0)
g.forecast <- predict(g, newdata=Data1)

plot(Data0$Date,  Data0$BeerProd, type='l', xlim=range(beer$Date), ylim=range(beer$BeerProd))
lines(Data1$Date, Data1$BeerProd, col='red')
lines(Data1$Date, g.forecast,  col='blue')

mean((Data1$BeerProd-g.forecast)**2)