rm(list=objects())

# Packages
library(magrittr)
library(dplyr)
##############################################################################
# Exercice 1 — Découverte des ARMA(p,q)
##############################################################################

# 1. Nom du processus
print("C'est un AR(1); ce processus est stationnaire si |Φ|<1; le reste au tableau")

# 2. et 3. -> Au tableau

# 4. Simulation de la série
sigma <- 1/2
phi   <- 0.9
n     <- 500

eps <- rnorm(n,0,sd=sigma)
?arima.sim
y <- arima.sim(n = n, list(ar = c(phi)),innov=eps)

plot(y,type='l')

# 5. Auto-covariance
# 5.a) Variance
Nsimu <- 100
v <- array(0, dim=Nsimu)

for(i in c(1:Nsimu)){
  eps<-rnorm(n,0, sd=sigma)
  y<-arima.sim(n=n, list(ar=c(phi)), innov=eps)
  v[i] <- var(y)
}
# Comparer à la valeur théorique connue
boxplot(v)
abline(h=sigma^2/(1-phi^2), col='red')

# 5.b) auto-corrélation
lmax <- 40
rho_est  <- acf(y,lag.max=lmax, plot = FALSE)
rho_theo <- phi^c(0:lmax)

plot(rho_est$acf, type='b', pch=20)
lines(rho_theo, col='red')

# 5.c) auto-covariance
gamma_est  <- rho_est$acf*var(y)
gamma_theo <- sigma^2/(1-phi^2)*phi^c(0:40)

plot(gamma_est,type='b',pch=20)
lines(gamma_theo,col='red')

# 6. Estimateur de Φ
phi_hat <- rho_est$acf[2]

estim_phi <- function(n, sigma, phi){
  eps <- rnorm(n,0,sd=sigma)
  y   <- arima.sim(n=n, list(ar=c(phi)), innov=eps)
  rho_est <- acf(y, lag.max=1, plot=FALSE)
  phi_hat <- rho_est$acf[2]
  return(phi_hat)
}

Nsimu <- 500
phi_hat <- lapply(rep(n,Nsimu), estim_phi, sigma=sigma,phi=phi)
phi_hat <- unlist(phi_hat)

# Histogramme
hist(phi_hat,breaks=30)
abline(v=phi,col='red')

# Intervalle de confiance (2 approches)
alpha <- 0.05
s_n   <- seq(50, 500, by=50)
IC    <- NULL
for(n in s_n){
  phi_h <- lapply(rep(n,Nsimu),
                estim_phi,sigma=sigma,phi=phi)
  phi_h <- unlist(phi_h)
  IC <- rbind(IC,quantile(phi_h,c(alpha/2,
                                1-alpha/2)))
  print(n)
}

plot(s_n, IC[,1], pch='-', ylim=range(IC), cex=3)
points(s_n, IC[,2], pch='-',cex=3)

l <- abs(IC[,2]-IC[,1])
plot(s_n,l,type='b',pch=20)

conv <- 1/sqrt(s_n)
reg  <- lm(l~conv-1)
lines(s_n, reg$coeff/sqrt(s_n), col='red')

##############################################################################
# Exercice 2 — Densité Spectrale
##############################################################################

# Dossier de travail (à adapter)
setwd()

# 1. Importation des données
data <- read.table("exercice2.txt", sep=';', header=T)

par(mfrow=c(1,1))
plot(data$y, type='l')

# 2. Calcul de la densité spectrale
?spectrum
s <- spectrum(data$y)
plot(s$freq,s$spec,type='l')

# 3. Identification des fréquences
a1 <- which.max(s$spec)
f1 <- s$freq[a1]
print(1/f1)

a2 <- which.max(s$spec[-a1])
f2 <- s$freq[a2]
a3 <- which.max(s$spec[-c(a1,a2)])
f3 <- s$freq[a3]

print(1/c(f1,f2,f3))

t <- c(1:length(data$y))
X1cos <- cos(2*pi*f1*data$t)
X1sin <- sin(2*pi*f1*data$t)
X2cos <- cos(2*pi*f2*data$t)
X2sin <- sin(2*pi*f2*data$t)
X3cos <- cos(2*pi*f3*data$t)
X3sin <- sin(2*pi*f3*data$t)

model1 <- lm(data$y~X1cos+X1sin+X2cos+X2sin+X3cos+X3sin-1)
summary(model1)

plot(data$y,type='l')
lines(model1$fitted, col='red')

data0 <- data.frame(y=data$y, X1cos=X1cos, X1sin=X1sin,
                    X2cos=X2cos, X2sin=X2sin, X3cos=X3cos,
                    X3sin=X3sin)

model1 <- lm(y~X1cos+X1sin+X2cos+X2sin+X3cos+X3sin-1, data=data0)
summary(model1)
predict(model1, newdata=data0)

model2 <- lm(y~X1cos+X2cos+X3cos-1, data=data0)
summary(model2)

plot(data$y,type='l')
lines(model2$fitted, col='red')


t <- c((length(data$y)+1) : (length(data$y)+50) )
X1cos<-cos(2*pi*f1*t)
X2cos<-cos(2*pi*f2*t)
X3cos<-cos(2*pi*f3*t)

data1 <- data.frame(X1cos=X1cos, X2cos=X2cos, X3cos=X3cos)

pred <- predict(model2, newdata=data1)
length(pred)
plot(t, pred, type='l')

##############################################################################
# Exercice 3 — Etude des ordres de processus ARMA(p,q)
##############################################################################

# 1. Importation des données
data <- read.table('exercice3.txt', header=T,sep=';')
attach(data)

# 2. Représentation graphiques des séries
x1 <- ts(x1) 
plot(x1)
x2 <- ts(x2)
plot(x2)
x3 <- ts(x3)
plot(x3)
x4 <- ts(x4)
plot(x4)

# 3. Type/Ordre des processus
# 3.a) X1
par(mfrow=c(1,2))
acf(x1)
pacf(x1)

# 3.b) X2
par(mfrow=c(1,2))
acf(x2)
pacf(x2)

# 3.c) X3
par(mfrow=c(1,2))
acf(x3)
pacf(x3)

# 3.d) X4
par(mfrow=c(1,2))
acf(x4)
pacf(x4)

# 4. Determination des coefficients
# 4.a) X1
x1.model <- arima(x1, order = c(2,0,0), method = c("ML"),
                SSinit = c("Rossignol2011"),
                optim.method = "BFGS", include.mean = F)
x1.model_3 <- arima(x1, order = c(3,0,0), method = c("ML"),
                  SSinit = c("Rossignol2011"),
                  optim.method = "BFGS", include.mean = F)
x1.model_5 <- arima(x1, order = c(5,0,0), method = c("ML"),
                  SSinit = c("Rossignol2011"),
                  optim.method = "BFGS", include.mean = F)

x1.model$aic
x1.model_3$aic
x1.model_5$aic

horizon <- 10
x1.forecast <- predict(x1.model, n.ahead=horizon, se.fit=F)

par(mfrow=c(1,1))
plot(x1, xlim=c(1,nrow(data)+horizon))
lines(nrow(data)*c(1:horizon), x1.forecast, col='red')

# 4.b) X2
x2.model <- arima(x2, order = c(1,0,0), method = c("ML"),
                  SSinit = c("Rossignol2011"),
                  optim.method = "BFGS", include.mean = F)

x2.model$aic

horizon <- 10
x2.forecast <- predict(x2.model, n.ahead=horizon, se.fit=F)

par(mfrow=c(1,1))
plot(x2,xlim=c(1,nrow(data)+horizon))
lines(nrow(data)*c(1:horizon),x2.forecast,col='red')

phi <- ARMAtoMA(ar =x2.model$coef, ma=0, 12) 

plot(phi,type='l')
?arima

# 4.c) X3
t.test(x3,alternative=c("two.sided"))

x3.model<-arima(x3, order = c(0,0,5), method = c("ML"), SSinit = c("Rossignol2011"),
                optim.method = "BFGS",include.mean = F)
print(x3.model)

horizon <- 10

x3.forecast <- predict(x3.model,n.ahead = horizon,se.fit =F)

par(mfrow=c(1,1))
plot(x3, type ="l", xlim=c(1,nrow(data)+horizon))
lines(nrow(data)*c(1:horizon),x3.forecast,col='red')

# 4.d) X4

x4.model <- arima(x4, order = c(0,0,20), method = c("ML"),SSinit = c("Rossignol2011"),optim.method = "BFGS",include.mean = F)

horizon <- 10

x4.forecast<-predict(x4.model,n.ahead = horizon,se.fit =F)

plot(x4,xlim=c(1,nrow(data)+horizon))
lines(nrow(data)*c(1:horizon),x4.forecast,col='red')
