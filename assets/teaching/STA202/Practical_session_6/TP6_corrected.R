rm(list=objects())

# Packages
library(magrittr)
library(forecast)
library(sarima)
##############################################################################
# Exercice 1 — (Re-)Découverte des ARMA(p,q)
##############################################################################

# 1. Nom du processus
print("C'est un ARMA(2,1)")

# 2. Simulation de la trajectoire
set.seed(110)
?arima.sim()
n     <- 1000
sigma <- 1
eps   <- rnorm(n,0,sd=sigma)
ar    <- c(1,-1/4)
ma    <- c(-1)

x1 <- arima.sim(n = n, list(ar = ar,ma = ma), innov=eps)

# 3. Représenter une trajectoire
par(mfrow=c(1,1))
plot(x1)
plot(head(x1,100))

# 4. Proposition des ordres
par(mfrow=c(1,2))
acf(x1)
pacf(x1)

# 5. Choix du model optimal
pmax <- 6
qmax <- 5

?expand.grid()
ordre <- expand.grid(p = c(0:pmax), q = c(0:qmax))
ordre <- cbind(ordre[,1],0,ordre[,2])
dim(ordre)

model <- apply(ordre, MARGIN=1, arima,x=x1, method = c("ML"), SSinit = c("Rossignol2011"), 
               optim.method = "BFGS", include.mean = F)
print( model[[10]]$aic)
print(-2*model[[10]]$loglik+2*(length(model[[10]]$coef)+1))

aic  <- lapply(model,function(x) x$aic) %>% unlist
bic  <- lapply(model,function(x) -2*x$loglik+log(x$nobs)*length(x$coef)) %>% unlist
like <- lapply(model,function(x) -2*x$loglik) %>% unlist


par(mfrow=c(1,1))
o <- order(aic)
plot(aic[o],type='b',pch=20,axes=F,
     ylim=range(aic,like))
points(like[o], col='red', pch=20, type='b')
axis(1,c(1:length(aic)),
     paste(ordre[o,1],ordre[o,3]),las=2)
axis(2)

par(mfrow=c(1,1))
o <- order(aic)
plot(aic[o[1:10]],type='b',pch=20,axes=F)
axis(1,c(1:10),paste(ordre[o[1:10],1],ordre[o[1:10],3]),las=2)
axis(2)

par(mfrow=c(1,1))
o <- order(bic)
plot(bic[o],type='b',pch=20,axes=F)
axis(1,c(1:length(aic)),paste(ordre[o,1],ordre[o,3]),las=2)
axis(2)
ordre[o,]

ordre.opt.aic <- ordre[which.min(aic),]
ordre.opt.bic <- ordre[which.min(bic),]

# Attention, il y a un pb de convergence pour ordre.opt.aic (à ignorer)
model.opt <- arima(x=x1, order=ordre.opt.bic, method = c("ML"),
                 SSinit = c("Rossignol2011"),
                 optim.method = "BFGS", include.mean = F)

model.opt
names(model.opt)
model.opt$coef
model.opt$sigma2
model.opt$var.coef

summary(model.opt)

# 6. Test de significativité des paramètres (cf page 7 du cours)
print(abs(model.opt$coef)/sqrt(diag(model.opt$var.coef))<1.96)
print("On rejette donc la nullité des coefficients, au risque 5%")

pvalue <- function(model){
  (1-pnorm(abs(model$coef)/sqrt(diag(model$var.coef))))*2
}

print(pvalue(model.opt))

# Bonus, validation des ordres en regardant l'ordre supérieur
model.opt_p1 <- arima(x=x1,order=ordre.opt.bic+c(1,0,0),
                    method = c("ML"),SSinit = c("Rossignol2011"),
                    optim.method = "BFGS",include.mean = F)
print(abs(model.opt_p1$coef)/sqrt(diag(model.opt_p1$var.coef))<1.96)
pvalue(model.opt_p1)

model.opt_q1 <- arima(x=x1,order=ordre.opt.bic+c(0,0,1),
                      method = c("ML"),SSinit = c("Rossignol2011"),
                      optim.method = "BFGS",include.mean = F)
pvalue(model.opt_q1)
print(abs(model.opt_q1$coef)/sqrt(diag(model.opt_q1$var.coef))<1.96)

# 7. Diagnostic des résidus
plot(model.opt$residuals,type='l')

par(mfrow=c(1,2))
acf(model.opt$residuals)
pacf(model.opt$residuals)
print("Visuellement, ca ressemble a un bruit blanc")

par(mfrow=c(1,1))
qqnorm(model.opt$residuals)

x <- seq(min(model.opt$residuals),
       max(model.opt$residuals),length=50)
hist(model.opt$residuals,breaks=50,freq=F)
lines(x,dnorm(x,mean(model.opt$residuals),model.opt$sigma2), col='red')

pvalue_BP <- function(model,K){
  rho <- acf(model$residuals,lag.max=K,plot=F)$acf[-1]
  n <- model$nobs
  pval <- (1-pchisq(n*sum(rho^2),df=K-length(model$coef)))
  return(pval)
}

pvalue_BP(model.opt,K=10)
pvalue_BP(model.opt,K=20)
print("On ne rejette pas l'hypothèse de blancheur des résidus au risque 5%")

# 8. Prévision 
x1_a <- x1[1:900]
x1_b <- x1[-c(1:900)]

model.opt <- arima(x=x1_a, order=ordre.opt.bic, method = c("ML"),
                 SSinit = c("Rossignol2011"),optim.method = "BFGS",
                 include.mean = F)
# Remarque : on "triche" puisque l'ordre optimal a été appris en partie sur x1_b
h <- 100
test <- predict(model.opt, n.ahead=h)

names(test)
plot(test$pred, type='l')
predict(model.opt, n.ahead=h)$pred

# Prévision h-ahead
forecast <- function(model,h){
  forecast <- array(0,dim=100)
  for(i in c(900:999)){
    mod <- arima(x=x1[1:(i-1)],order=ordre.opt.bic,
               fixed=model$coef,include.mean = F)
    forecast[i-899] <- tail(predict(mod,n.ahead=h)$pred,1)
  }
  return(forecast)
}

prev <- forecast(model.opt,h=1)

par(mfrow=c(1,1))
plot(x1_b,type='l')
lines(prev,col='red')

prevs  <- lapply(c(1:10), forecast, model=model.opt)
erreurs <- unlist(lapply(prevs, function(x){mean((x-x1_b)^2)}))

plot(erreurs,type='b')
print("Plus on essaie de prédire loin dans le temps sans revoir des données, plus les erreurs sont grandes")

##############################################################################
# Exercice 2 — ordre des SARIMA sur données réelles
##############################################################################

# Dossier de travail (à adapter)
setwd()

# 1. Importation des données
data <- read.table("TP6_data/TP6_exercice2.txt", sep=';', header=T)
attach(data)

# 2. Première série
par(mfrow=c(1,1))
plot(x1)

par(mfrow=c(1,2))
acf(x1,lag.max=60) 
pacf(x1,lag.max=60) 
print("saisonnalité d'ordre 12 + décroissante rapide vers 0")

s <- 12
par(mfrow=c(1,2))
acf(x1,lag.max=3*s)        # qmax= 1, Qmax=2
pacf(x1,lag.max=3*s)       # pmax= 2, Pmax=2


ordre <- expand.grid(p = c(0:2), q = c(0:1), P=c(0:2),Q=c(0:2))
ordre <- cbind(ordre[,1],0,ordre[,2],ordre[,3],0,ordre[,4])
dim(ordre)

sarima <- function(x, ordre, s){
  arima(x,order = ordre[1:3], seasonal = list(order = ordre[4:6], period = s)
        ,include.mean = F)
}

model.sarima <- apply(ordre,1,sarima,x=x1,s=12)
aic  <- lapply(model.sarima,function(x) x$aic) %>% unlist
bic  <- lapply(model.sarima,function(x) -2*x$loglik+x$nobs*length(x$coef)) %>% unlist
like <- lapply(model.sarima,function(x) -2*x$loglik) %>% unlist

par(mfrow=c(1,1))
o <- order(aic)
plot(aic[o],type='b',pch=20,axes=F,xlab='')
axis(1,c(1:length(aic)),paste(ordre[o,1],ordre[o,3],ordre[o,4],ordre[o,6]),las=2)
axis(2)

ordre[which.min(aic),]
ordre[which.min(bic),]
summary(model.sarima[[which.min(aic)]])
model.sarima[[which.min(aic)]]$coef
pvalue(model.sarima[[which.min(aic)]])

model.sarima[[order(aic)[2]]]$coef
pvalue(model.sarima[[order(aic)[2]]])

# 3. Deuxième série
par(mfrow=c(1,1))
plot(x2,type='l')

par(mfrow=c(2,1))
acf(x2, lag.max=30)
pacf(x2, lag.max=30)

s <- 7

par(mfrow=c(1,2))
acf(x2,lag.max=3*s)        # qmax= 3, Qmax=2
pacf(x2,lag.max=3*s)       # pmax= 1, Pmax=1

# Je ne mets pas la correction, c'est la même chose qu'avant

# 4. Troisième série
par(mfrow=c(1,1))
plot(x3,type='l')
print("Attention, c'est grossièrement non-stationnaire : il faut différencier")

par(mfrow=c(2,1))
acf(x3)            
pacf(x3)

x3.diff <- diff(x3,lag=1, differences = 1)

plot(x3.diff,type='l')
acf(x3.diff)       
print("Ca ressemble déjà plus à un processus stationnaire !")

par(mfrow=c(1,2))
acf(x3.diff,lag.max=20)        # qmax= 4
pacf(x3.diff,lag.max=20)       # pmax= 5