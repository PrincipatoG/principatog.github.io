rm(list=objects())
library(zoo)
library(timeDate)
#library(forecast)
library(xts)
library(dygraphs)

#install.packages()
#install.packages("timeDate")

#######################################################################################################################################
#############################################Exercice 1: beer production
#######################################################################################################################################

######Import des donnees
#setwd("/Users/yannig/Documents/Enseignement/2019_2020/M1_serie_chro/
#      Datasets/")
#Data/beer2.csv
beer<-read.csv("TP1_data/beer2.csv", header=TRUE, skip=1)


head(beer)
str(beer)
summary(beer)
plot(beer$BeerProd,type='b',pch=20)


#######creation de la date
date1<- strptime(c("01/01/91"), "%m/%d/%y")
date2<- strptime(c("08/01/95"), "%m/%d/%y")

seq(0, 1, length=10)
seq(0, 1, by=0.1)

Date<-seq(date1, date2, by = "1 month")

length(Date)
dim(beer)
#x=1
#x <- 1

beer <- data.frame(Date, beer$BeerProd)
names(beer)<-c("Date","BeerProd")


summary(beer)
plot(beer$Date, beer$BeerProd,type='l')


#########classe ts
beer.ts<-ts(beer$BeerProd, start=1991, frequency=12)
plot(beer.ts)

#########classe zoo
library(zoo)
beer.zoo<-zoo(beer$BeerProd, order.by=beer$Date)
plot(beer.zoo)


#install.packages("zoo")
######################statistiques de base
mean(beer$BeerProd)
sd(beer$BeerProd)
summary(beer)

boxplot(beer$BeerProd)

hist(beer$BeerProd, breaks=10, freq=T)
dim(beer)


year<-format(beer$Date,"%Y")
mean.year<-tapply(beer$BeerProd, as.factor(year),mean)
#mean.year<-tapply(beer$BeerProd, as.numeric(year),mean)
?sapply


plot(mean.year, type='b')


plot(mean.year,type='b', axes=F, xlab="")
axis(1,c(1:5),names(mean.year))
axis(2)


month<-format(beer$Date,"%m")
mean.month<-tapply(beer$BeerProd,as.factor(month),mean)
plot(mean.month,type='b',axes=F)
axis(1,c(1:12),names(mean.month))
axis(2)





#######################################################################################################################################
###############################exercice 2: consommation Ã©lectrique
#######################################################################################################################################
data<-read.table("TP1_data/conso_2015.csv", header=T, sep=';')
summary(data)

head(data) ###les donnÃ©es sont par jour puis par heure, 
#on prÃ©fÃ¨rera une 
#date incluant l'heure
date1<- strptime("01/01/2015 00:00:00", "%m/%d/%Y %H:%M:%S")
date2<- strptime("11/30/2015 23:30:00", "%m/%d/%Y %H:%M:%S")
Date<-seq.POSIXt(date1,date2, by = "30 min")

#Date<-seq(date1,date2, by = "30 min")


summary(Date)
head(Date)

X<-as.matrix(t(data[,-1]))
conso<-c(X)

plot(conso[1:(48*7*3)],type='l')
plot(Date,conso,type='l')


library(xts)
conso.xts<-xts(conso,order.by=Date)
plot(conso.xts)


##avec fenÃªtre 
dygraph(conso.xts)%>% dyRangeSelector()  


#####################stat desc.
#####stats.
mean(conso.xts)

month<-as.factor(.indexmon(conso.xts))
mean.month<-tapply(conso.xts,month,mean)
plot(mean.month,type='b')


?xts



#####profils
dow<-as.factor(.indexwday(conso.xts))
conso_day<-tapply(conso.xts,dow,mean)
plot(conso_day,type='b')

hour<-as.factor(.indexhour(conso.xts))
mean.dow.hour<-tapply(conso.xts,dow:hour,mean)

plot(mean.dow.hour,type='l')
abline(v=seq(1,24*7,by=24),col='red')

col.pal<-colorRampPalette(c("lightblue", "red"))( 12 )
sel<-which(.indexhour(conso.xts)==20)
boxplot(conso[sel]~ month[sel],col=col.pal)


par(mfrow=c(1,1))
acf(conso,lag.max=48)
acf(conso,lag.max=7*48)
acf(conso,lag.max=7*48*3)

n <- length(conso)
lag1 <- c(conso[1:48], conso[1:(n-48)])

plot(lag1, conso, pch='.')

cor(lag1, conso)

par(mfrow=c(2,1))
acf(conso,lag.max=48)
par(mfrow=c(1,1))
p <-pacf(conso,lag.max=48)
p

###############################################################autocorrelations
####fonction lag
lag.test<-lag.xts(conso.xts, k=48, na.pad=T)
lag.test[1:3]
conso.xts[1:3]
cor(conso.xts, lag.test, use="pairwise.complete.obs")

####fonction calculant l'autocorrÃ©lation d'ordre h
autoCorr<-function(x,h)
{
  x.lag<-lag.xts(x,k=h,na.pad=T)
  return(cor(x.lag,x,use="pairwise.complete.obs"))
}

autoCorr2<-function(x,h)
{
  x.lag<-c(x[1:h],head(x,length(x)-h))
  return(cor(x.lag,x))
}

a1<-sapply(c(1:336),autoCorr,x=conso.xts)


a2<-sapply(c(1:336),autoCorr2,x=conso.xts)
par(mfrow=c(1,1))
plot(a1,type='h',ylim=c(0,1))
lines(a2,col='red')

a1-a2

####2e mÃ©thode avec la fonction acf
a3<-acf(conso.xts,lag.max=336,type="correlation")




###on constate des diffÃ©rences:
plot(a3$acf[-1],type='h',ylim=c(0,1))
lines(a2,col='red')

plot(a3$acf[-1]-a2)
plot(a2-a1)

n <- length(conso.xts)
a3$acf[-1]-a1*(n-1)/n

mean(abs(a3$acf[-1]-a1))
mean(abs(a3$acf[-1]-a2))

mean(abs(a3$acf[-1]-a2*(n-1)/n))
cor(conso.xts, lag.xts(conso.xts,k=1,na.pad=T), use="pairwise.complete.obs") * (n-1)/n
cor(conso.xts[-1], lag.xts(conso.xts,k=1,na.pad=T)[-1]) * (n-1)/n
acf(conso.xts, type = "correlation", plot=F, na.action = na.pass)$acf[2]




# x = c(-2,-1,0,1,2)
# acf(x, plot = F, lag.max = NULL)
# Autocorrelations of series â€˜xâ€™, by lag
#   0    1    2    3    4 
# 1.0  0.4 -0.1 -0.4 -0.4 
# acf_lag_2 = sum(x*c(x[c(-1,-2)],NA,NA), na.rm = T) /
#   sqrt(sum(x*x)*sum(x*x))
# acf_lag_2
# 
# cor(x, c(0,1,2,NA,NA), use="pairwise.complete.obs") # = cor(c(-2,-1,0), c(0,1,2)) = 1
# 
# cor_lag_2 = sum((c(-2,-1,0)+1)*(c(0,1,2)-1)) /   # recall cor needs to demean both vectors
#   sqrt(sum(c(-1,0,1)*c(-1,0,1))*sum(c(-1,0,1)*c(-1,0,1)))
# cor_lag_2



###############################################################autocorrelations partielles

PartialAutoCorr<-function(x,h)
{
  x.lag<-lapply(c(1:h),lag.xts,x=x,na.pad=T)
  x.lag<-matrix(unlist(x.lag),ncol=length(x.lag))
  reg<-lm(x~x.lag-1)
  return(tail(reg$coef,1))
}

PartialAutoCorr(conso,h=1)
pa1<-sapply(c(1:50),PartialAutoCorr,x=conso.xts)
plot(pa1, type='b', pch='.')


pa2<-pacf(conso,lag.max=7*48*4) 
points(pa2$acf)

pa1-pa2$acf





#######################################################################################################################################
###############################exercice 3: simulation
#######################################################################################################################################

#######simulation d'une serie periodique
t<-c(1:200)
w=2*pi/12

x<-cos(w*t)+rnorm(length(t),0,1)
plot(x,type='l')

acf(x,lag.max=50)


#######simulation d'une serie avec tendance
t<-c(1:200)
#lin?aire
x<-t/10+rnorm(length(t),0,1)
plot(x,type='l')
acf(x)
pacf(x)

#lin?aire avec variance croissante
x<-t/10+rnorm(length(t),0,sd=log(t/10+1))
plot(x,type='l')
acf(x)



date1<- strptime("03/01/1986", "%m/%d/%Y")
date2<- strptime("04/01/2002", "%m/%d/%Y")
Date<-seq.POSIXt(date1,date2,by = "month")

n<-length(Date)
t<-c(1:n)

T<-log(t/10+1)

w=2*pi/12
S<-cos(w*t)
eps<-rnorm(n,0,1)


X<-T+S+eps

X<-xts(X,order.by=Date)
T<-xts(T,order.by=Date)
S<-xts(S,order.by=Date)
plot(X)
lines(T,col='red')
lines(T+S,col='blue')

indexFormat(X)

extractY<-function(y,X,Date)
{
  year<-format(Date,"%Y")
  return(as.numeric(X[which(year==y)]))
}

y<-c(1987,1990,1993)

plot(extractY(1987,X=X,Date=Date),type='l',ylim=range(X))
lines(extractY(1990,X=X,Date=Date),col='red')
lines(extractY(1993,X=X,Date=Date),col='blue')



###########modÃ¨le multiplicatif
#mars 1986 a avril 2002
date1<- strptime("03/01/1986", "%m/%d/%Y")
date2<- strptime("04/01/2002", "%m/%d/%Y")
Date<-seq.POSIXt(date1,date2,by = "month")

n<-length(Date)
t<-c(1:n)

T<-log(t/10+1)

w=2*pi/12
S<-cos(w*t)
eps<-rnorm(n,0,1/2)


X<-T*S*eps
X<-xts(X,order.by=Date)
T<-xts(T,order.by=Date)
S<-xts(S,order.by=Date)
plot(X)
lines(T,col='red')
lines(T*S,col='blue')








date1<- strptime("01/01/1900", "%m/%d/%Y")
date2<- strptime("01/01/2000", "%m/%d/%Y")
Date<-seq.POSIXt(date1,date2,by = "year")

n<-length(Date)
t<-c(1:n)

T<-t/20+1

w=2*pi/5
S<-cos(w*t)
eps<-rnorm(n,0,1)


par(mfrow=c(2,1))
X<-T+S+eps
X<-xts(X,order.by=Date)
T<-xts(T,order.by=Date)
S<-xts(S,order.by=Date)
plot(X)
lines(T,col='red')
lines(T+S,col='blue')



X<-T*S*eps
X<-xts(X,order.by=Date)
T<-xts(T,order.by=Date)
S<-xts(S,order.by=Date)
plot(X)
lines(T,col='red')
lines(T*S,col='blue')



############################################################################################################################################
###################################exercice 4: PV
############################################################################################################################################
Data1<-readRDS("TP1_data/Solar1.RDS")
head(Data1)


###prod. moyenne par mois, heure
AvMonProd=tapply(Data1$Z1,as.factor(Data1$Mois),mean)
barplot(AvMonProd,col="palegoldenrod")

AvHourProd=tapply(Data1$Z1,as.factor(Data1$Heure),mean)
barplot(AvHourProd,col="palevioletred")

AvHourMonthProd=tapply(Data1$Z1,as.factor(Data1$Mois):as.factor(Data1$Heure),mean)
AvHourMonthProd=matrix(AvHourMonthProd,nrow=12,ncol=24,byrow=T)
matplot(t(AvHourMonthProd),type='l',col=rainbow(12))

###########transformation des variables Ã  visualiser en objets xts
Z1=xts(Data1$Z1,order.by=Data1$date)
Ssrd=xts(Data1$Ssrd,order.by=Data1$date)
Tsr=xts(Data1$Tsr,order.by=Data1$date)
Tcc=xts(Data1$Tcc,order.by=Data1$date)
Ssrd.diff<-diff.xts(Ssrd)
Ssrd.diff[which(Data1$Heure==12)]<-Ssrd[which(Data1$Heure==12)]

#standardisation
Z1.sd=Z1/sd(Z1)
Ssrd.diff.sd=Ssrd.diff/sd(Ssrd.diff)


###########standardisation des variables ? visualiser
sd.time.series=cbind(Z1.sd,Ssrd.diff.sd)

names(sd.time.series)=c("Z1.sd","Ssrd.diff.sd")
##dygraph de base
dygraph(sd.time.series)

##avec fenÃªtre 
dygraph(sd.time.series)%>% dyRangeSelector()   ##equivalent?  dyRangeSelector(dygraph(sd.time.series))

