rm(list=objects())

# Packages
library(zoo)
library(timeDate)
library(xts)
library(dygraphs)

# Dossier de travail (à adapter)
data_path <- "TP1_data"

##############################################################################
# Exercice 1 — Production de bière
##############################################################################

# 1. Importation des données
beer_raw <- read.csv(file.path(data_path, "beer2.csv"), 
                     header = TRUE, skip = 1)

# Exploration rapide
head(beer_raw)
str(beer_raw)
summary(beer_raw)
plot(beer_raw$BeerProd, type = "b", pch = 20,
     main = "Production de bière", ylab = "Production")

# 2. Création de la variable Date
date_start <- as.POSIXct("1991-01-01")
date_end   <- as.POSIXct("1995-08-01")
Date <- seq(from = date_start, to = date_end, by = "month")

stopifnot(length(Date) == nrow(beer_raw))

beer <- data.frame(
  Date = Date,
  BeerProd = beer_raw$BeerProd
)

# Visualisation temporelle
plot(beer$Date, beer$BeerProd, type = "l",
     main = "Production mensuelle de bière")

# 3. Objets ts et zoo
beer_ts  <- ts(beer$BeerProd, start = 1991, frequency = 12)
beer_zoo <- zoo(beer$BeerProd, order.by = beer$Date)

plot(beer_ts, main = "Classe ts")
plot(beer_zoo, main = "Classe zoo")

# 4. Statistiques descriptives
mean(beer$BeerProd)
sd(beer$BeerProd)
boxplot(beer$BeerProd, main = "Distribution")
hist(beer$BeerProd, breaks = 10, main = "Histogramme")

# 5. Moyennes annuelles et mensuelles
year  <- format(beer$Date, "%Y")
month <- format(beer$Date, "%m")

mean_year  <- tapply(beer$BeerProd, year, mean)
mean_month <- tapply(beer$BeerProd, month, mean)

plot(mean_year, type = "b", main = "Moyenne annuelle")
plot(mean_month, type = "b", main = "Moyenne mensuelle")

print("Conclusion : La série semble non-stationnaire; tendance décroissante et périodicité annuelle forte.")

##############################################################################
# Exercice 2 — Consommation électrique
##############################################################################

# 1. Importation des données
conso_raw <- read.table(file.path(data_path, "conso_2015.csv"),
                        header = TRUE, sep = ";")
# 2. Format des données
str(conso_raw)
print("Le jeu est trié par instant : ce n'est pas adéquat...")

# 3. Création de la date (pas de 30 min)
date_start <- as.POSIXct("2015-01-01 00:00:00")
date_end   <- as.POSIXct("2015-11-30 23:30:00")
Date <- seq(from = date_start, to = date_end, by = "30 min")

# Mise en forme des données
X <- as.matrix(t(conso_raw[, -1]))
conso <- as.numeric(X)

conso_xts <- xts(conso, order.by = Date)

# Visualisation
plot(conso_xts, main = "Consommation électrique")
dygraph(conso_xts) %>% dyRangeSelector()

# 4. Calculs de Statistiques par mois / jour / heure
month <- as.factor(.indexmon(conso_xts))
dow   <- as.factor(.indexwday(conso_xts))
hour  <- as.factor(.indexhour(conso_xts))

mean_month <- tapply(conso_xts, month, mean)
mean_dow   <- tapply(conso_xts, dow, mean)
mean_dow_hour <- tapply(conso_xts, dow:hour, mean)

plot(mean_month, type = "b", main = "Moyenne mensuelle")
plot(mean_dow, type = "b", main = "Moyenne par jour")
plot(mean_dow_hour, type = "l", main = "Profil jour/heure")

abline(v = seq(1, 24 * 7, by = 24), col = "red")

# 5. Boxplots de la consommation mensuelle à 20h
sel_20h <- .indexhour(conso_xts) == 20

boxplot(conso_xts[sel_20h] ~ month[sel_20h],
        main = "Consommation à 20h par mois",
        xlab = "Mois",
        ylab = "Consommation",
        col  = "lightblue")

# 6. Autocorrélation et autocorrélation partielle
lags <- 1:336

# Autocorrélation
autoCorr <- function(x, h) {
  x_lag <- lag.xts(x, k = h, na.pad = TRUE)
  cor(x_lag, x, use = "pairwise.complete.obs")
}
a1 <- sapply(lags, autoCorr,  x = conso_xts)

plot(a1, type = "h", ylim = c(0, 1),
     main = "Autocorrélation")
a2 <- acf(conso_xts, lag.max = 336, type = "correlation", plot = FALSE)
lines(a2$acf[-1], col = "red")

# Autocorrélation Partielle
PartialAutoCorr <- function(x, h) {
  x_lag <- lapply(1:h, lag.xts, x = x, na.pad = TRUE)
  x_lag <- matrix(unlist(x_lag), ncol = length(x_lag))
  reg <- lm(x ~ x_lag - 1)
  tail(reg$coef, 1)
}
pa1 <- sapply(lags, PartialAutoCorr, x = conso_xts)

plot(pa1, type = "b", pch = ".",
     main = "Autocorrélations partielles")
pa2 <- pacf(conso, lag.max = 336, plot = FALSE)
lines(pa2$acf, col = "red")

##############################################################################
# Exercice 3 — Simulation
##############################################################################

# Echauffement :
# a. Série périodique
t <- 1:200
w <- 2 * pi / 12
x <- cos(w * t) + rnorm(length(t))
plot(x, type = "l", main = "Série périodique simulée")
acf(x, lag.max = 50)

# b. Série avec tendance
x <- t / 10 + rnorm(length(t))
plot(x, type = "l", main = "Tendance linéaire")
acf(x)
pacf(x)

# 1. Simulation de la série demandée
date_start <- as.POSIXct("1986-03-01")
date_end   <- as.POSIXct("2002-04-01")
Date <- seq(from = date_start, to = date_end, by = "month")

n <- length(Date)
t <- 1:n

Tt <- log(t / 10 + 1)
S <- cos(2 * pi * t / 12)
eps <- rnorm(n)

X <- xts(Tt + S + eps, order.by = Date)
plot(X, main = "Tendance + saisonnalité")
lines(xts(Tt, order.by = Date), col = "red")
lines(xts(S, order.by = Date), col = "blue")

# 2. Extraire et superposer sur le même graphique 3 années
extract_year <- function(y, X) {
  year <- format(index(X), "%Y")
  as.numeric(X[year == y])
}

years_sel <- c(1987, 1990, 1993)

plot(extract_year(1987, X),
     type = "l", ylim = range(X),
     main = "Comparaison de plusieurs années",
     ylab = "X_t")

lines(extract_year(1990, X), col = "red")
lines(extract_year(1993, X), col = "blue")

legend("topleft",
       legend = years_sel,
       col = c("black", "red", "blue"),
       lty = 1)

# 3. Même exercice pour un modèle multiplicatif
X_mult <- Tt * S * eps

X_mult <- xts(X_mult, order.by = Date)

plot(X_mult, main = "Modèle multiplicatif")


##############################################################################
# Exercice 4 — Production photovoltaïque
##############################################################################

Data1 <- readRDS(file.path(data_path, "Solar1.RDS"))

# 1. Pas de temps
View(Data1) # Pas de temps horaire

# 2. Production moyenne, par heure, par mois, puis par mois et heure
AvMonProd  <- tapply(Data1$Z1, Data1$Mois, mean)
AvHourProd <- tapply(Data1$Z1, Data1$Heure, mean)
AvMonHouProd <- tapply(Data1$Z1, as.factor(Data1$Mois):as.factor(Data1$Heure), mean)

barplot(AvMonProd, main = "Production moyenne mensuelle")
barplot(AvHourProd, main = "Production moyenne horaire")

AvHourMonthProd <- matrix(AvMonHouProd,
                          nrow = 12, ncol = 24, byrow = TRUE)

matplot(t(AvHourMonthProd),
        type = "l", lty = 1,
        main = "Production moyenne horaire par mois",
        xlab = "Heure", ylab = "Production")

legend("topright",
       legend = month.abb,
       col = 1:12, lty = 1, cex = 0.8)

# 3. Formatage rayonnement solaire
Z1   <- xts(Data1$Z1,   order.by = Data1$date)
Ssrd <- xts(Data1$Ssrd, order.by = Data1$date)

Ssrd_diff <- diff.xts(Ssrd)
Ssrd_diff[Data1$Heure == 12] <- Ssrd[Data1$Heure == 12]

# 4. Représentation par dygraph ()
Z1_sd        <- Z1 / sd(Z1)
Ssrd_diff_sd <- Ssrd_diff / sd(Ssrd_diff)

sd_series <- cbind(Z1_sd, Ssrd_diff_sd)
names(sd_series) <- c("Z1_sd", "Ssrd_diff_sd")

dygraph(sd_series) %>% dyRangeSelector()

