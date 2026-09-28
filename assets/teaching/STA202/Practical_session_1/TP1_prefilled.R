rm(list = objects())

# Packages
library(zoo)
library(timeDate)
library(xts)
library(dygraphs)

# Dossier de travail (à adapter)
data_path <- [...]

##############################################################################
# Exercice 1 — Production de bière
##############################################################################

# 1. Importation des données
beer_raw <- read.csv(file.path([...]), 
                     header = TRUE, skip = 1)

# Exploration rapide
head([...])
str([...])
summary([...])
plot([...], type = "b", pch = 20,
     main = "Production de bière", ylab = "Production")

# 2. Création de la variable Date
date_start <- [...]
date_end   <- [...]
Date <- seq([...])

stopifnot(length(Date) == nrow(beer_raw))

beer <- data.frame(
  Date = Date,
  BeerProd = beer_raw$BeerProd
)

# Visualisation temporelle
plot([...], [...], type = "l",
     main = "Production mensuelle de bière")

# 3. Objets ts et zoo
beer_ts  <- [...]
beer_zoo <- [...]

plot(beer_ts, main = "Classe ts")
plot(beer_zoo, main = "Classe zoo")

# 4. Statistiques descriptives
mean([...])
sd([...])
boxplot([...], main = "Distribution")
hist([...], main = "Histogramme")

# 5. Moyennes annuelles et mensuelles
year  <- [...]
month <- [...]

mean_year  <- tapply([...])
mean_month <- tapply([...])

plot(mean_year, type = "b", main = "Moyenne annuelle")
plot(mean_month, type = "b", main = "Moyenne mensuelle")

print("Conclusion : La série semble non-stationnaire ; tendance et saisonnalité marquées.")

##############################################################################
# Exercice 2 — Consommation électrique
##############################################################################

# 1. Importation des données
conso_raw <- read.table(file.path(data_path, [...]),
                        header = TRUE, sep = [...])

# Exploration
str(conso_raw)
print("[...]")

# 2. Création de la date (pas de 30 minutes)
date_start <- [...]
date_end   <- [...]
Date <- seq(from = [...], to = [...], by = [...])

# 3. Mise en forme des données
X <- as.matrix(t(conso_raw[, [...]]))
conso <- as.numeric(X)

conso_xts <- xts(conso, order.by = [...])

# Visualisation
plot([...], main = "Consommation électrique")
dygraph([...]) %>% dyRangeSelector()

# 4. Statistiques par mois / jour / heure
month <- as.factor([...])
dow   <- as.factor([...])
hour  <- as.factor([...])

mean_month <- tapply([...])
mean_dow   <- tapply([...])
mean_dow_hour <- tapply([...])

plot(mean_month, type = "b", main = "Moyenne mensuelle")
plot(mean_dow, type = "b", main = "Moyenne par jour")
plot(mean_dow_hour, type = "l", main = "Profil jour/heure")

abline(v = seq(1, [...], by = [...]), col = "red")

# 5. Boxplot de la consommation à 20h
sel_20h <- [...]

boxplot([...],
        main = "Consommation à 20h par mois",
        xlab = "Mois",
        ylab = "Consommation")

# 6. Autocorrélation empirique
lags <- 1:336

autoCorr <- function(x, h) {
  x_lag <- [...]
  [...]
}

a1 <- sapply(lags, autoCorr, x = [...])

plot(a1, type = "h", ylim = c(0, 1),
     main = "Autocorrélation empirique")

# Comparaison avec acf()
a2 <- acf([...], lag.max = 336, plot = FALSE)
lines([...], col = "red")

# 7. Autocorrélation partielle empirique
PartialAutoCorr <- function(x, h) {
  x_lag <- [...]
  [...]
  reg <- [...]
  [...]
}

pa1 <- sapply(1:50, PartialAutoCorr, x = [...])
plot(pa1, type = "b", pch = ".",
     main = "Autocorrélations partielles empiriques")

##############################################################################
# Exercice 3 — Simulation
##############################################################################

# 1. Série périodique
t <- 1:200
w <- [...]
x <- [...]
plot(x, type = "l", main = "Série périodique")
acf(x, lag.max = 50)

# 2. Série avec tendance
x <- [...]
plot(x, type = "l", main = "Tendance")
acf(x)
pacf(x)

# 3. Modèle additif
Date <- seq(from = [...], to = [...], by = [...])
n <- length(Date)
t <- 1:n

Tt  <- [...]
St  <- [...]
eps <- [...]

X <- xts([...], order.by = [...])
plot(X)

# 4. Superposition des années 1987, 1990, 1993
extract_year <- function(y, X) {
  [...]
}

plot(extract_year([...], X), type = "l")
lines(extract_year([...], X), col = "red")
lines(extract_year([...], X), col = "blue")

# 5. Modèle multiplicatif
X_mult <- xts([...], order.by = [...])
plot(X_mult, main = "Modèle multiplicatif")

##############################################################################
# Exercice 4 — Production photovoltaïque
##############################################################################

Data1 <- readRDS(file.path(data_path, [...]))

# 1. Moyennes
AvMonProd  <- tapply([...])
AvHourProd <- tapply([...])
AvMonHouProd <- tapply([...])

barplot(AvMonProd)
barplot(AvHourProd)

AvHourMonthProd <- matrix([...], nrow = 12, ncol = 24, byrow = TRUE)
matplot(t(AvHourMonthProd), type = "l")

# 2. Séries xts
Z1   <- xts([...])
Ssrd <- xts([...])

Ssrd_diff <- diff.xts([...])
Ssrd_diff[[...]] <- [...]

# 3. Visualisation normalisée
Z1_sd <- [...]
Ssrd_diff_sd <- [...]

sd_series <- cbind([...])
dygraph(sd_series) %>% dyRangeSelector()
