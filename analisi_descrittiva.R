source("lisciamento.R")

par(mfrow = c(1,1))

#media funzionale no2----
#no2_lisciata$fd$fdnames$time <- no2_1920$Giorno
mean.no2 <- mean.fd(no2_lisciata$fd)
plot(no2_lisciata$fd) 
lines(mean.no2, lwd = 4, lty = 2)

#media funzionale co----
mean.co <- mean.fd(co_lisciata$fd)
plot(co_lisciata) 
lines(mean.co, lwd = 4, lty = 2)

#media funzionale temperatura----
mean.temp <- mean.fd(temp_lisciata$fd)
plot(temp_lisciata) 
lines(mean.temp, lwd = 4, lty = 2)

#media funzionale so2----
mean.so2 <- mean.fd(so2_lisciata$fd)
plot(so2_lisciata) 
lines(mean.so2, lwd = 4, lty = 2)

#media funzionale pm10----
mean.pm10 <- mean.fd(pm10_lisciata$fd)
plot(pm10_lisciata) 
lines(mean.pm10, lwd = 4, lty = 2)

#media funzionale vento----
mean.wind <- mean.fd(wind_lisciata$fd)
plot(wind_lisciata) 
lines(mean.wind, lwd = 4, lty = 2)

#standard deviation funzionale no2----
std.no2 <- std.fd(no2_lisciata$fd)
plot(std.no2)

par(mfrow = c(1,2))
plot(no2_lisciata, ylab = "no2", xlab = "Giorno", main = "Media funzionale No2") 
lines(mean.no2, lwd = 4, lty = 2)
plot(std.no2, ylab = "standard error funzionale No2", xlab = "Giorno",
     main = "Standard error funzionale No2")

#standard deviation funzionale co----
std.co <- std.fd(co_lisciata$fd)
plot(std.co)

par(mfrow = c(1,2))
plot(co_lisciata, ylab = "co", xlab = "Giorno", main = "Media funzionale Co") 
lines(mean.co, lwd = 4, lty = 2)
plot(std.co, ylab = "standard error funzionale Co", xlab = "Giorno",
     main = "Standard error funzionale Co")

#standard deviation funzionale temperatura----
std.temp <- std.fd(temp_lisciata$fd)
plot(std.temp)

par(mfrow = c(1,2))
plot(temp_lisciata, ylab = "Temperatura", xlab = "Giorno", main = "Media funzionale temperatura") 
lines(mean.temp, lwd = 4, lty = 2)
plot(std.temp, ylab = "standard error funzionale temperatura", xlab = "Giorno",
     main = "Standard error funzionale temperatura")

#standard deviation funzionale so2----
std.so2 <- std.fd(so2_lisciata$fd)
plot(std.so2)

par(mfrow = c(1,2))
plot(so2_lisciata, ylab = "so2", xlab = "Giorno", main = "Media funzionale So2") 
lines(mean.so2, lwd = 4, lty = 2)
plot(std.so2, ylab = "standard error funzionale So2", xlab = "Giorno",
     main = "Standard error funzionale So2")

#standard deviation funzionale pm10----
std.pm10 <- std.fd(pm10_lisciata$fd)
plot(std.pm10)

par(mfrow = c(1,2))
plot(pm10_lisciata, ylab = "pm10", xlab = "Giorno", main = "Media funzionale Pm10") 
lines(mean.pm10, lwd = 4, lty = 2)
plot(std.pm10, ylab = "standard error funzionale Pm10", xlab = "Giorno",
     main = "Standard error funzionale Pm10")

#standard deviation funzionale vento----
std.wind <- std.fd(wind_lisciata$fd)
plot(std.wind)

par(mfrow = c(1,2))
plot(wind_lisciata, ylab = "vento", xlab = "Giorno", main = "Media funzionale vento") 
lines(mean.wind, lwd = 4, lty = 2)
plot(std.wind, ylab = "standard error funzionale vento", xlab = "Giorno",
     main = "Standard error funzionale vento")


#covarianza funzionale no2----
var.no2 <- var.fd(no2_lisciata$fd)
grid <- seq(1,731,10)
varcov_no2 <- eval.bifd(grid, grid, var.no2)
par(mfrow = c(1,1))
persp(grid, grid, varcov_no2, theta = -45, phi = 25, r = 2,
      expand = 0.5, xlab = "Giorno", ylab = "Giorno",zlab = "Covarianza", ticktype = "detailed",
      main = "Covarianza funzionale No2")
contour(grid, grid, varcov_no2)

#covarianza funzionale co----
var.co <- var.fd(co_lisciata$fd)
grid <- seq(1,731,10)
varcov_co <- eval.bifd(grid, grid, var.co)
par(mfrow = c(1,1))
persp(grid, grid, varcov_co, theta = -45, phi = 25, r = 2,
      expand = 0.5, xlab = "Giorno", ylab = "Giorno", zlab = "Covarianza", ticktype = "detailed",
      main = "Covarianza funzionale Co")
contour(grid, grid, varcov_co)

#covarianza funzionale temperatura----
var.temp <- var.fd(temp_lisciata$fd)
grid <- seq(1,731,10)
varcov_temp <- eval.bifd(grid, grid, var.temp)
par(mfrow = c(1,1))
persp(grid, grid, varcov_temp, theta = -45, phi = 25, r = 2,
      expand = 0.5, xlab = "Giorno", ylab = "Giorno", zlab = "Covarianza", ticktype = "detailed",
      main = "Covarianza funzionale temperatura")
contour(grid, grid, varcov_temp)

#covarianza funzionale wind----
var.wind <- var.fd(wind_lisciata$fd)
grid <- seq(1,731,10)
varcov_wind <- eval.bifd(grid, grid, var.wind)
par(mfrow = c(1,1))
persp(grid, grid, varcov_wind, theta = -45, phi = 25, r = 2,
      expand = 0.5, xlab = "Giorno", ylab = "Giorno", zlab = "Covarianza",ticktype = "detailed",
      main = "Covarianza funzionale vento")
contour(grid, grid, varcov_wind)

#covarianza funzionale pm10----
var.pm10 <- var.fd(pm10_lisciata$fd)
grid <- seq(1,731,10)
varcov_pm10 <- eval.bifd(grid, grid, var.pm10)
par(mfrow = c(1,1))
persp(grid, grid, varcov_pm10, theta = -45, phi = 25, r = 2,
      expand = 0.5, xlab = "Giorno", ylab = "Giorno", zlab = "Covarianza",ticktype = "detailed",
      main = "Covarianza funzionale Pm10")
contour(grid, grid, varcov_pm10)

#covarianza funzionale so2----
var.so2 <- var.fd(so2_lisciata$fd)
grid <- seq(1,731,10)
varcov_so2 <- eval.bifd(grid, grid, var.so2)
par(mfrow = c(1,1))
persp(grid, grid, varcov_so2, theta = -45, phi = 25, r = 2,
      expand = 0.5, xlab = "Giorno", ylab = "Giorno", zlab = "Covarianza",ticktype = "detailed",
      main = "Covarianza funzionale So2")
contour(grid, grid, varcov_so2)
#correlazione funzionale tra so2 e wind----
grid <- seq(1,731,10)
corr.so2wind <- cor.fd(grid, so2_lisciata$fd, grid, wind_lisciata$fd)
par(mfrow = c(1,1))
persp(grid, grid, corr.so2wind, theta = -45, phi = 25, r = 2,
      expand = 0.5, xlab = "Giorno", ylab = "Giorno", zlab = "Correlazione",ticktype = "detailed",
      main = "Correlazione funzionale tra So2 e vento")
contour(grid, grid, corr.so2wind)
max(abs(corr.so2wind))
min(abs(corr.so2wind))

#correlazione funzionale tra no2 e co----
grid <- seq(1,731,10)
corr.no2co <- cor.fd(grid, no2_lisciata$fd, grid, co_lisciata$fd)
par(mfrow = c(1,1))
persp(grid, grid, corr.no2co, theta = -45, phi = 25, r = 2,
      expand = 0.5, xlab = "Giorno", ylab = "Giorno", zlab = "Correlazione",ticktype = "detailed",
      main = "Correlazione funzionale tra No2 e Co")
contour(grid, grid, corr.no2co)
max(abs(corr.no2co))
min(abs(corr.no2co))

par(mfrow = c(1,1))

#boxplot funzionale no2----
no2_box <- boxplot.fd(no2_lisciata$fd, method = "BD2") #banda rossa racchiude il 50%
plot(no2_box$depth) #profondità di ciascuna funzione

#boxplot funzionale co----
co_box <- boxplot.fd(co_lisciata$fd) #banda rossa racchiude il 50%
plot(co_box$depth) #profondità di ciascuna funzione

#boxplot funzionale temperatura----
temp_box <- boxplot.fd(temp_lisciata$fd) #banda rossa racchiude il 50%
plot(temp_box$depth) #profondità di ciascuna funzione

#boxplot funzionale vento----
wind_box <- boxplot.fd(wind_lisciata$fd, method = "BD2") #banda rossa racchiude il 50%
plot(wind_box$depth) #profondità di ciascuna funzione

#boxplot funzionale pm10----
pm10_box <- boxplot.fd(pm10_lisciata$fd, method = "BD2") #banda rossa racchiude il 50%
plot(pm10_box$depth) #profondità di ciascuna funzione

#boxplot funzionale so2----
so2_box <- boxplot.fd(so2_lisciata$fd) #banda rossa racchiude il 50%
plot(so2_box$depth) #profondità di ciascuna funzione
