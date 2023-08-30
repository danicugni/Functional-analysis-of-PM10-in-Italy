#load("dati_finali.RData")

library(fda)

matplot(no2_1920$Giorno ,no2_1920[,-which(colnames(no2_1920) == "Giorno")], type = "l", xlab = "Giorno",
        ylab = "No2", main = "No2")
matplot(co_1920$Giorno,co_1920[,-which(colnames(co_1920) == "Giorno")], type = "l", xlab = "Giorno",
        ylab = "Co", main = "Co")
matplot(temp_1920$Giorno,temp_1920[,-which(colnames(temp_1920) == "Giorno")], type = "l", xlab = "Giorno",
        ylab = "Temperatura", main = "Temperatura")
matplot(so2_1920$Giorno,so2_1920[,-which(colnames(so2_1920) == "Giorno")], type = "l", xlab = "Giorno",
        ylab = "So2", main = "So2")
matplot(pm10_1920$Giorno,pm10_1920[,-which(colnames(pm10_1920) == "Giorno")], type = "l", xlab = "Giorno",
        ylab = "Pm10", main = "Pm10")
matplot(wind_1920$Giorno, wind_1920[,-which(colnames(wind_1920) == "Giorno")], type = "l", xlab = "Giorno",
        ylab = "Velocità del vento", main = "Velocità del vento")
matplot(prec_1920$Giorno,prec_1920[,-which(colnames(prec_1920) == "Giorno")], type = "l", xlab = "Giorno",
        ylab = "Precipitazioni", main = "Precipitazioni")

#Lisciamento no2----
bb <- create.bspline.basis(c(1, nrow(no2_1920)),norder = 4, nbasis = 50)
no2_lisciata <- smooth.basis(1:nrow(no2_1920), as.matrix(no2_1920[,-c(31,32)]), bb)
par(mfrow = c(1,2))
plot(no2_lisciata) 
abline(h = 0, lty = 2)
matplot(no2_1920[,-which(colnames(no2_1920) == "Giorno")], type = "l")
abline(h=0, lty = 2)

#Lisciamento co----
bb <- create.bspline.basis(c(1, nrow(co_1920)), nbasis=32)
co_lisciata <- smooth.basis(1:nrow(co_1920), as.matrix(co_1920[,-c(31,32)]), bb)
par(mfrow = c(1,2))
plot(co_lisciata) 
matplot(co_1920[,-which(colnames(co_1920) == "Giorno")], type = "l")
abline(v=365)

#Lisciamento temperatura----
bb <- create.fourier.basis(c(1, nrow(temp_1920)), 25,period=365)
temp_lisciata <- smooth.basis(1:nrow(temp_1920), as.matrix(temp_1920[,-c(31,32)]), bb)
par(mfrow = c(1,2))
plot(temp_lisciata) 
matplot(temp_1920[,-which(colnames(temp_1920) == "Giorno")], type = "l")
abline(h=0, lty = 2)

#Lisciamento so2----
bb <- create.bspline.basis(c(1, nrow(so2_1920)), nbasis = 40)
so2_lisciata <- smooth.basis(1:nrow(so2_1920), as.matrix(so2_1920[,-c(31,32)]), bb)
par(mfrow = c(1,2))
plot(so2_lisciata) 
matplot(so2_1920[,-which(colnames(so2_1920) == "Giorno")], type = "l")

#Lisciamento pm10 ----
bb <- create.bspline.basis(c(1, nrow(pm10_1920)), nbasis = 30)
pm10_lisciata <- smooth.basis(1:nrow(pm10_1920), as.matrix(pm10_1920[,-c(31,32)]), bb)
par(mfrow = c(1,2))
plot(pm10_lisciata) 
plot(eval.fd(1:731,pm10_lisciata$fd))

matplot(pm10_1920[,-which(colnames(pm10_1920) == "Giorno")], type = "l")

matplot(pm10_1920[,which(colnames(pm10_1920) == "pm10_BZ")], type = "l",ylim=c(1,4))

#Lisciamento velocità del vento----
bb <- create.bspline.basis(c(1, nrow(wind_1920)), nbasis = 80)
wind_lisciata <- smooth.basis(1:nrow(wind_1920), as.matrix(wind_1920[,-c(31,32)]), bb)
par(mfrow = c(1,2))
plot(wind_lisciata) 
matplot(wind_1920[,-which(colnames(wind_1920) == "Giorno")], type = "l")

#Lisciamento precipitazioni----
bb <- create.bspline.basis(rangeval = c(1,731), norder = 1, nbasis = 731) 
prec_lisciata <- smooth.basis(1:nrow(prec_1920), as.matrix(prec_1920[,-31]), bb)

dati_lisciati<-list(pm10=pm10_lisciata,co=co_lisciata,no2=no2_lisciata,so2=so2_lisciata,
                    prec=prec_lisciata,temp=temp_lisciata,wind=wind_lisciata)

dati_lisciati$loc=c('NO','C','NO','C','S','NE','NO','NE','S','S','S','NE','C','NO',
                    'NO','S','S','NE','C','NE','S','NE','C','NE','NO','NE','NE','NE','NE','NE')

dati_lisciati$loc=factor(dati_lisciati$loc)  

  
  
  