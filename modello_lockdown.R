#Modello a intercetta casuale per effetto lockdown

library(refund)

yfd=pm10_lisciata$fd
y<-t(eval.fd(1:731,yfd))
temp<-t(eval.fd(1:731,dati_lisciati$temp$fd))
wind<-t(eval.fd(1:731,dati_lisciati$wind$fd))
no2<-t(eval.fd(1:731,dati_lisciati$no2$fd))
so2<-t(eval.fd(1:731,dati_lisciati$so2$fd))
co<-t(eval.fd(1:731,dati_lisciati$co$fd))
prec<-t(eval.fd(1:731,dati_lisciati$prec$fd))
loc<-dati_grezzi2$loc

y_19<-y[,1:365]
temp_19<-temp[,1:365]
wind_19<-wind[,1:365]
prec_19<-prec[,1:365]
no2_19<-prec[,1:365]
so2_19<-prec[,1:365]
co_19<-prec[,1:365]

y_20<-y[,366:730]
temp_20<-temp[,366:730]
wind_20<-wind[,366:730]
prec_20<-prec[,366:730]

y_19_20<-rbind(y_19,y_20)
temp_19_20<-rbind(temp_19,temp_20)
wind_19_20<-rbind(wind_19,wind_20)
prec_19_20<-rbind(prec_19,prec_20)

par(mfrow=c(1,1))
matplot(t(rbind(dati_grezzi2$pm10[,1:365],dati_grezzi2$pm10[,366:730])),type='l',col=c(rep(1,30),rep(2,30)))

matplot(t(y_19_20),type='l',col=c(rep(1,30),rep(2,30)),lty=1,lwd=2,ylab='log(PM10)',xlab='Giorno',cex.lab=1.5,cex.main=2)
legend(x=180,y=4.15,c('2019','2020'),lwd=3,col=1:2,box.lty = 0,cex=1.5)

anno<-factor(c(rep('2019',30),rep('2020',30)))
loc_intercept<-factor(rep(loc,2))
city<-c("AL","AN","AO","AQ","BA","BO","BS","BZ","CA","CB","CZ","FE","FI","GE",
        "MI","NA","PA","PD","PG","PR","PZ","RA","RO","TN","TO","TR","UD","VE","VI","VR")
city_intercept<-factor(rep(city,2))


lisc=c(-1,1e10,-1,-1,-1,-1)
m0_lock=pffr(y_19_20~anno+temp_19_20+wind_19_20+prec_19_20,sp=lisc)
summary(m0_lock)
plot(m0_lock)
par(mar=c(5,5,4,2))
par(mfrow=c(3,2))
plot(m0_lock,select=1,xlab='Giorno',main='Intercetta',ylab=expression(mu~'(t)'),cex.lab=1.5,cex.main=2)
abline(h=0,lty=2)
plot(m0_lock,select=3,xlab='Giorno',main='Anno (2020)',ylab=expression(beta[Anno]~'(t)'),cex.lab=1.5,cex.main=2)
abline(h=0,lty=2)
abline(v=69,lwd=0.3)
abline(v=139,lwd=0.3)
plot(m0_lock,select=4,xlab='Giorno',main='Temperatura',ylab=expression(beta[temp]~'(t)'),cex.lab=1.5,cex.main=2)
abline(h=0,lty=2)
plot(m0_lock,select=5,xlab='Giorno',main='Velocità del vento',ylab=expression(beta[wind]~'(t)'),cex.lab=1.5,cex.main=2)
abline(h=0,lty=2)
plot(m0_lock,select=6,xlab='Giorno',main='Precipitazioni',ylab=expression(beta[prec]~'(t)'),cex.lab=1.5,cex.main=2)
abline(h=0,lty=2)


m0_city=pffr(y_19_20~anno+temp_19_20+wind_19_20+prec_19_20+s(city_intercept,bs='re'))
summary(m0_city)
par(mfrow=c(2,3))

save(m0_city,file='m_randeff_completo.RData')

par(mar=c(5,5,4,2))
par(mfrow=c(3,2))
plot(m0_city,select=1,xlab='Giorno',main='Intercetta',ylab=expression(mu~'(t)'),cex.lab=1.5,cex.main=2)
abline(h=0,lty=2)
plot(m0_city,select=3,xlab='Giorno',main='Anno (2020)',ylab=expression(beta[Anno]~'(t)'),cex.lab=1.5,cex.main=2)
abline(h=0,lty=2)
abline(v=69,lwd=0.3)
abline(v=139,lwd=0.3)
plot(m0_city,select=4,xlab='Giorno',main='Temperatura',ylab=expression(beta[temp]~'(t)'),cex.lab=1.5,cex.main=2)
abline(h=0,lty=2)
plot(m0_city,select=5,xlab='Giorno',main='Velocità del vento',ylab=expression(beta[wind]~'(t)'),cex.lab=1.5,cex.main=2)
abline(h=0,lty=2)
plot(m0_city,select=6,xlab='Giorno',main='Precipitazioni',ylab=expression(beta[prec]~'(t)'),cex.lab=1.5,cex.main=2)
abline(h=0,lty=2)

m0_city$smooth$`ti(city_intercept,yindex.vec)`[[1]][[1]][[11]]
#curve delle intercette casuali funzionali
par(mfrow=c(1,2))
matplot(t(predict(m0_city,type='terms')$`ti(city_intercept,yindex.vec)`),type='l',col=loc,ylim=c(-1.5,1),
        xlab='Giorno', ylab='città(t)',main='Intercette casuali',cex.lab=1.5,cex.main=2)

res.lock<-y_19_20-fitted(m0_lock)
res.city<-y_19_20-fitted(m0_city)
par(mfrow=c(1,2))
matplot(t(res.lock),type='l',ylim=c(-1.5,1))
matplot(t(res.city),type='l',ylim=c(-1.5,1),
        xlab='Giorno', ylab='residui(t)',main='Residui',cex.lab=1.5,cex.main=2)

# andamento R2 funzionale
rsq.lock<-apply(res.lock,2,var)/apply(y_19_20,2,var)
rsq.city<-apply(res.city,2,var)/apply(y_19_20,2,var)

par(mfrow=c(1,1))
plot(1-rsq.lock,type='l',ylim=c(-0.5,1),main=expression(R^2~' funzionale'),ylab=expression(R^2),lwd=2,xlab='Giorno',cex.lab=1.5,cex.main=2) 
lines(1-rsq.city,col=2,lwd=2)
legend(x=240,y=-.05,c('senza intercetta casuale','con intercetta casuale'),lwd=3,col=1:2,box.lty = 0,cex=1.5)


#commento su modello per verificare l'effetto del lockdown:
#si osserva che nel caso di modello simultaneo e covariata scalare indicante l'anno 
#di registrazione della curva nel 2020 si osserva un livello di PM10 inferiore nel periodo
#dell'anno corrispondente al periodo del lockdown (leggermente traslato) e 
#complessivamente la differenza tra i due anni è significativa. 
#Tuttavia va considerato che il modello contiene due osservazioni relative a due anni consecutivi
#per ogni città rilevata, il che implica una struttura di correlazione tra coppie di curve che non 
#andrebbe trascurata. Pertanto si prende in considerazione la possibilità di aggiungere un effetto
#casuale per la città. Le stime degli effetti fissi restano invariate, a eccezione dell'intercetta
#globale (plausibile) e della variabile temperatura il cui effetto cambia di segno (normale?).
#Le curve delle previsioni delle intercette casuali sembrano spiegare una variabilità discreta
#anche rapportata alla variabilità residua

