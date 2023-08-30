## Modello con effetti bivariati

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

m0_biv=pffr(y~ff(temp,xind=1:731)+ff(wind,xind=1:731)+ff(no2,xind=1:731)+ff(so2,xind=1:731)+
              ff(co, xind = 1:731)+ff(prec, xind = 1:731))

load('mod_completo_biv.RData')
m0_biv<-mod.completo
rm(mod.completo)

par(mfrow=c(2,2))

plot(m0_biv,scale=F,pers=T,theta=0,select=1,xaxt='n',main='Intercetta',ylab=expression(mu~'(t)'),cex.lab=1.5,cex.main=2,xlab='Anno')
axis(1, at = c(0,365, 731), labels = c("2019", "2020", "2021"))
plot(m0_biv,scale=F,scheme=2,theta=0,select=2,cex.lab=1.5,cex.main=2,main='Temperatura',
     xlab='s',ylab='t')
axis(1, at = c(1,365, 731), labels = c("2019", "2020", "2021"))
axis(2, at = c(1,365, 731), labels = c("2019", "2020", "2021"))
abline(0,1,lwd=0.7)
plot(m0_biv,scale=F,scheme=2,theta=0,select=3,cex.lab=1.5,cex.main=2,main='Velocità del vento',
     xlab='s',ylab='t',xaxt='n',yaxt='n',legend=T)
axis(1, at = c(1,365, 731), labels = c("2019", "2020", "2021"))
axis(2, at = c(1,365, 731), labels = c("2019", "2020", "2021"))
abline(0,1,lwd=0.7)
plot(m0_biv,scale=F,scheme=2,theta=0,select=7,cex.lab=1.5,cex.main=2,main='Precipitazioni',
     xlab='s',ylab='t',xaxt='n',yaxt='n')
axis(1, at = c(1,365, 731), labels = c("2019", "2020", "2021"))
axis(2, at = c(1,365, 731), labels = c("2019", "2020", "2021"))
abline(0,1,lwd=0.7)
plot(m0_biv,scale=F,scheme=2,theta=0,select=4,cex.lab=1.5,cex.main=2,main='No2',
     xlab='s',ylab='t',xaxt='n',yaxt='n')
axis(1, at = c(1,365, 731), labels = c("2019", "2020", "2021"))
axis(2, at = c(1,365, 731), labels = c("2019", "2020", "2021"))
abline(0,1,lwd=0.7)
plot(m0_biv,scale=F,scheme=2,theta=0,select=5,cex.lab=1.5,cex.main=2,main='So2',
     xlab='s',ylab='t',xaxt='n',yaxt='n')
axis(1, at = c(1,365, 731), labels = c("2019", "2020", "2021"))
axis(2, at = c(1,365, 731), labels = c("2019", "2020", "2021"))
abline(0,1,lwd=0.7)
plot(m0_biv,scale=F,scheme=2,theta=0,select=6,cex.lab=1.5,cex.main=2,main='Co',
     xlab='s',ylab='t',xaxt='n',yaxt='n')
axis(1, at = c(1,365, 731), labels = c("2019", "2020", "2021"))
axis(2, at = c(1,365, 731), labels = c("2019", "2020", "2021"))
abline(0,1,lwd=0.7)


par(mfrow=c(1,2))
matplot(t(y),type='l')
matplot(t(fitted(m0_biv)),type='l')

res<-y-fitted(m0_biv)
matplot(t(res),type='l')

# andamento R2 funzionale
rsq<-apply(res,2,var)/apply(y,2,var)
par(mfrow=c(1,1))
plot(1-rsq,type='l') 

#save(m0_biv,file='m_biv_completo.RData')


