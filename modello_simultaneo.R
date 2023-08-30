#Modello simultaneo

################################################################################
## CON REFUND

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

m0=pffr(y~temp+wind+prec+no2+so2+co,bs.yindex = list(bs='ps',k=8))

?pffr
matplot(t(dati_grezzi2$pm10),type='l')
matplot(t(fitted(m0)),type='l')

plot(m0)
par(mar=c(5,5,4,2))
par(mfrow=c(2,2))
plot(m0,select=1,xlab='Anno',main='Intercetta',ylab=expression(mu~'(t)'),cex.lab=1.5,cex.main=2,xaxt='n')
axis(1, at = c(0,365, 731), labels = c("2019", "2020", "2021"))
abline(h=0,lty=2)
plot(m0,select=2,xlab='Anno',main='Temperatura',ylab=expression(beta[temp]~'(t)'),cex.lab=1.5,cex.main=2,xaxt='n')
axis(1, at = c(0,365, 731), labels = c("2019", "2020", "2021"))
abline(h=0,lty=2)
plot(m0,select=3,xlab='Anno',main='Velocità del vento',ylab=expression(beta[wind]~'(t)'),cex.lab=1.5,cex.main=2,xaxt='n')
axis(1, at = c(0,365, 731), labels = c("2019", "2020", "2021"))
abline(h=0,lty=2)
plot(m0,select=4,xlab='Anno',main='Precipitazioni',ylab=expression(beta[prec]~'(t)'),cex.lab=1.5,cex.main=2,xaxt='n')
axis(1, at = c(0,365, 731), labels = c("2019", "2020", "2021"))
abline(h=0,lty=2)
plot(m0,select=5,xlab='Anno',main='No2',ylab=expression(beta[No2]~'(t)'),cex.lab=1.5,cex.main=2,xaxt='n')
axis(1, at = c(0,365, 731), labels = c("2019", "2020", "2021"))
abline(h=0,lty=2)
plot(m0,select=6,xlab='Anno',main='So2',ylab=expression(beta[So2]~'(t)'),cex.lab=1.5,cex.main=2,xaxt='n')
axis(1, at = c(0,365, 731), labels = c("2019", "2020", "2021"))
abline(h=0,lty=2)
plot(m0,select=7,xlab='Anno',main='Co',ylab=expression(beta[Co]~'(t)'),cex.lab=1.5,cex.main=2,xaxt='n')
axis(1, at = c(0,365, 731), labels = c("2019", "2020", "2021"))
abline(h=0,lty=2)


res<-y-fitted(m0)
matplot(t(res),type='l')

# andamento R2 funzionale
rsq<-apply(res,2,var)/apply(y,2,var)
par(mfrow=c(1,1))
plot(1-rsq,type='l') 


##############################################################
# con FDA

#variabile risposta
yfd=pm10_lisciata$fd

#covariate
xlist=list()
xlist[[1]] = rep(1, ncol(dati_grezzi$pm10)-1)
xlist[[2]] = prec_lisciata$fd
xlist[[3]] = temp_lisciata$fd
xlist[[4]] = wind_lisciata$fd
xlist[[5]] = no2_lisciata$fd
xlist[[6]] = so2_lisciata$fd
xlist[[7]] = co_lisciata$fd

#beta
D2 = int2Lfd(2)
basi = create.bspline.basis(c(1,731), 15)

betalist = lapply(1:7, function(x)  fdPar(basi))

m0 = fRegress(yfd,xfdlist=xlist,betalist=betalist)
par(mfrow = c(3,3))
lapply(m0$betaestlist, plot)

################ 
#regolazione del primo
xtmp = xlist[[1]]
l_grid = 10^seq(-1,6,l=15)
err = numeric(length(l_grid))
for(i in 1:length(l_grid)){
  betatmp = list(fdPar(basi,D2, l_grid[i]))
  mtmp = fRegress.CV(yfd,xtmp,betatmp)
  err[i] = mtmp$SSE.CV
  cat(i, "\n" )
}
par(mfrow=c(1,1))
plot(log(l_grid), err,type='b')
l_grid[which.min(err)]

betatmp = list(fdPar(basi,D2, l_grid[which.min(err)]))
m0 = fRegress(yfd,xtmp,betatmp)
lapply(m0$betaestlist, plot)

## regolazione del secondo
betalist[[1]] = fdPar(basi, D2,l_grid[which.min(err)])
betatmp = list(betalist[[1]],betalist[[2]])
l_grid = 10^seq(2,10,l=15)
err = numeric(length(l_grid))
xtmp = list(xlist[[1]],xlist[[3]])

for(i in 1:length(l_grid)){
  betatmp[[2]] = fdPar(basi,D2, l_grid[i])
  mtmp = fRegress.CV(yfd, xtmp,betatmp)
  err[i] = mtmp$SSE.CV
  cat(i, "\n" )
}
plot(log(l_grid), err)
l_grid[which.min(err)]

## regolazione del terzo
betalist[[2]] = fdPar(basi, D2, l_grid[which.min(err)])
betatmp = list(betalist[[1]],betalist[[2]],betalist[[3]])
l_grid = 10^seq(2,10,l=15)
err = numeric(length(l_grid))
xtmp = list(xlist[[1]],xlist[[3]],xlist[[4]])

for(i in 1:length(l_grid)){
  betatmp[[3]] = fdPar(basi,D2, l_grid[i])
  mtmp = fRegress.CV(yfd, xtmp,betatmp)
  err[i] = mtmp$SSE.CV
  cat(i, "\n" )
}
plot(log(l_grid), err)
l_grid[which.min(err)]

betatmp[[3]] = fdPar(basi, D2, l_grid[which.min(err)])
m0 = fRegress(yfd, xtmp, betatmp)
par(mfrow = c(2,2))
lapply(m0$betaestlist,plot)

## regolazione del quarto
betalist[[3]] = fdPar(basi, D2, l_grid[which.min(err)])
betatmp = list(betalist[[1]],betalist[[2]],betalist[[3]],betalist[[4]])
l_grid = 10^seq(1,10,l=15)
err = numeric(length(l_grid))
xtmp = list(xlist[[1]],xlist[[3]],xlist[[4]],xlist[[5]])

for(i in 1:length(l_grid)){
  betatmp[[4]] = fdPar(basi,D2, l_grid[i])
  mtmp = fRegress.CV(yfd, xtmp,betatmp)
  err[i] = mtmp$SSE.CV
  cat(i, "\n" )
}
plot(log(l_grid), err)
l_grid[which.min(err)]

betatmp[[4]] = fdPar(basi, D2, l_grid[which.min(err)])
m0 = fRegress(yfd, xtmp, betatmp)
par(mfrow = c(2,2))
lapply(m0$betaestlist,plot)


################
#Regolazione per numero di basi

#regolazione del primo
xtmp = xlist[[1]]
basi_grid<-15:25
err = numeric(length(basi_grid))
for(i in 1:length(basi_grid)){
  basi=create.bspline.basis(c(1,731), basi_grid[i])
  betatmp = list(fdPar(basi))
  mtmp = fRegress.CV(yfd,xtmp,betatmp)
  err[i] = mtmp$SSE.CV
  cat(i, "\n" )
}
par(mfrow=c(1,1))
plot(basi_grid, err,type='b')
basi_grid[which.min(err)]

basi=create.bspline.basis(c(1,731), basi_grid[which.min(err)])
betatmp = list(fdPar(basi))
m0 = fRegress(yfd,xtmp,betatmp)
lapply(m0$betaestlist, plot)
