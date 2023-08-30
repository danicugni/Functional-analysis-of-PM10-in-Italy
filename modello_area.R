#Modelli

matplot(dati_grezzi$pm10[,-31],type='l',col=as.numeric(dati_grezzi$loc))

loc.matrix<-model.matrix(~-1+dati_grezzi$loc)
colnames(loc.matrix)<-c('C','NE','NO','S')

xlist<-lapply(1:(ncol(loc.matrix)-1),function(x) loc.matrix[,x])

xint = list(c(rep(1,30),0))
# vincolo risposta
By = cbind(dati_lisciati$pm10$fd$coefs, 0)
bb <- create.bspline.basis(c(1, 731), nbasis = 30)
yfd = fd(By,bb,dati_lisciati$pm10$fd$fdnames)
xlist = lapply(1:NCOL(loc.matrix), function(x) c(loc.matrix[,x],1))
#xlist[[1]][31] = 0
xlist = c(xint,xlist)

#basi beta
D2 = int2Lfd(2)

basi = create.bspline.basis(c(1,731), 15)
betalist<-list()
lambda=5e6
for(j in 1:length(xlist)) betalist[[j]] = fdPar(basi,D2,lambda)

l_grid = 5*seq(1e3, 6e4, l = 15)
err = numeric(length(l_grid))

#selezione del parametro di lisciamento 
for(i in 1:length(l_grid)){
  for(j in 1:length(betalist)) betalist[[j]] = fdPar(basi, D2, l_grid[i])
  mCV = fRegress.CV(yfd,xlist,betalist,CVobs = 1:30)
  err[i] = mCV$SSE.CV
  cat(i, "\n")
}

plot(log(l_grid[-c(1,2)]),err[-c(1,2)],type='b',cex.lab=1.5,xlab=expression('log('~lambda~')'),ylab='Errore')
for(j in 1:length(xlist)) betalist[[j]] = fdPar(basi,D2,l_grid[which.min(err)])

m1 = fRegress(yfd,xlist,betalist)
par(mfrow = c(1,1))
plot(m1$betaestlist[[1]]$fd,ylim=c(2.15,3.6),lwd=2,xaxt='n',xlab='Anno',ylab='log(PM10)',cex.lab=1.2,cex.main=2)
axis(1,at=c(0,365,731),labels=c('2019','2020','2021'))
lines(m1$betaestlist[[1]]$fd+m1$betaestlist[[2]]$fd,col=2,lty=2,lwd=2)
lines(m1$betaestlist[[1]]$fd+m1$betaestlist[[3]]$fd,col=4,lty=2,lwd=2)
lines(m1$betaestlist[[1]]$fd+m1$betaestlist[[4]]$fd,col=3,lty=2,lwd=2)
lines(m1$betaestlist[[1]]$fd+m1$betaestlist[[5]]$fd,col=7,lty=2,lwd=2)
legend(x=450,y=3.65,legend=c('Mean','Nord-Ovest','Nord-Est','Centro','Sud e Isole'),col=c(1,3,4,2,7),lwd=2,
       lty=c(1,rep(2,4)),cex=1.05,box.lty = 0)

#osservazione: le zone geografiche sembrano mostrare uno scostamento dall'andamento
#medio principalmente nel periodo invernale, in cui Nord-Est e Nord-Ovest hanno livelli
#di pm10 maggiori mentre il centro e in particolare il sud hanno livelli minori

#### Intervalli di confidenza
res<-yfd-m1$yhatfdobj
y_fd<-eval.fd(1:731,yfd)
yhatmat<-eval.fd(1:731,m1$yhatfdobj)
res<-y_fd-yhatmat
sigma_eps<-var(t(res))
beta_se<-fRegress.stderr(m1,pm10_lisciata$y2cMap,sigma_eps)
beta_std=beta_se$betastderrlist

#Graficamente
par(mar=c(5,5,4,2))
par(mfrow=c(3,2))
plot(m1$betaestlist[[1]]$fd,ylim=c(2.2,3.3),xaxt='n',main='Intercetta',ylab=expression(mu~'(t)'),cex.lab=1.5,cex.main=2)
axis(1, at = c(0,365, 731), labels = c("2019", "2020", "2021"))
lines(m1$betaestlist[[1]]$fd+qnorm(0.975)*beta_std[[1]],lty=3)
lines(m1$betaestlist[[1]]$fd-qnorm(0.975)*beta_std[[1]],lty=3)
plot(m1$betaestlist[[4]],ylim=c(-0.2,0.5),xaxt='n',main='Nord-Ovest',ylab=expression('area'[1]~'(t)'),cex.lab=1.5,cex.main=2)
axis(1, at = c(0,365, 731), labels = c("2019", "2020", "2021"))
lines(m1$betaestlist[[4]]$fd+qnorm(0.975)*beta_std[[1]],lty=3)
lines(m1$betaestlist[[4]]$fd-qnorm(0.975)*beta_std[[1]],lty=3)
plot(m1$betaestlist[[3]],ylim=c(-0.2,0.6),xaxt='n',main='Nord-Est',ylab=expression('area'[2]~'(t)'),cex.lab=1.5,cex.main=2)
axis(1, at = c(0,365, 731), labels = c("2019", "2020", "2021"))
lines(m1$betaestlist[[3]]$fd+qnorm(0.975)*beta_std[[1]],lty=3)
lines(m1$betaestlist[[3]]$fd-qnorm(0.975)*beta_std[[1]],lty=3)
plot(m1$betaestlist[[2]],ylim=c(-0.4,0.2),xaxt='n',main='Centro',ylab=expression('area'[3]~'(t)'),cex.lab=1.5,cex.main=2)
axis(1, at = c(0,365, 731), labels = c("2019", "2020", "2021"))
lines(m1$betaestlist[[2]]$fd+qnorm(0.975)*beta_std[[1]],lty=3)
lines(m1$betaestlist[[2]]$fd-qnorm(0.975)*beta_std[[1]],lty=3)
plot(m1$betaestlist[[5]],ylim=c(-0.7,0.3),xaxt='n',main='Sud e Isole',ylab=expression('area'[4]~'(t)'),cex.lab=1.5,cex.main=2)
axis(1, at = c(0,365, 731), labels = c("2019", "2020", "2021"))
lines(m1$betaestlist[[5]]$fd+qnorm(0.975)*beta_std[[1]],lty=3)
lines(m1$betaestlist[[5]]$fd-qnorm(0.975)*beta_std[[1]],lty=3)

#osservazione: considerando gli intervalli di confidenza puntuali, l'unica parte dell'anno
#in cui emergono differenze significativamente diverse dal comportamento medio è il periodo invernale
#in cui in particolare il Sud mostra un livello di di pm10 inferiore, mentre il NE superiore

#R^2 funzionale
par(mfrow=c(1,2))
rsq<-apply(res,1,var)/apply(y_fd,1,var)
plot(1-rsq,type='l',main=expression(R^2~' funzionale'),ylab=expression(R^2),xaxt='n',xlab='Anno',ylim=c(0.6,1),cex.lab=1.5,cex.main=2) 
axis(1, at = c(0,365, 731), labels = c("2019", "2020", "2021"))

#osservazione: il modello ha una buona capacità esplicativa su tutto il periodo considerato,
#in particolare nei periodi estivi

#Test F di permutazione 
#tt<-Fperm.fd(yfd,xlist,betalist,plotres = T,xaxt='n',ylab='statistica F')
plot(tt$Fvals,type='l',col=2,lwd=2,ylim=c(0,12.5),xaxt='n',xlab='Anno',ylab='statistica F',main='Test F di permutazione',cex.lab=1.5,cex.main=2)
points(tt$qvals.pts,type='l',lty=2,col='lightblue',lwd=2)
abline(h=tt$qval,type='l',lty=2,col=4,lwd=2)
axis(1, at = c(0,50, 100), labels = c("2019", "2020", "2021"))
legend(x=50,y=12.8,c('statistica F osservata', 'pointwise 0.05 critical','maximum 0.05 critical'),
       lty=c(1,2,2),lwd=2,col=c(2,'lightblue',4),cex=0.8,box.lty=0)
#osservazione: la distinzione delle osservazioni basata sulla zona geografica risulta
#significativa sia a livello puntuale sia a livello globale nel modello
