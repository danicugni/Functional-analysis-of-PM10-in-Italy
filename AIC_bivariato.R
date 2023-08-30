rm(list=ls())

library(refund)

dati_aic_sim <- list( y = y, temp = temp, wind = wind, no2 = no2, so2 = so2, co = co, prec = prec)

#Procedura backward----
nomi <- c("y", "temp", "wind", "no2", "so2", "co", "prec")
nomi
AIC.global <- c()
AIC.local <- c()

#load('/Users/leonardogenesin/Desktop/unipd_magistrale/Metodi Statistici per Dati Funzionali/progetto_fda/m_biv_completo.RData')
#mod.completo <- pffr(y ~ ff(temp) + ff(wind) + ff(no2) + ff(so2) + ff(co) + ff(prec))
mod.completo<-pffr(y~ff(temp,xind=1:731)+ff(wind,xind=1:731)+ff(no2,xind=1:731)+ff(so2,xind=1:731)+
                     ff(co, xind = 1:731)+ff(prec, xind = 1:731))
#save(mod.completo,file='mod_completo_biv.RData')
#sp<-mod.completo$sp
#sp.nomi<-c("intercept","temp","temp","wind","wind","no2","no2","so2","so2","co","co","prec","prec")
AIC.global <- c(AIC.global, AIC(mod.completo))
AIC.local[[1]] <- AIC.global
df <- round(summary(mod.completo)$edf,2)

p <- length(dati_aic_sim)
cond <- FALSE
i <- 2
out <- c()

while(cond == FALSE & i < p) {
  dati.tmp <- c()
  index.tmp <- setdiff(1:p, out)
  nomi.tmp <- nomi[index.tmp]
  #sp.tmp<-sp[c(1,which(sp.nomi %in% nomi.tmp))]
  AIC.tmp <- c()
  cat("i: ", i, "\n")
  for(z in index.tmp[-1]) { 
    formula.tmp <- as.formula(paste("y ~", paste(paste("ff(",nomi.tmp[-c(1,z)],",xind=1:731)"), collapse = "+", sep = " ")))
    print(formula.tmp)
    AIC.tmp <- c(AIC.tmp, AIC(pffr(formula.tmp)))
    print(AIC.tmp)
    cat("z : ", z, "\n")
  }
  AIC.local[[i]] <- AIC.tmp
  AIC.global <- c(AIC.global, min(AIC.tmp))
  if(AIC.global[length(AIC.global)] > AIC.global[length(AIC.global) -1])
    cond <- TRUE
  out <- c(out, index.tmp[-1][which.min(AIC.tmp)])
  i <- i + 1
}

AIC.global <- c(AIC.global, AIC(pffr(y ~ 1, data = dati_aic_sim)))
AIC.local[[1]] <- AIC.global

rm(m0_city)
rm(m0_cityfact)
rm(m0_lock)
rm(m0)
rm(mod.nullo)
rm(mod.tmp)
rm(m.red)
rm(m.completo)
rm(co_lisciata)
rm(no2_lisciata)
rm(pm10_lisciata)
rm(prec_lisciata)
rm(wind_lisciata)
rm(so2_lisciata)
rm(temp_lisciata)
rm(yfd)
rm(mod.completo)

rm(wind_1920)
rm(temp_1920)
rm(prec_1920)
rm(no2_1920)
rm(pm10_1920)
rm(so2_1920)
rm(prec_marzo)
rm(dati_lisciati)
rm(co_1920)
rm(dati_grezzi)
rm(dati_grezzi2)
rm(hourly.date.prec)
rm(hourly.date1)
rm(hourly.date2)

#Procedura forward----

nomi <- c("y", "temp", "wind", "no2", "so2", "co", "prec")
nomi
AIC.global <- c()
AIC.local <- c()
mod.nullo <- pffr(y ~ 1, data = dati_aic_sim)
# sp <- matrix(-1, nrow=7,ncol=2)
# sp1 <- mod.nullo$sp
# sp

AIC.global <- c(AIC.global, AIC(mod.nullo))
AIC.local[[1]] <- AIC.global

p <- length(dati_aic_sim)
cond <- FALSE
i <- 2
incluse <- c()

while(cond == FALSE & i <= p) {
  dati.tmp <- c()
  index.tmp <- setdiff(1:p, incluse)
  nomi.incluse.tmp <- nomi[-index.tmp]
  nomi.escluse.tmp <- nomi[index.tmp]
  AIC.tmp <- c()
  cat("i: ", i, "\n")
  for(z in index.tmp[-1]) {
    formula.tmp <- as.formula(paste("y ~", paste(paste("ff(",c(nomi.incluse.tmp, nomi[z]),",xind=1:731)"), collapse = "+", sep = " ")))
    print(formula.tmp)
    mod.tmp <- pffr(formula.tmp)
    AIC.tmp <- c(AIC.tmp, AIC(mod.tmp))
    print(AIC.tmp)
    #sp[z,] <- mod.tmp$sp[c(length(mod.tmp$sp)-1,length(mod.tmp$sp))]
    #print(sp)
    cat("z : ", z, "\n")
  }
  AIC.local[[i]] <- AIC.tmp
  AIC.global <- c(AIC.global, min(AIC.tmp))
  if(AIC.global[length(AIC.global)] > AIC.global[length(AIC.global) -1]){
    cond <- TRUE
    index.tmp <- setdiff(1:p, incluse)
    nomi.incluse.finali <- nomi[-index.tmp]
  }
  incluse <- c(incluse, index.tmp[-1][which.min(AIC.tmp)])
  i <- i + 1
  if(i > p)
    nomi.incluse.finali <- nomi[-index.tmp]
  gc()
}

save.image('AIC_bivariato_giusto.RData')
#load('AIC_bivariato.RData')


plot(AIC.global,type='b',lwd=2, col = 2, pch = 19, ylab = "AIC", xlab = "Numero di covariate",
     cex.lab=1.2,cex.main=2)
points(x = rep(2, 6), AIC.local[[2]])
points(x = rep(3, 5), AIC.local[[3]])
points(x = rep(4, 4), AIC.local[[4]])
points(x = rep(5, 3), AIC.local[[5]])
points(x = rep(6, 2), AIC.local[[6]])

