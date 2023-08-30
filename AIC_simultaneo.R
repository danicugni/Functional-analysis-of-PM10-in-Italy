library(refund)

dati_aic_sim <- list( y = y, temp = temp, wind = wind, no2 = no2, so2 = so2, co = co, prec = prec)

#Procedura backward----
nomi <- c("y", "temp", "wind", "no2", "so2", "co", "prec")
nomi
AIC.global <- c()
AIC.local <- c()

mod.completo <- pffr(y ~ temp + wind + no2 + so2 + co + prec ,bs.yindex = list(bs='ps',k=8), data = dati_aic_sim)
#sp<-mod.completo$sp
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
  #sp.tmp<-sp[index.tmp]
  AIC.tmp <- c()
  cat("i: ", i, "\n")
  for(z in index.tmp[-1]) {
    formula.tmp <- as.formula(paste("y ~", paste(nomi.tmp[-c(1,z)], collapse = "+", sep = " ")))
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


#Procedura forward----

nomi <- c("y", "temp", "wind", "no2", "so2", "co", "prec")
nomi
AIC.global <- c()
AIC.local <- c()
mod.nullo <- pffr(y ~ 1, data = dati_aic_sim)
#sp <- rep(-1, p)
#sp[1] <- mod.nullo$sp
#sp
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
    formula.tmp <- as.formula(paste("y ~", paste(c(nomi.incluse.tmp, nomi[z]), collapse = "+", sep = " ")))
    print(formula.tmp)
    mod.tmp <- pffr(formula.tmp,bs.yindex = list(bs='ps',k=8))
    AIC.tmp <- c(AIC.tmp, AIC(mod.tmp))
    print(AIC.tmp)
    #sp[z] <- mod.tmp$sp[length(mod.tmp$sp)]
    #print(sp)
    cat("z : ", z, "\n")
  }
  AIC.local[[i]] <- AIC.tmp
  AIC.global <- c(AIC.global, min(AIC.tmp))  
  incluse <- c(incluse, index.tmp[-1][which.min(AIC.tmp)])
  if(AIC.global[length(AIC.global)] > AIC.global[length(AIC.global) -1]){
    cond <- TRUE
    index.tmp <- setdiff(1:p, incluse)
    nomi.incluse.finali <- nomi[-index.tmp]
  }
  i <- i + 1
  if(i > p)
    nomi.incluse.finali <- nomi[-1]
}

#load('AIC_simultaneo.RData')

par(mfrow=c(1,1))
plot(AIC.global,type='b',lwd=2, col = 2, pch = 19, ylab = "AIC", xlab = "Numero di covariate",
     cex.lab=1.2,cex.main=2)
points(x = rep(2, 6), AIC.local[[2]],lwd=1)
points(x = rep(3, 5), AIC.local[[3]],lwd=1)
points(x = rep(4, 4), AIC.local[[4]],lwd=1)
points(x = rep(5, 3), AIC.local[[5]],lwd=1)
points(x = rep(6, 2), AIC.local[[6]],lwd=1)

#save.image(file='AIC_simultaneo.RData')
