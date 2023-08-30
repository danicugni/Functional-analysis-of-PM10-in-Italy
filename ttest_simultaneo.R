## Implementazione a mano di un test di permutazione
# Funzione che calcola la statistica test
Tt = function(model,j){
  beta_hat=coef(model)$smterms[[j]]$coef$value
  beta_hat.se=coef(model)$smterms[[j]]$coef$se
  beta_hat/beta_hat.se
}

# Implementazione del test di permutazione
B = 100
Tb = numeric(B)
set.seed(1)

#Modello ridotto (vero sotto H0)
m.red=pffr(y~wind+no2+so2+co+prec)
res=y-fitted(m.red)

for(b in 1:B){
  res.perm = res[sample(1:nrow(y)),]
  y.perm=fitted(m.red)+res.perm
  mtmp = pffr(y.perm~wind+no2+so2+co+prec+temp)
  Tb[b]=max(Tt(mtmp,length(coef(mtmp)$smterms)))
  cat(b,"\n")
}

m.completo=pffr(y~wind+no2+so2+co+prec+temp)
Toss=max(Tt(m.completo,length(coef(m.completo)$smterms)))
plot(m.completo)
plot(coef(m.completo)$smterms[[7]]$coef$value)

# salvo risultato
q = quantile(Tb, 0.95)
# 95% 

par(mfrow=c(1,1))
plot(Tt(m.completo, length(coef(m.completo)$smterms)),type='l')
abline(h = q, col = 3, lty = 3)
