rm(list=ls())
setwd("/Users/leonardogenesin/Desktop/unipd_magistrale/Metodi Statistici per Dati Funzionali/progetto_fda/dataset")

hourly.date <- seq(as.POSIXlt("2019-01-01 00:00:00"), 
                   as.POSIXlt("2020-12-31 23:00:00"),
                   by="hour")
head(hourly.date)
tail(hourly.date)
hourly.date <- format(hourly.date, "%m/%d/%Y")
head(hourly.date)
tail(hourly.date)
hourly.date <- as.Date(hourly.date, "%m/%d/%Y")
head(hourly.date)
tail(hourly.date)
length(which(hourly.date == "2019-01-01"))
length(which(hourly.date == "2020-12-31"))

hourly.date1 <- seq(as.POSIXlt("2019-01-01 00:00:00"), 
                   as.POSIXlt("2020-02-29 23:00:00"),
                   by="hour")
head(hourly.date)
tail(hourly.date)
hourly.date1 <- format(hourly.date1, "%m/%d/%Y")
head(hourly.date1)
tail(hourly.date1)
hourly.date1 <- as.Date(hourly.date1, "%m/%d/%Y")
head(hourly.date1)
tail(hourly.date1)
length(which(hourly.date1 == "2019-01-01"))
length(which(hourly.date1 == "2020-02-29"))

hourly.date2 <- seq(as.POSIXlt("2020-04-01 00:00:00"), 
                    as.POSIXlt("2020-12-31 23:00:00"),
                    by="hour")
head(hourly.date2)
tail(hourly.date2)
hourly.date2 <- format(hourly.date2, "%m/%d/%Y")
head(hourly.date2)
tail(hourly.date2)
hourly.date2 <- as.Date(hourly.date2, "%m/%d/%Y")
head(hourly.date2)
tail(hourly.date2)
length(which(hourly.date2 == "2020-04-01"))
length(which(hourly.date2 == "2020-12-31"))
table(hourly.date2)
hourly.date.prec<-c(hourly.date1,hourly.date2)

#Precipitazioni----
load("prec_2019.RData")
names(prec_2019)
load("prec_2020.RData")
names(prec_2020)


prec_1920 <- as.data.frame(rbind(as.matrix(prec_2019),
                                 as.matrix(prec_2020)))
prec_1920<-prec_1920[-(10201:10854),]
prec_1920 <- cbind(prec_1920, hourly.date.prec)

rm(prec_2019)
dim(prec_1920)
colnames(prec_1920) <- c("prec_AL", "prec_AN", "prec_AO", "prec_AQ","prec_BA", "prec_BO",
                         "prec_BS", "prec_BZ", "prec_CA", "prec_CB", "prec_CZ", "prec_FE",
                         "prec_FI", "prec_GE","prec_MI", "prec_NA", "prec_PA",
                         "prec_PD", "prec_PG", "prec_PR", "prec_PZ", "prec_RA", "prec_RO",
                         "prec_TN", "prec_TO", "prec_TR", "prec_UD","prec_VE", "prec_VI",
                         "prec_VR", "Data oraria")
colnames(prec_1920)
#matplot(prec_1920[,-which(colnames(prec_1920) == "Anno")], type = "l")

#Co2----
load("co_2019.RData")
names(co_2019)
load("co_2020.RData")
names(co_2020)

dim(co_2019)
co_1920 <- as.data.frame(rbind(as.matrix(co_2019), 
                       as.matrix(co_2020)))
co_1920 <- cbind(co_1920, hourly.date)
rm(co_2019)
rm(co_2020)
dim(co_1920)
colnames(co_1920) <- c("co_AL", "co_AN", "co_AO", "co_AQ","co_BA", "co_BO", "co_BS", 
                       "co_BZ", "co_CA", "co_CB", "co_CZ", "co_FE", "co_FI", "co_GE",
                       "co_MI", "co_NA", "co_PA", "co_PD", "co_PG", "co_PR", 
                       "co_PZ", "co_RA", "co_RO", "co_TN", "co_TO", "co_TR", "co_UD",
                       "co_VE", "co_VI", "co_VR", "Data oraria")
colnames(co_1920)
#matplot(co_1920[,-which(colnames(co_1920) == "Anno")], type = "l")


#Pm10----
load("pm10_2019.RData")
names(pm10_2019)
load("pm10_2020.RData")
names(pm10_2020)

pm10_1920 <- as.data.frame(rbind(as.matrix(log(pm10_2019)), 
                               as.matrix(log(pm10_2020))))
pm10_1920 <- cbind(pm10_1920, hourly.date)
rm(pm10_2019)
rm(pm10_2020)
dim(pm10_1920)
colnames(pm10_1920) <- c("pm10_AL", "pm10_AN", "pm10_AO", "pm10_AQ","pm10_BA", "pm10_BO",
                         "pm10_BS", "pm10_BZ", "pm10_CA", "pm10_CB", "pm10_CZ", "pm10_FE",
                         "pm10_FI", "pm10_GE","pm10_MI", "pm10_NA", "pm10_PA", 
                         "pm10_PD", "pm10_PG", "pm10_PR", "pm10_PZ", "pm10_RA", "pm10_RO",
                         "pm10_TN", "pm10_TO", "pm10_TR", "pm10_UD","pm10_VE", "pm10_VI",
                         "pm10_VR", "Data oraria")
colnames(pm10_1920)
#matplot(pm10_1920[,-which(colnames(pm10_1920) == "Anno")], type = "l")

#Temperatura----
library(weathermetrics)
load("temp_2019.RData")
names(temp_2019)
temp_2019 <- round(kelvin.to.celsius(temp_2019),1)
load("temp_2020.RData")
names(temp_2020)
temp_2020 <- round(kelvin.to.celsius(temp_2020),1)

temp_1920 <- as.data.frame(rbind(as.matrix(temp_2019), 
                                 as.matrix(temp_2020)))
temp_1920 <- cbind(temp_1920, hourly.date)
rm(temp_2019)
rm(temp_2020)
dim(temp_1920)
colnames(temp_1920) <- c("temp_AL", "temp_AN", "temp_AO", "temp_AQ","temp_BA", "temp_BO",
                         "temp_BS", "temp_BZ", "temp_CA", "temp_CB", "temp_CZ", "temp_FE",
                         "temp_FI", "temp_GE","temp_MI", "temp_NA", "temp_PA", 
                         "temp_PD", "temp_PG", "temp_PR", "temp_PZ", "temp_RA", "temp_RO",
                         "temp_TN", "temp_TO", "temp_TR", "temp_UD","temp_VE", "temp_VI",
                         "temp_VR", "Data oraria")
colnames(temp_1920)
#matplot(temp_1920[,-which(colnames(temp_1920) == "Anno")], type = "l")

#Velocità del vento----
load("wind_2019.RData")
names(wind_2019)
load("wind_2020.RData")
names(wind_2020)
imp <- apply(wind_2019, 2, mean)
imp
wind_1920 <- as.data.frame(rbind(rbind(log(imp), log(imp)), as.matrix(log(wind_2019)), 
                                as.matrix(log(wind_2020))))
wind_1920 <- cbind(wind_1920, hourly.date)
rm(wind_2019)
rm(wind_2020)
rm(imp)
dim(wind_1920)
colnames(wind_1920) <- c("wind_AL", "wind_AN", "wind_AO", "wind_AQ","wind_BA", "wind_BO",
                         "wind_BS", "wind_BZ", "wind_CA", "wind_CB", "wind_CZ", "wind_FE",
                         "wind_FI", "wind_GE","wind_MI", "wind_NA", "wind_PA", 
                         "wind_PD", "wind_PG", "wind_PR", "wind_PZ", "wind_RA", "wind_RO",
                         "wind_TN", "wind_TO", "wind_TR", "wind_UD","wind_VE", "wind_VI",
                         "wind_VR", "Data oraria")
colnames(wind_1920)
matplot(wind_1920[,-which(colnames(wind_1920) == "Data oraria")], type = "l")

#So2----
load("so2_2019.RData")
names(so2_2019)
load("so2_2020.RData")
names(so2_2020)

so2_1920 <- as.data.frame(rbind(as.matrix(so2_2019), 
                                 as.matrix(so2_2020)))
so2_1920 <- cbind(so2_1920, hourly.date)
rm(so2_2019)
rm(so2_2020)
dim(so2_1920)
colnames(so2_1920) <- c("so2_AL", "so2_AN", "so2_AO", "so2_AQ","so2_BA", "so2_BO",
                         "so2_BS", "so2_BZ", "so2_CA", "so2_CB", "so2_CZ", "so2_FE",
                         "so2_FI", "so2_GE","so2_MI", "so2_NA", "so2_PA", 
                         "so2_PD", "so2_PG", "so2_PR", "so2_PZ", "so2_RA", "so2_RO",
                         "so2_TN", "so2_TO", "so2_TR", "so2_UD","so2_VE", "so2_VI",
                         "so2_VR", "Data oraria")
colnames(so2_1920)
#matplot(so2_1920[,-which(colnames(so2_1920) == "Anno")], type = "l")

#No2----
load("no2_2019.RData")
names(no2_2019)

load("no2_2020.RData")
names(no2_2020)

no2_1920 <- as.data.frame(rbind(as.matrix(no2_2019), 
                                as.matrix(no2_2020)))
no2_1920 <- cbind(no2_1920, hourly.date)
rm(no2_2019)
rm(no2_2020)
dim(no2_1920)
colnames(no2_1920) <- c("no2_AL", "no2_AN", "no2_AO", "no2_AQ","no2_BA", "no2_BO",
                        "no2_BS", "no2_BZ", "no2_CA", "no2_CB", "no2_CZ", "no2_FE",
                        "no2_FI", "no2_GE","no2_MI", "no2_NA", "no2_PA", 
                        "no2_PD", "no2_PG", "no2_PR", "no2_PZ", "no2_RA", "no2_RO",
                        "no2_TN", "no2_TO", "no2_TR", "no2_UD","no2_VE", "no2_VI",
                        "no2_VR", "Data oraria")
colnames(no2_1920)
#matplot(no2_1920[,-which(colnames(no2_1920) == "Anno")], type = "l")

#Aggregazione giornaliera----

co_1920.new <- matrix(NA, nrow = 731, ncol = ncol(co_1920)-1)
no2_1920.new <- matrix(NA, nrow = 731, ncol = ncol(no2_1920)-1)
pm10_1920.new <- matrix(NA, nrow = 731, ncol = ncol(pm10_1920)-1)
prec_1920.new <- matrix(NA, nrow = 700, ncol = ncol(prec_1920)-1)
so2_1920.new <- matrix(NA, nrow = 731, ncol = ncol(so2_1920)-1)
temp_1920.new <- matrix(NA, nrow = 731, ncol = ncol(temp_1920)-1)
wind_1920.new <- matrix(NA, nrow = 731, ncol = ncol(wind_1920)-1)

for(i in 1:(ncol(co_1920)-1)) {
  co_1920.new[,i] <- aggregate(co_1920[,i] ~ co_1920[,ncol(co_1920)], data = co_1920, 
                           FUN = mean)[,2]
  no2_1920.new[,i] <- aggregate(no2_1920[,i] ~ no2_1920[,ncol(no2_1920)], data = no2_1920, 
                               FUN = mean)[,2]
  pm10_1920.new[,i] <- aggregate(pm10_1920[,i] ~ pm10_1920[,ncol(pm10_1920)], data = pm10_1920, 
                               FUN = mean)[,2]
  prec_1920.new[,i] <- aggregate(prec_1920[,i] ~ prec_1920[,ncol(prec_1920)], data = prec_1920, 
                                FUN = sum)[,2]*1000
  so2_1920.new[,i] <- aggregate(so2_1920[,i] ~ so2_1920[,ncol(so2_1920)], data = so2_1920, 
                               FUN = mean)[,2]
  temp_1920.new[,i] <- aggregate(temp_1920[,i] ~ temp_1920[,ncol(temp_1920)], data = temp_1920, 
                               FUN = mean)[,2]
  wind_1920.new[,i] <- aggregate(wind_1920[,i] ~ wind_1920[,ncol(wind_1920)], data = wind_1920, 
                               FUN = mean)[,2]
}


inizio <- length(which(unique(hourly.date) <= "2020-03-09"))
fine <- length(which(unique(hourly.date) <= "2020-05-18"))

covid <- rep(0, nrow(co_1920.new))
covid[inizio:fine] <- 1
table(covid)

co_1920.new <- as.data.frame(co_1920.new)
co_1920.new <- cbind(co_1920.new, unique(hourly.date))
colnames(co_1920.new) <- c(colnames(co_1920)[-31], "Giorno")
colnames(co_1920.new)

no2_1920.new <- as.data.frame(no2_1920.new)
no2_1920.new <- cbind(no2_1920.new, unique(hourly.date))
colnames(no2_1920.new) <- c(colnames(no2_1920)[-31], "Giorno")
colnames(no2_1920.new)

pm10_1920.new <- as.data.frame(pm10_1920.new)
pm10_1920.new <- cbind(pm10_1920.new,  unique(hourly.date))
colnames(pm10_1920.new) <- c(colnames(pm10_1920)[-31], "Giorno")
colnames(pm10_1920.new)

load('prec_marzo.RData')
prec_1920.new <- rbind((prec_1920.new[1:425,]),as.matrix((prec_marzo)),(prec_1920.new[426:700,]))
prec_1920.new <- as.data.frame(prec_1920.new)
prec_1920.new <- cbind(prec_1920.new, unique(hourly.date))
colnames(prec_1920.new) <- c(colnames(prec_1920)[-31], "Giorno")
colnames(prec_1920.new)


so2_1920.new <- as.data.frame(so2_1920.new)
so2_1920.new <- cbind(so2_1920.new,   unique(hourly.date))
colnames(so2_1920.new) <- c(colnames(so2_1920)[-31], "Giorno")
colnames(so2_1920.new)

temp_1920.new <- as.data.frame(temp_1920.new)
temp_1920.new <- cbind(temp_1920.new, unique(hourly.date))
colnames(temp_1920.new) <- c(colnames(temp_1920)[-31], "Giorno")
colnames(temp_1920.new)

wind_1920.new <- as.data.frame(wind_1920.new)
wind_1920.new <- cbind(wind_1920.new, unique(hourly.date))
colnames(wind_1920.new) <- c(colnames(wind_1920)[-31], "Giorno")
colnames(wind_1920.new)


rm(co_1920)
rm(no2_1920)
rm(pm10_1920)
rm(prec_1920)
rm(so2_1920)
rm(temp_1920)
rm(wind_1920)

co_1920 <- co_1920.new
no2_1920 <- no2_1920.new
pm10_1920 <- pm10_1920.new
prec_1920 <- prec_1920.new
so2_1920 <- so2_1920.new
temp_1920 <- temp_1920.new
wind_1920 <- wind_1920.new

rm(co_1920.new)
rm(no2_1920.new)
rm(pm10_1920.new)
rm(prec_1920.new)
rm(so2_1920.new)
rm(temp_1920.new)
rm(wind_1920.new)
rm(fine)
rm(inizio)

#pulizia precipitazioni----
media <- sapply(1:30, function(x) mean(prec_1920[,x]))
mean(media)
hist(as.matrix(prec_1920[,-c(31,32)]), nclass = 250, xlim = c(0,10))
abline( v = 1)
sum(prec_1920[,-c(31,32)] > 0)/(731*32)
prec_ind <- sapply(1:30, function(x) ifelse(prec_1920[,x] > 0, 1, 0))
matplot(prec_ind, type = "l")
plot(prec_ind[,1])

prec_ind_new <- matrix(NA, nrow = nrow(prec_ind), ncol = ncol(prec_ind))
for(i in 1:30) {
  cat(i, "\n")
  for(j in 2:730){
    if(prec_ind[j,i] == 0) {
      if(sum(prec_ind[c(j-1, j+1), i]) == 2)
        prec_ind_new[j,i] <- 1 
      else prec_ind_new[j,i] <- prec_ind[j,i]
    }
    else
      prec_ind_new[j,i] <- prec_ind[j,i]
  }
}

prec_ind_new[1,] <- prec_ind[1,]
prec_ind_new[731, ] <- prec_ind[731,]
table(prec_ind, prec_ind_new)
prec_ind <- prec_ind_new
prec_ind <- cbind(prec_ind, unique(hourly.date))
prec_ind <- as.data.frame(prec_ind)
colnames(prec_ind) <- c("prec_AL", "prec_AN", "prec_AO", "prec_AQ","prec_BA", "prec_BO",
                         "prec_BS", "prec_BZ", "prec_CA", "prec_CB", "prec_CZ", "prec_FE",
                         "prec_FI", "prec_GE","prec_MI", "prec_NA", "prec_PA",
                         "prec_PD", "prec_PG", "prec_PR", "prec_PZ", "prec_RA", "prec_RO",
                         "prec_TN", "prec_TO", "prec_TR", "prec_UD","prec_VE", "prec_VI",
                         "prec_VR", "Giorno")
rm(prec_ind_new)
rm(hourly.date)
rm(prec_1920)
rm(prec_2020)
prec_1920<-prec_ind
rm(prec_ind)

co_1920 <- cbind((co_1920[,-31] - mean(as.matrix(co_1920[,-31])))/sd(as.matrix(co_1920[,-31])),
                 co_1920[,31])
colnames(co_1920)[31] <- "Giorno"
matplot(co_1920[,-31], type = "l")

no2_1920 <- cbind((no2_1920[,-31] - mean(as.matrix(no2_1920[,-31])))/sd(as.matrix(no2_1920[,-31])),
                 no2_1920[,31])
colnames(no2_1920)[31] <- "Giorno"
matplot(no2_1920[,-31], type = "l")


so2_1920 <- cbind((so2_1920[,-31] - mean(as.matrix(so2_1920[,-31])))/sd(as.matrix(so2_1920[,-31])),
                 so2_1920[,31])
colnames(so2_1920)[31] <- "Giorno"
matplot(so2_1920[,-31], type = "l")

temp_1920 <- cbind((temp_1920[,-31] - mean(as.matrix(temp_1920[,-31])))/sd(as.matrix(temp_1920[,-31])),
                                                  temp_1920[,31])
colnames(temp_1920)[31] <- "Giorno"
matplot(temp_1920[,-31], type = "l")


wind_1920 <- cbind((wind_1920[,-31] - mean(as.matrix(wind_1920[,-31])))/sd(as.matrix(wind_1920[,-31])),
                                                  wind_1920[,31])
colnames(wind_1920)[31] <- "Giorno"
matplot(wind_1920[,-31], type = "l")

dati_grezzi<-list(pm10=pm10_1920,co=co_1920,no2=no2_1920,so2=so2_1920,prec=prec_1920,
                  temp=temp_1920,wind=wind_1920)

dati_grezzi$loc=c('NO','C','NO','C','S','NE','NO','NE','S','S','S','NE','C','NO',
                    'NO','S','S','NE','C','NE','S','NE','C','NE','NO','NE','NE','NE','NE','NE')

dati_grezzi$loc=factor(dati_grezzi$loc)  

dati_grezzi2<-list(pm10=t(as.matrix(pm10_1920[,-31])),co=t(as.matrix(co_1920[,-31])),
                   no2=t(as.matrix(no2_1920[,-31])),so2=t(as.matrix(so2_1920[,-31])),
                   prec=t(as.matrix(prec_1920[,-31])),temp=t(as.matrix(temp_1920[,-31])),
                   wind=t(as.matrix(wind_1920[,-31])),loc=dati_grezzi$loc)
str(dati_grezzi2)

