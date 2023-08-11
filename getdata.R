library(crypto2)
library(lubridate)
library(tidyverse)
library(coinmarketcapr)

# recuperation des infos et des 20 premieres crypto
setup(api_key = "19d11e6a-f90f-4d45-8ec5-9abd9a1115f9")

# recuperation du classement
ranklist <- as.data.frame(get_crypto_listings(currency = "USD", latest = TRUE))[,c(1,2,3,11)]
top_crypto <- ranklist[c(1:20),]

# recuperation des infos des crypto
coinlist <- crypto_list()

logo <- as.data.frame(crypto_info(coinlist %>% filter(id %in% top_crypto$id))[,c(1,7)])

top_crypto <- top_crypto[,c(1,2,3)]

top_crypto$rank = row.names(top_crypto)

top_crypto <- inner_join(top_crypto, logo, "id")


if(length(top_crypto$id) > 10){
  write.csv(top_crypto, "top_crypto.csv",row.names = F)
}
# recuperation des donnees
date_lim <- Sys.Date()-365

date_mois <- Sys.Date()-30
date_6mois <- Sys.Date()-180

top_crypto_1 <- top_crypto[c(1:10),]

# data au mois 
dtmois <- as.data.frame(crypto_history(interval = "daily",start_date = as_datetime(date_mois),coinlist %>% filter(id %in% top_crypto_1$id)))[,c(1,2,4,8,9)]
dtmois$moy <- round((dtmois$low + dtmois$high)/2,3)
colnames(dtmois) <- c("Date","id","name","min","max","Mean")
dtmois$Date <- as.Date(dtmois$Date)

# data 6 mois
dt6mois <- as.data.frame(crypto_history(interval = "weekly",start_date = as_datetime(date_6mois),coinlist %>% filter(id %in% top_crypto_1$id)))[,c(1,2,4,8,9)]
dt6mois$moy <- round((dt6mois$low + dt6mois$high)/2,3)
colnames(dt6mois) <- c("Date","id","name","min","max","Mean")
dt6mois$Date <- as.Date(dt6mois$Date)

# data 1 an 
dt <- as.data.frame(crypto_history(interval = "weekly",start_date = as_datetime(date_lim),coinlist %>% filter(id %in% top_crypto_1$id)))[,c(1,2,4,8,9)]
dt$moy <- round((dt$low + dt$high)/2,3)
colnames(dt) <- c("Date","id","name","min","max","Mean")
dt$Date <- as.Date(dt$Date)

# data histo
dthist <- as.data.frame(crypto_history(interval = "weekly",coinlist %>% filter(id %in% top_crypto$id)))[,c(1,2,4,8,9)]
dthist$moy <- round((dthist$low + dthist$high)/2,3)
colnames(dthist) <- c("Date","id","name","min","max","Mean")
dthist$Date <- as.Date(dthist$Date)


if(length(dtmois$Mean > 10)){
write.csv(dtmois, "data_mois.csv", row.names = F)
}

if(length(dt6mois$Mean > 10)){
write.csv(dt6mois, "data_6mois.csv", row.names = F)
}

if(length(dt$Mean > 20)){
write.csv(dt, "data.csv", row.names = F)
}

if(length(dthist$Mean > 100)){
  write.csv(dthist, "data_hist.csv", row.names = F)
}

