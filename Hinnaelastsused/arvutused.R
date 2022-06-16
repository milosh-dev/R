###########################################
# Diiselkütuse elastsusarvutused
# Raoul Lättemäe 2021
###########################################

library(tidyverse)    # andmeanalüüsi pakett
library(xts)          # aegread
#library(tseries)      # Aegrea testid
#library(urca)         # Johansen cointegration test
#library(seasonal)     # X-13ARIMA-SEASTS sesoonsuse kõrvaldamine
#library(texreg)       # R väljund LaTeX'isse
#library(stargazer)    # R väljund LaTeX'isse
library(fpoplot)      # Jooniste kujundus
#library(dynlm)        # Viitaegadega lineaarne funktsioon
#library(strucchange)  # Struktuursete muudatuste testimine

# Määra töökataloog
setwd("/home/raoul/Dokumendid/R/Hinnaelastsused")

# Loe sisse andmed tibble formaadis
andmed <- read.csv("andmed_f.csv", sep=";") %>% as_tibble
# Tühjad lahtrid on NA
andmed[andmed == ""] <- NA

# Muuda veergude formaat korrektseks
# aeg on kuupöev ja ülejäänud on numbrid
andmed$aeg <- as.Date(andmed$aeg, "%d.%m.%Y")

andmed$Diisel <- as.double(gsub(",",".", andmed$Diisel))
andmed$Diisel_S <- as.double(gsub(",",".", andmed$Diisel_S))
andmed$D_akt <- as.double(gsub(",",".", andmed$D_akt))
andmed$D_hind_ee <- as.double(gsub(",",".", andmed$D_hind_ee))
andmed$D_hind_lv <- as.double(gsub(",",".", andmed$D_hind_lv))
andmed$D_hind_lt <- as.double(gsub(",",".", andmed$D_hind_lt))

andmed$Bensiin <- as.double(gsub(",",".", andmed$Bensiin))
andmed$Bensiin_S <- as.double(gsub(",",".", andmed$Bensiin_S))
andmed$B_Akt <- as.double(gsub(",",".", andmed$B_Akt))
andmed$B_hind_ee <- as.double(gsub(",",".", andmed$B_hind_ee))
andmed$B_hind_lv <- as.double(gsub(",",".", andmed$B_hind_lv))
andmed$B_hind_lt <- as.double(gsub(",",".", andmed$B_hind_lt))

andmed$Kuu_palk <- as.double(gsub(",", ".", andmed$Kuu_palk))
andmed$SKP_ph <- as.double(gsub(",",".", andmed$SKP_ph))

andmed$Barrel <- as.double(gsub(",",".", andmed$Barrel))
andmed$X.kurss <- as.double(gsub(",",".", andmed$X.kurss))

andmed$Veos <- as.double(gsub(",",".", andmed$Veos))
andmed$Mnt_käive <- as.double(gsub(",",".", andmed$Mnt_käive))
andmed$Jae_Hulgi <- as.double(gsub(",",".", andmed$Jae_Hulgi))
andmed$Veoautod.registris <- as.double(gsub(",",".", andmed$Veoautod.registris))

skp <- ts(andmed$SKP_ph, start=c(2005,1), frequency = 4)
skpsa <- seas(skp)
skpsa <- as.data.frame(skpsa)

andmed$skp.sa <- skpsa$final
# SKP
ggplot(andmed, aes(x=aeg)) + theme_fpo() +
  geom_line(aes(y=skp.sa), color="red") + 
  geom_line(aes(y=SKP_ph))

# Diisli hind
ggplot(andmed, aes(x=aeg)) + theme_fpo() +
  geom_line(aes(y=D_hind_ee), color="blue") + 
  geom_line(aes(y=D_hind_lv), color="red") + 
  geom_line(aes(y=D_hind_lt), color="green")

# Bensiini hind
ggplot(andmed, aes(x=aeg)) + theme_fpo() +
  geom_line(aes(y=B_hind_ee), color="blue") + 
  geom_line(aes(y=B_hind_lv), color="red") + 
  geom_line(aes(y=B_hind_lt), color="green")

# Veos
ggplot(andmed, aes(x=aeg, y=Veos)) + geom_line() + theme_fpo()
# Maante käive
ggplot(andmed, aes(x=aeg, y=Mnt_käive)) + geom_line() + theme_fpo()
# Jae_hulgi käive
ggplot(andmed, aes(x=aeg, y=Jae_Hulgi)) + geom_line() + theme_fpo()

# Salvesta andmeset
saveRDS(andmed, "andmed.rds")
#andmed <- readRDS("andmed.rds")

# Salvesta aegridadena
#andmed.ts <- ts(andmed, start = c(2003,1), frequency = 4)
andmed.xts <- xts(andmed[,-1], order.by=andmed$aeg)
save(andmed.xts, file = "andmed.RData")
#load(andmed.RData)

andmed.full <- andmed.xts["2005/2020"]
plot(andmed.full)

# Logaritmitud andmed
andmed.ln <- log(andmed[,-1])
andmed.ln$aeg <- andmed$aeg
andmed.l.xts <- xts(andmed.ln[,1:length(andmed.ln)-1], order.by=andmed$aeg)
