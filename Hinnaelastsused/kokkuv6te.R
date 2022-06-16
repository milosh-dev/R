###########################################
# Diiselkütuse elastsusarvutused
# Raoul Lättemäe 2021
###########################################

library(tidyverse)    # andmeanalüüsi pakett
library(xts)          # aegread
library(tseries)      # Aegrea testid
library(urca)         # Johansen cointegration test
library(seasonal)     # X-13ARIMA-SEASTS sesoonsuse kõrvaldamine
library(texreg)       # R väljund LaTeX'isse
library(stargazer)    # R väljund LaTeX'isse
library(fpoplot)      # Jooniste kujundus
library(dynlm)        # Viitaegadega lineaarne funktsioon
library(strucchange)  # Struktuursete muudatuste testimine
library(forecast)     # Prognoosi hindamine

# Määra töökataloog
setwd("/home/raoul/Dokumendid/R/Hinnaelastsused")
load("andmed.RData")

# Me peame jätma välja puuduvad andmed. Kasutame 2005-2020 numbreid
andmed.full <- andmed.xts["2005/2020"]

# Piaajaline seos (aktiivne on lõplik mudel. Kasutasin valikuks mudeli stabiilsust ja Akaike kriteeriumit)
#
# ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt) + L(log(D_akt),-1)
# ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))
# ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) #+ trend#+ log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))
# ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lv) + trend#+ log(Kuu_palk)#+ log(trend(Diisel_S))
# ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lv) + dummy + trend #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))
# ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + dummy + trend #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))
# ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + dummy + trend #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))
# ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) #+ trend#+ log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))
# ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt) #+ trend#+ log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))
# ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(d_marginaal_ee/d_marginaal_lv) #+ trend#+ log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))
# ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(d_marginaal_ee/d_marginaal_lt) #+ dummy + trend #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))
# ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(d_marginaal_ee/d_marginaal_lt) + trend #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))
# ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + trend + dummy #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))
# ecm.eq <- log(Bensiin) ~ log(SKP_ph) + log(B_hind_ee) #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))
# ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))

#ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))
ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) #+ log(Kuu_palk)#+ log(trend(Diisel_S))

# Hinda pikaajaline seos
ecm.model <- lm(ecm.eq, as.ts(andmed.full))

# Tulemused
checkresiduals(ecm.model, theme=theme_fpo())
    # Jääkliikmed ei tundu visuaalselt statsionaarsed...
adf.test(ecm.model$residuals)
    # Ei ole kah...
summary(ecm.model)
    # Lisaks on hinnaalastsus on positiivne...

# Tegelikult oli pusimist rohkem, enne kui tuli mõte testida, ega andmetes ei ole struktuurset nihet
# 2x2 plots
par(mfrow=c(2,2))
# CUSUM test
rec <- efp(ecm.eq, data = ts(andmed.full, start=c(2005,1), frequency = 4))
plot(rec)
ols <- efp(ecm.eq, data = ts(andmed.full, start=c(2005,1), frequency = 4), type = "OLS-CUSUM")
plot(ols)
re <- efp(ecm.eq, data = ts(andmed.full, start=c(2005,1), frequency = 4), type = "fluctuation")
plot(re)
fs <- Fstats(ecm.eq, data = ts(andmed.full, start = c(2005, 1), frequency = 4), from = 0.1)
plot(fs, main="F-Statistics test")
    # Kõik neli testi kinnitavad, et andmetes on struktuurne nihe (statistik läheb punasest välja)

# Uurime välja, millal
bp <- breakpoints(ecm.eq, data = ts(andmed.full, start=c(2005,1), frequency = 4))
summary(bp)
      # Viimasest kahest reast: 
          # BIC järgi on üks struktuurne nihe (statistiu väärtus madalaim)
          # RSS järgi võiks ju minna lausa viie nihkeni, aga tegelikult statistiku 
          # väärtus peale ühte nihet oluliselt edasi ei alane, ehk ka üks
      # Pisut üleval pool on kirjas ka kuupäevad, millal nihe toimus (2011 IV kvartalis)
par(mfrow=c(1,1))
plot(bp)

# Selle saab ka kenasti graafikule panna, sh veel ka usaldusnivoodega.
# Diisli deklareerimine
plot(ts(andmed.xts$Diisel_S, start=c(2003,1), frequency = 4), ylab="Deklareeritud diislikütuse kogus", 
     xlab="aeg", main="Struktuurne muutus BIC testi põhjal",
     col="blue", lwd=2)
lines(fitted(bp, breaks = 2), col = 4)
lines(confint(bp, breaks = 2))

# ... või SKP
plot(ts(andmed.xts$skp.sa, start=c(2003,1), frequency = 4), ylab="SKP püsihindades", 
     xlab="aeg", main="Struktuurne muutus BIC testi põhjal",
     col="blue", lwd=2)
lines(fitted(bp, breaks = 1), col = 4)
lines(confint(bp, breaks = 1))

# ... või hinnaindeksi järgi
plot(ts(andmed.xts$D_hind_ee/1000, start=c(2003,1), frequency = 4), ylab="Diisli hind", 
     xlab="aeg", main="Struktuurne muutus BIC testi põhjal",
     col="red", lwd=2)
lines(fitted(bp, breaks = 3), col = 4)
lines(confint(bp, breaks = 3))

# ... või aktsiisimäära järgi
plot(ts(andmed.xts$D_akt, start=c(2003,1), frequency = 4), ylab="Aktsiisimäär", 
     xlab="aeg", main="Struktuurne muutus BIC testi põhjal",
     col="red", lwd=2)
lines(fitted(bp, breaks = 3), col = 4)
lines(confint(bp, breaks = 3))

# ... või hinnaerinevus leeduga järgi
plot(ts(log(andmed.xts$D_hind_ee/andmed.xts$D_hind_lt), start=c(2003,1), frequency = 4), ylab="Hinnaerinevus Leedu suhtes", 
     xlab="aeg", main="Struktuurne muutus BIC testi põhjal",
     col="red", lwd=2)
lines(fitted(bp, breaks = 4), col = 4)
lines(confint(bp, breaks = 4))

# OK, oletame, et 2011-IV kvartalis oli struktuurne nihe ja hindame eraldi: 2005-2011 ja 2012-2020
andmed.late <- andmed.xts["2012/2020"]
andmed.early <- andmed.xts["2005/2011"]

# Esmalt algne seos lihtsalt hindade ja SKPga
ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt) + trend #+ log(Kuu_palk)#+ log(trend(Diisel_S))

# Hinda pikaajaline seos
ecm.model <- dynlm(ecm.eq, as.ts(andmed.full))

# Tulemused
checkresiduals(ecm.model, theme=theme_fpo())
# Jääkliikmed paistavad visuaalselt statsionaarsed...
adf.test(ecm.model$residuals)
# aga siiski ei ole...
summary(ecm.model)
# Lisaks on hinnaalastsus on positiivne...



accuracy(ecm.eq)



# Dummy struktuursele nihkele
andmed.xts$dummy <- 0
break.date = as.date(as.POSIXct("2012-01-01"))
andmed.xts$dummy <- ifelse(index(andmed.xts) < break.date, 1, 0)
andmed.xts$trend <- ifelse(index(andmed.xts) < break.date, row(andmed.xts) - 9, 0)

