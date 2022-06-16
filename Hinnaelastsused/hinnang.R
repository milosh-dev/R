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

citation("xts")

# Määra töökataloog
setwd("/home/raoul/Dokumendid/R/Hinnaelastsused")

# Lae andmed
andmed <- readRDS("andmed.rds")
load("andmed.RData")



# Dummy for break-point
andmed.xts$dummy <- 0
start.date = as.POSIXct("2007-01-01")
end.date = as.POSIXct("2012-01-01")
andmed.xts$dummy <- ifelse(index(andmed.xts) < as.Date(end.date), 1, 0)
andmed.xts$trend <- ifelse(index(andmed.xts) < as.Date(end.date), row(andmed.xts) - 9, 0)

andmed.xts$dummy_kriis <- ifelse(index(andmed.xts) > as.Date(start.date) & index(andmed.xts) < as.Date(end.date), 1, 0)
andmed.xts$kriis_trend <- andmed.xts$dummy_kriis * andmed.xts$trend

andmed.xts$d_marginaal_lv <- andmed.xts$D_hind_lv - andmed.xts$Barrel*1000/158.987295
andmed.xts$d_marginaal_ee <- andmed.xts$D_hind_ee - andmed.xts$Barrel*1000/158.987295
andmed.xts$d_marginaal_lt <- andmed.xts$D_hind_lt - andmed.xts$Barrel*1000/158.987295

ggplot(data = andmed.xts, aes(x=index(andmed.xts))) +
#  geom_line(aes(y = Barrel*1000/158.987295)) +
  geom_line(aes(y = d_marginaal_ee)) + 
  geom_line(aes(y = d_marginaal_lv)) + 
  geom_line(aes(y = d_marginaal_lt)) +
  theme_fpo()

andmed.full <- andmed.xts["2005/2020"]
andmed.late <- andmed.xts["2012/2020"]
andmed.early <- andmed.xts["2005/2011"]
andmed.fore <- andmed.xts["2005/2018-06"]
#plot(andmed.full)

andmed.l.full <- log(andmed.xts["2012/2018"])


# Logaritmitud andmed
andmed.ln <- log(andmed[,-1])
andmed.ln$aeg <- andmed$aeg
andmed.l.xts <- xts(andmed.ln[,1:length(andmed.ln)-1], order.by=andmed$aeg)

# Long-Term relationship
ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt) + L(log(D_akt),-1)
ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))
ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) #+ trend#+ log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))

ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))

ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lv) + trend#+ log(Kuu_palk)#+ log(trend(Diisel_S))

ecm.eq <- log(Bensiin) ~ log(SKP_ph) + log(B_hind_ee) #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))

ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt) + dummy + trend #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))
ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + dummy + trend #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))

ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + dummy + trend #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))

ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) #+ trend#+ log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))
ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt) #+ trend#+ log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))

ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(d_marginaal_ee/d_marginaal_lv) #+ trend#+ log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))
ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(d_marginaal_ee/d_marginaal_lt) #+ dummy + trend #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))


ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(d_marginaal_ee/d_marginaal_lt) + trend #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))

ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + trend + dummy #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))

ecm.eq.full <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt) + trend + dummy #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))
ecm.eq.early <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt) + trend #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))
ecm.eq.late <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt)#+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))

#ecm.eq.full <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) + trend + dummy #+ (dummy * trend(Diisel_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))

ecm.eq.fore <- log(Diisel_S) ~ log(SKP_ph) + log(D_hind_ee) + log(D_hind_ee/D_hind_lt) + trend + dummy

ecm.model.late <- dynlm(ecm.eq.late, as.ts(andmed.late))
ecm.model.early <- dynlm(ecm.eq.early, as.ts(andmed.early))
ecm.model.full <- dynlm(ecm.eq.full, as.ts(andmed.full))

#ecm.model.full <- dynlm(ecm.eq.full, as.ts(andmed.fore))

ecm.model.bens <- dynlm(ecm.eq, as.ts(andmed.full))

AIC(ecm.model.full)
summary(ecm.model.full)
library(forecast)
ˇforecast::checkresiduals(ecm.model.full, theme=theme_fpo())

forecast(ecm.model.full, as.data.frame(andmed.full))

ldiisel_s = log(diisel)

arim <- auto.arima(ldiisel_s)
summary(arim)
accuracy(ecm.model.full)

plot(forecast(arim))

ecm.model.kriis <- dynlm(ecm.eq.late, as.ts(andmed.kriis))
AIC(ecm.model.full)
summary(ecm.model.full)

View(ecm.model.full$coefficients)

stargazer(ecm.model.early, ecm.model.late, ecm.model.full, 
          title="Veaparandusmudeli pikaajaline seos",
          align=TRUE,
          dep.var.labels=c("2005-2007","2011-2020", "2005-2020"),
          covariate.labels=c("ln(SKP)","ln(Diislikütuse hind)",
                             "ln(EE/LT hinnaerinevus)","Trend","Dummy (2005-2011 = 1)", "Konstant"),
          keep.stat=c("n","rsq","adj.rsq", "aic", "bic"), 
          no.space=TRUE)


andmed.max <- andmed.xts["2005/2025"]

p <- ts(predict(ecm.model.full), start=(c(2005,1)), end=(c(2025,4)), frequency = 4, andmed.max)
accuracy(ecm.model.full)
diisel_s <- ts(andmed.max$Diisel_S, start=(c(2005,1)), frequency = 4)
diisel <- ts(andmed.max$Diisel, start=(c(2005,1)), frequency = 4)
lr <- cbind(exp(p), diisel, diisel_s)
ts.plot(lr, col=c("red", "grey", "black", "black"), lty = c(1,2,1,1), lwd=c(1.5,1,2,2))
legend(2005, 300, 
       legend = c("mudeli hinnang", "tegelik", "tegelik (varumine silutud)"),
       col=c("red", "grey", "black", "black"), 
       lty = c(1,2,1,1), 
       lwd=c(1.5,1,2,2),
       box.lty = 0)

ecm.one <- d(log(Diisel_S)) ~ d(log(skp.sa)) + d(log(D_hind_ee)) - d(log(D_hind_ee/D_hind_lt)) + 
  L(log(Diisel_S),1) + L(log(skp.sa),1) + L(log(D_hind_ee),1) + L(log(D_hind_ee/D_hind_lt),1) + trend + dummy

ecm.full <- dynlm(ecm.one, as.ts(andmed.full))
AIC(ecm.full)
summary(ecm.full)

accuracy(ecm.full)

plot(ecm.full$residuals)
summary(ur.df(ecm.full$residuals))

fore <- forecast(ecm.full)

dp <- predict(ecm.full)
p <- cumsum(dp) + 4.497652
p <- ts(p, start=(c(2005,3)), end=c(2020,4), frequency = 4, andmed.full)
plot(exp(p))
diisel_s <- ts(andmed.full$Diisel_S, start=(c(2005,1)), frequency = 4)
diisel <- ts(andmed.full$Diisel, start=(c(2005,1)), frequency = 4)
lr <- cbind(exp(p), diisel, diisel_s)
ts.plot(lr, col=c("red", "grey", "black", "black"), lty = c(1,2,1,1), lwd=c(1.5,1,2,2))
legend(2005, 300, 
       legend = c("mudeli hinnang", "tegelik", "tegelik (varumine silutud)"),
       col=c("red", "grey", "black", "black"), 
       lty = c(1,2,1,1), 
       lwd=c(1.5,1,2,2),
       box.lty = 0)




ecm.term <- ecm.model.full$residuals
ecm.term.raw <- log(diisel) - p
plot(ecm.term)

ecm.term <- lag(ts(ecm.term, start = c(2005,1), frequency = 4),-1)
ecm.term.raw <- lag(ts(ecm.term.raw, start = c(2005,1), frequency = 4),-1)

andmed.full <- andmed.xts["2005/2020"]
andmed.full <- ts(andmed.full, start = c(2005,1), frequency = 4)

andmed.late <- andmed.xts["2012/2020"]
andmed.late <- ts(andmed.late, start = c(2012,1), frequency = 4)

andmed.early <- andmed.xts["2005/2011"]
andmed.early <- ts(andmed.early, start = c(2005,1), frequency = 4)


c <- cbind(andmed.full, ecm.term, ecm.term.raw)
  
ecm.eq.s <- d(log(Diisel)) ~ d(log(skp.sa)) + d(log(D_hind_ee)) + L(d(log(D_akt)),-1) + L(d(log(D_akt)),0) + L(ecm.term.raw,1)
mudel.1 <- dynlm(ecm.eq.s, as.ts(andmed.full))

ecm.eq.s.f <- d(log(Diisel_S)) ~ d(log(skp.sa)) + d(log(D_hind_ee)) - d(log(D_hind_ee/D_hind_lt)) + L(ecm.model.full$residuals,1)
ecm.eq.s.e <- d(log(Diisel_S)) ~ d(log(skp.sa)) + d(log(D_hind_ee)) - d(log(D_hind_ee/D_hind_lt)) + L(ecm.model.early$residuals,1)
ecm.eq.s.l <- d(log(Diisel_S)) ~ d(log(skp.sa)) + d(log(D_hind_ee)) - d(log(D_hind_ee/D_hind_lt)) + L(ecm.model.late$residuals,1)
mudel.early <- dynlm(ecm.eq.s.e, as.ts(andmed.early))
mudel.late <- dynlm(ecm.eq.s.l, as.ts(andmed.late))
mudel.full <- dynlm(ecm.eq.s.f, as.ts(andmed.full))

mudel.full <- dynlm(ecm.eq.s.f, as.ts(andmed.fore))

AIC(mudel.early)
AIC(mudel.late)
AIC(mudel.full)

stargazer(mudel.early, mudel.late, mudel.full,
          title="Veaparandusmudeli lühiajaline seos",
          align=TRUE,
          keep.stat=c("n","rsq","adj.rsq", "aic", "bic"), 
          no.space=TRUE)


summary(mudel.full)
AIC(mudel)

cc <- predict(mudel)

cc <- ts(cumsum(cc) + 4.376699, start = c(2005,2), frequency = 4)
cc <- ts(cumsum(cc) + 4.823217, start = c(2012,1), frequency = 4)

cs <- ts((cc) , start = c(2005,2), frequency = 4)
sr <- cbind(cumsum(cs), diff(log(diisel)), diff(log(diisel_s)))
ts.plot(sr, col=c("red", "grey", "black", "black"), lty = c(1,2,1,1), lwd=c(1.5,1,2,2))
legend(2005, 300, 
       legend = c("mudeli hinnang", "tegelik", "tegelik (varumine silutud)"),
       col=c("red", "grey", "black", "black"), 
       lty = c(1,2,1,1), 
       lwd=c(1.5,1,2,2),
       box.lty = 0)


#cc <- (cumsum(cc)) + 4.5
#cd <- log(diisel)

dl <- diff(log(diisel_s))

ts.plot(cbind(ecm.term, dl), col=c("red", "black"))

plot(cbind(exp(cc), (diisel_s)), type = ("l"))

sr <- cbind(exp(cc), diisel, diisel_s)
ts.plot(sr, col=c("red", "grey", "black", "black"), lty = c(1,2,1,1), lwd=c(1.5,1,2,2))
legend(2005, 300, 
       legend = c("mudeli hinnang", "tegelik", "tegelik (varumine silutud)"),
       col=c("red", "grey", "black", "black"), 
       lty = c(1,2,1,1), 
       lwd=c(1.5,1,2,2),
       box.lty = 0)

View(log(diisel))
plot(log(diisel_s))

summary(ur.df(mudel$residuals))
plot(ts(mudel$residuals, start=c(2005,1), frequency = 4))

pr <- predict(mudel)
plot(cumsum(pr), type="l")

andmed.full.lt <- andmed.full
andmed.full.lt$D_hind_ee <- ifelse(andmed.full.lt$D_akt > 400, andmed.full.lt$D_hind_ee - andmed.full.lt$D_akt + 392.93, andmed.full.lt$D_hind_ee)

p.t <- ts(predict(ecm.model.full, newdata = andmed.full.lt), start=(c(2005,1)), end=c(2020,4), frequency = 4)
diisel_s <- ts(andmed.full$Diisel_S, start=(c(2005,1)), frequency = 4)
diisel <- ts(andmed.full$Diisel, start=(c(2005,1)), frequency = 4)
lr <- cbind(exp(p.t), diisel, diisel_s)
ts.plot(lr, col=c("red", "grey", "black", "black"), lty = c(1,2,1,1), lwd=c(1.5,1,2,2))
legend(2005, 300, 
       legend = c("mudeli hinnang", "tegelik", "tegelik (varumine silutud)"),
       col=c("red", "grey", "black", "black"), 
       lty = c(1,2,1,1), 
       lwd=c(1.5,1,2,2),
       box.lty = 0)

simul <- exp(p) / exp(p.t)
plot(simul, col="red", lwd=2)

AIC(ecm.model)
summary(ecm.model)
plot(ecm.model$residuals)

summary(ur.df(ecm.model.early$residuals))
summary(ur.df(ecm.model.late$residuals))
summary(ur.df(ecm.model.full$residuals))

co.test <- ca.jo(andmed.l.xts[, c("Diisel_S", "skp.sa", "D_hind_ee", "dummy_kriis", "trend")])
summary(co.test)

dynlm(Bensiin_S ~ season(Bensiin_S), as.ts(andmed.full))


plot(benssa$final, type="l")
               
ecm.eq <- log(Bensiin_S) ~ log(skp.sa) + log(B_hind_ee) #+ log(B_hind_ee/B_hind_lv) + (dummy * trend(Bensiin_S))#+ log(Kuu_palk)#+ log(trend(Diisel_S))


plot(andmed.late[,c("Diisel", "Diisel_S")])
plot(andmed.full[,c("Bensiin", "Bensiin_S")])

# Mudeli kriteeriumid
AIC(ecm.model)
plot(ecm.model$residuals, type="l")
summary(ecm.model)

summary(ur.df(ecm.model$residuals))

# Struktuurne muutus
## historical tests
ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee) - log(d_marginaal_ee/d_marginaal_lt) #+ trend + dummy#+ log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))
ecm.eq <- log(Diisel_S) ~ log(skp.sa) + log(D_hind_ee)# + log(d_marginaal_ee/d_marginaal_lt) + dummy + trend #+ log(D_hind_ee/D_hind_lt) #+ log(Kuu_palk)#+ log(trend(Diisel_S))

d <- as.ts(andmed.full, start = c(2005, 1), end = c(2020,4))

# 2x2 plots
par(mfrow=c(2,2))

rec <- efp(ecm.eq, data = ts(andmed.fore, start=c(2005,1), frequency = 4))
plot(rec)

ols <- efp(ecm.eq, data = ts(andmed.fore, start=c(2005,1), frequency = 4), type = "OLS-CUSUM")
plot(ols)

re <- efp(ecm.eq, data = ts(andmed.fore, start=c(2005,1), frequency = 4), type = "fluctuation")
plot(re)

fs <- Fstats(ecm.eq, data = ts(andmed.fore, start = c(2005, 1), frequency = 4), from = 0.1)
plot(fs, main="F-Statistics test")

accuracy(ecm.eq)

# Millal
bp <- breakpoints(ecm.eq, data = ts(andmed.fore, start=c(2005,1), frequency = 4))
summary(bp)
plot(bp)

# Kus on andmeridades muutused
plot(ts(andmed$Diisel_S, start=c(2003,1), frequency = 4), ylab="Deklareeritud diislikütuse kogus", 
     xlab="aeg", main="Struktuurne muutus BIC testi põhjal",
     col="blue", lwd=2)
lines(fitted(bp, breaks = 2), col = 4)
lines(confint(bp, breaks = 2))

plot(ts(andmed$skp.sa, start=c(2003,1), frequency = 4), ylab="SKP püsihindades", 
     xlab="aeg", main="Struktuurne muutus BIC testi põhjal",
     col="blue", lwd=2)
lines(fitted(bp, breaks = 2), col = 4)
lines(confint(bp, breaks = 2))


plot(ts(andmed$D_akt, start=c(2003,1), frequency = 4))
lines(fitted(bp, breaks = 2), col = 4)
lines(confint(bp, breaks = 2))

?print(accuracy(ecm.model))
