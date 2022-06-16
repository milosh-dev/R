#################################################
# Kütuse omahinna ülekandumine jaehindadesse
#
# Raoul Lättemäe 2021
#################################################
library(forecast)
library(tseries)
library(dynlm)
library(urca)

# Tühjenda andmed
rm(list = ls())

setwd("/home/raoul/Dokumendid/R/Hinnaelastsused/ylekandeefektid")
#saveRDS(andmed, "andmed.rds")
andmed <- readRDS("andmed.rds")

# Pikaajaline seos
eq.ee <- log(EE_netohind) ~ log(EE_Aktsiis) + log(EE_omahind)
ee.model <- dynlm(eq.ee, ts(andmed, start=c(2007,1), frequency = 12))

summary(ee.model)
checkresiduals(ee.model)

p <- predict(ee.model)

data <- ts.union(ts(exp(p)), andmed$EE_netohind)

plot(data, type="l", col=c("red", "black"))

adf.test(ee.model$residuals)
kpss.test(ee.model$residuals)

# Cointegration test
po.test(log(andmed[,c("EE_netohind", "EE_Aktsiis", "EE_omahind")]))
summary(ca.jo(log(andmed[,c("EE_netohind", "EE_Aktsiis", "EE_omahind")])))

andmed$EE_pred <- exp(p)
andmed$EE.resid.sym <- ee.model$residuals
andmed$EE.resid.pos <- ifelse(ee.model$residuals > 0, ee.model$residuals, 0)
andmed$EE.resid.neg <- ifelse(ee.model$residuals < 0, ee.model$residuals, 0)

# ECM relation
eq.sym <- d(log(EE_netohind)) ~ d(log(EE_omahind)) - L(d(log(EE_netohind)),1) - L(d(log(EE_omahind)),1) + L(EE.resid.sym, 1)
ee.sym.model <- dynlm(eq.sym, ts(andmed, start=c(2007,1), frequency = 12))

AIC(ee.sym.model)

summary(ee.sym.model)
checkresiduals(ee.sym.model)

adf.test(ee.sym.model$residuals)
kpss.test(ee.sym.model$residuals)


eq.ee.asym <- d(log(EE_netohind)) ~ d(log(EE_omahind)) + L(EE.resid.pos, 1) + L(EE.resid.neg, 1)
ee.asym.model <- dynlm(eq.ee.asym, ts(andmed, start=c(2007,1), frequency = 12))

summary(ee.asym.model)
checkresiduals(ee.asym.model)

adf.test(ee.asym.model$residuals)
kpss.test(ee.asym.model$residuals)

library(car)
linearHypothesis(ee.asym.model, c("L(EE.resid.pos, 1) = L(EE.resid.neg, 1)"))

##############################
eq.full <- d(log(EE_netohind)) ~ d(log(EE_omahind)) + L(log(EE_netohind), 1) + L(log(EE_Aktsiis), 1) + L(log(EE_omahind),1)
ee.full.model <- dynlm(eq.full, ts(andmed, start=c(2007,1), frequency = 12))

summary(ee.full.model)

checkresiduals(ee.full.model)
adf.test(ee.full.model$residuals)
kpss.test(ee.full.model$residuals)

##################################################
# Pikaajaline seos
eq.lv <- log(LV_netohind) ~ log(LV_Aktsiis) + log(LV_omahind)
lv.model <- dynlm(eq.lv, ts(andmed, start=c(2007,1), frequency = 12))

summary(lv.model)
checkresiduals(lv.model)

adf.test(lv.model$residuals)
kpss.test(lv.model$residuals)

# Cointegration test
po.test(log(andmed[,c("LV_netohind", "LV_Aktsiis", "LV_omahind")]))
summary(ca.jo(log(andmed[,c("LV_netohind", "LV_Aktsiis", "LV_omahind")])))

andmed$LV.resid.sym <- lv.model$residuals
andmed$LV.resid.pos <- ifelse(lv.model$residuals > 0, lv.model$residuals, 0)
andmed$LV.resid.neg <- ifelse(lv.model$residuals < 0, lv.model$residuals, 0)

# ECM relation
eq.sym <- d(log(LV_netohind)) ~ d(log(LV_omahind)) - L(d(log(LV_netohind)),1) - L(d(log(LV_omahind)),1) + L(LV.resid.sym, 1)
lv.sym.model <- dynlm(eq.sym, ts(andmed, start=c(2007,1), frequency = 12))

AIC(lv.sym.model)

summary(lv.sym.model)
checkresiduals(lv.sym.model)

adf.test(lv.sym.model$residuals)
kpss.test(lv.sym.model$residuals)


eq.lv.asym <- d(log(LV_netohind)) ~ d(log(LV_omahind)) - L(d(log(LV_netohind)),1) - L(d(log(LV_omahind)),1) + L(LV.resid.pos, 1) + L(LV.resid.neg, 1)
lv.asym.model <- dynlm(eq.lv.asym, ts(andmed, start=c(2007,1), frequency = 12))

summary(lv.asym.model)
checkresiduals(lv.asym.model)

adf.test(lv.asym.model$residuals)
kpss.test(lv.asym.model$residuals)

linearHypothesis(eq.lv.asym, c("L(LV.resid.pos, 1) = L(LV.resid.neg, 1)"))

##############################
##################################################
# Pikaajaline seos
eq.lt <- log(LT_netohind) ~ log(LT_Aktsiis) + log(LT_omahind)
lt.model <- dynlm(eq.lt, ts(andmed, start=c(2007,1), frequency = 12))

summary(lt.model)
checkresiduals(lt.model)

adf.test(lt.model$residuals)
kpss.test(lt.model$residuals)

# Cointegration test
po.test(log(andmed[,c("LT_netohind", "LT_Aktsiis", "LT_omahind")]))
summary(ca.jo(log(andmed[,c("LT_netohind", "LT_Aktsiis", "LT_omahind")])))

andmed$LT.resid.sym <- lt.model$residuals
andmed$LT.resid.pos <- ifelse(lt.model$residuals > 0, lt.model$residuals, 0)
andmed$LT.resid.neg <- ifelse(lt.model$residuals < 0, lt.model$residuals, 0)

# ECM relation
eq.lt.sym <- d(log(LT_netohind)) ~ d(log(LT_omahind)) - L(d(log(LT_netohind)),1) - L(d(log(LT_omahind)),1) + L(LT.resid.sym, 1)
lt.sym.model <- dynlm(eq.lt.sym, ts(andmed, start=c(2007,1), frequency = 12))

AIC(lt.sym.model)

summary(lt.sym.model)
checkresiduals(lt.sym.model)

adf.test(lt.sym.model$residuals)
kpss.test(lt.sym.model$residuals)


eq.lt.asym <- d(log(LT_netohind)) ~ d(log(LT_omahind)) - L(d(log(LT_netohind)),1) - L(d(log(LT_omahind)),1) + L(LT.resid.pos, 1) + L(LT.resid.neg, 1)
lt.asym.model <- dynlm(eq.lt.asym, ts(andmed, start=c(2007,1), frequency = 12))

summary(lt.asym.model)
checkresiduals(lt.asym.model)

adf.test(lt.asym.model$residuals)
kpss.test(lt.asym.model$residuals)

##############################

summary(ee.asym.model)
summary(lv.asym.model)
summary(lt.asym.model)
