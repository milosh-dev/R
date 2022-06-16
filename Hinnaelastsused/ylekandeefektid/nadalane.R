library(fredr)
library(tidyverse)
library(openxlsx)     # impordi excelist
library(dynlm)
library(urca)
library(forecast)
library(tseries)


# Tühjenda sessioon andmetest
rm(list = ls())

#
# Tõmba naftahinnad ja usd/eur vahetuskruss St. Louis Fedi andmebaasist
#

params <- list(
  series_id = c("DCOILBRENTEU", "DEXUSEU"), # Brent oil in EU $/barerl, USD/EUR exchange rate
  observation_start = rep(as.Date("2007-01-01"), times = 2),
  frequency = rep("wesu", times = 2)  # Nädala keskmine, nädal lõppeb pühapäevaga
#  units = rep("pc1", times = 3) # pc1 - aastakasv
)

val <- pmap_dfr(
  .l = params,
  .f = ~ fredr(series_id = ..1, observation_start = ..2, frequency = ..3)
)

nafta <- val %>% 
  select(series_id, value, date) %>%
  pivot_wider(names_from = series_id, values_from = value)

# Lisa kuupäevale üks päev (eelmise nädala keskmine hind vs esmaspäevane hind?)
nafta$date <- as.Date(nafta$date) + 1

# Leia hind eurodes liitri kohta
nafta$price <- nafta$DCOILBRENTEU / nafta$DEXUSEU / 158.987295

# Kustuta ebavajalikud andmed
rm(params)
rm(val)

# Loe sisse Baltikumi hinnaandmed
setwd("/home/raoul/Dokumendid/R/Hinnaelastsused/ylekandeefektid")
data <- read.xlsx(xlsxFile = "andmed.xlsx")
data$date <- as.Date(data$date, origin="1899-12-30")

# ühenda Eesti andmed naftahindade andmetega
data <- left_join(data, nafta, by = c("date" = "date"))

# Teisenda aktsiis 1000 liitrilt liitrile
data$EE_aktsiis <- data$EE_aktsiis / 1000
data$LV_aktsiis <- data$LV_aktsiis / 1000
data$LT_aktsiis <- data$LT_aktsiis / 1000

# leia käibemaksuvaba lõpphind
data$EE_netohind <- data$EE / (1 + data$EE_VAT)
data$LV_netohind <- data$LV / (1 + data$LV_VAT)
data$LT_netohind <- data$LT / (1 + data$LT_VAT)

#data$dummy <- ifelse(data$date == "2020-04-27", 1, 0)

data$dprice <- d(price)

data <- data %>%
  filter(date > as.Date("2012-01-01"))

# Pikaajaline seos
eq.ee <- log(EE_netohind) ~ log(EE_aktsiis) + log(price)
ee.model <- dynlm(eq.ee, ts(data, start=c(2007,1), frequency = 365.25/7))

summary(ee.model)
checkresiduals(ee.model)

p <- predict(ee.model)

da <- ts.union(ts(exp(p)), data$EE_netohind)

# Joonista
plot.zoo(da, plot.type="single", col=c("red", "black"))

adf.test(ee.model$residuals)
kpss.test(ee.model$residuals)

# Cointegration test
po.test(log(data[,c("EE_netohind", "EE_aktsiis", "price")]))
summary(ca.jo(log(data[,c("EE_netohind", "EE_aktsiis", "price")])))

plot(data)

data$EE_pred <- exp(p)
data$EE.resid.sym <- ee.model$residuals
data$EE.resid.pos <- ifelse(ee.model$residuals > 0, ee.model$residuals, 0)
data$EE.resid.neg <- ifelse(ee.model$residuals < 0, ee.model$residuals, 0)

# ECM relation
eq.sym <- d(log(EE_netohind)) ~ d(log(price)) - L(d(log(EE_netohind)),1) - L(d(log(price)),1) + L(EE.resid.sym, 1)
ee.sym.model <- dynlm(eq.sym, ts(data, start=c(2007,1), frequency = 365.25/7))

AIC(ee.sym.model)

summary(ee.sym.model)
checkresiduals(ee.sym.model)

adf.test(ee.sym.model$residuals)
kpss.test(ee.sym.model$residuals)


eq.ee.asym <- d(log(EE_netohind)) ~ d(log(price)) + L(EE.resid.pos, 1) + L(EE.resid.neg, 1)
ee.asym.model <- dynlm(eq.ee.asym, ts(data, start=c(2007,1), frequency = 365.25/7))

summary(ee.asym.model)
checkresiduals(ee.asym.model)

adf.test(ee.asym.model$residuals)
kpss.test(ee.asym.model$residuals)

library(car)
linearHypothesis(ee.asym.model, c("L(EE.resid.pos, 1) = L(EE.resid.neg, 1)"))

#################################
# Pikaajaline seos
eq.lv <- log(LV_netohind) ~ log(LV_aktsiis) + log(price)
lv.model <- dynlm(eq.lv, ts(data, start=c(2007,1), frequency = 365.25/7))

summary(lv.model)
checkresiduals(lv.model)

p <- predict(lv.model)

da <- ts.union(ts(exp(p)), data$LV_netohind)

plot(da, type="l", col=c("red", "black"))

adf.test(lv.model$residuals)
kpss.test(lv.model$residuals)

# Cointegration test
po.test(log(data[,c("LV_netohind", "LV_aktsiis", "price")]))
summary(ca.jo(log(data[,c("LV_netohind", "LV_aktsiis", "price")])))

plot(da)

data$LV_pred <- exp(p)
data$LV.resid.sym <- lv.model$residuals
data$LV.resid.pos <- ifelse(lv.model$residuals > 0, lv.model$residuals, 0)
data$LV.resid.neg <- ifelse(lv.model$residuals < 0, lv.model$residuals, 0)

# ECM relation
eq.sym <- d(log(LV_netohind)) ~ d(log(price)) - L(d(log(LV_netohind)),1) - L(d(log(price)),1) + L(LV.resid.sym, 1)
lv.sym.model <- dynlm(eq.sym, ts(data, start=c(2007,1), frequency = 365.25/7))

AIC(lv.sym.model)

summary(lv.sym.model)
checkresiduals(lv.sym.model)

adf.test(lv.sym.model$residuals)
kpss.test(lv.sym.model$residuals)


eq.lv.asym <- d(log(LV_netohind)) ~ d(log(price)) + L(LV.resid.pos, 1) + L(LV.resid.neg, 1)
lv.asym.model <- dynlm(eq.lv.asym, ts(data, start=c(2007,1), frequency = 365.25/7))

summary(lv.asym.model)
checkresiduals(lv.asym.model)

adf.test(lv.asym.model$residuals)
kpss.test(lv.asym.model$residuals)

library(car)
linearHypothesis(lv.asym.model, c("L(LV.resid.pos, 1) = L(LV.resid.neg, 1)"))

#################################
# Pikaajaline seos
eq.lt <- log(LT_netohind) ~ log(LT_aktsiis) + log(price)
lt.model <- dynlm(eq.lt, ts(data, start=c(2007,1), frequency = 365.25/7))

summary(lt.model)
checkresiduals(lt.model)

p <- predict(lt.model)

da <- ts.union(ts(exp(p)), data$LT_netohind)

plot(da, type="l", col=c("red", "black"))

adf.test(lt.model$residuals)
kpss.test(lt.model$residuals)

# Cointegration test
po.test(log(data[,c("LT_netohind", "LT_aktsiis", "price")]))
summary(ca.jo(log(data[,c("LT_netohind", "LT_aktsiis", "price")])))

plot(da)

data$LT_pred <- exp(p)
data$LT.resid.sym <- lt.model$residuals
data$LT.resid.pos <- ifelse(lt.model$residuals > 0, lt.model$residuals, 0)
data$LT.resid.neg <- ifelse(lt.model$residuals < 0, lt.model$residuals, 0)

# ECM relation
eq.sym <- d(log(LT_netohind)) ~ d(log(price)) - L(d(log(LT_netohind)),1) - L(d(log(price)),1) + L(LT.resid.sym, 1)
lt.sym.model <- dynlm(eq.sym, ts(data, start=c(2007,1), frequency = 365.25/7))

AIC(lt.sym.model)

summary(lt.sym.model)
checkresiduals(lt.sym.model)

adf.test(lt.sym.model$residuals)
kpss.test(lt.sym.model$residuals)


eq.lt.asym <- d(log(LT_netohind)) ~ d(log(price)) + L(LT.resid.pos, 1) + L(LT.resid.neg, 1)
lt.asym.model <- dynlm(eq.lt.asym, ts(data, start=c(2007,1), frequency = 365.25/7))

summary(lt.asym.model)
checkresiduals(lt.asym.model)

adf.test(lt.asym.model$residuals)
kpss.test(lt.asym.model$residuals)

library(car)
linearHypothesis(lt.asym.model, c("L(LT.resid.pos, 1) = L(LT.resid.neg, 1)"))

