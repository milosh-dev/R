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

d.ee.omahind <- diff(andmed$EE_omahind)
d.ee.netohind <- diff(andmed$EE_netohind)
d.ee.aktsiis <- diff(andmed$EE_Aktsiis)

d.ee.omahind.pos <- ifelse(d.ee.omahind > 0, d.ee.omahind, 0)
d.ee.omahind.neg <- ifelse(d.ee.omahind < 0, d.ee.omahind, 0)

eq.ee.lr <- EE_netohind ~ EE_omahind + EE_Aktsiis
model.ee.lr <- lm(eq.ee.lr, andmed)
summary(model.ee.lr)

l.ee.resid <- as.data.frame(lag(model.ee.lr$residuals))
l.ee.resid <- l.ee.resid[-1,]
l.ee.resid.pos <- ifelse(l.ee.resid > 0, l.ee.resid, 0)
l.ee.resid.neg <- ifelse(l.ee.resid < 0, l.ee.resid, 0)

eq.ee.sr <- d.ee.netohind ~ d.ee.omahind + lag(d.ee.omahind, 1) - d.ee.aktsiis - lag(d.ee.aktsiis, 1) + l.ee.resid
model.ee.sr <- lm(eq.ee.sr)
AIC(model.ee.sr)
summary(model.ee.sr)


eq.ee.asr <- d.ee.netohind ~ 
  d.ee.omahind.pos + lag(d.ee.omahind.pos, 1) - lag(d.ee.omahind.pos, 2) - lag(d.ee.omahind.pos, 3) + 
  d.ee.omahind.neg + lag(d.ee.omahind.neg, 1) - lag(d.ee.omahind.neg, 2) - lag(d.ee.omahind.neg, 3) - 
  d.ee.aktsiis - lag(d.ee.aktsiis, 1) - lag(d.ee.aktsiis, 2) - lag(d.ee.aktsiis, 3) +
  l.ee.resid.pos + l.ee.resid.neg

model.ee.asr <- lm(eq.ee.asr)
AIC(model.ee.asr)
summary(model.ee.asr)

