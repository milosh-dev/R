# Cointegration model for interest rates
# USe government bond yields for 10 year and 3 month treasuries
# https://people.brandeis.edu/~blebaron/classes/fin250a/cointegration/errorCorrection.html

library(urca)
library(forecast)
setwd("/home/raoul/Dokumendid/R/Hinnaelastsused")
GB.data <- read.csv("USTreasury10.csv")
# Convert to TS, note this is monthly
gs10.ts <- ts(GB.data$GS10,start=c(1953,4),freq=12)

TB.data <- read.csv("USTreasury3M.csv")
TB.ts <- ts(TB.data$TB3MS, start=c(1934,1),freq=12)
# shorten this longer series to align with gp series
TB.ts <- window(TB.ts,start=c(1953,4))

# Joonista
ts.data <- ts.union(TB.ts, gs10.ts)
plot(ts.data, main="Data")

# Johansen cointegration test (viitab ühele kointegratsioonivektorile)
co.test <- ca.jo(ts.data)
summary(co.test)

# Cointegration model and residuals
# We wont bother testing stuff
creg.model <- lm( gs10.ts ~ TB.ts)
creg.res <- residuals(creg.model)
creg.ts <- ts(creg.res,start=c(1953,4),freq=12)

plot(creg.ts)

# Augmented Dickey-Fuller test
adf.test <- ur.df(creg.ts)
summary(adf.test)

dTB.ts <- diff(TB.ts)
dgs10.ts <- diff(gs10.ts)

# you can use residuals, or even the simple difference
# creg.ts <- gs10.ts - TB.ts


dTBlag.ts <- stats::lag(dTB.ts,-1,na.pad=TRUE)
dgs10lag.ts <- stats::lag(dgs10.ts,-1,na.pad=TRUE)
creglag.ts <- stats::lag(creg.ts,-1,na.pad=TRUE)

veclags.ts <- cbind( dTB.ts, dgs10.ts, dTBlag.ts, dgs10lag.ts, creg.ts, creglag.ts)

vecseriestrain.ts <- window(veclags.ts, end=c(1999,12))
vecseriesvalid.ts <- window(veclags.ts, start=c(2000,1))

# Full sample
ec.model <- lm( dTB.ts ~ dTBlag.ts + dgs10lag.ts + creglag.ts, data=veclags.ts )
# print("Full sample: EC model")
print(summary(ec.model))

ecbench.model <- lm( dTB.ts ~  dTBlag.ts + dgs10lag.ts, data=veclags.ts )
# print("Full sample: bench")
print(summary(ecbench.model))
# print("Full sample: bench")
# print(accuracy(fitted(ecbench.model),dTB.ts))



# Do training and validation predictions
# first full model
ectrain.model <- lm(dTB.ts ~ dTBlag.ts + dgs10lag.ts + creglag.ts, data = vecseriestrain.ts )

pred <- predict(ectrain.model,vecseriesvalid.ts)
plot(pred, type="l")

print("in and out of sample: ec model")
print(accuracy(ectrain.model))
print(accuracy(pred,as.vector(vecseriesvalid.ts[,1])))


# now benchmark model w/o error correction component

ectrain.model <- lm(dTB.ts ~ dTBlag.ts + dgs10lag.ts, data = vecseriestrain.ts )

pred <- predict(ectrain.model,vecseriesvalid.ts)

print("in and out of sample: benchmark model")
print(accuracy(ectrain.model))
print(accuracy(pred,as.vector(vecseriesvalid.ts[,1])))

