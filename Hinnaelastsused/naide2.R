# Consumption income cointegration
# Part 2:  Estimate a basic error correction model

library(urca)
library(forecast)
library(stats)

c.data <- read.csv("pConsumption.csv")
# Convert to TS, note this is quarterly 
c.ts <- ts(c.data$PCECC96,start=c(1947,1),end=c(2016,3),freq=4)

y.data <- read.csv("income.csv")
# Convert to TS, note this is quarterly 
y.ts <- ts(y.data$DPIC96,start=c(1947,1),end=c(2016,3),freq=4)

# take logs
ly.ts = log(y.ts)
lc.ts = log(c.ts)

# run all models in first differences
dc.ts <- diff(lc.ts)
dy.ts <- diff(ly.ts)

# get cointegrating resisuals
creg.model <- lm(lc.ts ~ ly.ts)
resid <- residuals(creg.model)
resid.ts <- ts(resid,start=c(1947,1),freq = 4)

# build lag change in c and y
dclag.ts <- stats::lag(dc.ts,-1,na.pad=TRUE)
dylag.ts <- stats::lag(dy.ts,-1,na.pad=TRUE)
residlag.ts <- stats::lag(resid.ts,-1,na.pad=TRUE)
lags.ts <- cbind(dc.ts,dy.ts,dclag.ts,dylag.ts,resid.ts,residlag.ts)
c.model <-   lm( dc.ts ~ dclag.ts + dylag.ts,data=lags.ts)
ecc.model <- lm(dc.ts ~ dclag.ts + dylag.ts + residlag.ts,data=lags.ts)

print(summary(c.model))
print(summary(ecc.model))
print(accuracy(c.model))
print(accuracy(ecc.model))

# now repeat for income
y.model <-   lm( dy.ts ~ dclag.ts + dylag.ts,data=lags.ts)
ecy.model <- lm( dy.ts ~ dclag.ts + dylag.ts + residlag.ts, data=lags.ts)


print(summary(y.model))
print(summary(ecy.model))
print(accuracy(y.model))
print(accuracy(ecy.model))
