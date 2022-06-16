###########################################
# Diiselkütuse elastsusarvutused
# Raoul Lättemäe 2021
###########################################

library(tidyverse)    # andmeanalüüsi pakett
library(xts)          # aegread
library(tseries)      # Aegrea testid
library(urca)         # Johansen cointegration test
library(seasonal)     # X-13ARIMA-SEASTS sesoonsuse kõrvaldamine
library(texreg)       # R väljund LaTex'isse
library(fpoplot)      # Jooniste kujundus
library(dynlm)        # Viitaegadega lineaarne funktsioon

# Määra töökataloog
setwd("/home/raoul/Dokumendid/R/Hinnaelastsused")

# Loe sisse andmed tibble formaadis
andmed <- read.csv("andmed.csv", sep=";") %>% as_tibble

# Tühjad lahtrid on NA
andmed[andmed == ""] <- NA

# Muuda veergude formaat korrektseks
# aeg on kuupöev ja ülejäänud on numbrid
andmed$aeg <- as.Date(andmed$aeg, "%d.%m.%Y")
andmed$D_kogus <- as.double(gsub(",",".", andmed$D_kogus))
andmed$D_hind_ee <- as.double(gsub(",",".", andmed$D_hind_ee))
andmed$D_hind_lv <- as.double(gsub(",",".", andmed$D_hind_lv))
andmed$D_hind_lt <- as.double(gsub(",",".", andmed$D_hind_lt))
andmed$D_akt <- as.double(gsub(",",".", andmed$D_akt))
andmed$SKP_ph <- as.double(gsub(",",".", andmed$SKP_ph))
andmed$Barrel <- as.double(gsub(",",".", andmed$Barrel))
andmed$kurss <- as.double(gsub(",",".", andmed$kurss))
andmed$LVD <- ifelse(andmed$D_hind_lv < andmed$D_hind_ee, 1, 0)


# Diisli kogus
ggplot(andmed, aes(x=aeg, y=D_kogus)) + geom_line(color="red") + theme_fpo()
# Sessoonne tasandus
diisel <- ts(andmed$D_kogus, start=(c(2003,1)), end=c(2021,1), frequency = 4)
d <- decompose(diisel)

#diisel <- ts(d.trend, start=(c(2003,3)), end=c(2021,1), frequency = 4)
#diisel_sa <- seas(diisel)
plot(diisel)

# Aga piiritleme andmed kasutuses olevaga
andmed <- na.omit(andmed)


diisel <- ts(andmed$D_kogus, start=(c(2005,1)), end=c(2021,1), frequency = 4)
skp <- ts(andmed$SKP_ph, start=(c(2005,1)), end=c(2021,1), frequency = 4)
skpsa <- seas(as.ts(skp))
plot(skpsa)

skpsa <- as.data.frame(skpsa)
skp.sa <- ts(skpsa$final, start=(c(2005,1)), end=c(2021,1), frequency = 4)

hind <- ts(andmed$D_hind_ee, start=(c(2005,1)), end=c(2021,1), frequency = 4)
hind_lv <- ts(andmed$D_hind_lv, start=(c(2005,1)), end=c(2021,1), frequency = 4)
hind_lt <- ts(andmed$D_hind_lt, start=(c(2005,1)), end=c(2021,1), frequency = 4)
barrel <- ts(andmed$Barrel, start=(c(2005,1)), end=c(2021,1), frequency = 4)
veoautod <- ts(andmed$Veoautod.registris, start=(c(2005,1)), end=c(2021,1), frequency = 4)
lv_dummy <- ts(andmed$LVD, start=(c(2005,1)), end=c(2021,1), frequency = 4)
tax = ts(andmed$D_akt, start=(c(2005,1)), end=c(2021,1), frequency = 4)


vat <- hind * (1 - 1/1.2)
netohind <- hind - vat
marginaal <- netohind - barrel - tax

plot(ts.union(hind, barrel))

test <- dynlm(hind ~  barrel + tax)
plot(test$residuals)
summary(ur.df(test$residuals))

co_test <- ca.jo(ts.union(log(hind), log(barrel)))
summary(co_test)

#plot(seas(as.ts(diisel)))

plot(marginaal/hind)

h <- decompose(hind)

test <- ts.union(diisel, skp.sa)
plot(test)
test.co <- ca.jo(ts.union(log(diisel), log(skp.sa)))
summary(test.co)

# ADF unit root test. H0 - unit root (nontstationary), one has to prove
summary(ur.df(diisel))

long <- lm (log(diisel) ~ log(skp.sa) + log(hind))
plot(long$residual, type="l")

# Statsionaarne
summary(ur.df(long$residual, lags = 1, type="none"))
summary(long)

hind.ts <- ts.union(hind, hind_lv, hind_lt)
plot(hind.ts)
co_test <- ca.jo(hind.ts, ecdet="none")
summary(co_test)

plot(hind.w)
co_test <- ca.jo(hind.w, ecdet="trend")
summary(co_test)

Trend <- seq_along(hind)
Trend <- ts(Trend, start=(c(2005,1)), end=c(2021,1), frequency = 4)
m.w <- lm (hind ~ barrel + Trend)
plot(m.w$residuals, type="l")

s <- series(skpsa, "fct")

plot(andmed)


data.ts <- ts.union(diisel, skp.sa, hind, veoautod)
plot(data.ts)

co_test <- ca.jo(data.ts)
summary(co_test)

model <- lm(log(diisel) ~ log(skp.sa) + Trend + log(hind))
plot(model$residuals, type="l")

summary(model)
adf.test(model$residuals)

diisel = d$trend

# Logaritmieds
ldiisel_v = log(diisel/veoautod)
ldiisel = log(diisel)
lskp = log(skp.sa)
lskp_v = log(skp.sa/veoautod)
lhind = log(hind)
lhind_lv = log(hind_lv)
lv_diff = log(hind/hind_lv)
lt_diff = log(hind/hind_lt)
ltax = log(tax)
lautod = log(veoautod)

# Hinda pikaajalist seost
co_test <- ca.jo(ts.union(ldiisel,lskp,lhind, lt_diff), ecdet = "trend")
summary(co_test)

Näitajad <- ts.union(diisel,skp.sa,hind,veoautod/1000, diisel/veoautod*1000)
colnames(Näitajad) <- c("Dekl. Diisel", "SKP", "Diislikütuse hind Eestis", "Veoautode arv (tuh)", "Dekl. Diisel veoauto kohta")

plot(Näitajad,
     col="red",
     xlab = (c("Aeg","Aeg","Aeg","Aeg","Aeg","Aeg")))

Hinnaandmed <- ts.union(barrel, tax, hind,hind/hind_lt, hind/hind_lv)
colnames(Hinnaandmed) <- c("Nafta maailmaturuhind", "Aktsisiimäär", "Diislikütuse hind Eestis", "EE/LT hinnaerinevus", "EE/LV hinnaerinevus")

plot(Hinnaandmed,
     col="red",
     xlab = (c("Aeg","Aeg","Aeg","Aeg","Aeg","Aeg")),
     main="") + 
  title("Näitajad (jätk)") 



plot(ts.union(diisel,skp.sa,hind,hind/hind_lt, veoautod, diisel/veoautod*1000), 
     main="", col="red",
     xlab = (c("Aeg","Aeg","Aeg","Aeg","Aeg","Aeg"))) + 
  title("Näitajad")

summary(dynlm(ldiisel ~ lskp + lhind + lt_diff + log(Trend)))

test.ecm <- dynlm(ldiisel_v ~ lskp_v + lhind + lt_diff + log(Trend))
summary(test.ecm)
AIC(test.ecm)

summary(ur.df(test.ecm$residuals))

summary(dynlm(test.ecm$residuals ~ L(test.ecm$residuals, 1)))

ld <- ldiisel
plot(log(Trend))

lt.ecm <- dynlm(ldiisel ~ lskp + log(netohind) + d(ltax))
AIC(lt.ecm)
summary(lt.ecm)

summary(dynlm(ldiisel ~ log(skp) + lhind + log(hind/hind_lt) + L(ltax,-1)))

long.ecm <- dynlm(ldiisel ~ log(skp) + lhind + log(hind/hind_lt))

summary(ur.df(long.ecm$residuals))
long.co <- ca.jo(ts.union(ldiisel, log(skp), lhind, log(hind/hind_lt)))
summary(long.co)
AIC(long.ecm)
summary(long.ecm)
plot(long.ecm$residuals)

p <- ts(predict(long.ecm), start=(c(2005,1)), end=c(2020,4), frequency = 4)

lr <- ts.union(exp(p), exp(ldiisel))
ts.plot(lr, col=c("red", "black"))

resid = long.ecm$residuals
plot(resid, type="l")

AIC(long.ecm)
adf.test(long.ecm$residuals)
summary(ur.df(long.ecm$residuals, type="none"))
plot(long.ecm$residuals, type="l")
acf(long.ecm$residuals)
pacf(long.ecm$residuals)

log_Diisel <- log(diisel)

ldiisel_v.d <- diff(ldiisel_v)
lskp_v.d <- diff(lskp_v)
lskp.d <- diff(lskp)
lhind.d <- diff(lhind)
lt_diff.d <- diff(lt_diff)
lv_diff.d <- diff(lv_diff)
ecm.term <- lt.ecm$residuals[-1]

ecm.term <- long.ecm$residuals


r <- long.ecm$residuals
rr <- long.ecm$residuals[-1]

summary(dynlm(ecm.term ~ L(ecm.term, 1)))

plot(ts.union(ldiisel_v.d, lskp.d, lhind.d, lt_diff.d, ecm.term))

summary(dynlm(d(ldiisel) ~ d(lskp) + d(lhind) + L(ldiisel,1) + L(lhind,1) + L(lskp,1) + L(lt_diff,1) + L(ltax, -1)))

mudel <- dynlm(d(ldiisel) ~ d(lskp) + d(lhind) + L(ldiisel,1) + L(lhind,1) + L(lskp,1) + L(lt_diff,1) + L(ltax, -1))
summary(ur.df(mudel$residuals))
plot(mudel$residuals)

p <- ts(predict(mudel), start=(c(2010,4)), end=c(2020,4), frequency = 4)
cs <- exp(cumsum(p)+4.38)

lr <- ts.union(cs, diisel)
ts.plot(lr, col=c("red", "black"))


ecm.resid = ts(long.ecm$residuals, start=(c(2005,1)), end=c(2021,1), frequency = 4)

short.ecm <- dynlm(d(ldiisel) ~ d(lskp) + d(lt_diff) + d(lhind) + L(d(ltax),-1) + L(ecm.resid,1))
AIC(short.ecm)
summary(short.ecm)
stargazer(short.ecm)
summary(test.ecm)

d.predixt <- predict(short.ecm)

s <- ts(predict(short.ecm), start=(c(2004,4)), end=c(2020,4), frequency = 4)
s <- cumsum(s)
sr <- ts.union(s, ldiisel)
ts.plot(sr, col=c("red", "black"))


a <- ts.union(d.predixt, diff(ldiisel))
a <- as.data.frame(a)
a <- setNames(a, c('f', 'e'))

plot(cumsum(a$f), type="l")
plot(cumsum(a$e), type="l")


b <- ts(a, start=(c(2005,1)), end=c(2021, 4), frequency = 4)

AIC(short.ecm)
adf.test(short.ecm$residuals)
summary(ur.df(short.ecm$residuals, type="none"))
plot(short.ecm$residuals, type="l")
acf(short.ecm$residuals)
pacf(short.ecm$residuals)



#library(corrplot)

set <- ts.union(ldiisel_v, lskp_v, hind, lt_diff, Trend)
set.df <- as.data.frame(set)
set.df <- na.omit(set.df)


# See mudel töötab.. Miks - kas kointegratsioon on SKP ja maksude vahel
data.log <- ts.union(ldiisel_v, lskp, lhind, lv_diff, ltax)
plot(data.log)
co_test <- ca.jo(data.log, ecdet="trend")
summary(co_test)

pikk <- lm(ldiisel_v ~ lskp + lhind + lv_diff + ltax)
summary(pikk)
plot(pikk$residuals, type="l")
adf.test(pikk$residuals)
pikk.resid <- ts(pikk$residuals, start=(c(2003,1)), end=c(2020,4), frequency = 4)
plot(pikk.resid)

plot(seas(pikk.resid))

plot(ts.union(ldiisel_v, ldiisel, lskp, lhind, lv_diffr, ltax, barrel))

dldiisel_v = diff(ldiisel_v)
dlskp = diff(lskp)
dlhind = diff(lhind)
dlv_diff = diff(lv_diff)
dltax = diff(ltax)

ldl <- ts(lag(as.tibble(dldiisel_v)), start=(c(2003,1)), end=c(2020,4), frequency = 4)
# Veaparandusmudel
mudel <- lm(dldiisel_v ~ ldl + dlskp + dlv_diff + dltax + pikk.resid)
summary(mudel)


library(ecm)
dd = as.data.frame(ldiisel_v)

data.log.df <- as.data.frame(data.log)
# Jäta NA vahele
data.log.df <- data.log.df[complete.cases(data.log.df), ]

model.ecm <- ecm(data.log.df[c("ldiisel_v")], data.log.df[c("ldiisel_v", "lskp", "lhind", "lv_diff", "ltax")], lags=1, data.log.df[c("lskp", "lhind", "lv_diff")])
summary(model.ecm)

# Latexisse
library(xtable)
print(xtable(summary(model.ecm), type="latex"))
xtable(pikk, caption = "Veaparandusmudel")

# Latexisse
library(texreg)
texreg(pikk, single.row = TRUE)

# Latexisse
library(stargazer)
stargazer(pikk)
stargazer(model.ecm)

resid <- ts(model$residuals, start=(c(2003,1)), end=c(2021,1), frequency = 4)
plot(resid)
plot(andmed$Barrel, type="l")

resid.d <- as.data.frame(resid)

plot(data.ts)

plot(model$residuals, type="l")
plot(andmed$D_hind_lv - andmed$D_hind_ee, type="l")

ggplot(andmed, aes(x=aeg)) +
  geom_line(aes(y=D_kogus * 1000000/Veoautod.registris), color="red") +
  geom_line(aes(y=D_hind_ee), color="blue") + 
  geom_line(aes(y=(D_hind_ee-D_hind_lt)*5), color="green") + 
  geom_line(aes(y=SKP_ph*20000/Veoautod.registris))


ggplot(andmed, aes(x=aeg)) + theme_fpo() +
  geom_line(aes(y=D_kogus*1000000/Veoautod.registris)) + 
  geom_line(aes(y=Veoautod.registris/100)) + 
  geom_line(aes(y=D_kogus*10), color="red") + 
  labs(
    title = "Deklareeritud diislikütus registrisse kantud veoauto kohta",
    subtitle = "(liitrites)",
    fill = "",
    color = "",
    caption = paste(
      "Allikas: Maksuamet, Statistikaamet, Rahandusministeeriumi arvutused", 
      #      ". Joonis: Raoul Lä ttemäe, Rahandusministeerium",
      sep = " "), 
    y = element_blank(), 
    x = element_blank()
  ) #+  theme_fpo()


pilt <- as.ts(andmed %>% select(D_kogus), start=(c(2003,1)), end=c(2021,1), frequency = 4)
autoplot.zoo(pilt, facet = TRUE) + theme_fpo()

ggplot(pilt, aes(index, value)) + geom_line() + theme_fpo() +  facet_grid(id ~ .)

# Konverteeri aegreaks
ts <- xts(andmed, andmed$aeg)
ggplot(ts, aes(x = aeg, y = D_hind_ee)) + geom_line()

autoplot.zoo(ts[,"D_kogus"], multipanel = TRUE, facets = NULL, geom="line")

lm.1 <- lm(log(D_kogus/Veoautod.registris) ~ log(D_hind_ee) + log(D_hind_lv/D_hind_ee) + log(SKP_ph), andmed)
lm.2 <- lm(log(D_hind_ee) ~ log(Barrel/kurss) + log(D_akt), andmed)
summary(lm.2)

lm.l.2 <- lm((D_hind_ee) ~ (Barrel/kurss) + (D_akt), andmed)
summary(lm.l.2)
adf.test(lm.l.2$residuals)

plot(log(andmed$D_kogus), pch=16, col = "blue")
abline(lm.2)

# save predictions of the model in the new data frame 
# together with variable you want to plot against
predicted_df <- data.frame(mpg_pred = predict(lm.2, andmed), aeg = (andmed$aeg), D_hind_ee=log(andmed$D_hind_ee))

ggplot(data = predicted_df, aes(x = aeg)) + 
  geom_line(aes(y=D_hind_ee)) + 
  geom_line(aes(y=mpg_pred), color="red")

# this is the predicted line of multiple linear regression
ggplot(data = andmed, aes(x = aeg, y = hp)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = predicted_df, aes(x=mpg_pred, y=hp))


#lm.3 <- lm(diff(log(D_hind_ee)) ~ diff(log(Barrel/kurss)) + diff(log(D_akt)) + paste("lagg"), andmed)

# Testid

adf.test(na.omit(ts[,"Veoautod.registris"]))
kpss.test(na.omit(ts[,"Veoautod.registris"]), null="Trend", lshort=FALSE)
po.test(na.omit(ts[, c("D_hind_ee", "D_hind_lv")]))

jotest <- ca.jo(na.omit(andmed[, c("D_kogus", "D_hind_ee", "SKP_ph", "Veoautod.registris")]))
summary(jotest)
