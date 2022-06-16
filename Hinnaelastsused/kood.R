###########################################
# Diiselkütuse elastsusarvutused
# Puhas fail
# Raoul Lättemäe 2021
###########################################
# Teegid
library(xts)          # Aegread
library(urca)         # Johansen cointegration test
library(dynlm)        # Aegridadele orienteeritud lineaarsete mudelite hindamine 
library(tseries)      # Aegrea testid

# Määra töökataloog
setwd("/home/raoul/Dokumendid/R/Hinnaelastsused")

# Lae andmed
#andmed <- readRDS("andmed.rds")    # Dataframe. Aga vist ei ole vajalik 
load("andmed.RData")

# Arvuta diisli hinnaerinevus Eesti ja teiste Balti riikide vahel
andmed.xts$D_ee_lv = andmed.xts$D_hind_ee/andmed.xts$D_hind_lv
andmed.xts$D_ee_lt = andmed.xts$D_hind_ee/andmed.xts$D_hind_lt

# Vii kõik andmed logaritmkujule
andmed.ln <- as.xts(log(andmed.xts))

# Mudelite hindamine eeldab, et NA liikmeid ei ole... Kitsendame andmevalimi ajavahemikule 2005-2020
andmed.full <- as.xts(andmed.ln["2005/2020"])

# Testi andmete statsionaarsust (Ükski pole taseme-statsionaarne)
adf.test(andmed.full$Diisel_S)
adf.test(andmed.full$skp.sa)
adf.test(andmed.full$D_hind_ee)
adf.test(andmed.full$D_ee_lv)
adf.test(andmed.full$D_ee_lt)

# Testi kointegratsiooni olemasolu 
# Johansen feilib
summary(ca.jo(andmed.full[, c("Diisel_S", "skp.sa", "D_hind_ee", "D_ee_lt")]))
# Philips-Oularis feilib
po.test(andmed.full[, c("Diisel_S", "skp.sa", "D_hind_ee")])

# Johanseni test feilib: Statistik on 19.86, vaja oleks üle 27.14...
# Testi lineaarse funktsiooni jääkliikmete statsionaarsust
test.eq <- d(Diisel) ~ skp.sa + D_hind_ee - D_ee_lt
test.ln <- dynlm(test.eq, as.ts(andmed.full))

f <- forecast(test.ln, as.data.frame(andmed.full))
plot(f)
theme_fpo()


library(ggplot2)
library(fpoplot)

# Function to get residuals
resid.test <- function(resid) {
  residuals <- (resid - mean(resid)) / sd(resid)
  hist(resid, freq = FALSE)
  curve(dnorm, add = TRUE)
}

resid.test(test.ln$residuals)

forecast::checkresiduals(test.ln, theme="theme_fpo")

#qplot(resid,
#      geom = "histogram",
#      bins = 10) +
#  labs(title = "Histogram of residuals",
#       x = "residual") + theme_fpo()


# ADF statistik jääkliikmete statsionaarsusele
adf.test(test.ln$residuals)
kpss.test(test.ln$residuals, null="Trend")

# Testi struktuurse muudatuse olemasolu

