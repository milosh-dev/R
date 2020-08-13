##############################################
# Arvutab Herfindhali indeksi
# 
# (c) 2020 - Raoul Lättemäe
# 
##############################################

library(foreign)
Panel <- read.dta("http://dss.princeton.edu/training/Panel101.dta")
coplot(y ~ year|country, type="l", data=Panel) 
