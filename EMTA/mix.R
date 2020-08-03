##############################################
# Põimib äriregistri andmed andmebaasiks
# 
# (c) 2020 - Raoul Lättemäe
# 
##############################################

library(tidyverse)

load("2020_ii.rdata")
ii.2020 <- data
rm(data)

load("2020_i.rdata")
i.2020 <- data
rm(data)

load("2019_iv.rdata")
iv.2019 <- data
rm(data)

load("2019_iii.rdata")
iii.2019 <- data
rm(data)

load("2019_ii.rdata")
ii.2019 <- data
rm(data)

load("2019_i.rdata")
i.2019 <- data
rm(data)

# Koosta ettevõtete loend
reg_code <- select(ii.2020, Registrikood)
reg_code_A <- select(i.2020, Registrikood)
reg_new <- setdiff(reg_code_A, reg_code)

register <- select(ii.2020, Registrikood, Nimi, Liik, KMKR, EMTAK.kood, EMTAK, Omavalitsus, Maakond, Vald, Linn)
register_A <- select(i.2020, Registrikood, Nimi, Liik, KMKR, EMTAK.kood, EMTAK, Omavalitsus, Maakond, Vald, Linn)

register_A <- left_join(reg_new, register_A, by="Registrikood")
register <- rbind(register, register_A)

# uuesti
reg_code <- select(register, Registrikood)
register_A <- select(i.2019, Registrikood, Nimi, Liik, KMKR, EMTAK.kood, EMTAK, Omavalitsus, Maakond, Vald, Linn)
reg_code_A <- select(register_A, Registrikood)
reg_new <- setdiff(reg_code_A, reg_code)


register_A <- left_join(reg_new, register_A, by="Registrikood")
register <- rbind(register, register_A)


length(unique(register[["Registrikood"]]))

str(register)
register
tail(register)
View(register)

save(register, file="register.rdata")
