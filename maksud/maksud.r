#######################################################
# See kood tõmbab maksuametist tasutud maksude andmed ja teisendab need 
# R-i andmebaasiks
#
# Autor: Raoul Lättemäe
# Versioon 1.0 17.06.2020
#######################################################

# Määra töökeskkond
# Määra töökaust, mida kasutatakse andmete laadimiseks
setwd(file.path("/home/raoul/Dokumendid/RM/maksud"))
# Kontrolli töökataloogi asukohta
getwd()

# Lae andmed EMTA koduleheküljelt
url <- "https://www.emta.ee/sites/default/files/kontaktid-ja-ametist/maksulaekumine-statistika/tasutud-maksud/tasutud_maksud_2019_iv_kvartal.csv"
data <- read.csv(file = url, sep = ";", fileEncoding = "latin1")

# kontrolli andmeid
str(data)

# korrasta nimetused
colnames(data) <- c("Registrikood", 
                    "Nimi", 
                    "Liik", 
                    "KMKR", 
                    "EMTAK",
                    "Maakond",
                    "Maksud",
                    "Tööjõumaksud",
                    "Käive",
                    "Töötajad")

# puhasta ja korrasta andmed
data$Liik <- as.factor(data$Liik)
data$KMKR <- as.factor(data$KMKR)
data$EMTAK <- as.factor(data$EMTAK)
data$Maakond <- as.factor(data$Maakond)
data$Maksud <- as.numeric(sub(" ","", sub(",",".", data$Maksud, fixed = TRUE), fixed = TRUE))
data$Tööjõumaksud <- as.numeric(sub(" ","", sub(",",".", data$Tööjõumaksud, fixed = TRUE), fixed = TRUE))
data$Käive <- as.numeric(sub(" ","", sub(",",".", data$Käive, fixed = TRUE), fixed = TRUE))
data$Töötajad <- as.numeric(data$Töötajad)

# lisa kuupäev
data$Kuupäev <- as.Date("31.12.2019", "%d.%m.%y")

# kontrolli andmed
str(data)

# salvesta andmed
save(data, file="2019-IV.rdata")
# lae andmed

# vaata andmeid
library(tidyverse)
b <- ggplot(data, aes(x = Käive/Töötajad, y=(Maksud-Tööjõumaksud)/Käive))

b <- ggplot(data, aes(x = Käive/Töötajad, y=Tööjõumaksud/Töötajad))

b + geom_point(aes(size=Käive), colour="deepskyblue2", alpha = 0.25)

b + geom_jitter(aes(y=Liik), shape=16, position=position_jitter(0.2), colour="deepskyblue2",  aplha = I(0.1)) +
  geom_boxplot(aes(y=Liik), alpha = I(0.6), outlier.shape=NA) + xlim(0,1000000)

# Piira skaalat
b + 
  geom_point(aes(size=Käive), colour="deepskyblue2", alpha = 0.1) +
  ylim(-1,5) +
  xlim(0,1000000)
