##############################################
# Üldandmed meedet kasutanud ettevõtte kohta
# 
# (c) 2020 - Raoul Lättemäe
# 
##############################################

library(tidyverse)
# Lae käibe- ja statistikanumbrid
load("~/Dokumendid/R/EMTA/2019-2020.rdata")


andmed$Käive.2019 = rowSums(andmed[,c("Käive.i.2019", "Käive.ii.2019", "Käive.iii.2019", "Käive.iv.2019")], na.rm = TRUE)
andmed$Maksud.2019 = rowSums(andmed[,c("Maksud.i.2019", "Maksud.ii.2019", "Maksud.iii.2019", "Maksud.iv.2019")], na.rm = TRUE)
andmed$Tööjõumaksud.2019 = rowSums(andmed[,c("Tööjõumaksud.i.2019", "Tööjõumaksud.ii.2019", "Tööjõumaksud.iii.2019", "Tööjõumaksud.iv.2019")], na.rm = TRUE)
andmed$Töötajad.2019 = rowSums(andmed[,c("Töötajad.i.2019", "Töötajad.ii.2019", "Töötajad.iii.2019", "Töötajad.iv.2019")], na.rm = TRUE)/4
andmed$Käive.i = rowSums(andmed[c("Käive.i.2020")], na.rm = TRUE) - rowSums(andmed[c("Käive.i.2019")], na.rm = TRUE) 
andmed$Käive.ii = rowSums(andmed[c("Käive.ii.2020")], na.rm = TRUE) - rowSums(andmed[c("Käive.ii.2019")], na.rm = TRUE) 

my.andmed <- andmed %>%
  select(Registrikood, Käive.2019, Maksud.2019, Tööjõumaksud.2019, Töötajad.2019, Käive.i, Käive.ii)

# Lae ettevõtete andmed
load("~/Dokumendid/R/EMTA/2020_ii.rdata")

# Lisa Ettevõtete registrist täiendavad koodid
andmed <- left_join(my.andmed, data %>% select(Registrikood, Nimi, Liik, KMKR, EMTAK.kood, EMTAK, Maakond, Linn), by = "Registrikood")
data <- NULL

# lae töötukassa andmed
load("~/Dokumendid/R/Töötukassa/koond.rdata")

koond <- andmed %>%
  group_by(EMTAK.kood) %>%
  summarise(Töötajad = sum(Töötajad.2019), Maksud = sum(Maksud.2019), Tööjõumaksud = sum(Tööjõumaksud.2019)) 


hyvitis.koond <- left_join(data.koond, andmed, by = c("Registrikood"))

hyvitis.sum <- hyvitis.koond %>%
  group_by(EMTAK.kood) %>%
  summarise(Töötajad = sum(Töötajad.2019), Maksud = sum(Maksud.2019), Tööjõumaksud = sum(Tööjõumaksud.2019)) 

hyvitis.prop = left_join(hyvitis.sum, koond, by = c("EMTAK.kood"), suffix = c("hyvitis", "kokku"))

hyvitis.prop$töötajadpc = hyvitis.prop$Töötajadhyvitis/hyvitis.prop$Töötajadkokku
hyvitis.prop$maksudpc = hyvitis.prop$Maksudhyvitis/hyvitis.prop$Maksudkokku
hyvitis.prop$tööjõumaksudpc = hyvitis.prop$Tööjõumaksudhyvitis/hyvitis.prop$Tööjõumaksudkokku

write.csv(hyvitis.prop, "~/Dokumendid/R/Töötukassa/prop.csv")
