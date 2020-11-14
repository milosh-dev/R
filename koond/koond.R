################################################
# Kui palju on likvideerimisel äriühinguid
#
# Autor: Raoul Lättemäe, august 2020
################################################

library(tidyverse)

# koondandmed
load("~/Dokumendid/R/koondmeetmed.rdata")

# Äriregistri andmed
load("~/Dokumendid/R/Äriregister/ariregister.rdata")

koond <- left_join(koond, data, by=c("Registrikood"))

# Likvideerimisel olevad ettevõtted
likvideerimisel <- koond %>%
  filter(Staatus != "R")

kes.likvideerimisel <- likvideerimisel %>%
  select(Staatus, Registrikood, Nimi) %>%
  group_by(Staatus, Registrikood) %>%
  summarise(Arv = n())

ev.kaupa <- likvideerimisel %>%
  group_by(Staatus, Meede) %>%
  summarise(Arv = n(), Kokku = sum(Summa))

# Ettevõtjaid, keda pole äriregistris
kustutadud <- koond %>%
  filter(is.na(Nimi)) %>%
  filter(nchar(Registrikood) == 8)

kustutatud.sum <- kustutadud %>%
  group_by(Meede) %>%
  summarise(Kokku = sum(Summa))

likvideerimisel.sum <- likvideerimisel %>%
  group_by(Meede) %>%
  summarise(Kokku = sum(Summa))

# Kui palju töötas neis ettevõtetes
load("../EMTA/registrikoodiga_full.rdata")
koond.registriga <- rbind(likvideerimisel, kustutadud)

kogusumma <- koond.registriga %>%
  summarize(Kogusumma = sum(Summa))

koond.registriga <- left_join(koond.registriga, kitsend, by = "Registrikood")

# Asenda "Töötajate arv teadmata" nulliga

koond.registriga$Töötajad <- ifelse(is.na(koond.registriga$Töötajad), 0, koond.registriga$Töötajad)

koond.registriga$Kuupäev <- as.factor(koond.registriga$Kuupäev)

koond.registriga <- koond.registriga %>%
  filter(!is.na(Kuupäev)) %>%
  select(Summa, Töötajad, Kuupäev)


koond.arv <- koond.registriga %>%
  group_by(Kuupäev) %>%
  summarise(Töötajate.arv = sum(Töötajad), Kogusumma = sum(Summa))
