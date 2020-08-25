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

likvideerimisel <- koond %>%
  filter(Staatus != "R")

ev.kaupa <- likvideerimisel %>%
  group_by(Staatus, Meede) %>%
  summarise(Arv = n(), Kokku = sum(Summa))
  