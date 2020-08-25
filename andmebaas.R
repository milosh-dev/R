################################################
# Koostab meetmete koondi
#
# Autor: Raoul Lättemäe, august 2020
################################################

library(psych)  # kokkuvõtte tegemiseks
library(tidyverse)

# Töötukassa
load("~/Dokumendid/R/Töötukassa/koond.rdata")

describe(data.koond)
summary(data.koond)

# Asenda NA registrikoodid asutuse nimega
data.koond$Registrikood <- ifelse(is.na(data.koond$Registrikood), data.koond$Nimi, data.koond$Registrikood)


tootasu <- data.koond %>%
  select(Registrikood, Kogukulu) %>%
  `colnames<-`(c("Registrikood", "Summa"))

tootasu$Meede <- as.factor("Töötasu hüvitis")
tootasu$Asutus <- as.factor("Töötukassa")

tootasu <- tootasu %>% 
  relocate(Meede, .after = Registrikood) %>%
  relocate(Asutus, .after = Meede)

# Sulge algandmed
data.koond <- NULL

# Kredex
load("~/Dokumendid/R/Kredex/laenud.rdata")

describe(my.data)
summary(my.data)

kredex.laenud <- my.data %>%
  select(Registrikood, Teenus, Laenusumma) %>%
  `colnames<-`(c("Registrikood", "Meede", "Summa"))

kredex.laenud$Asutus <- as.factor("Kredex")

kredex.laenud <- kredex.laenud %>% 
  relocate(Asutus, .after = Meede)

my.data <- NULL

load("~/Dokumendid/R/Kredex/käendused.rdata")

describe(my.data)

kredex.kaendus <- my.data %>%
  select(Registrikood, Teenus, Käendussumma) %>%
  `colnames<-`(c("Registrikood", "Meede", "Summa"))

kredex.kaendus$Asutus <- as.factor("Kredex")

kredex.kaendus <- kredex.kaendus %>% 
  relocate(Asutus, .after = Meede)


my.data <- NULL

# EAS
load("~/Dokumendid/R/EAS/toetused.rdata")
my.data$Registrikood <- ifelse(my.data$Registrikood == "#N/A", my.data$Nimi, my.data$Registrikood)

describe(my.data)

eas <- my.data %>% 
  select(Registrikood, Meede, Summa) 

eas$Asutus <- as.factor("EAS")

eas <- eas %>%
  relocate(Asutus, .after = Meede)

my.data <- NULL

# MES
load("~/Dokumendid/R/MES/laenud.rdata")
mes.laenud <- my.data %>%
  select(Registrikood, Laenusumma) %>%
  `colnames<-`(c("Registrikood", "Summa"))

mes.laenud$Meede <- as.factor("MES laen")
mes.laenud$Asutus <- as.factor("MES")

mes.laenud <- mes.laenud %>% 
  relocate(Meede, .after = Registrikood) %>%
  relocate(Asutus, .after = Meede)

my.data <- NULL


load("~/Dokumendid/R/MES/käendus.rdata")
mes.kaendus <- my.data %>%
  select(Registrikood, Käendussumma) %>%
  `colnames<-`(c("Registrikood", "Summa"))

mes.kaendus$Meede <- as.factor("MES käendus")
mes.kaendus$Asutus <- as.factor("MES")

mes.kaendus <- mes.kaendus %>% 
  relocate(Meede, .after = Registrikood) %>%
  relocate(Asutus, .after = Meede)

my.data <- NULL

# HTM ja KUL
load("~/Dokumendid/R/HTMKUL/toetused.rdata")
describe(my.data)
summary(my.data)

htm.meede <- my.data %>%
  filter(Asutus == "Haridus- ja Teadusministeerium") %>%
  select(Registrikood, Meede, Asutus, Summa)

kul.meede <- my.data %>%
  filter(Asutus != "Haridus- ja Teadusministeerium") %>%
  select(Registrikood, Meede, Asutus, Summa)

my.data <- NULL
# summary(eas)

# Ühenda andmed
koond <- rbind(kredex.kaendus, kredex.laenud)
koond <- rbind(koond, mes.kaendus)
koond <- rbind(koond, mes.laenud)
koond <- rbind(koond, eas)
koond <- rbind(koond, tootasu)
koond <- rbind(koond, htm.meede)
koond <- rbind(koond, kul.meede)

describe(koond)
save(koond, file="~/Dokumendid/R/koondmeetmed.rdata")

detach("package:psych", unload = TRUE)
