##############################################
# Võtab kvartaliandmed ühtseks andmebaasiks
# 
# (c) 2020 - Raoul Lättemäe
# 
##############################################
library(tidyverse)
library(openxlsx)     # impordi excelist

# Me kasutame algandmeid kõige esimeselt sheetilt 
my.data <- read.xlsx(xlsxFile="Käendused.xlsx")

# Konverteri tibbleks
my.data <- as_tibble(my.data)

# Muganda veerunimed
colnames(my.data) <- c("Registrikood", "Nimi", "Meede", "Teenus", "Laenusumma", "Käendussumma", "Tähtaeg")

# Korrigeeri andmed
my.data$Registrikood <- as.character(my.data$Registrikood)
my.data$Meede <- as.factor(my.data$Meede)
my.data$Teenus <- as.factor(my.data$Teenus)
my.data$Tähtaeg <- as.Date(my.data$Tähtaeg, origin = "1899-12-30")

# Kontrolli andmed
View(my.data)

# Salvesta laenuandmed
save(my.data, file="käendused.rdata")

# Koondsumma
kokku <- my.data %>%
  summarise(Kokku = sum(Laenusumma), Arv = n())
