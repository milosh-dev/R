##############################################
# Võtab kvartaliandmed ühtseks andmebaasiks
# 
# (c) 2020 - Raoul Lättemäe
# 
##############################################
library(tidyverse)
library(openxlsx)     # impordi excelist

# Me kasutame algandmeid kõige esimeselt sheetilt 
my.data <- read.xlsx(xlsxFile="Toetused.xlsx")

# Konverteri tibbleks
my.data <- as_tibble(my.data)

# Muganda veerunimed
colnames(my.data) <- c("Registrikood", "Nimi", "Meede", "Summa")

# Korrigeeri andmed
my.data$Registrikood <- as.character(my.data$Registrikood)
my.data$Meede <- as.factor(my.data$Meede)

# Kontrolli andmed
View(my.data)

# Salvesta laenuandmed
save(my.data, file="toetused.rdata")
