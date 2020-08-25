##############################################
# Koostab HTM ja KUL andmetest andmebaasi
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

# kasuta üksnes analüüsiks otstarbekaid andmeid
my.data <- my.data %>%
  select(TAOTLUSVOOR, ASUTUS, TAOTLEJA, TAOTLETUD.SUMMA, ERALDATUD.SUMMA, Registrikood)

# Muganda veerunimed
colnames(my.data) <- c("Meede", "Asutus", "Nimi", "Taotletud", "Summa", "Registrikood")

# järjesta veerud ümber
my.data <- my.data %>% 
  relocate(Registrikood, .before = Meede) %>%
  relocate(Nimi, .after = Registrikood)

# Korrigeeri andmed
my.data$Registrikood <- as.character(my.data$Registrikood)
my.data$Meede <- as.factor(my.data$Meede)
my.data$Asutus <- as.factor(my.data$Asutus)

# Kontrolli andmed
View(my.data)

# Salvesta laenuandmed
save(my.data, file="toetused.rdata")
