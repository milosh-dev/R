##############################################
# Andmete laadimine äriregistrist
# 
# (c) 2020 - Raoul Lättemäe
# 
##############################################
library(openxlsx)     # impordi excelist
detach(package:dplyr)  
library(plyr)         # EMTAK 2020 koodide asendamiseks - muutmiseks
library(tidyverse)    # tidyverse
library(lubridate)    # kuupäevafunktsioonid
#library(gsubfn)       # grep asendus tekstile

#detach(package:tidyverse)
#detach(package:plyr)

# Lae andmed EMTA kodulehelt
# muuda vajadusel allolev näitaja 
kv <- '2020_iii'
my.date <- "30.09.20"
# alates 2019-II kvartalist CSV
# url <- paste("https://www.emta.ee/sites/default/files/kontaktid-ja-ametist/maksulaekumine-statistika/tasutud-maksud/tasutud_maksud_",
#              kv,
#              "_kvartal.csv", sep = ""
# )
# data <- read.csv(file = url, sep = ";", fileEncoding = "latin1")
# vanemad andmed on excelis
url <- paste("https://www.emta.ee/sites/default/files/kontaktid-ja-ametist/maksulaekumine-statistika/tasutud-maksud/tasutud_maksud_",
             kv,
             "_kvartal.xlsx", sep = ""
)
#url <- "https://www.emta.ee/sites/default/files/kontaktid-ja-ametist/maksulaekumine-statistika/tasutud-maksud/tasutud_maksud_2017_i_kv.xlsx"
data <- read.xlsx(xlsxFile = url)

data <- as_tibble(data)

# kontrolli laetud andmed
data
str(data)
head(data)
tail(data)
View(data)

# korrasta nimetused
colnames(data) <- c("Registrikood", 
                    "Nimi", 
                    "Liik", 
                    "KMKR", 
                    "EMTAK",
                    "Omavalitsus",
                    "Maksud",
                    "Tööjõumaksud",
                    "Käive",
                    "Töötajad")

# puhasta ja korrasta andmed
data$Liik <- as.factor(data$Liik)
data$KMKR <- as.factor(data$KMKR)
data$EMTAK <- as.factor(data$EMTAK)
data$Omavalitsus <- as.factor(data$Omavalitsus)
data$Maksud <- as.numeric(gsub("\\s","", sub(",",".", data$Maksud, fixed = TRUE)))
data$Tööjõumaksud <- as.numeric(gsub("\\s","", sub(",",".", data$Tööjõumaksud, fixed = TRUE)))
data$Käive <- as.numeric(gsub("\\s","", sub(",",".", data$Käive, fixed = TRUE)))
#data$Töötajad <- as.numeric(data$Töötajad)

# lisa kuupäev
data$Kuupäev <- as.Date(my.date, "%d.%m.%y")
data$kv <- as.factor(quarter(data$Kuupäev))
data$aasta <- as.factor(year(data$Kuupäev))

# Eralda vald maakonnast
data$Vald <- str_trim(gsub('.*\\((.*)\\).*', '\\1', data$Omavalitsus))
data$Maakond <- str_trim(gsub('(.*)\\(.*', '\\1', data$Omavalitsus))
data$Vald <- as.factor(data$Vald)
data$Maakond <- as.factor(data$Maakond)

# Omavalitsus sisaldab sõna "linn"
data$Linn <- FALSE
data$Linn[grep('linn', data$Omavalitsus)] <- TRUE
data$Linn <- as.factor(data$Linn)

# Üldjuhul on väärtus 0 ja NA erineva tähendusega.
#
#   arv „0" – alusandmete (vastavad deklaratsioonid, töötamise register jne) põhjal 
#   on vastava välja väärtuseks saadud null;
#
#   tühi – vastava välja väärtuse leidmiseks alusandmed puuduvad, nt vastavad 
#   deklaratsioonid on esitamata või neid deklaratsioone ei peagi esitama 
#   (nt need ettevõtted, kes kuuluvad käibemaksugruppi ja seetõttu esitavad koos 
#   ühise käibedeklaratsiooni).


# Lisa veerg EMTAK koodide jaoks
unique(data$EMTAK)

# lisa EMTAK.koodide rida
data$EMTAK.kood <- data$EMTAK

data <- mutate(data, EMTAK.kood = 
                 revalue(EMTAK.kood, 
                         c(
                           "HULGI- JA JAEKAUBANDUS; MOOTORSÕIDUKITE JA MOOTORRATASTE REMONT" = "G",
                           "HULGI- JA JAEKAUBANDUS; MOOTORSŐIDUKITE JA MOOTORRATASTE REMONT" = "G",
                           "INFO JA SIDE" = "J",
                           "PÕLLUMAJANDUS, METSAMAJANDUS JA KALAPÜÜK" = "A",
                           "PŐLLUMAJANDUS, METSAMAJANDUS JA KALAPÜÜK" = "A",
                           "TÖÖTLEV TÖÖSTUS" = "C",
                           "KUTSE-, TEADUS- JA TEHNIKAALANE TEGEVUS" = "M",
                           "EHITUS" = "F",
                           "KINNISVARAALANE TEGEVUS" = "L",
                           "VEONDUS JA LAONDUS" = "H",
                           "HALDUS- JA ABITEGEVUSED" = "N",
                           "MAJUTUS JA TOITLUSTUS" = "I",
                           "TERVISHOID JA SOTSIAALHOOLEKANNE" = "Q",
                           "VEEVARUSTUS; KANALISATSIOON; JÄÄTME- JA SAASTEKÄITLUS" = "E",
                           "MUUD TEENINDAVAD TEGEVUSED" = "S",
                           "FINANTS- JA KINDLUSTUSTEGEVUS" = "K",
                           "MÄETÖÖSTUS" = "B",
                           "HARIDUS" = "P",
#                           "" = NA,
                           "KUNST, MEELELAHUTUS JA VABA AEG" = "R",
                           "ELEKTRIENERGIA, GAASI, AURU JA KONDITSIONEERITUD ŐHUGA VARUSTAMINE" = "D",
                           "ELEKTRIENERGIA, GAASI, AURU JA KONDITSIONEERITUD ÕHUGA VARUSTAMINE" = "D",
                           " AVALIK HALDUS JA RIIGIKAITSE; KOHUSTUSLIK SOTSIAALKINDLUSTUS" = "O",
                           "AVALIK HALDUS JA RIIGIKAITSE; KOHUSTUSLIK SOTSIAALKINDLUSTUS" = "O",
                           "EKSTERRITORIAALSETE ORGANISATSIOONIDE JA ÜKSUSTE TEGEVUS" = "U",
                           "KODUMAJAPIDAMISTE KUI TÖÖANDJATE TEGEVUS; KODUMAJAPIDAMISTE OMA TARBEKS MŐELDUD ERISTAMATA KAUPAD" = "T",
                           "KODUMAJAPIDAMISTE KUI TÖÖANDJATE TEGEVUS; KODUMAJAPIDAMISTE OMA TARBEKS MÕELDUD ERISTAMATA KAUPAD" = "T"
                         )
                 )
          )

unique(data$EMTAK.kood)
#?revalue

# lisa veerud, kus NA on asendatud nullidega
data$Maksud.na.null <- ifelse(is.na(data$Maksud), 0, data$Maksud)
data$Tööjõumaksud.na.null <- ifelse(is.na(data$Tööjõumaksud), 0, data$Tööjõumaksud)
data$Käive.na.null <- ifelse(is.na(data$Käive), 0, data$Käive)
data$Töötajad.na.null <- ifelse(is.na(data$Töötajad), 0, data$Töötajad)


# Tõsta veerud ümber
data <- data %>%
  relocate(EMTAK.kood, .before = EMTAK) %>%
  relocate(Maakond, .after = Omavalitsus) %>%
  relocate(Vald, .after = Maakond) %>%
  relocate(Linn, .after = Vald) %>%
  relocate(Kuupäev, .before = Registrikood) %>%
  relocate(aasta, .after = Kuupäev) %>%
  relocate(kv, .after=aasta)

# Kontrolli andmeid
str(data)
glimpse(data)
View(data)

# salvesta andmed
save(data, file=paste(kv, ".rdata", sep=""))
#write_csv(data, paste(kv, ".csv", sep=""))
#write.xlsx(data, paste(kv, ".xlsx", sep=""))
