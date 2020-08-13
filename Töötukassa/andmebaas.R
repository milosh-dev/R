################################################
# Impordib exceli andmed Töötukassa koduleheküljelt
#
# Autor: Raoul Lättemäe, juuli-august 2020
################################################

library(openxlsx)
library(tidyverse)

# Andmete asukoht
url <- "https://www.tootukassa.ee/sites/tootukassa.ee/files/asutuste_nimekiri_09.08.2020.xlsx"
local <- file.path(getwd(), "andmed_09082020.xlsx")

# Lae fail alla
download.file(url, local)

# Lae andmed
data.weeks <- read.xlsx(xlsxFile=local, 
                        sheet = 1,
                        startRow=2)
data.03 <- read.xlsx(xlsxFile=local, 
                     sheet = 2,
                     startRow=2)
data.04 <- read.xlsx(xlsxFile=local, 
                     sheet = 3,
                     startRow=2)
data.05 <- read.xlsx(xlsxFile=local, 
                     sheet = 4,
                     startRow=2)
data.06 <- read.xlsx(xlsxFile=local, 
                     sheet = 5,
                     startRow=2)
data.koond<- read.xlsx(xlsxFile=local, 
                       sheet = 6,
                       startRow=2)

#my.data <- read.xlsx(xlsxFile="/home/raoul/Dokumendid/RM/Töötasu/19.07.2020/andmed.xlsx", 
#                      sheet = 6,
#sheetName="Asutuste kokkuvõte_koond", 
#                      startRow=2)

# Konverteri tibbleks
data.03 <- as_tibble(data.03)
data.04 <- as_tibble(data.04)
data.05 <- as_tibble(data.05)
data.06 <- as_tibble(data.06)
data.koond <- as_tibble(data.koond)
data.weeks <- as_tibble(data.weeks)

# Korrasta andmed
colnames(data.koond) <- c("Registrikood", "Nimi", "Maakond", "EMTAK.1", "EMTAK.2", "EMTAK.2.kood", "Saajate.arv", "Saajad.1.kuu", "Saajad.2.kuud", "Saajad.3.kuud", "Brutosumma", "Kogukulu")

data.koond$Registrikood <- as.character(data.koond$Registrikood)
data.koond$Maakond <- as.factor(data.koond$Maakond)
data.koond$EMTAK.1 <- as.factor(data.koond$EMTAK.1)
data.koond$EMTAK.2 <- as.factor(data.koond$EMTAK.2)
data.koond$EMTAK.2.kood <- as.factor(data.koond$EMTAK.2.kood)
data.koond$Saajate.arv <- as.numeric(data.koond$Saajate.arv)
data.koond$Saajad.1.kuu <- as.numeric(data.koond$Saajad.1.kuu)
data.koond$Saajad.2.kuud <- as.numeric(data.koond$Saajad.2.kuud)
data.koond$Saajad.3.kuud <- as.numeric(data.koond$Saajad.3.kuud)

# Lisa EMTAK.1.kood
data.koond$EMTAK.1.kood = data.koond$EMTAK.1

# Aseta EMTAK.1.kood EMTAK'i järele
data.koond <- data.koond %>%
  relocate(EMTAK.1.kood, .after = EMTAK.1)

# Asenda nimetused EMTAK koodiga
detach(package:dplyr)  
library(plyr)         # EMTAK 2020 koodide asendamiseks - muutmiseks
data.koond <- mutate(data.koond, EMTAK.1.kood = 
                    revalue(EMTAK.1.kood, 
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
detach(package:plyr)
library(dplyr)

save(data.koond, file="koond.rdata")

# Korrasta Märtsi andmed
colnames(data.03) <- c("Registrikood", "Nimi", "Maakond", "EMTAK.1", "EMTAK.2", "EMTAK.2.kood", "Kuu", "Saajate.arv", "Brutosumma", "Kogukulu")

data.03$Registrikood <- as.character(data.03$Registrikood)
data.03$Maakond <- as.factor(data.03$Maakond)
data.03$EMTAK.1 <- as.factor(data.03$EMTAK.1)
data.03$EMTAK.2 <- as.factor(data.03$EMTAK.2)
data.03$EMTAK.2.kood <- as.factor(data.03$EMTAK.2.kood)
data.03$Kuu <- as.Date(data.03$Kuu, origin = "1899-12-30")
data.03$Saajate.arv <- as.numeric(data.03$Saajate.arv)

# Lisa EMTAK.1.kood
data.03$EMTAK.1.kood = data.03$EMTAK.1

# Aseta EMTAK.1.kood EMTAK'i järele
data.03 <- data.03 %>%
  relocate(EMTAK.1.kood, .after = EMTAK.1)

# Asenda nimetused EMTAK koodiga
detach(package:dplyr)  
library(plyr)         # EMTAK 2020 koodide asendamiseks - muutmiseks
data.03 <- mutate(data.03, EMTAK.1.kood = 
                    revalue(EMTAK.1.kood, 
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
detach(package:plyr)
library(dplyr)

# Kui palju selle kuu hüvitiste saajaid jäid ühekuiseks saajaks?
#data.03 <- left_join(data.03, data.koond %>% select(Registrikood, Nimi, Saajad.1.kuu), by=c("Registrikood", "Nimi"))

# Lisa saajate arv, kes said hüvitist ka järgnevatel kuudel
#data.03$Saajad.yle.1.kuu <- data.03$Saajate.arv - data.03$Saajad.1.kuu

save(data.03, file="märts.rdata")

# Korrasta Aprilli andmed
colnames(data.04) <- c("Registrikood", "Nimi", "Maakond", "EMTAK.1", "EMTAK.2", "EMTAK.2.kood", "Kuu", "Saajate.arv", "Brutosumma", "Kogukulu")

data.04$Registrikood <- as.character(data.04$Registrikood)
data.04$Maakond <- as.factor(data.04$Maakond)
data.04$EMTAK.1 <- as.factor(data.04$EMTAK.1)
data.04$EMTAK.2 <- as.factor(data.04$EMTAK.2)
data.04$EMTAK.2.kood <- as.factor(data.04$EMTAK.2.kood)
data.04$Kuu <- as.Date(data.04$Kuu, origin = "1899-12-30")
data.04$Saajate.arv <- as.numeric(data.04$Saajate.arv)

# Lisa EMTAK.1.kood
data.04$EMTAK.1.kood = data.04$EMTAK.1

# Aseta EMTAK.1.kood EMTAK'i järele
data.04 <- data.04 %>%
  relocate(EMTAK.1.kood, .after = EMTAK.1)

# Asenda nimetused EMTAK koodiga
detach(package:dplyr)  
library(plyr)         # EMTAK 2020 koodide asendamiseks - muutmiseks
data.04 <- mutate(data.04, EMTAK.1.kood = 
                    revalue(EMTAK.1.kood, 
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
detach(package:plyr)
library(dplyr)

# Kui palju selle kuu hüvitiste saajaid jäid ühekuiseks saajaks?
#data.04 <- left_join(data.04, data.koond %>% select(Registrikood, Nimi, Saajad.1.kuu, Saajad.2.kuud), by=c("Registrikood", "Nimi"))
#data.04 <- left_join(data.04, data.03 %>% select(Registrikood, Nimi, Saajad.yle.1.kuu), by=c("Registrikood", "Nimi"))

# Arvuta saajate arv
#data.04$Saajad.1.kuu.f <- ifelse(is.na(Saajad.yle.1.kuu), Saajad.1.kuu, )


# Lisa saajate arv, kes said hüvitist ka järgnevatel kuudel
#data.04$Saajad.yle.1.kuu <- data.04$Saajate.arv - data.04$Saajad.1.kuu

save(data.04, file="aprill.rdata")

# Korrasta Mai andmed
colnames(data.05) <- c("Registrikood", "Nimi", "Maakond", "EMTAK.1", "EMTAK.2", "EMTAK.2.kood", "Kuu", "Saajate.arv", "Brutosumma", "Kogukulu")

data.05$Registrikood <- as.character(data.05$Registrikood)
data.05$Maakond <- as.factor(data.05$Maakond)
data.05$EMTAK.1 <- as.factor(data.05$EMTAK.1)
data.05$EMTAK.2 <- as.factor(data.05$EMTAK.2)
data.05$EMTAK.2.kood <- as.factor(data.05$EMTAK.2.kood)
data.05$Kuu <- as.Date(data.05$Kuu, origin = "1899-12-30")
data.05$Saajate.arv <- as.numeric(data.05$Saajate.arv)

# Lisa EMTAK.1.kood
data.05$EMTAK.1.kood = data.05$EMTAK.1

# Aseta EMTAK.1.kood EMTAK'i järele
data.05 <- data.05 %>%
  relocate(EMTAK.1.kood, .after = EMTAK.1)

# Asenda nimetused EMTAK koodiga
detach(package:dplyr)  
library(plyr)         # EMTAK 2020 koodide asendamiseks - muutmiseks
data.05 <- mutate(data.05, EMTAK.1.kood = 
                    revalue(EMTAK.1.kood, 
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
detach(package:plyr)
library(dplyr)

# Kui palju selle kuu hüvitiste saajaid jäid ühekuiseks saajaks?
# data.05 <- left_join(data.05, data.koond %>% select(Registrikood, Nimi, Saajad.1.kuu), by=c("Registrikood", "Nimi"))

# Lisa saajate arv, kes said hüvitist ka järgnevatel kuudel
# data.05$Saajad.yle.1.kuu <- data.05$Saajate.arv - data.05$Saajad.1.kuu

save(data.05, file="mai.rdata")


# Korrasta juuni andmed
colnames(data.06) <- c("Registrikood", "Nimi", "Maakond", "EMTAK.1", "EMTAK.2", "EMTAK.2.kood", "Kuu", "Saajate.arv", "Brutosumma", "Kogukulu")

data.06$Registrikood <- as.character(data.06$Registrikood)
data.06$Maakond <- as.factor(data.06$Maakond)
data.06$EMTAK.1 <- as.factor(data.06$EMTAK.1)
data.06$EMTAK.2 <- as.factor(data.06$EMTAK.2)
data.06$EMTAK.2.kood <- as.factor(data.06$EMTAK.2.kood)
data.06$Kuu <- as.Date(data.06$Kuu, origin = "1899-12-30")
data.06$Saajate.arv <- as.numeric(data.06$Saajate.arv)

# Lisa EMTAK.1.kood
data.06$EMTAK.1.kood = data.06$EMTAK.1

# Aseta EMTAK.1.kood EMTAK'i järele
data.06 <- data.06 %>%
  relocate(EMTAK.1.kood, .after = EMTAK.1)

# Asenda nimetused EMTAK koodiga
detach(package:dplyr)  
library(plyr)         # EMTAK 2020 koodide asendamiseks - muutmiseks
data.06 <- mutate(data.06, EMTAK.1.kood = 
                    revalue(EMTAK.1.kood, 
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
detach(package:plyr)
library(dplyr)

# Kui palju selle kuu hüvitiste saajaid jäid ühekuiseks saajaks?
# data.06 <- left_join(data.06, data.koond %>% select(Registrikood, Nimi, Saajad.1.kuu), by=c("Registrikood", "Nimi"))

# Lisa saajate arv, kes said hüvitist ka järgnevatel kuudel
# data.06$Saajad.yle.1.kuu <- data.06$Saajate.arv - data.06$Saajad.1.kuu

save(data.06, file="juuni.rdata")


# Korrasta juuni andmed
colnames(data.weeks) <- c("Registrikood", "Nimi", "Maakond", "EMTAK.1", "EMTAK.2", "EMTAK.2.kood", "Kuu", "Määramise.nädal", "Saajate.arv", "Brutosumma", "Kogukulu")

data.weeks$Registrikood <- as.character(data.weeks$Registrikood)
data.weeks$Maakond <- as.factor(data.weeks$Maakond)
data.weeks$EMTAK.1 <- as.factor(data.weeks$EMTAK.1)
data.weeks$EMTAK.2 <- as.factor(data.weeks$EMTAK.2)
data.weeks$EMTAK.2.kood <- as.factor(data.weeks$EMTAK.2.kood)
data.weeks$Kuu <- as.Date(data.weeks$Kuu, origin = "1899-12-30")
data.weeks$Määramise.nädal <- as.factor(data.weeks$Määramise.nädal)
data.weeks$Saajate.arv <- as.numeric(data.weeks$Saajate.arv)

# Lisa EMTAK.1.kood
data.weeks$EMTAK.1.kood = data.weeks$EMTAK.1

# Aseta EMTAK.1.kood EMTAK'i järele
data.weeks <- data.weeks %>%
  relocate(EMTAK.1.kood, .after = EMTAK.1)

# Asenda nimetused EMTAK koodiga
detach(package:dplyr)  
library(plyr)         # EMTAK 2020 koodide asendamiseks - muutmiseks
data.weeks <- mutate(data.weeks, EMTAK.1.kood = 
                       revalue(EMTAK.1.kood, 
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
detach(package:plyr)
library(dplyr)

# Kui palju selle kuu hüvitiste saajaid jäid ühekuiseks saajaks?
# data.weeks <- left_join(data.weeks, data.koond %>% select(Registrikood, Nimi, Saajad.1.kuu), by=c("Registrikood", "Nimi"))

# Lisa saajate arv, kes said hüvitist ka järgnevatel kuudel
# data.weeks$Saajad.yle.1.kuu <- data.weeks$Saajate.arv - data.weeks$Saajad.1.kuu

save(data.weeks, file="määramised.rdata")





