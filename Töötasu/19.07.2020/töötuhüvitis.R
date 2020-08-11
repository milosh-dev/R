################################################
# Impordib exceli andmed Töötukassa koduleheküljelt
#
# Autor: Raoul Lättemäe, juuli-august 2020
################################################

# Lae andmed salvestatud excelist
library(openxlsx)
library(tidyverse)

# my.data <- NULL

# Määra töökaust, mida kasutatakse andmete laadimiseks
setwd(file.path("/home/raoul/Dokumendid/R/Töötasu"))
getwd()   # Kontrolli töökataloogi asukohta

# 1. - sisesta veebilehe aadress (sõltub kuupäevast)
url <- "https://www.tootukassa.ee/sites/tootukassa.ee/files/asutuste_nimekiri_09.08.2020.xlsx"

# Määra (kaust) ja fail kuhu andmed salvestatakse
folder <- "August.2020"
dir.create(folder)
local <- file.path(folder, "andmed_09082020.xlsx")

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
# Nimetused
colnames(data.koond) <- c("kood", "nimi", "maakond", "EMTAK.1", "EMTAK.2", "EMTAK.2.kood", "arv", "arv_1_kuu", "arv_2_kuu", "arv_3_kuu", "brutosumma", "kogukulu")


# asenda tedmata arvud NA väärtusega
data.koond$arv <- as.numeric(ifelse(data.koond$arv == '-', NA, data.koond$arv))
data.koond$arv_1_kuu <- as.numeric(ifelse(data.koond$arv_1_kuu == '-', NA, data.koond$arv_1_kuu))
data.koond$arv_2_kuu <- as.numeric(ifelse(data.koond$arv_2_kuu == '-', NA, data.koond$arv_2_kuu))
data.koond$arv_3_kuu <- as.numeric(ifelse(data.koond$arv_3_kuu == '-', NA, data.koond$arv_3_kuu))
data.koond$maakond = as.factor(data.koond$maakond)
data.koond$EMTAK.1 = as.factor(data.koond$EMTAK.1)
data.koond$EMTAK.2 = as.factor(data.koond$EMTAK.2)
data.koond$EMTAK.2.kood = as.factor(data.koond$EMTAK.2.kood)

# Lisa EMTAK.1 kood
data.koond$EMTAK.1.kood = data.koond$EMTAK.1

unique(data.koond$EMTAK.1.kood)

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

#rename(data.koond, EMTAK = EMTAK.1)
#rename(data.koond, EMTAK.kood = EMTAK.1.kood)
  

# Aseta EMTAK.1.kood EMTAK'i järele
data.koond <- data.koond %>%
  rename(EMTAK = EMTAK.1) %>%
  rename(EMTAK.kood = EMTAK.1.kood) %>%
  relocate(EMTAK.kood, .after = EMTAK)

save(data.koond, file="koond.rdata")


# Korrasta juuni andmed


str(data.06)


colnames(data.06) <- c("kood", "nimi", "maakond", "EMTAK", "EMTAK.2", "EMTAK.2.kood", "kuu", "arv", "brutosumma", "kogukulu")

# asenda tedmata arvud NA väärtusega
data.06$arv <- as.numeric(ifelse(data.06$arv == '-', NA, data.06$arv))
data.06$maakond = as.factor(data.06$maakond)
data.06$EMTAK = as.factor(data.06$EMTAK)
data.06$EMTAK.2 = as.factor(data.06$EMTAK.2)
data.06$EMTAK.2.kood = as.factor(data.06$EMTAK.2.kood)


# Lisa EMTAK.1 kood
data.06$EMTAK.kood = data.06$EMTAK

unique(data.06$EMTAK.kood)

detach(package:dplyr)  
library(plyr)         # EMTAK 2020 koodide asendamiseks - muutmiseks
data.06 <- mutate(data.06, EMTAK.kood = 
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
detach(package:plyr)
library(dplyr)

# Aseta EMTAK.1.kood EMTAK'i järele
data.06 <- data.06 %>%
  relocate(EMTAK.kood, .after = EMTAK)

save(data.06, file="juuni.rdata")

# Koosta ühendandmebaas
data.koond.06 <- left_join(data.koond, data.06, by=c("kood", "nimi"), suffix=(c("", ".y")))


data.koond.06.f <- select(data.koond.06, -c("maakond.y", "EMTAK.y", "EMTAK.kood.y", "EMTAK.2.y", "EMTAK.2.kood.y")) %>%
  rename(arv_4_kuu = arv.y) %>%
  relocate(arv_4_kuu, .after=arv_3_kuu)


# Kui hüvitiste saajate arv on üldiselt teada, aga juuni on NA, siis see tähendab, et juunis ei taotletudaa
data.koond.06.f$arv_4_kuux = ifelse(is.na(data.koond.06.f$arv_4_kuu), 0, data.koond.06.f$arv_4_kuu)
data.koond.06.f$arv_4_kuu = ifelse(is.na(data.koond.06.f$arv), NA, data.koond.06.f$arv_4_kuux)

# viska liigne minema
str(data.koond.06.f)


TRUE && TRUE

str(data.koond.06.f)
  

?right_join



set.outer = merge(set.a, set.b, by="Registrikood", all = TRUE, suffixes = c("I.2020", "II.2020"))












tail(data.weeks)















my.data <- as_tibble(my.data)

my.data
str(my.data)

# ka ei toimi
# my.data = read.table(file = "clipboard", sep = "\t", header = TRUE)

# Toimib
#my.data = read_csv("/home/raoul/Dokumendid/RM/Töötasu/17.07.2020/andmed.csv")

# Konverteeri osad lahtrid numbriteks
my.data$arv = as.numeric(as.character(my.data$Hüvitise.saajate.arv))
my.data$summa = as.numeric(as.character(gsub(",", "\\.", my.data$Hüvitiste.brutosumma.EUR)))
my.data$kulu = as.numeric(as.character(gsub(",", "\\.", my.data$Hüvitiste.kogukulu.EUR)))

# kustuta liigsed veerud
my.data$Tegevusala..EMTAK.2.kood. <- NULL
my.data$Hüvitise.saajate.arv <- NULL
my.data$Hüvitiste.brutosumma.EUR <- NULL
my.data$Hüvitiste.kogukulu.EUR <- NULL

# Konverteeri Osad andmed faktoriteks
# my.data$Asutuse.registrikood = as.factor(my.data$Asutuse.registrikood)

# Muuda nimed
colnames(my.data) <- c("kood", "nimi", "maakond", "tegevusala", "EMTAK.2", "arv_1_kuu", "arv_2_kuu", "arv_3_kuu", "kogukulu", "arv", "brutosumma")

# konverteeri veel faktoriteks
# my.data$nimi <- as.factor(my.data$nimi)
my.data$maakond <- as.factor(my.data$maakond)
my.data$tegevusala <- as.factor(my.data$tegevusala)
my.data$EMTAK.2 <- as.factor(my.data$EMTAK.2)

# tagasi
my.data$kood <- as.character(my.data$kood)
my.data$nimi <- as.character(my.data$nimi)

# ja numbriteks
my.data$arv_1_kuu = as.numeric(as.character(my.data$arv_1_kuu))
my.data$arv_2_kuu = as.numeric(as.character(my.data$arv_2_kuu))
my.data$arv_3_kuu = as.numeric(as.character(my.data$arv_3_kuu))


# konttrolli andmeid
str(my.data)
head(my.data)
tail(my.data)

# salvesta andmed
save(my.data, file="andmed.rdata")
load(andmed.rdata)

# koosta boxplot
library(tidyverse)
#library(ggplot2)

# basic
b <- ggplot(data.koond, aes(x = brutosumma/arv, y=fct_infreq(EMTAK)))

# filtreeritud
b.ehitus <- subset(data.koond, EMTAK %in% c("VEONDUS JA LAONDUS"))
b.ehitus <- subset(my.data, tegevusala %in% c("VEEVARUSTUS; KANALISATSIOON; JÄÄTME- JA SAASTEKÄITLUS"))
b.ehitus <- subset(my.data, tegevusala %in% c("TÖÖTLEV TÖÖSTUS"))
b.ehitus <- subset(my.data, tegevusala %in% c("TERVISHOID JA SOTSIAALHOOLEKANNE"))
b.ehitus <- subset(my.data, tegevusala %in% c("PÕLLUMAJANDUS, METSAMAJANDUS JA KALAPÜÜK"))
b.ehitus <- subset(my.data, tegevusala %in% c("MÄETÖÖSTUS"))
b.ehitus <- subset(my.data, tegevusala %in% c("MUUD TEENINDAVAD TEGEVUSED"))
b.ehitus <- subset(my.data, tegevusala %in% c("MAJUTUS JA TOITLUSTUS"))
b.ehitus <- subset(my.data, tegevusala %in% c("KUTSE-, TEADUS- JA TEHNIKAALANE TEGEVUS"))
b.ehitus <- subset(my.data, tegevusala %in% c("KUNST, MEELELAHUTUS JA VABA AEG"))
b.ehitus <- subset(my.data, tegevusala %in% c("KINNISVARAALANE TEGEVUS"))
b.ehitus <- subset(my.data, tegevusala %in% c("INFO JA SIDE"))
b.ehitus <- subset(my.data, tegevusala %in% c("HULGI- JA JAEKAUBANDUS; MOOTORSÕIDUKITE JA MOOTORRATASTE REMONT"))
b.ehitus <- subset(my.data, tegevusala %in% c("HARIDUS"))
b.ehitus <- subset(my.data, tegevusala %in% c("HALDUS- JA ABITEGEVUSED"))
b.ehitus <- subset(my.data, tegevusala %in% c("FINANTS- JA KINDLUSTUSTEGEVUS"))
b.ehitus <- subset(my.data, tegevusala %in% c("ELEKTRIENERGIA, GAASI, AURU JA KONDITSIONEERITUD ÕHUGA VARUSTAMINE"))
b.ehitus <- subset(my.data, tegevusala %in% c("EHITUS"))
b.ehitus <- subset(my.data, tegevusala %in% c(""))
b.plot.ehitus <- ggplot(b.ehitus, aes(x = brutosumma/arv, y=EMTAK.2)) + 
            theme(
              text = element_text(size=12),
              axis.text.y = element_text(size=12)
            )
b.plot.ehitus + geom_jitter(shape=16, position=position_jitter(0.2), colour="deepskyblue2",  aplha = I(0.6)) +
  geom_boxplot(alpha = I(0.6), outlier.shape=NA)

#järjekord
b.plot.ehitus <- ggplot(b.ehitus, aes(x = brutosumma/arv, fct_infreq(EMTAK.2))) + 
  theme(
    text = element_text(size=12),
    axis.text.y = element_text(size=12)
  )

b.plot.ehitus + geom_jitter(shape=16, position=position_jitter(0.2), colour="deepskyblue2",  aplha = I(0.6)) +
  geom_boxplot(alpha = I(0.6), outlier.shape=NA)

# Violin
b.plot.ehitus + geom_jitter(shape=16, position=position_jitter(0.2), colour="deepskyblue2",  aplha = I(0.6)) +
  geom_violin(alpha = I(0.6), outlier.shape=NA) +
  geom_boxplot(width=0.1)


# boxplot
b + geom_boxplot(aes(y=maakond), outlier.colour="red", outlier.shape=8,) +
                 geom_jitter(shape=16, position=position_jitter(0.2))

  # boxplot withoutoutliers
b + geom_jitter(shape=16, position=position_jitter(0.2), colour="deepskyblue2",  aplha = I(0.1)) +
  geom_boxplot(aes(y=maakond), alpha = I(0.6), outlier.shape=NA)

# vastavalt b-le  
b + geom_jitter(shape=16, position=position_jitter(0.2), colour="deepskyblue2",  aplha = I(0.6)) +
  geom_boxplot(alpha = I(0.6), outlier.shape=NA)

# Violin
b + geom_jitter(shape=16, position=position_jitter(0.2), colour="deepskyblue2",  aplha = I(0.6)) +
  geom_violin(alpha = I(0.6), outlier.shape=NA)


#b + geom_jitter(shape=16, position=position_jitter(0.2), aes(colour=EMTAK.2),  aplha = I(0.6)) +
#  geom_boxplot(alpha = I(0.6), outlier.shape=NA)


# ilma boxplotita
b + geom_jitter(shape=16, position=position_jitter(0.2), colour="deepskyblue2",  aplha = I(0.6))

qplot(my.data$arv, my.data$kogukulu)

# basic versioon
boxplot(brutosumma/arv~tegevusala, data=my.data)

#save workspace
save.image(file="workspace.rdata")
#load("workspace.rdata")
