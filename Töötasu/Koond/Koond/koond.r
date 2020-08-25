##############################################
# Töötasutoetuse andmete analüüs
# 
# (c) 2020 - Raoul Lättemäe
# 
##############################################
library(openxlsx)
library(tidyverse)

########################################
# 1. Lae andmed töötukassa koduleheküljelt
########################################

url <- "https://www.tootukassa.ee/sites/tootukassa.ee/files/asutuste_nimekiri_19.07.2020.xlsx"
local <- file.path("andmed.xlsx")
download.file(url, local)

# Me kasutame algandmeid kõige esimeselt sheetilt 
my.data <- read.xlsx(xlsxFile="andmed.xlsx", 
                     sheet = 1,
                     #sheetName="Asutuste kokkuvõte_koond", 
                     startRow=2) # Failistruktuur on selline, et andmed hakkavad teisest reast 

# Konverteri tibbleks
my.data <- as_tibble(my.data)

# Kontrolli, et andmeid oleks korrektselt laetud
my.data
str(my.data)
head(my.data)
teail(my.data)

########################################
# 2. Kohanda tabel analüüsiks sobivasse struktuuri
######################################## 
# Muuda veerunimed loetavamaks
colnames(my.data) <- c("registrikood", 
                       "nimi", 
                       "maakond", 
                       "tegevusala", 
                       "EMTAK.2", 
                       "EMTAK.2.kood",
                       "hyvitise.kuu",
                       "määramise.nädal",
                       "saajate.arv",
                       "brutosumma",
                       "kogukulu")


# Konverteeri lahtrite struktuur

# registrikood ei ole number
my.data$registrikood <- as.character(my.data$registrikood)

# Kategooriaandmed
my.data$maakond <- as.factor(my.data$maakond)
my.data$tegevusala <- as.factor(my.data$tegevusala)
my.data$EMTAK.2 <- as.factor(my.data$EMTAK.2)
my.data$EMTAK.2.kood <- as.factor(my.data$EMTAK.2.kood)
my.data$määramise.nädal <- as.factor(my.data$määramise.nädal)

# Kuud on exceli formaadis kuupäevad: konverteeri need kuupäevaks
my.data$hyvitise.kuu <- convertToDate(my.data$hyvitise.kuu)

# Töötukassa ei näita andmeid ettevõtetele, kellel on kuni kolm töötajat
# loo uus veerg, kajastamaks määramata andmed
my.data$avalik <- ifelse(my.data$saajate.arv == "-", FALSE, TRUE)

# tühjenda veerud, mille väärtus on "-"
my.data$saajate.arv <- ifelse(my.data$saajate.arv == "-", "", my.data$saajate.arv)
my.data$saajate.arv <- as.numeric(my.data$saajate.arv)

tail(my.data)


########################################
# 2. Kohanda tabel analüüsiks sobivasse struktuuri
######################################## 

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

# testi graafikut
b <- ggplot(my.data, aes(x = brutosumma/saajate.arv, y=fct_infreq(tegevusala)))


# konttrolli andmeid
str(my.data)
head(my.data)
tail(my.data)

# salvesta andmed
save(my.data, file="andmed.rdata")
load("~/Dokumendid/R/Töötasu/koond.rdata")

# koosta boxplot
library(tidyverse)
#library(ggplot2)

b <- ggplot(data.koond, aes(x = brutosumma/arv, y=fct_infreq(EMTAK))) + 
  theme(
    text = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  )

# basic
b <- ggplot(my.data, aes(x = brutosumma/saajate.arv, y=fct_infreq(tegevusala))) +
  theme(
    text = element_text(size=12),
    axis.text.y = element_text(size=12),
    axis.text.x = element_text(size=12)
  )

# boxplot
b + geom_jitter(shape=16, position=position_jitter(0.2), colour="deepskyblue2",  aplha = I(0.6)) +
  geom_boxplot(alpha = I(0.6), outlier.shape=NA)

#geom bar
ggplot(my.data) + geom_bar(mapping = aes(y = fct_infreq(tegevusala)))
ggplot(my.data) + geom_bar(mapping = aes(x = saajate.arv, y = tegevusala, fill = hyvitise.kuu), stat = "identity")
ggplot(my.data) + geom_bar(mapping = aes(x = stat(prop), y = tegevusala))
ggplot(my.data) + geom_bar(mapping = aes(y = saajate.arv, fill = hyvitise.kuu), position = "dodge")

# frequency
ggplot(my.data, aes(x = saajate.arv, y = ..density..))
  + geom_freqpoly(binwidth=500)

# filtreeritud
b.ehitus <- subset(my.data, tegevusala %in% c("VEONDUS JA LAONDUS"))
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
b.plot.ehitus <- ggplot(b.ehitus, aes(x = brutosumma/saajate.arv, y=EMTAK.2)) + 
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
