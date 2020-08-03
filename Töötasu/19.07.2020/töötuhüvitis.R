#Impordi exceli andmed Töötukassa koduleheküljelt
# 1. veebilehe aadress (sõltub kuupäevast)
url <- "https://www.tootukassa.ee/sites/tootukassa.ee/files/asutuste_nimekiri_19.07.2020.xlsx"

# Määra töökaust, mida kasutatakse andmete laadimiseks
setwd(file.path("/home/raoul/Dokumendid/RM/Töötasu"))
# Kontrolli töökataloogi asukohta
getwd()

# Määra (kaust) ja fail kuhu andmed salvestatakse
folder <- "19.07.2020"
dir.create(folder)
local <- file.path(folder, "andmed.xlsx")

# Lae fail alla
download.file(url, local)

# Lae andmed salvestatud excelist
# install.packages("openxlsx")
library(openxlsx)
library(tidyverse)

# Toimib
my.data <- read.xlsx(xlsxFile="/home/raoul/Dokumendid/RM/Töötasu/19.07.2020/andmed.xlsx", 
                      sheet = 6,
                      #sheetName="Asutuste kokkuvõte_koond", 
                      startRow=2)

# Konverteri tibbleks
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
#load(andmed.rdata)

# koosta boxplot
library(tidyverse)
#library(ggplot2)

# basic
b <- ggplot(my.data, aes(x = brutosumma/arv, y=fct_infreq(tegevusala)))

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
