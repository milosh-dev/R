#######################################################
# See kood testib eri andmesettide ühendamist merge käsu abil  
#
# Autor: Raoul Lättemäe
# Versioon 1.0 19.06.2020
#######################################################

# Esmalt seame keskkonna korda
# Määra töökaust, mida kasutatakse andmete laadimiseks
setwd(file.path("/home/raoul/Dokumendid/RM/merge"))
# Kontrolli töökataloogi asukohta
getwd()

# impordi andmed
set.a <- read.csv(file = "test_a.csv", sep = ";")
set.b <- read.csv(file = "test_b.csv", sep = ";")

# korrasta pealkirjad
colnames(set.a) <- c("Registrikood", 
                    "Nimi", 
                    "Liik", 
                    "KMKR", 
                    "EMTAK",
                    "Maakond",
                    "Maksud",
                    "Tööjõumaksud",
                    "Käive",
                    "Töötajad")

colnames(set.b) <- c("Registrikood", 
                     "Nimi", 
                     "Liik", 
                     "KMKR", 
                     "EMTAK",
                     "Maakond",
                     "Maksud",
                     "Tööjõumaksud",
                     "Käive",
                     "Töötajad")

# puhasta ja korrasta andmed
set.a$Liik <- as.factor(set.a$Liik)
set.a$KMKR <- as.factor(set.a$KMKR)
set.a$EMTAK <- as.factor(set.a$EMTAK)
set.a$Maakond <- as.factor(set.a$Maakond)
set.a$Maksud <- as.numeric(sub(" ","", sub(",",".", set.a$Maksud, fixed = TRUE), fixed = TRUE))
set.a$Tööjõumaksud <- as.numeric(sub(" ","", sub(",",".", set.a$Tööjõumaksud, fixed = TRUE), fixed = TRUE))
set.a$Käive <- as.numeric(sub(" ","", sub(",",".", set.a$Käive, fixed = TRUE), fixed = TRUE))
set.a$Töötajad <- as.numeric(set.a$Töötajad)

# asenda NA nullidega
set.a$Maksud[is.na(set.a$Maksud)] <- 0
set.a$Tööjõumaksud[is.na(set.a$Tööjõumaksud)] <- 0
set.a$Käive[is.na(set.a$Käive)] <- 0
set.a$Töötajad[is.na(set.a$Töötajad)] <- 0

set.b$Liik <- as.factor(set.b$Liik)
set.b$KMKR <- as.factor(set.b$KMKR)
set.b$EMTAK <- as.factor(set.b$EMTAK)
set.b$Maakond <- as.factor(set.b$Maakond)
set.b$Maksud <- as.numeric(sub(" ","", sub(",",".", set.b$Maksud, fixed = TRUE), fixed = TRUE))
set.b$Tööjõumaksud <- as.numeric(sub(" ","", sub(",",".", set.b$Tööjõumaksud, fixed = TRUE), fixed = TRUE))
set.b$Käive <- as.numeric(sub(" ","", sub(",",".", set.b$Käive, fixed = TRUE), fixed = TRUE))
set.b$Töötajad <- as.numeric(set.b$Töötajad)

# asenda NA nullidega
set.b$Maksud[is.na(set.b$Maksud)] <- 0
set.b$Tööjõumaksud[is.na(set.b$Tööjõumaksud)] <- 0
set.b$Käive[is.na(set.b$Käive)] <- 0
set.b$Töötajad[is.na(set.b$Töötajad)] <- 0

#lisa kuupäevad
set.a$Kuupäev <- as.Date("31.03.2020", "%d.%m.%y")
set.b$Kuupäev <- as.Date("30.06.2020", "%d.%m.%y")

# asenda NA nullidega
set.a$Töötajad[is.na(set.a$Töötajad)] <- 0

# kontrolli andmed
str(set.a)
head(set.a)
tail(set.a)

str(set.b)
head(set.b)
tail(set.b)

# transponeeri
set.a.t <- t(set.a)
set.a.t

#outer join (sisemine funktsioon)
set.outer = merge(set.a, set.b, by="Registrikood", all = TRUE, suffixes = c("I.2020", "II.2020"))
library(dplyr)
rename(set.outer,
  Nimi = NimiI.2020)

set.outer
?rename

names(set.outer)[2] <- "Nimi"
names(set.outer)[3] <- "Liik"
names(set.outer)[4] <- "KMKR"
names(set.outer)[5] <- "EMTAK"
names(set.outer)[6] <- "Maakond"

set.outer$NimiII.2020 <- NULL
set.outer$LiikII.2020 <- NULL
set.outer$KMKRII.2020 <- NULL
set.outer$EMTAKII.2020 <- NULL
set.outer$MaakondII.2020 <- NULL

write.csv(set.outer, "outer.csv")

str(set.outer) 

# Alternatiivne
library(dplyr)
set.outer.d <- full_join(set.a, set.b, by="Registrikood")
head(set.outer.d)
?full_join
?merge
