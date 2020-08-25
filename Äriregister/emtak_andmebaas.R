################################################
# Koostab EMTAK koodide loendi äriregistri koodide järgi.
# Esmalt kasutatakse EMTA andmeid EMTAK.1 taseme saamiseks.
# Seejärel lisatakse Töötukassa EMTAK.2 koodid
# Lisaks koostatakse eraldi andmebaasid EMTAK koodide jaoks
#
# Autor: Raoul Lättemäe, juuli-august 2020
################################################
library(tidyverse)

# Kuupäevade loend
aastad <- c("2020", "2019", "2018", "2017")
kv <- c("iv", "iii", "ii", "i")

# Loo vektor kuupäevadega
vector <- character()
for(a in aastad) {
  for (k in kv) {
    vector <- c(vector, paste(a, k, sep = "_"))
  }
}

# Loo faililoend vastavalt sellele, mis kvartalite andmed on olemas.
files <- character()
for (date in vector[3:length(vector)]){
  files <- c(files, paste("~/Dokumendid/R/EMTA/", date, ".rdata", sep = ""))
}

# loo tühi emtak koodi andmestik ja lisa sinna tühi registrikoodi veerg
emtak <- tibble()
emtak$Registrikood <- character()

# nüüd lisa kõikidest failidest iga registrikoodi kohta EMTAK info
for(file in files) {
  load(file)  # lae fail
  data <- subset(data, !(Registrikood %in% emtak$Registrikood))   # ainult andmed, mida veel ei ole laetud
  data <- data %>%
    filter(EMTAK.kood != "") %>%  #jäta tühjad EMTAK koodid vahele
    select(Registrikood, EMTAK.kood)
  emtak <- rbind(emtak, data)
  data <- NULL
}

# nüüd lae EMTAK.2 andmed Töötukassa andmebaasist
load("~/Dokumendid/R/Töötukassa/koond.rdata")

# Lisda emtak.2 koodid
emtak <- left_join(emtak, data.koond %>% filter(!is.na(EMTAK.2.kood)) %>% select(Registrikood, EMTAK.2.kood), by = c("Registrikood"))

# Salvesta registrikoodide ja EMTAK koodide seosed
save(emtak, file="emtak.rdata")

# salvesta EMTAK andmebaasid
load("~/Dokumendid/R/EMTA/2020_ii.rdata")
emtak_2 <- data %>%
  filter(!is.na(EMTAK.2.kood)) %>%
  group_by(EMTAK.2.kood, EMTAK.2) %>%
  summarise(Arv = n()) %>%
  select(EMTAK.2.kood, EMTAK.2)


emtak_1 <- data %>%
  filter(EMTAK.kood != "") %>%
  group_by(EMTAK.kood, EMTAK) %>%
  summarise(Arv = n()) %>%
  select(EMTAK.kood, EMTAK)

emtak_1$EMTAK <- str_trim(emtak_1$EMTAK)

save(emtak_1, file="emtak_1.rdata")
save(emtak_2, file="emtak_2.rdata")
