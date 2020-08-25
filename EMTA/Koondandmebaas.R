##############################################
# Koostab Andmebaasid käivete, maksude ja töötajatega
# 
# (c) 2020 - Raoul Lättemäe
# 
##############################################
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

# Lühenda vektorit vastavalt sellele, mis kvartalite andmed on olemas.
vector <- vector[3:length(vector)]

# loo tühi andmestik ja lisa sinna tühi registrikoodi veerg
aegrida <- tibble()
aegrida$Registrikood <- character()

for (date in vector){
  # Lae fail
  load(paste("~/Dokumendid/R/EMTA/", date, ".rdata", sep = ""))
  nimi = paste(date, "", sep="")
  
  i <- data %>%
    mutate(!!nimi := Käive) %>%
    select(Registrikood, !!nimi)
  
  aegrida <- full_join(aegrida, i, by = "Registrikood")
  i <- NULL
  data <- NULL
}

aegrida.t <- aegrida %>%
  mutate(Keskmine = select(., 2:15) %>% apply(1, mean, na.rm = TRUE)) %>%
  mutate(Stdev = select(., 2:15) %>% apply(1, sd, na.rm = TRUE))

aegrida.t <- aegrida.t %>%
  mutate(Pihtas = ifelse(`2020_ii` < Keskmine - 1 * Stdev, TRUE, FALSE))

save(aegrida.t, file="kaibed.rdata")
  
aegrida.pihtas <- aegrida.t %>%
  filter(Pihtas == TRUE) %>%
  select(Registrikood)

save(aegrida.pihtas, file="pihtas_1_sigma.rdata")

load("2017_i.rdata")
load("2017_ii.rdata")

andmed <- left_join(aegrida.pihtas, data, by = "Registrikood")

i <- data %>%
  select(Registrikood, Käive)

aegrida <- full_join(aegrida, i, by = "Registrikood")



load("full.rdata")

a <- full_join(aegrida, data)

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