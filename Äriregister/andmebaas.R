################################################
# Impordib Äriregistri andmed äriregistri avaandmete baasist
#
# Autor: Raoul Lättemäe, juuli-august 2020
################################################

library(openxlsx)
library(tidyverse)

# Andmete asukoht
url <- "https://avaandmed.rik.ee/andmed/ARIREGISTER/ariregister_csv.zip"
local <- file.path(getwd(), "ariregister_csv.zip")

# Lae fail alla
download.file(url, local)

# paki lahti
unzip(local)

# lae csv andmed
data <- read_csv2("ettevotja_rekvisiidid_2020-08-13.csv")

data <- as_tibble(data)
data <- data %>%
  select(nimi, ariregistri_kood, ettevotja_staatus, ettevotja_oiguslik_vorm, ettevotja_staatus_tekstina, ettevotja_esmakande_kpv)

colnames(data) <- c("Nimi", "Registrikood", "Staatus", "Tüüp", "Staatus.tekstina", "Asutamiskuupäev")

data$Registrikood <- as.character(data$Registrikood)
data$Staatus <- as.factor(data$Staatus)
data$Tüüp <- as.factor(data$Tüüp)
data$Staatus.tekstina <- as.factor(data$Staatus.tekstina)
data$Asutamiskuupäev <- as.Date(data$Asutamiskuupäev, format = c("%d.%m.%Y"))

save(data, file="ariregister.rdata")
