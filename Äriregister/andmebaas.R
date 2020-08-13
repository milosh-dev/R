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
