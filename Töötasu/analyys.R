############################################
# Töötab juba ettevalmistatud tööturu andmetega
############################################
# Kontrolli töökataloogi asukohta
setwd(file.path("/home/raoul/Dokumendid/R/Töötasu"))
getwd()

# lae andmed
load("andmed.rdata")

tail(my.data)
View(my.data)
