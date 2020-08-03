##############################################
# Ettevõtete analüüs
# 
# (c) 2020 - Raoul Lättemäe
# 
##############################################
library(tidyverse)

load("2020_ii.rdata")
ii.2020 <- data
rm(data)

load("2020_i.rdata")
i.2020 <- data
rm(data)

load("2019_iv.rdata")
iv.2019 <- data
rm(data)

load("2019_iii.rdata")
iii.2019 <- data
rm(data)

load("2019_ii.rdata")
ii.2019 <- data
rm(data)

load("2019_i.rdata")
i.2019 <- data
rm(data)

load("2018_iv.rdata")
iv.2018 <- data
rm(data)

load("2018_iii.rdata")
iii.2018 <- data
rm(data)

load("2018_ii.rdata")
ii.2018 <- data
rm(data)

load("2018_i.rdata")
i.2018 <- data
rm(data)

load("2017_iv.rdata")
iv.2017 <- data
rm(data)

load("2017_iii.rdata")
iii.2017 <- data
rm(data)

load("2017_ii.rdata")
ii.2017 <- data
rm(data)

load("2017_i.rdata")
i.2017 <- data
rm(data)

# Liida kõik üheks andmebaasisk
andmed <- i.2017
andmed <- rbind(andmed, ii.2017)
andmed <- rbind(andmed, iii.2017)
andmed <- rbind(andmed, iv.2017)
andmed <- rbind(andmed, i.2018)
andmed <- rbind(andmed, ii.2018)
andmed <- rbind(andmed, iii.2018)
andmed <- rbind(andmed, iv.2018)
andmed <- rbind(andmed, i.2019)
andmed <- rbind(andmed, ii.2019)
andmed <- rbind(andmed, iii.2019)
andmed <- rbind(andmed, iv.2019)
andmed <- rbind(andmed, i.2020)
andmed <- rbind(andmed, ii.2020)

save(andmed, file="full.rdata")
