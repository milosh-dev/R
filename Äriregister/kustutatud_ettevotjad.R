################################################
# Kas mõni toetust saanud ettevõte on Äriregistrist kustutatud?
#
# Autor: Raoul Lättemäe, august 2020
################################################
library(tidyverse)

load("ariregister.rdata")
load("~/Dokumendid/R/koondmeetmed.rdata")

puudu <- subset(koond, !(Registrikood %in% data$Registrikood))  

puudu.EE <- puudu %>%
  filter(grepl(pattern = "^[0-9]*$", x = Registrikood))

save(puudu.EE, file="~/Dokumendid/R/kustutatud_ettevotted.rdata")

puudu.EE.koond <- puudu.EE %>%
  group_by(Meede) %>%
  summarise(Kokku = sum(Summa))
