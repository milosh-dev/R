################################################
# Analüüsi koondnäitajaid
#
# Autor: Raoul Lättemäe, juuli-august 2020
################################################

library(psych)  # kokkuvõtte tegemiseks
library(tidyverse)

load("~/Dokumendid/R/koondmeetmed.rdata")

# üldkokkuvõte meetme kaupa
d <- koond %>%
  group_by(Meede, Asutus) %>%
  summarise(Kokku = sum(Summa), Arv = n())

write.csv(d, file = "~/Dokumendid/R/kokku.csv")

# Meetmete ristkasutus
t <- koond %>% 
  group_by(Registrikood) %>%
  summarise(Kokku = sum(Summa), Arv = n())

y <- t %>% 
  group_by(Arv) %>%
  summarise(Kokku = sum(Kokku), tk = n())

write.csv(y, file = "~/Dokumendid/R/meetmete_arv_ettevottele.csv")

f <- t %>%
  filter(Arv >= 3)

# üle kolme meetme
suur <- left_join(f, koond, by = c("Registrikood"))

# Lisa äriregistri tunnused
load("~/Dokumendid/R/Äriregister/ariregister.rdata")
suur <- left_join(suur, data, by = c("Registrikood"))

write.csv(suur, file = "~/Dokumendid/R/yle_3_meetme.csv")

# Kui mitmest asutusest
s <- suur %>%
  group_by(Registrikood, Asutus) %>%
  summarise(Kokku = sum(Summa), Arv = n())

s.ev <- s %>%
  group_by(Registrikood) %>%
  summarise(Asutused = n(), Meetmeid = sum(Arv), Kokku = sum(Kokku))

s.ev.sum <- s.ev %>%
  group_by(Asutused) %>%
  summarise(arv = n())
