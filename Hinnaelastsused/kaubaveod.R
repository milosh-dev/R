
# Lae andmebaas
devtools::load_all("~/Dokumendid/R/packages/andmebaas")
library(tidyverse)
library(fpoplot)
library(scales) # Scaling axis

# Lae emtak andmed
data(ev.emtak)
data(meta.emtak)

ev.emtak.r <- left_join(ev.emtak, meta.emtak, by = c("EMTAK.2" = "kood"))

kaubavedu.raudteel <- ev.emtak.r %>%
  filter(EMTAK.2 == "5320") %>%
  filter(Kustutatud %in% c(NA))

kaubavedu.maanteel <- ev.emtak.r %>%
  filter(EMTAK.2 == "4941") #%>%
#  filter(Kustutatud %in% c(NA))

kolimisteenused <- ev.emtak.r %>%
  filter(EMTAK.2 == "4942") %>%
  filter(Kustutatud %in% c(NA))

# Kõik emta andmed
#data(emta.andmebaas)

nimi <- unique(kaubavedu.raudteel$nimi)
nimi <- unique(kaubavedu.maanteel$nimi)
nimi <- unique(kolimisteenused$nimi)

# Ühenda andmebaasid
andmed <- inner_join(emta.kaibed.kasv, kaubavedu.maanteel, by = "Registrikood")
andmed.all <- inner_join(emta.andmebaas, kaubavedu.maanteel, by = "Registrikood")

andmed <- inner_join(emta.kaibed.kasv, kaubavedu.raudteel, by = "Registrikood")
andmed.all <- inner_join(emta.andmebaas, kaubavedu.raudteel, by = "Registrikood")

andmed <- inner_join(emta.kaibed.kasv, kolimisteenused, by = "Registrikood")
andmed.all <- inner_join(emta.andmebaas, kolimisteenused, by = "Registrikood")


`%!in%` = Negate(`%in%`)

andmed.filter <- andmed.all %>%
  group_by(Kuupäev) %>%
  summarise(kokku = sum(Käive, na.rm = TRUE))
#  summarise(kokku = sum(Maksud, na.rm = TRUE))
#  summarise(kokku = sum(Töötajad, na.rm = TRUE))

ggplot(andmed.filter, aes(x=Kuupäev)) + 
  geom_line(aes(y=kokku)) + theme_fpo() +
  scale_y_continuous(labels = label_number(suffix = " mln €", scale = 1e-6)) + 
  labs(y = "Käive", title=nimi)

andmed.filter.c <- andmed.all %>%
  filter(Registrikood %!in% c('14945253')) %>%
  group_by(Kuupäev) %>%
  summarise(kokku = sum(Käive, na.rm = TRUE))

andmed.f <- left_join(andmed.filter, andmed.filter.c, by =c("Kuupäev"))

ggplot(andmed.f, aes(x=Kuupäev)) + geom_line(aes(y=kokku.x), color = "red") + geom_line(aes(y=kokku.y)) + theme_fpo() +
  scale_y_continuous(labels = label_number(suffix = " mln €", scale = 1e-6)) + 
  labs(y = "käive EMTA andmetel", title="5320 Post- ja kullerteenused")
