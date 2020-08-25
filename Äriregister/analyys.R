################################################
# Äriregistri andmete analüüs
#
# Autor: Raoul Lättemäe, august 2020
################################################

library(tidyverse)
library(psych)  # kokkuvõtte tegemiseks

load("ariregister.rdata")
describe(data)

tyybid <- data %>%
  group_by(Tüüp) %>%
  summarise(Kokku = n())


asutamised <- data %>%
  filter(Tüüp %in% c("Osaühing", "Aktsiaselts", "Füüsilisest isikust ettevõtja", "Tulundusühistu", "Täisühing", "Usaldusühing", "Välismaa äriühingu filiaal")) %>%
  filter(Asutamiskuupäev < ISOdate(2020,8,1)) %>%
  mutate(month = format(Asutamiskuupäev, "%m"), year = format(Asutamiskuupäev, "%Y")) %>%
  mutate(Asutamine = as.Date(ISOdate(year, month, 15))) %>%
  # group_by(Asutamiskuupäev) %>%
  group_by(Asutamine) %>%
  summarise(Kokku = n())

ggplot(asutamised, aes(x = Asutamine, y = Kokku)) + 
  geom_line() + 
  xlab("") + 
  ggtitle("Kuu aja jooksul asutatud uute äriühingute arv") +
  theme(text=element_text(size=21))
