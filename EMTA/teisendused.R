##############################################
# Maksuameti andmete teisendused, jms. 
# 
# (c) 2020 - Raoul Lättemäe
# 
##############################################
#View(
#  data %>%
#    mutate(maksu.osakaal = Maksud / Käive)
#)

str(data)
View(data)

#?mutate

#?mapvalues
#mutate(data, 
#    F.Maksud = ifelse(is.na(Maksud), 0, Maksud)
#  )

#summarise(data, arv = sum(Käive), group_by())

View(data)




# Võta andmed kokku "134 923 770,24"
data %>%
  group_by(aasta) %>%
  filter(EMTAK == 'EHITUS', !is.na(Käive), !is.na(Töötajad)) %>%
  summarise(
    sum(Käive)/sum(Töötajad),
    sum(Käive)
  )

data %>%
  group_by(Nimi) %>%
  filter(EMTAK == 'EHITUS') %>%
  summarise(
    Käive, Töötajad
  ) %>%
  arrange(desc(Käive))

