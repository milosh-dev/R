##############################################
# Koostab 2019-2020 joonised
# 
# (c) 2020 - Raoul Lättemäe
# 
##############################################
library(tidyverse)
# Lae käibe- ja statistikanumbrid
load("../EMTA/2019-2020.rdata")
# Lae ettevõtete andmed
load("../EMTA/2020_ii.rdata")

# Lisa Ettevõtete registrist täiendavad koodid
andmed <- left_join(andmed, data %>% select(Registrikood, EMTAK, EMTAK.kood, Liik, KMKR, Maakond, Linn), by = "Registrikood")
data <- NULL

# Lae koondnumbrid
load("../koondmeetmed.rdata")

andmed.abi <- subset(andmed, (Registrikood %in% koond$Registrikood))  
andmed.abita <- subset(andmed, !(Registrikood %in% koond$Registrikood))

# Käibe jaotuse joonised
d <- andmed.abita %>% 
  #filter(Asutus != "Haridus- ja Teadusministeerium") %>%
  #filter(Maakond == c("Lääne", "Saare", "Hiiu")) %>%
  # filter(Linn != TRUE) %>%
  # filter(EMTAK.kood == 'I') %>%
  # filter(is.na(arv)) %>%
  #filter(arv_2_kuu == 0 & arv_3_kuu == 0) %>%
  # filter(Töötajad.i.2020 <= 10) %>%
  # filter(!is.na(Töötajad.ii.2020)) %>%
  #  filter(Käive.ii.2020 > 1000000) %>%
  # filter(Käive.ii.2019 > 0 && Käive.i.2019 > 0) %>%
  mutate(Käive = Käive.i.2019 + Käive.ii.2019 + Käive.iii.2019 + Käive.iv.2019) %>%
  #filter(Käive < 1000000) %>%
  filter(Käive > 0) %>%
  mutate(II.kv.kasv = (Käive.ii.2020/Käive.ii.2019 - 1)*100) %>%
  mutate(I.kv.kasv = (Käive.i.2020/Käive.i.2019 - 1)*100) %>%
  filter(is.finite(I.kv.kasv)) %>%
  filter(is.finite(II.kv.kasv)) %>%
  gather(key, value, I.kv.kasv:II.kv.kasv) %>%
  # gather(key, value, I.kv.kasv) %>%
  #  mutate(Muut = (Käive.ii.2020-Käive.ii.2019)*100/Käive) %>%
  # arrange(desc(key)) %>%
  #  mutate(rn = row_number()) %>%
  select(Käive, key, value)
# select(Käive, I.kv.kasv, II.kv.kasv)

# joonista kaaludeta
ggplot(d) +
  geom_density(aes(x = value, fill=key), alpha=I(0.4)) + 
  #  geom_density(aes(x = value, fill=key, y = ..count..), alpha=I(0.4)) + 
  #  geom_density(aes(x = value, fill=I("#56B4E9")), alpha=I(0.4)) + 
  theme(legend.position="bottom") +
  xlab("käibe % muutus võrreldes eelmise aastaga") +
  ylab("tihedus") +
  scale_fill_manual(values=c("deepskyblue", "brown1", "green")) +
  guides(fill=guide_legend(title="periood")) +
  geom_vline(aes(xintercept=-30, color=I("red")), linetype = "dashed") +
  xlim(-120,150)


# joonista kaaludega 
ggplot(d) +
  geom_density(aes(x = value, weights=Käive, fill=key), alpha=I(0.4)) + 
  #  geom_density(aes(x = value, weights=Käive, y = ..count.., fill=key), alpha=I(0.4)) + 
  #  geom_density(aes(x = Muut.ii, fill=I(1"#56B4E9")), alpha=I(0.4)) + 
  theme(legend.position="bottom") +
  xlab("käibe % muutus võrreldes eelmise aastaga") +
  ylab("Käibega kaalutud tihedus") +
  scale_fill_manual(values=c("deepskyblue", "brown1", "green")) +
  guides(fill=guide_legend(title="periood")) +
  geom_vline(aes(xintercept=-30, color=I("red")), linetype = "dashed") +
  #  geom_vline(xinterc ept = modes(d)$key) +
  xlim(-120,150) +
  theme(axis.text.y=element_blank())
