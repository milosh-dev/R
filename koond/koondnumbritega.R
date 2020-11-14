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
  # filter(Maakond == c("Harju")) %>%
  # filter(Linn != TRUE) %>%
  # filter(EMTAK.kood == 'I') %>%
  # filter(is.na(arv)) %>%
  #filter(arv_2_kuu == 0 & arv_3_kuu == 0) %>%
  # filter(Töötajad.i.2020 < 10) %>%
  # filter(!is.na(Töötajad.ii.2020)) %>%
  # filter(Käive.iv.2019 <= 250000) %>%
  # filter(Käive.ii.2019 > 0 && Käive.i.2019 > 0) %>%
  mutate(Käive = Käive.i.2019 + Käive.ii.2019 + Käive.iii.2019 + Käive.iv.2019) %>%
  #filter(Käive < 1000000) %>%
  filter(Käive > 0) %>%
  mutate(III.kv.kasv = (Käive.iii.2020/Käive.iii.2019 - 1)*100) %>%
  mutate(II.kv.kasv = (Käive.ii.2020/Käive.ii.2019 - 1)*100) %>%
  mutate(I.kv.kasv = (Käive.i.2020/Käive.i.2019 - 1)*100) %>%
  filter(is.finite(I.kv.kasv)) %>% 
  filter(is.finite(II.kv.kasv)) %>%
  filter(is.finite(III.kv.kasv)) %>%
  gather(key, value, I.kv.kasv:III.kv.kasv) %>%
  # gather(key, value, I.kv.kasv) %>%
  #  mutate(Muut = (Käive.ii.2020-Käive.ii.2019)*100/Käive) %>%
  # arrange(desc(key)) %>%
  #  mutate(rn = row_number()) %>%
  select(Käive, key, value)
# select(Käive, I.kv.kasv, II.kv.kasv)

#labels = c("I kv", "II kv")

# joonista kaaludeta
ggplot(d) +
  #geom_density(aes(x = value, fill=key), alpha=I(0.4)) + 
  geom_density(aes(x = value, fill=key, y = ..count..), alpha=I(0.4)) + 
  #  geom_density(aes(x = value, fill=I("#56B4E9")), alpha=I(0.4)) + 
  xlab("käibe % muutus võrreldes eelmise aastaga") +
  ylab("tihedus (ettevõtete arv)") +
  scale_fill_manual(values=c("deepskyblue", "brown1", "green"),  name='periood', labels=c("2020 I kv", "2020 II kv", "2020 III kv")) +
#  scale_fill_manual(values=c("deepskyblue", "brown1", "green"), name='Paddling type',labels=c("2020 I kv", "2020 II kv"))+
#  guides(fill=guide_legend(title="periood")) +
  geom_vline(aes(xintercept=-30, color=I("red")), linetype = "dashed") +
  xlim(-120,150) +
  theme(legend.position="bottom", 
        text = element_text(size=16),
        legend.text = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16))
#  ylim(0,550)


# joonista kaaludega 
ggplot(d) +
  geom_density(aes(x = value, weights=Käive, fill=key), alpha=I(0.4)) + 
  #  geom_density(aes(x = value, fill=I("#56B4E9")), alpha=I(0.4)) + 
  xlab("käibe % muutus võrreldes eelmise aastaga") +
  ylab("Käibega kaalutud tihedus") +
  scale_fill_manual(values=c("deepskyblue", "brown1", "green"),  name='periood', labels=c("2020 I kv", "2020 II kv", "2020 III kv")) +
  #  scale_fill_manual(values=c("deepskyblue", "brown1", "green"), name='Paddling type',labels=c("2020 I kv", "2020 II kv"))+
  #  guides(fill=guide_legend(title="periood")) +
  # scale_linetype_manual(values=c("dashed", "dotted", "dotted"),  name='periood', labels=c("2020 I kv", "2020 II kv", "2020 III kv")) +
  geom_vline(aes(xintercept=-30, color=I("red")), linetype = "dashed") +
  xlim(-120,150) +
  theme(legend.position="bottom", 
        text = element_text(size=16),
        legend.text = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16))
#  ylim(0,550)
