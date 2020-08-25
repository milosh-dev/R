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

# Lisa n-ö pihta saanud ettevõtete andmed
load("pihtas.rdata")

# Lisa Kredexi andmed
load("~/Dokumendid/R/Kredex/laenud.rdata")
kredex.laenud <- my.data

load("~/Dokumendid/R/Kredex/käendused.rdata")
kredex.käendused <- my.data

kredex = rbind(kredex.laenud %>% select(Registrikood), kredex.käendused %>% select(Registrikood))
andmed.kredex = left_join(kredex, andmed, by = c("Registrikood"))


# Lisa MESi andmed
load("~/Dokumendid/R/MES/laenud.rdata")
mes.laenud <- my.data

load("~/Dokumendid/R/MES/käendus.rdata")
mes.käendused <- my.data

mes = rbind(mes.laenud %>% select(Registrikood), mes.käendused %>% select(Registrikood))
andmed.mes = left_join(mes, andmed, by = c("Registrikood"))

# lisa htm ja kul andmed
load("~/Dokumendid/R/HTMKUL/toetused.rdata")
htmkul.toetused <- my.data

andmed.kul <- left_join(htmkul.toetused, andmed, by = c("Registrikood"))

# Teisenda registrikood võrreldavaks
# data.koond$kood <- as.character(data.koond$kood)

test <- left_join(andmed, koond, by = c("Registrikood"))

# Lisa andmebaasile Töötukassa täiendavad koodid
andmed <- left_join(andmed, data.koond %>% select(kood, EMTAK.2, EMTAK.2.kood, arv, arv_1_kuu, arv_2_kuu, arv_3_kuu, brutosumma, kogukulu), by = c("Registrikood" = "kood"))
data.koond <- NULL

andmed.pihtas <- right_join(andmed, aegrida.pihtas, by = "Registrikood")

# Hakkame tegema kokkuvõtteid
my.symm <- summary(andmed.pihtas)
capture.output(my.symm, file="kokkuvõte.txt")

unique(andmed$Maakond)

# Käibe jaotuse joonised
d <- andmed.pihtas %>% 
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

d.halb <- andmed %>% 
  # mutate(Käive = Käive.i.2019 + Käive.ii.2019 + Käive.iii.2019 + Käive.iv.2019) %>%
  # mutate(II.kv.kasv = Käive.ii.2020 - Käive.ii.2019) %>%
  # mutate(I.kv.kasv = Käive.i.2020 - Käive.i.2019) %>%
  # select(II.kv.kasv, I.kv.kasv, Käive.ii.2020, Käive.i.2020, Käive.ii.2019, Käive.i.2019) %>%
  filter(Käive.i.2020 < 0) %>%
  filter(Käive.i.2019 < 0)

load("~/Dokumendid/R/Äriregister/ariregister.rdata")

d.halb.nimega <- left_join(d.halb, data, by=c("Registrikood"))


# joonista kaaludeta
my.plot <- ggplot(d) +
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

my.plot

ttt <- andmed.pihtas %>%
  group_by(EMTAK) %>%
  summarise(Kokku.i = sum(Käive.i.2020, na.rm = TRUE), Kokku.ii = sum(Käive.ii.2020, na.rm = TRUE), arv = n())

uuu <- andmed %>%
  group_by(EMTAK) %>%
  summarise(Kokku.i = sum(Käive.i.2020, na.rm = TRUE), Kokku.ii = sum(Käive.ii.2020, na.rm = TRUE), arv = n())

total <- left_join(uuu, ttt, by = "EMTAK", suffix = c(".total", ".pihtas"))

total.summary <- total %>%
  mutate(Käive = Kokku.i.pihtas / Kokku.i.total, Arv = arv.pihtas / arv.total) %>%
  select(EMTAK, Käive, Arv) %>%
  arrange(Käive)

write_csv(total.summary, "2std.csv")

# Joonista proportsioonid lollipop
ggplot(total.summary) + 
  geom_segment( aes(x=reorder(EMTAK, Käive), xend=EMTAK, y=Käive, yend=Arv), color="grey") +
  geom_point( aes(x=EMTAK, y=Käive), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=EMTAK, y=Arv), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  #  theme_ipsum() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Value of Y")

# Joonista barplot
ggplot(total.summary) +
  geom_bar(aes(x = EMTAK, y = Käive), stat="identity", position="stack") +
  geom_bar(aes(x = EMTAK, y = Arv), stat="identity", position="stack") +
  coord_flip()


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

my.plot + bbc_style()

finalise_plot(plot_name = my.plot, 
              source = "© Raoul Lättemäe 2020, Andmed: Maksuamet, Töötukassa",
              save_filepath = "1_kuu.png",
              width_pixels = 640,
              height_pixels = 550,
              logo_image_path = "logo_p.png")

