##############################################
# Võtab kvartaliandmed ühtseks andmebaasiks
# 
# (c) 2020 - Raoul Lättemäe
# 
##############################################
library(tidyverse)
library(openxlsx)     # impordi excelist
#install.packages("hexbin")
library(hexbin)

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

view(i.2020)

# Koosta andmete koond
andmed <- select(ii.2020, Registrikood, Käive, Maksud, Töötajad, Tööjõumaksud)
str(andmed)
colnames(andmed) <- c("Registrikood", "Käive.ii.2020", "Maksud.ii.2020", "Töötajad.ii.2020", "Tööjõumaksud.ii.2020")

andmed_i <- select(i.2020, Registrikood, Käive, Maksud, Töötajad, Tööjõumaksud)
colnames(andmed_i) <- c("Registrikood", "Käive.i.2020", "Maksud.i.2020", "Töötajad.i.2020", "Tööjõumaksud.i.2020")

#andmed_b <- left_join(andmed, andmed_i, by = "Registrikood", suffix = c(".ii.2020", ".i.2020"))
andmed <- left_join(andmed, andmed_i, by = "Registrikood")

andmed_i <- select(i.2019, Registrikood, Käive, Maksud, Töötajad, Tööjõumaksud)
colnames(andmed_i) <- c("Registrikood", "Käive.i.2019", "Maksud.i.2019", "Töötajad.i.2019", "Tööjõumaksud.i.2019")
andmed <- left_join(andmed, andmed_i, by = "Registrikood")


save(andmed, file = "2019-2020.rdata")

#######################
# Lae andmed
load("2019-2020.rdata")
load("2020_ii.rdata")

# Lisa EMTAK.kood
andmed <- left_join(andmed, data %>% select(Registrikood, EMTAK.kood), by = "Registrikood")

ggplot(andmed) + geom_bin2d(aes(x = Töötajad.i.2020, y=(Käive.ii.2020/Käive.ii.2019))) + ylim(-50,200) + scale_x_log10() + scale_y_log10()


andmed

vaheandmed <- andmed %>%
       mutate(Käive = Käive.iv.2019 + Käive.iii.2019 + Käive.ii.2019 + Käive.i.2019) %>%
       mutate(Kasv = Käive.ii.2020 - Käive.ii.2019) %>%
       mutate(KasvPct = Kasv/Käive.ii.2019 * 100) %>%
      select(Registrikood, Käive, Käive.ii.2020, Kasv, KasvPct, Töötajad.ii.2020)   

load("register.rdata")

baas <- right_join(register, vaheandmed, by="Registrikood")

write.xlsx(baas, "baas.xlsx")


d <- andmed %>% 
  filter(EMTAK.kood == 'F') %>%
  #  filter(Käive.ii.2020 > 1000000) %>%
  # filter(Käive.ii.2019 > 0 && Käive.i.2019 > 0) %>%
  mutate(Käive = Käive.i.2019 + Käive.ii.2019 + Käive.iii.2019 + Käive.iv.2019) %>%
  filter(Käive > 0) %>%
  mutate(I.kv.kasv = (Käive.i.2020/Käive.i.2019 - 1)*100) %>%
  mutate(II.kv.kasv = (Käive.ii.2020/Käive.ii.2019 - 1)*100) %>%
  gather(key, value, II.kv.kasv:I.kv.kasv) %>%
  #  mutate(Muut = (Käive.ii.2020-Käive.ii.2019)*100/Käive) %>%
  #  arrange(Käive) %>%
  #  mutate(rn = row_number()) %>%
  select(Käive, key, value)
# select(Käive, I.kv.kasv, II.kv.kasv)

my.density <- density(log10(d$value, na.rm = TRUE))

# Leia mood
# https://stackoverflow.com/questions/58785930/r-find-maximum-of-density-plot
modes <- function(d){
  i <- which(diff(sign(diff(d$key))) < 0) + 1
  data.frame(x = d$value[i], y = d$key[i])
}

modes(d)
d$Käive[which.max(d$value)]

View(d)

my.density <- density(log10(d$value), na.rm = TRUE, weights = d$Käive)

plot(my.density)
plot(my.density, xlim = c(-10000, 500000))

View(my.density)

modes(my.density)$value

my.density$value[which.max(my.density$value)]

# joonista kaaludega
ggplot(d) +
  geom_density(aes(x = value, weights=Käive, y = ..count.., fill=key), alpha=I(0.4)) + 
  #  geom_density(aes(x = Muut.ii, fill=I("#56B4E9")), alpha=I(0.4)) + 
  theme(legend.position="bottom") +
  xlab("käibe % muutus võrreldes eelmise aastaga") +
  ylab("ettevõtete arv") +
  geom_vline(aes(xintercept=-30, color=I("red")), linetype = "dashed") +
#  geom_vline(xintercept = modes(d)$key) +
  xlim(-120,150)

# joonista kaaludeta
ggplot(d) +
  geom_density(aes(x = value, fill=key, y = ..count..), alpha=I(0.4)) + 
  #  geom_density(aes(x = Muut.ii, fill=I("#56B4E9")), alpha=I(0.4)) + 
  theme(legend.position="bottom") +
  xlab("käibe % muutus võrreldes eelmise aastaga") +
  ylab("ettevõtete arv") +
  geom_vline(aes(xintercept=-30, color="red"), linetype = "dashed") +
  xlim(-120,150)

















d <- andmed %>% 
#  filter(Käive.ii.2020 > 1000000) %>%
#  filter(Käive.ii.2020 > 0 & Käive.ii.2019 > 0) %>%
  mutate(Käive = Käive.i.2019 + Käive.ii.2019 + Käive.iii.2019 + Käive.iv.2019) %>%
#  filter(Käive > 0) %>%
  mutate(I.kv.kasv = (Käive.i.2020/Käive.i.2019 - 1)*100) %>%
  mutate(II.kv.kasv = (Käive.ii.2020/Käive.ii.2019 - 1)*100) %>%
  gather(key, value, I.kv.kasv:II.kv.kasv) %>%
  #  mutate(Muut = (Käive.ii.2020-Käive.ii.2019)*100/Käive) %>%
#  arrange(Käive) %>%
#  mutate(rn = row_number()) %>%
  select(Käive, key, value)

d <- andmed %>% 
  filter(EMTAK.kood == 'I') %>%
  #  filter(Käive.ii.2020 > 1000000) %>%
  #  filter(Käive.ii.2020 > 0 & Käive.ii.2019 > 0) %>%
  mutate(Käive = Käive.i.2019 + Käive.ii.2019 + Käive.iii.2019 + Käive.iv.2019) %>%
  #  filter(Käive > 0) %>%
  mutate(I.kv.kasv = (Käive.i.2020/Käive.i.2019 - 1)*100) %>%
  mutate(II.kv.kasv = (Käive.ii.2020/Käive.ii.2019 - 1)*100) %>%
  gather(key, value, II.kv.kasv:I.kv.kasv) %>%
  #  mutate(Muut = (Käive.ii.2020-Käive.ii.2019)*100/Käive) %>%
  #  arrange(Käive) %>%
  #  mutate(rn = row_number()) %>%
  select(Käive, key, value)
  # select(Käive, I.kv.kasv, II.kv.kasv)


# joonista
ggplot(d) +
  geom_density(aes(x = value, fill=key, y = ..count..), alpha=I(0.4)) + 
  #  geom_density(aes(x = Muut.ii, fill=I("#56B4E9")), alpha=I(0.4)) + 
  theme(legend.position="bottom") +
  xlab("käibe % muutus võrreldes eelmise aastaga") +
  ylab("ettevõtete arv") +
  geom_vline(aes(xintercept=-30, color="red"), linetype = "dashed") +
  xlim(-120,150)

# Töötajad
d <- andmed %>% 
  #  filter(Käive.ii.2020 > 1000000) %>%
  #  filter(Käive.ii.2020 > 0 & Käive.ii.2019 > 0) %>%
  #mutate(Käive = Käive.i.2019 + Käive.ii.2019 + Käive.iii.2019 + Käive.iv.2019) %>%
  #  filter(Käive > 0) %>%
  # mutate(I.kv.kasv = Töötajad.i.2020) %>%
  # mutate(II.kv.kasv = Töötajad.ii.2020) %>%
  group_by(Registrikood) %>%
  gather(key, value, Töötajad.i.2020:Töötajad.ii.2020) %>%
  #  mutate(Muut = (Käive.ii.2020-Käive.ii.2019)*100/Käive) %>%
  #  arrange(Käive) %>%
  #  mutate(rn = row_number()) %>%
  select(key, value)


str(d)
View(d)
tail(d)
# joonista
ggplot(d) +
  geom_density(aes(x = value, fill=key, y = ..count..), alpha=I(0.4)) + 
#  geom_density(aes(x = Muut.ii, fill=I("#56B4E9")), alpha=I(0.4)) + 
  theme(legend.position="bottom") +
  xlab("käibe % muutus võrreldes eelmise aastaga") +
  ylab("ettevõtete arv") +
  geom_vline(aes(xintercept=-30, color="red"), linetype = "dashed") +
  xlim(-120,150)



ggplot(d, aes(x = II.kv.kasv, y = I.kv.kasv)) +
  geom_point(alpha=I(0.05)) +
#  geom_bin2d() +
  ylim(-50,50) +
  xlim(-50,50)


ggplot(d) +
  geom_histogram(aes(x = value, fill=key), alpha=I(0.4)) + 
  #  geom_density(aes(x = Muut.ii, fill=I("#56B4E9")), alpha=I(0.4)) + 
  theme(legend.position="bottom") +
  geom_vline(aes(xintercept=-30, color="red"), linetype = "dashed") +
  xlim(-50,30)



p <- ggplot(d, aes(x = key, fill=I("Deepskyblue2")))

p + geom_density(alpha=I(0.4)) + xlim(-50,30) + 
  geom_vline(aes(xintercept=-30, color="red"), linetype = "dashed")

p + geom_step(aes(y=rn))

p + geom_histogram() + xlim(-100,30)

p + geom_point() + ylim(-250,1000)
