##############################################
# Täisandmebaas
# 
# (c) 2020 - Raoul Lättemäe
# 
##############################################
library(tidyverse)
#install.packages("psych")
#library(psych)
# detach("package:psych", unload = TRUE)

# install.packages('devtools')
##############################################
# BBC stiilis infograafikud
# vt: https://github.com/bbc/bbplot/
# (c) 2020 - Raoul Lättemäe
# 
##############################################
# devtools::install_github('bbc/bbplot')
library(bbplot)


load("full.rdata")
str(andmed)
unique(andmed$EMTAK.kood)

#andmed %>% 
#  filter(str_length(EMTAK.kood)>1) %>%
#  group_by(Kuupäev)

kitsend <- select(andmed, Registrikood, EMTAK.kood, Kuupäev, aasta, kv, Käive, Töötajad, Maksud, Tööjõumaksud)
rm(andmed)

str(kitsend)
kitsend$EMTAK.kood = as.character(kitsend$EMTAK.kood)
kitsend$EMTAK.kood = ifelse(is.na(kitsend$EMTAK.kood), "-", kitsend$EMTAK.kood)
kitsend$EMTAK.kood = ifelse(kitsend$EMTAK.kood == "", "-", kitsend$EMTAK.kood)

# NB! selleks, et dplyr töötaks tuleb plyr välja lülitada
# ... (sic!). vt. https://stackoverflow.com/questions/21653295/dplyr-issues-when-using-group-bymultiple-variables
# detach(package:plyr)    
# library(dplyr)

abi <- kitsend %>% 
  group_by(EMTAK.kood, Kuupäev) %>%
  summarize(avg = mean(Maksud, na.rm = TRUE)) %>%
  arrange(EMTAK.kood, Kuupäev)

# Ilma väärtusteta väljadel tuleb ka nimi panna
# abi$EMTAK = ifelse(is.na(abi$EMTAK), "-", abi$EMTAK)

view(abi)
#?arrange


# Ja factor tagasi tekstiks muuta
#abi$EMTAK.kood = as.character(abi$EMTAK.kood)

# EMTAK koodid tuleb tõsta veergudeks
abi_w <- abi %>%
  pivot_wider(names_from = EMTAK.kood, values_from = avg)

abi_w
View(abi_w)

# Ja nüüd saab teha aegrea
abi_w$Kuupäev <- NULL
abi_ts <- ts(abi_w, start = c(2017, 1), end = c(2020, 2), frequency = 4)

#abi_ts <- as_tibble(abi_ts)
abi_ts

# tööstus <- abi_ts$C

# vt. https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_ts.html
#install.packages("ggfortify")
library(ggfortify)
install.packages("ggthemes")
library(ggthemes)
autoplot(abi_ts, facets = FALSE, columns = c('C', 'G', 'I', 'F', 'R'), size = 2) #+ 
#  scale_color_manual(values=c("Red", "Black", "Blue")) + 
#  theme_economist()

#ggplot(abi_ts, aes=(x = )) + geom_point()

?autoplot

# VANAN

abi <- kitsend %>%
  select(EMTAK.kood, Kuupäev, Käive, Töötajad, Maksud, Tööjõumaksud) %>%
  group_by(EMTAK.kood, Kuupäev) %>%
  summarize(sKäive = sum(Käive), mTöötajad = mean(Töötajad), sMaksud = sum(Maksud), sTööjõumaksud = sum(Tööjõumaksud)) %>%
  gather(key, value, EMTAK.kood)

  
  

test <- ggplot(kitsend) +
  geom_bar(mapping = aes(x = EMTAK.kood)) +
  bbc_style()

# Salvesta pilt
finalise_plot(plot_name = test, 
    source = "© Raoul Lättemäe (2020), Andmed: Maksuamet",
    save_filepath = "test1.png",
    width_pixels = 640,
    height_pixels = 550,
    logo_image_path = "logo.png")


kitsend %>%
  group_by(aasta, EMTAK.kood) %>%
  summarize(mm = count(Käive))

kitsend$Registrikood <- NULL

aasta_kaupa <- aggregate(kitsend$Käive, by=list(aasta = kitsend$aasta, EMTAK.kood = kitsend$EMTAK.kood), 
                         FUN=mean)

glimpse(aasta_kaupa)

View(aasta_kaupa)

?aggregate

aasta_kaupa <- kitsend %>% 
  filter(!is.na(Käive)) %>%
  group_by(EMTAK.kood) %>%
  summarize(Käive = sum(Käive))

#save(kitsend, file="registrikoodiga_full.rdata")

# Esmane andmete ülevaatus
str(kitsend)
View(kitsend)
glimpse(kitsend)
summary(kitsend)
describe(kitsend, na.rm = TRUE)

# correlatsion

# Andmete ülevaade
?plot_num()

grouped <- kitsend %>%
  group_by(Registrikood, aasta)

