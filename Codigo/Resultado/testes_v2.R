#=========================================================================
# 9º ano, Final
#=========================================================================
library(tidyverse)
library(dplyr)
library(readr)
library(car)

escolas <- read_csv("SAEB_IDEB_ESCOLAS_5o_9o_2019_cactus.csv") %>% 
  select(-1) %>% 
  mutate(cactus = as.factor(cactus))


#### MT 9o ano #####

final <- escolas %>% 
  select(`Código do Município`, `Código da Escola`, cactus, ano, MT_9o) %>% 
  pivot_wider(names_from = ano, values_from = MT_9o) %>% 
  mutate(delta_19 = `2019` - `2017`,
         delta_17 = `2017` - `2015`,
         delta_15 = `2015` - `2013`,
         delta_13 = `2013` - `2011`,
         delta_11 = `2011` - `2009`,
         delta_09 = `2009` - `2007`,
         delta_07 = `2007` - `2005`,
         delta_perc_19 = delta_19 / `2017`,
         delta_perc_17 = delta_17 / `2015`,
         delta_perc_15 = delta_15 / `2013`,
         delta_perc_13 = delta_13 / `2011`,
         delta_perc_11 = delta_11 / `2009`,
         delta_perc_09 = delta_09 / `2007`,
         delta_perc_07 = delta_07 / `2005`)

# Tabela 19 contra 17
tab_19 <- final %>% 
  filter(!is.na(delta_19)) %>% 
  group_by(cactus) %>% 
  summarise(media_17 = mean(`2017`),
            media_19 = mean(`2019`),
            delta = round(mean(delta_19), 3),
            delta_perc = round(mean(delta_perc_19), 4),
            n = n()) 

# Tabela 17 contra 15
tab_17 <- final %>% 
  filter(!is.na(delta_17)) %>% 
  group_by(cactus) %>% 
  summarise(media_15 = mean(`2015`),
            media_17 = mean(`2017`),
            delta = round(mean(delta_17), 3),
            delta_perc = round(mean(delta_perc_17), 4),
            n = n())

# Tabela 15 contra 13
tab_15 <- final %>% 
  filter(!is.na(delta_15)) %>% 
  group_by(cactus) %>% 
  summarise(media_13 = mean(`2013`),
            media_15 = mean(`2015`),
            delta = round(mean(delta_15), 3),
            delta_perc = round(mean(delta_perc_15), 4),
            n = n())

# Tabela 13 contra 11
tab_13 <- final %>% 
  filter(!is.na(delta_13)) %>% 
  group_by(cactus) %>% 
  summarise(media_11 = mean(`2011`),
            media_13 = mean(`2013`),
            delta = round(mean(delta_13), 3),
            delta_perc = round(mean(delta_perc_13), 4),
            n = n())

# Testes Delta 2019
filtrado_19 <- final %>% 
  filter(!is.na(delta_19)) %>% 
  select(`Código do Município`, `Código da Escola`, cactus, delta_19, delta_perc_19)

cactus19 <- filtrado_19 %>% 
  filter(cactus == "cactus19")
n_cactus <- filtrado_19 %>% 
  filter(cactus == "não cactus")

shapiro.test(n_cactus$delta_19) # Não normal
shapiro.test(cactus19$delta_19) # Não normal
var.test(filtrado_19$delta_19 ~ cactus, data = filtrado_19) # p-valor de 58,17%
kruskal.test(filtrado_19$delta_19 ~ cactus, data = filtrado_19) # p-valor de 17,93%

t.test(filtrado_19$delta_19 ~ cactus, data = filtrado_19,
       alternative = "greater") # p-valor de 5,697% para cactus19 maior
wilcox.test(filtrado_19$delta_19 ~ cactus, data = filtrado_19,
            exact = FALSE, alternative = "greater") # p-valor de 8,966% para delta maior em cactus19

# Testes Delta 2017
filtrado_17 <- final %>% 
  filter(!is.na(delta_17)) %>% 
  select(`Código do Município`, `Código da Escola`, cactus, delta_17, delta_perc_17)

cactus19_1 <- filtrado_17 %>% 
  filter(cactus == "cactus19")
n_cactus_1 <- filtrado_17 %>% 
  filter(cactus == "não cactus")

shapiro.test(n_cactus_1$delta_17) # Não normal
shapiro.test(cactus19_1$delta_17) # Normal
var.test(filtrado_17$delta_17 ~ cactus, data = filtrado_17) # p-valor de 1,849%
kruskal.test(filtrado_17$delta_17 ~ cactus, data = filtrado_17) # p-valor de 0,67%

t.test(filtrado_17$delta_17 ~ cactus, data = filtrado_17,
       alternative = "greater",
       var.equal = FALSE) # p-valor de 0,3181% para cactus19 maior
wilcox.test(filtrado_17$delta_17 ~ cactus, data = filtrado_17,
            exact = FALSE, alternative = "greater") # p-valor de 0,3353% para cactus 19 maior


##### IDEB 9o ano #####

final <- escolas %>% 
  select(`Código do Município`, `Código da Escola`, cactus, ano, IDEB_9o) %>% 
  pivot_wider(names_from = ano, values_from = IDEB_9o) %>% 
  mutate(delta_19 = `2019` - `2017`,
         delta_17 = `2017` - `2015`,
         delta_15 = `2015` - `2013`,
         delta_13 = `2013` - `2011`,
         delta_11 = `2011` - `2009`,
         delta_09 = `2009` - `2007`,
         delta_07 = `2007` - `2005`,
         delta_perc_19 = delta_19 / `2017`,
         delta_perc_17 = delta_17 / `2015`,
         delta_perc_15 = delta_15 / `2013`,
         delta_perc_13 = delta_13 / `2011`,
         delta_perc_11 = delta_11 / `2009`,
         delta_perc_09 = delta_09 / `2007`,
         delta_perc_07 = delta_07 / `2005`)

# Tabela 19 contra 17
tab_19 <- final %>% 
  filter(!is.na(delta_19)) %>% 
  group_by(cactus) %>% 
  summarise(media_17 = mean(`2017`),
            media_19 = mean(`2019`),
            delta = round(mean(delta_19), 3),
            delta_perc = round(mean(delta_perc_19), 4),
            n = n()) 

# Tabela 17 contra 15
tab_17 <- final %>% 
  filter(!is.na(delta_17)) %>% 
  group_by(cactus) %>% 
  summarise(media_15 = mean(`2015`),
            media_17 = mean(`2017`),
            delta = round(mean(delta_17), 3),
            delta_perc = round(mean(delta_perc_17), 4),
            n = n())

# Tabela 15 contra 13
tab_15 <- final %>% 
  filter(!is.na(delta_15)) %>% 
  group_by(cactus) %>% 
  summarise(media_13 = mean(`2013`),
            media_15 = mean(`2015`),
            delta = round(mean(delta_15), 3),
            delta_perc = round(mean(delta_perc_15), 4),
            n = n())

# Tabela 13 contra 11
tab_13 <- final %>% 
  filter(!is.na(delta_13)) %>% 
  group_by(cactus) %>% 
  summarise(media_11 = mean(`2011`),
            media_13 = mean(`2013`),
            delta = round(mean(delta_13), 3),
            delta_perc = round(mean(delta_perc_13), 4),
            n = n())

# Testes Delta 2019
filtrado_19 <- final %>% 
  filter(!is.na(delta_19)) %>% 
  select(`Código do Município`, `Código da Escola`, cactus, delta_19, delta_perc_19)

cactus19 <- filtrado_19 %>% 
  filter(cactus == "cactus19")
n_cactus <- filtrado_19 %>% 
  filter(cactus == "não cactus")

shapiro.test(n_cactus$delta_19) # Não normal
shapiro.test(cactus19$delta_19) # Não normal
var.test(filtrado_19$delta_19 ~ cactus, data = filtrado_19) # p-valor de 58,17%
kruskal.test(filtrado_19$delta_19 ~ cactus, data = filtrado_19) # p-valor de 17,93%

t.test(filtrado_19$delta_19 ~ cactus, data = filtrado_19,
       alternative = "greater") # p-valor de 5,697% para cactus19 maior
wilcox.test(filtrado_19$delta_19 ~ cactus, data = filtrado_19,
            exact = FALSE, alternative = "greater") # p-valor de 8,966% para delta maior em cactus19

# Testes Delta 2017
filtrado_17 <- final %>% 
  filter(!is.na(delta_17)) %>% 
  select(`Código do Município`, `Código da Escola`, cactus, delta_17, delta_perc_17)

cactus19_1 <- filtrado_17 %>% 
  filter(cactus == "cactus19")
n_cactus_1 <- filtrado_17 %>% 
  filter(cactus == "não cactus")

shapiro.test(n_cactus_1$delta_17) # Não normal
shapiro.test(cactus19_1$delta_17) # Normal
var.test(filtrado_17$delta_17 ~ cactus, data = filtrado_17) # p-valor de 1,849%
kruskal.test(filtrado_17$delta_17 ~ cactus, data = filtrado_17) # p-valor de 0,67%

t.test(filtrado_17$delta_17 ~ cactus, data = filtrado_17,
       alternative = "greater",
       var.equal = FALSE) # p-valor de 0,3181% para cactus19 maior
wilcox.test(filtrado_17$delta_17 ~ cactus, data = filtrado_17,
            exact = FALSE, alternative = "greater") # p-valor de 0,3353% para cactus 19 maior
