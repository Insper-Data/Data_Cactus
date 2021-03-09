#=========================================================================
# 9º ano, Final
#=========================================================================
library(tidyverse)
library(readr)
library(car)
library(readxl)
library(broom)
library(tidyr)

escolas <- read_csv("../../BaseDados/Avaliacoes/SAEB_IDEB_ESCOLAS_5o_9o_2019_cactus.csv") %>% 
  select(-1) %>% 
  mutate(cactus = as.factor(cactus))


# escolas_pareado <- read_excel("base_pareada.xlsx")

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







#### testes primeira versão ####




# Temos alguns valores zerados na base, não sei o que isso pode significar,
# mas provavelmente distorce os dados.

# Acho que os nomes dos municípios estão estranhos.

# Seria ideal ter valores do número de alunos prestando a prova por escola
# para poder calcular uma média ponderada.

#=========================================================================
# Gráficos extra
#=========================================================================

escolas %>% 
  mutate(cactus = ifelse(cactus == "cactus19_5_9", "Cactus 19",
                         ifelse(cactus == "cactus20_5_9", "Cactus 20", "Não Cactus"))) %>% 
  ggplot() +
  geom_density(aes(x = MT_5o, fill = cactus), 
               alpha = 0.6) +
  facet_wrap(~ ano) +
  theme_bw() +
  ylab("Densidade") +
  xlab("Proficiência média em Matemática") +
  labs(title = "Distribuição das notas em Matemática",
       subtitle = "5º ano")

escolas %>% 
  mutate(cactus = ifelse(cactus == "cactus19_5_9", "Cactus 19",
                         ifelse(cactus == "cactus20_5_9", "Cactus 20", "Não Cactus"))) %>% 
  ggplot() +
  geom_density(aes(x = MT_9o, fill = cactus), 
               alpha = 0.6) +
  facet_wrap(~ ano) +
  theme_bw() +
  ylab("Densidade") +
  xlab("Proficiência média em Matemática") +
  labs(title = "Distribuição das notas em Matemática",
       subtitle = "9º ano")


#=========================================================================
# 9º ano, 2017
#=========================================================================

# Boxplot
escolas %>% 
  filter(ano == 2017) %>% 
  ggplot() +
  geom_boxplot(aes(y = MT_9o, x = cactus, fill = cactus), 
               alpha = 0.8) +
  theme_bw() +
  xlab("Grupos") +
  ylab("Proficiência média em Matemática") +
  labs(title = "Distribuição das notas em Matemática",
       subtitle = "SAEB 2017, 9º ano")

# Filtrando para alguns testes de normalidade

shapiro.test(escolas17_c19$MT_9o) # normal, mas amostra pequena
shapiro.test(escolas17_c20$MT_9o) # normal, mas amostra pequena
shapiro.test(escolas17_nc$MT_9o) # não normal

# Teste para comparar variâncias de três grupos diferentes
fligner.test(MT_9o ~ cactus, data = escolas_2017) # variâncias diferentes

# Teste de igualdade das médias entre mais de dois grupos
kruskal.test(MT_9o ~ cactus, data = escolas_2017) # pelo menos uma das médias é diferente

# cactus 19 x cactus 20
var.test(MT_9o ~ cactus, Ftest_1, 
         alternative = "two.sided") # variâncias iguais
t.test(MT_9o ~ cactus, Ftest_1, 
       alternative = "greater") # cactus 19 é maior que cactus 20 (p-valor de 2,5%)

# cactus 19 x não cactus
fligner.test(MT_9o ~ cactus, data = Ftest_2) # variâncias diferentes
wilcox.test(MT_9o ~ cactus, data = Ftest_2,
            exact = FALSE, alternative = "greater") # cactus 19 é maior que não cactus

# cactus 20 x não cactus
Ftest_8 <- escolas_2017 %>% 
  filter(cactus == "cactus20_5_9" | cactus == "não cactus_5_9")

fligner.test(MT_9o ~ cactus, data = Ftest_8) # variâncias iguais (p-valor de 8,6%)
wilcox.test(MT_9o ~ cactus, data = Ftest_8,
            exact = FALSE, alternative = "greater") # cactus 20 é maior que não cactus (p-valor de 3,71%)

#=========================================================================
# 9º ano, 2019
#=========================================================================
# Boxplot
escolas %>% 
  filter(ano == 2019) %>% 
  ggplot() +
  geom_boxplot(aes(y = MT_9o, x = cactus, fill = cactus), 
               alpha = 0.8) +
  theme_bw() +
  xlab("Grupos") +
  ylab("Proficiência média em Matemática") +
  labs(title = "Distribuição das notas em Matemática",
       subtitle = "SAEB 2019, 9º ano")

# Filtrando para alguns testes de normalidade

shapiro.test(escolas19_c19$MT_9o) # não normal, mas amostra pequena
shapiro.test(escolas19_c20$MT_9o) # normal, mas amostra pequena
shapiro.test(escolas19_nc$MT_9o) # não normal

# Teste para comparar variâncias de três grupos diferentes
fligner.test(MT_9o ~ cactus, data = escolas_2019) # variâncias diferentes

# Teste de igualdade das médias entre mais de dois grupos
kruskal.test(MT_9o ~ cactus, data = escolas_2019) # pelo menos uma das médias é diferente

# cactus 19 x cactus 20
fligner.test(MT_9o ~ cactus, data = Ftest_3) # variâncias diferentes
wilcox.test(MT_9o ~ cactus, data = Ftest_3,
            exact = FALSE, alternative = "greater") # cactus 19 é maior que cactus 20

# cactus 19 x não cactus
fligner.test(MT_9o ~ cactus, data = Ftest_4) # variâncias diferentes
wilcox.test(MT_9o ~ cactus, data = Ftest_4,
            exact = FALSE, alternative = "greater") # cactus 19 é maior que não cactus

# cactus 20 x não cactus
Ftest_9 <- escolas_2019 %>% 
  filter(cactus == "cactus20_5_9" | cactus == "não cactus_5_9")

fligner.test(MT_9o ~ cactus, data = Ftest_9) # variâncias iguais (p-valor de 31,12%)
wilcox.test(MT_9o ~ cactus, data = Ftest_9,
            exact = FALSE, alternative = "greater") # cactus 20 não maior que não cactus (p-valor de 10,07%)

#=========================================================================
# 9º ano, 2017 para 2019
#=========================================================================
# Filtrando para alguns testes de normalidade

shapiro.test(pareado_17$MT_9o) # normal
shapiro.test(pareado_19$MT_9o) # normal

# cactus 19 (2017) x cactus 19 (2019)
var.test(MT_9o ~ ano, Ftest_5, 
         alternative = "two.sided") # p-valor de 8,23%, variâncias iguais
t.test(MT_9o ~ ano, Ftest_5,
       paired = TRUE,
       alternative = "less") # valor em 2019 é maior que em 2017 (p-valor de 0,0097%)

# cactus 20 (2017) x cactus 20 (2019)
shapiro.test(pareado_17_2$MT_9o) # normal
shapiro.test(pareado_19_2$MT_9o) # normal

var.test(MT_9o ~ ano, Ftest_6, 
         alternative = "two.sided") # as duas tem distribuição normal, variâncias iguais
t.test(MT_9o ~ ano, Ftest_6, 
       paired = TRUE,
       alternative = "two.sided") # p-valor de 21,18%, parece não ter mudado de 2017 para 2019

# não cactus (2017) x não cactus (2019)
shapiro.test(pareado_17_3$MT_9o) # não é normal
shapiro.test(pareado_19_3$MT_9o) # não é normal

fligner.test(MT_9o ~ ano, Ftest_7) # as duas não tem distribuição normal, variâncias iguais
wilcox.test(MT_9o ~ ano, data = Ftest_7,
            exact = FALSE, alternative = "less") # 2017 é menor que 2019, p-valor de quase 0



