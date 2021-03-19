library(readxl)
library(readr)
library(tidyverse)

base_final <- read_excel("Desktop/Data_Cactus/BaseDados/Microdados/base_final.xlsx")

ano_19 <- base_final %>% filter(ano == 2019)

ano_19 <- ano_19 %>% 
  mutate(across(36:41, ~ str_replace_all(.x, "NA", NA_character_))) %>% 
  mutate(across(36:41, ~ as.numeric(.x)))


#===============================================
# Regressões sem controles, dummy cactus
#===============================================
summary(lm(MT_9o ~ cactus, data = ano_19)) # Nota de matemática 9º ano
summary(lm(IDEB_9o ~ cactus, data = ano_19)) # Nota IDEB 9º ano
summary(lm(aprovacao_9o ~ cactus, data = ano_19)) # Taxa de aprovação 9º ano
summary(lm(abandono_9o ~ cactus, data = ano_19)) # Taxa de abandono 9º ano
summary(lm(reprovacao_9o ~ cactus, data = ano_19)) # Taxa de reprovacao 9º ano
summary(lm(`Proficiência Média` ~ cactus, data = ano_19)) # Proficiência SPAECE 9º ano
