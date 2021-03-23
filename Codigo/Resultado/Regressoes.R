library(readxl)
library(readr)
library(tidyverse)

base_final <- read_excel("~/Desktop/Data_Cactus/BaseDados/Microdados/base_final.xlsx")

ano_19 <- base_final %>% filter(ano == 2019)

ano_19 <- ano_19 %>% 
  mutate(across(36:41, ~ str_replace_all(.x, "NA", NA_character_))) %>% 
  mutate(across(36:41, ~ as.numeric(.x)))

ano_17 <- base_final %>% 
  filter(ano == 2017) %>% 
  select(`Código da Escola`, MT_9o, IDEB_9o, `Proficiência Média`) %>% 
  rename(ID_ESCOLA = `Código da Escola`,
         MT_9o_17 = MT_9o,
         IDEB_9o_17 = IDEB_9o,
         SPAECE_17 = `Proficiência Média`) 

ano_17 <- ano_17 %>% na.omit

#########################
# Juntando os controles #
#########################

controles1 <- read_xlsx("~/Desktop/Data_Cactus/BaseDados/Microdados/controleSAEB.xlsx")
controles2 <- read_csv("~/Desktop/Data_Cactus/BaseDados/Microdados/Controles_2.csv")
urbano <- read_csv("~/Desktop/Data_Cactus/BaseDados/Microdados/urbano.csv")


controles1 <- controles1 %>% select(-1) %>% mutate(ano = 2019)

controles <- left_join(controles2, controles1, by = c("ID_ESCOLA" = "CO_ENTIDADE", "ano"))

urbano <- urbano %>% mutate(urbano17 = ifelse(urbano17 == 2, 0, 1))

ano_19 <- ano_19 %>% select(-c("MT_5o",
                               "IDEB_5o",
                               "Rede",
                               "Código da CREDE",
                               "CREDE",
                               "Escola",
                               "Desvio Padrão",
                              "Indicação do Padrão de Desempenho",
                               "Muito Crítico",
                               "Crítico",
                               "Intermediário",
                               "Adequado",
                               "Alunos Previstos",
                               "Alunos Efetivos",
                               "Percentual de Participação",
                               "Taxa de Participação",
                               "Proficiência Padronizada",
                              "Fator de Ajuste",
                               "Número de Alunos no Muito Crítico",
                               "Número de Alunos no Crítico",
                               "Número de Alunos no Intermediário",
                               "Número de Alunos no Adequado",
                               "IDE EF MT",
                               "IDE Médio EF",
                               "Nome do Município",
                               "Nome da Escola.y"))

ano_19 <- ano_19 %>% na.omit


base2019 <- left_join(ano_19, controles, by = c("Código da Escola" = "ID_ESCOLA", "ano"))

base2019 <- left_join(base2019, ano_17, by = c("Código da Escola" = "ID_ESCOLA"))

na.omit(base2019) %>% 
  count(cactus)

base2019 <- na.omit(base2019)

base2019 <- base2019 %>% 
  left_join(urbano, by = c("Código da Escola" = "ID_ESCOLA"))


#===============================================
# Regressões sem controles, dummy cactus
#===============================================

ano_19 %>% 
  filter(cactus == "cactus19") %>% 
  ggplot(aes(Escola, MT_9o, color = cactus)) +
  geom_point() +
  ylim(150,400) +
  theme_classic()
  # geom_hline(yintercept = mean(ano_19$MT_9o),
  #            color = "blue",
  #            linetype = 2)


#################
# SEM CONTROLES #
#################


summary(lm(MT_9o ~ cactus, data = base2019)) # Nota de matemática 9º ano

summary(lm(IDEB_9o ~ cactus, data = base2019)) # Nota IDEB 9º ano

summary(lm(aprovacao_9o ~ cactus, data = base2019)) # Taxa de aprovação 9º ano

summary(lm(abandono_9o ~ cactus, data = base2019)) # Taxa de abandono 9º ano

summary(lm(reprovacao_9o ~ cactus, data = base2019)) # Taxa de reprovacao 9º ano

summary(lm(`Proficiência Média` ~ cactus, data = base2019)) # Proficiência SPAECE 9º ano


#################
# COM CONTROLES #
#################


summary(lm(MT_9o ~ cactus +
             porc_cor_prePar +
             porc_sexo_masc +
             reprovacao +
             superior +
             porc_trans +
             carro +
             quanti +
             urbano17, data = base2019))


summary(lm(IDEB_9o ~ cactus +
             porc_cor_prePar +
             porc_sexo_masc +
             reprovacao +
             superior +
             porc_trans +
             carro +
             quanti +
             urbano17, data = base2019))


summary(lm(`Proficiência Média` ~ cactus +
             porc_cor_prePar +
             porc_sexo_masc +
             reprovacao +
             superior +
             porc_trans +
             carro +
             quanti +
             urbano17, data = base2019))


summary(lm(reprovacao_9o ~ cactus +
             porc_cor_prePar +
             porc_sexo_masc +
             reprovacao +
             superior +
             porc_trans +
             carro +
             quanti, data = base2019))

# Com lag 2017

summary(lm(MT_9o ~ cactus + MT_9o_17, data = base2019))
summary(lm(IDEB_9o ~ cactus + IDEB_9o_17, data = base2019))
summary(lm(`Proficiência Média` ~ cactus + SPAECE_17, data = base2019))

summary(lm(MT_9o ~ cactus +
             porc_cor_prePar +
             porc_sexo_masc +
             reprovacao +
             superior +
             porc_trans +
             carro +
             quanti + 
             MT_9o_17 +
             urbano17, data = base2019))

summary(lm(IDEB_9o ~ cactus +
             porc_cor_prePar +
             porc_sexo_masc +
             reprovacao +
             superior +
             porc_trans +
             carro +
             quanti +
             IDEB_9o_17 +
             urbano17, data = base2019))

summary(lm(`Proficiência Média` ~ cactus +
             porc_cor_prePar +
             porc_sexo_masc +
             reprovacao +
             superior +
             porc_trans +
             carro +
             quanti +
             SPAECE_17 +
             urbano17, data = base2019))

# Log-nível

summary(lm(log(MT_9o) ~ cactus +
             porc_cor_prePar +
             porc_sexo_masc +
             reprovacao +
             superior +
             porc_trans +
             carro +
             quanti + 
             MT_9o_17 +
             urbano17, data = base2019))

summary(lm(log(IDEB_9o) ~ cactus +
             porc_cor_prePar +
             porc_sexo_masc +
             reprovacao +
             superior +
             porc_trans +
             carro +
             quanti +
             IDEB_9o_17 +
             urbano17, data = base2019))

summary(lm(log(`Proficiência Média`) ~ cactus +
             porc_cor_prePar +
             porc_sexo_masc +
             reprovacao +
             superior +
             porc_trans +
             carro +
             quanti +
             SPAECE_17 +
             urbano17, data = base2019))
