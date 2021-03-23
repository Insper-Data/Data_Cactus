library(readxl)
library(readr)
library(tidyverse)

base_final <- read_excel("base_final.xlsx")

ano_19 <- base_final %>% filter(ano == 2019)

ano_19 <- ano_19 %>% 
  mutate(across(36:41, ~ str_replace_all(.x, "NA", NA_character_))) %>% 
  mutate(across(36:41, ~ as.numeric(.x)))


#########################
# Juntando os controles #
#########################

controles1 <- read_xlsx("controleSAEB.xlsx")

controles2 <- read_csv("Controles_2.csv")


controles1 <- controles1 %>% select(-1) %>% mutate(ano = 2019)

controles <- left_join(controles2, controles1, by = c("ID_ESCOLA" = "CO_ENTIDADE", "ano"))


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
             quanti, data = base2019))


summary(lm(IDEB_9o ~ cactus +
             porc_cor_prePar +
             porc_sexo_masc +
             reprovacao +
             superior +
             porc_trans +
             carro +
             quanti, data = base2019))


summary(lm(`Proficiência Média` ~ cactus +
             porc_cor_prePar +
             porc_sexo_masc +
             reprovacao +
             superior +
             porc_trans +
             carro +
             quanti, data = base2019))


summary(lm(reprovacao_9o ~ cactus +
             porc_cor_prePar +
             porc_sexo_masc +
             reprovacao +
             superior +
             porc_trans +
             carro +
             quanti, data = base2019))
