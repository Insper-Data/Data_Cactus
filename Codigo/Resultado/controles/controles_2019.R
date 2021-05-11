library(tidyverse)
library(readr)

db <- read_csv("SAEB_9o_2019.csv")

# filtrando alunos que responderam o questionário e selecionando as perguntas de interesse
db <- db %>% 
  filter(IN_PREENCHIMENTO_QUESTIONARIO == 1) %>% 
  select(ID_REGIAO, ID_UF, ID_MUNICIPIO, ID_AREA, ID_ESCOLA, 
         ID_LOCALIZACAO, ID_ESCOLA, ID_ALUNO, TX_RESP_Q009G, 
         TX_RESP_Q004, TX_RESP_Q005, TX_RESP_Q015) 

# filtrando alunos que não responderam alguma pergunta e criando as variáveis dummy de interesse
db <- db %>% 
  mutate(across(c(8:11), ~ str_replace_all(.x, "\\.", NA_character_))) %>% 
  na.omit() %>% 
  mutate(reprovacao = ifelse(TX_RESP_Q015 != "A", 1, 0),
         carro = ifelse(TX_RESP_Q009G != "A", 1, 0),
         mae_superior = ifelse(TX_RESP_Q004 == "E", 1, 0),
         pai_superior = ifelse(TX_RESP_Q005 == "E", 1, 0),
         superior = ifelse(mae_superior == 1 | pai_superior == 1, 1, 0)) %>% 
  select(-c(8:11, 14:15))

escolas <- db %>% 
  group_by(ID_ESCOLA) %>% 
  summarise(reprovacao = mean(reprovacao),
            carro = mean(carro),
            superior = mean(superior)) %>% 
  mutate(ano = 2019)


#

teste <- read_csv("controles_17.csv")

library(readxl)

df <- read_xlsx("controleSAEB.xlsx") %>% select(-c(1, 5, 6)) %>% rename("ID_ESCOLA" = "CO_ENTIDADE")

escolas <- escolas %>% left_join(df)


base <- read_csv("TS_ESCOLA_2019.csv")

base <- base %>% 
  filter(ID_UF == 23,
         ID_DEPENDENCIA_ADM == 3) %>% 
  select(c(1:8, 12, 37))

base <- base %>% select(6:10)


escolas <- escolas %>% left_join(base)


write_csv(escolas, "controles_19.csv")
