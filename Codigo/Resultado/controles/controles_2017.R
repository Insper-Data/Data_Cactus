
library(tidyverse)

# https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/saeb

# microdados de 2017 pra escolas 

df <- read_csv("TS_ESCOLA_2017.csv")

# controles escola

df <- df %>% 
  filter(ID_UF == 23,
         ID_DEPENDENCIA_ADM == 3) %>% 
  select(c(1:6, 10, 35, 86))


# controles alunos

# microdados 2017 pra alunos

base <- read_csv("TS_ALUNO_9EF_2017.csv") %>% 
  filter(ID_UF == 23,
         ID_DEPENDENCIA_ADM == 3,
         IN_PREENCHIMENTO_QUESTIONARIO == 1) %>% 
  select(c(1:14, 37, 38, 49, 55, 59, 85))


aluno <- base %>% 
  mutate(porc_sexo_masc = ifelse(TX_RESP_Q001 == "A", 1, 0),
         porc_cor_prePar = ifelse(TX_RESP_Q002 == "B" | TX_RESP_Q002 == "C", 1, 0),
         carro = ifelse(TX_RESP_Q013 != "A", 1, 0),
         mae_superior = ifelse(TX_RESP_Q019 == "F", 1, 0),
         pai_superior = ifelse(TX_RESP_Q023 == "F", 1, 0),
         superior = ifelse(mae_superior == 1 | pai_superior == 1, 1, 0),
         reprovacao = ifelse(TX_RESP_Q049 != "A", 1, 0)) %>% 
  select(-c(15:20))


escolas <- aluno %>% 
  group_by(ID_ESCOLA) %>% 
  summarise(reprovacao = mean(reprovacao, na.rm = TRUE),
            carro = mean(carro, na.rm = TRUE),
            superior = mean(superior, na.rm = TRUE),
            porc_sexo_masc = mean(porc_sexo_masc, na.rm = TRUE),
            porc_cor_prePar = mean(porc_cor_prePar, na.rm = TRUE))


# base controles

controles_17 <- left_join(df, escolas)

controles_17 <- controles_17 %>% 
  select(-c(1:3, 9))


write_csv(controles_17, "controles_17.csv")
