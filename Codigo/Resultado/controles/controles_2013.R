
# https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/saeb

# microdados de 2013 pra escolas 

df <- read_csv("TS_ESCOLA.csv")

# controles escola

df <- df %>% 
  filter(ID_UF == 23,
         ID_DEPENDENCIA_ADM == 3) %>% 
  select(c(1:6, 9, 33, 59))


# controles alunos

# microdados 2013 pra alunos

base <- read_csv("TS_ALUNO_9EF.csv") %>% 
  filter(ID_UF == 23,
         ID_DEPENDENCIA_ADM == 3,
         IN_PREENCHIMENTO_QUESTIONARIO == 1) %>% 
  select(c(1:14, 36, 37, 47, 54, 58, 83))


aluno <- base %>% 
  mutate(porc_sexo_masc = ifelse(TX_RESP_Q001 == "A", 1, 0),
         porc_cor_prePar = ifelse(TX_RESP_Q002 == "B" | TX_RESP_Q002 == "C", 1, 0),
         carro = ifelse(TX_RESP_Q012 != "A", 1, 0),
         mae_superior = ifelse(TX_RESP_Q019 == "F", 1, 0),
         pai_superior = ifelse(TX_RESP_Q023 == "F", 1, 0),
         superior = ifelse(mae_superior == 1 | pai_superior == 1, 1, 0),
         reprovacao = ifelse(TX_RESP_Q048 != "A", 1, 0)) %>% 
  select(-c(15:20))


escolas <- aluno %>% 
  group_by(ID_ESCOLA) %>% 
  summarise(reprovacao = mean(reprovacao, na.rm = TRUE),
            carro = mean(carro, na.rm = TRUE),
            superior = mean(superior, na.rm = TRUE),
            porc_sexo_masc = mean(porc_sexo_masc, na.rm = TRUE),
            porc_cor_prePAr = mean(porc_cor_prePAr, na.rm = TRUE))
         
         
# base controles

controles_13 <- left_join(df, escolas)

controles_13 <- controles_13 %>% 
  select(-c(1:3, 9))


write_csv(controles_13, "controles_13.csv")
