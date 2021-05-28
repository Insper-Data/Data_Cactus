library(readxl)
library(readr)
library(tidyverse)
library(RColorBrewer)
library(ggtext)
library(scales)
library(extrafont)

base_final <- read_excel("base_final.xlsx")

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

controles1 <- read_xlsx("controleSAEB.xlsx")
controles2 <- read_csv("Controles_2.csv")
urbano <- read_csv("urbano.csv")


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

base2019 <- base2019 %>% 
  mutate(cactus = ifelse(cactus == "cactus19", 1, 0))


#===============================================
# Regressões sem controles, dummy cactus
#===============================================


###############
# Descritivas #
###############


# MAT

x <- base2019 %>% select(MT_9o, cactus) %>% filter(cactus == 0) %>% 
  mutate(media = mean(MT_9o))

y <- base2019 %>% select(MT_9o, cactus) %>% filter(cactus == 1) %>% 
  mutate(media = mean(MT_9o))


SAEB <- base2019 %>% 
  ggplot(aes(Nome_Escola, MT_9o, color = cactus)) +
  geom_point(aes(alpha = cactus, size = cactus)) +
  geom_hline(yintercept = mean(x$media),
              color = "deepskyblue1",
              linetype = 1, size = 3) +
  geom_hline(yintercept = mean(y$media),
             color = "dodgerblue3",
             linetype = 1, size = 3) +
  scale_color_continuous(high = "dodgerblue3", low = "deepskyblue1") +
  scale_alpha(range = c(0.2, 1)) +
  scale_size(range = c(5, 10)) +
  ylim(150,400) +
  labs(x = "Escolas",
       y = "Nota de matemática no SAEB",
       title = "Notas de matemática no SAEB do 9º em 2019",
       subtitle = "<span style='color:deepskyblue1'>Não Cactus</span> Vs. 
    <span style='color:dodgerblue3'>Cactus</span>") +
  theme_classic(base_size = 36) +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        plot.subtitle = element_markdown())

ggsave("grafico_SAEB.png", plot = SAEB, units = "mm",
       height = 400, width = 400, bg = "transparent")


# IDEB

x <- base2019 %>% select(IDEB_9o, cactus) %>% filter(cactus == 0) %>% 
  mutate(media = mean(IDEB_9o))

y <- base2019 %>% select(IDEB_9o, cactus) %>% filter(cactus == 1) %>% 
  mutate(media = mean(IDEB_9o))

IDEB <- base2019 %>% 
  ggplot(aes(Nome_Escola, IDEB_9o, color = cactus)) +
  geom_point(aes(alpha = cactus, size = cactus)) +
  geom_hline(yintercept = mean(x$media),
             color = "deepskyblue1",
             linetype = 1, size = 3) +
  geom_hline(yintercept = mean(y$media),
             color = "dodgerblue3",
             linetype = 1, size = 3) +
  scale_color_continuous(high = "dodgerblue3", low = "deepskyblue1") +
  scale_alpha(range = c(0.2, 1)) +
  scale_size(range = c(5, 10)) +
  ylim(0, 10) +
  labs(x = "Escolas",
       y = "IDEB",
       title = "Notas do IDEB do 9º 2019",
       subtitle = "<span style='color:deepskyblue1'>Não Cactus</span> Vs. 
    <span style='color:dodgerblue3'>Cactus</span>") +
  theme_classic(base_size = 36) +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        plot.subtitle = element_markdown())

ggsave("grafico_IDEB.png", plot = IDEB, units = "mm",
       height = 400, width = 400, bg = "transparent")


# ABANDONO

x <- base2019 %>% select(abandono_9o, cactus) %>% filter(cactus == 0) %>% 
  mutate(media = mean(abandono_9o))

y <- base2019 %>% select(abandono_9o, cactus) %>% filter(cactus == 1) %>% 
  mutate(media = mean(abandono_9o))

base2019 %>% 
  ggplot(aes(Nome_Escola, abandono_9o, color = cactus)) +
  geom_point(aes(alpha = cactus, size = cactus)) +
  geom_hline(yintercept = mean(x$media),
             color = "deepskyblue1",
             linetype = 1, size = 1.5) +
  geom_hline(yintercept = mean(y$media),
             color = "dodgerblue3",
             linetype = 1, size = 1.5) +
  scale_color_continuous(high = "dodgerblue3", low = "deepskyblue1") +
  scale_alpha(range = c(0.2, 1)) +
  scale_size(range = c(1, 2)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 5))  +
  labs(x = "Escolas",
       y = "% abandono do total de alunos",
       title = "Notas do IDEB do 9º 2019",
       subtitle = "<span style='color:deepskyblue1'>Não Cactus</span> Vs. 
    <span style='color:dodgerblue3'>Cactus</span>") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        
        legend.position = "none",
        axis.ticks.x = element_blank(),
        plot.subtitle = element_markdown())



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


#####

## PAINEL ##

painel <- read_csv("painel.csv")

df_9_2013 <- read_xlsx("2013_df9ano_cac.xlsx") %>% select(c(5,15))

painel <- left_join(painel, df_9_2013)

painel <- painel %>% mutate(cactus = ifelse(cactus == "cactus19", 1, 0))

painel %>% group_by(ano) %>% count(cactus) # cactus por ano


df <- read_csv("TS_ESCOLA_2019.csv") %>% 
  filter(ID_UF == 23,
         ID_DEPENDENCIA_ADM == 3) %>% 
  select(ID_ESCOLA, ID_MUNICIPIO)

painel <- left_join(painel, df, by = c("id_escola" = "ID_ESCOLA"))

painel <- painel %>% mutate(ano_cactus = case_when(cactus == 1 & ID_MUNICIPIO == 2300200 & ano >= 2019 ~ 1,
                                                   cactus == 1 & ID_MUNICIPIO == 2302909 & ano >= 2016 ~ 1,
                                                   cactus == 1 & ID_MUNICIPIO == 2304251 & ano >= 2019 ~ 1,
                                                   cactus == 1 & ID_MUNICIPIO == 2307254 & ano >= 2018 ~ 1,
                                                   cactus == 1 & ID_MUNICIPIO == 2313302 & ano >= 2014 ~ 1,
                                                   TRUE ~ 0))

# write_csv(painel, "painel_final.csv")

sum(painel$cactus)
sum(painel$ano_cactus)

painel %>% group_by(ano) %>% count(cactus)






#### REGRESSÕES EM PAINEL ####

library(tidyverse)
library(plm)
library(lmtest)

painel <- read_csv("painel_final.csv")

df_painel <- pdata.frame(painel, index = c("id_escola", "ano"))


formula1 <- nota_matematica ~ ano_cactus + ano + porc_sexo_masc + porc_cor_prePar + reprovacao + superior + carro + NU_MATRICULADOS_CENSO_9EF + ID_LOCALIZACAO

formula2 <- ideb ~ ano_cactus + porc_sexo_masc + porc_cor_prePar + reprovacao + superior + carro + NU_MATRICULADOS_CENSO_9EF + ID_LOCALIZACAO


reg1 <- plm(formula1, data = df_painel, model = "within", effect = "twoways")

reg2 <- plm(formula2, data = df_painel, model = "within", effect = "twoways")

reg1c <- coeftest(reg1, vcovHC(reg1, type="sss", cluster = "group", method = "white2"))[,2]

reg2c <- coeftest(reg2, vcovHC(reg2, type="sss", cluster = "group", method = "white2"))[,2]


summary(reg1)

summary(reg2)


painel %>% 
  filter(ano == 2019) %>% 
  summarise(a = mean(nota_matematica))


painel %>% 
  filter(ano == 2019) %>% 
  mutate(cactus = as.factor(cactus)) %>% 
  group_by(cactus) %>% 
  summarise(mSAEB = mean(nota_matematica)) %>% 
  mutate(cactus = fct_reorder(cactus, desc(mSAEB))) %>% 
  ggplot(aes(cactus, mSAEB, fill = factor(cactus, labels = c("Não Cactus", "Cactus")))) +
  geom_col() +
  scale_fill_manual(values = c("#04cc64", "#fbab04")) +
  scale_x_discrete(labels = c("Cacus", "Não Cactus")) +
  labs(fill = "Escolas",
       x = "",
       y = "Nota média de matemática no SAEB em 2019") +
  theme_classic() 


  
# Testes de validacao:


# Breusch Pagan:

bptest(formula1, data = df_painel)# H0 é homocedasticidade

bptest(formula2, data = df_painel)


### rejeita H0 -> heterocedastico 


# Teste de Hausmann:

FE <- plm(formula1, data = df_painel, model = "within")

RE <- plm(formula1, data = df_painel, model = "random")

hausmann <- phtest(FE, RE) 

### Temos FE

# Autocorrelação AR(1)

## Durbin Watson

pbnftest(reg1)

pbnftest(reg2)


## Baltagi WU LBI

pbnftest(reg1, test = "lbi")

pbnftest(reg2, test = "lbi")


## Wooldridge 2002

pwartest(formula1, data = df_painel, type = "HC3")

pwartest(formula2, data = df_painel, type = "HC3")


























## TESTES PLACEBO + REGRESSÕES ##

# ideb

summary(lm(ideb ~ cactus, data = painel %>% filter(ano == 2013)))

summary(lm(ideb ~ cactus, data = painel %>% filter(ano == 2015)))

summary(lm(ideb ~ cactus, data = painel %>% filter(ano == 2017)))

summary(lm(ideb ~ cactus, data = painel %>% filter(ano == 2019)))


# mat

summary(lm(nota_matematica ~ cactus, data = painel %>% filter(ano == 2013)))

summary(lm(nota_matematica ~ cactus, data = painel %>% filter(ano == 2015)))

summary(lm(nota_matematica ~ cactus, data = painel %>% filter(ano == 2017)))

summary(lm(nota_matematica ~ cactus, data = painel %>% filter(ano == 2019)))



# ideb

summary(lm(ideb ~ cactus +
             porc_sexo_masc +
             porc_cor_prePar +
             reprovacao +
             superior +
             carro +
             NU_MATRICULADOS_CENSO_9EF +
             ID_LOCALIZACAO, data = painel %>% filter(ano == 2013)))

summary(lm(ideb ~ cactus +
             porc_sexo_masc +
             porc_cor_prePar +
             reprovacao +
             superior +
             carro +
             NU_MATRICULADOS_CENSO_9EF +
             ID_LOCALIZACAO, data = painel %>% filter(ano == 2015)))


summary(lm(ideb ~ cactus +
             porc_sexo_masc +
             porc_cor_prePar +
             reprovacao +
             superior +
             carro +
             NU_MATRICULADOS_CENSO_9EF +
             ID_LOCALIZACAO, data = painel %>% filter(ano == 2017)))


summary(lm(ideb ~ cactus +
             porc_sexo_masc +
             porc_cor_prePar +
             reprovacao +
             superior +
             carro +
             NU_MATRICULADOS_CENSO_9EF +
             ID_LOCALIZACAO, data = painel %>% filter(ano == 2019)))


# mat

summary(lm(nota_matematica ~ cactus +
             porc_sexo_masc +
             porc_cor_prePar +
             reprovacao +
             superior +
             carro +
             NU_MATRICULADOS_CENSO_9EF +
             ID_LOCALIZACAO, data = painel %>% filter(ano == 2013)))

summary(lm(nota_matematica ~ cactus +
             porc_sexo_masc +
             porc_cor_prePar +
             reprovacao +
             superior +
             carro +
             NU_MATRICULADOS_CENSO_9EF +
             ID_LOCALIZACAO, data = painel %>% filter(ano == 2015)))


summary(lm(nota_matematica ~ cactus +
             porc_sexo_masc +
             porc_cor_prePar +
             reprovacao +
             superior +
             carro +
             NU_MATRICULADOS_CENSO_9EF +
             ID_LOCALIZACAO, data = painel %>% filter(ano == 2017)))


summary(lm(nota_matematica ~ cactus +
             porc_sexo_masc +
             porc_cor_prePar +
             reprovacao +
             superior +
             carro +
             NU_MATRICULADOS_CENSO_9EF +
             ID_LOCALIZACAO, data = painel %>% filter(ano == 2019)))
