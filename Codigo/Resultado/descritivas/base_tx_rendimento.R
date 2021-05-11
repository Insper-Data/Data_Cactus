
library(tidyverse)
library(readxl)
library(geobr)
library(sf)
library(magrittr)
library(plotly)
library(readr)



base <- read_xlsx("../../BaseDados/Avaliacoes/taxas_rendimento_ceara_publico_2017.xlsx") %>% 
  select(-c("Ano", "Região", "UF", "Localização", "Dependência Administrativa"))

mun <- read_municipality(code_muni = "CE", year = 2017)


mun <- mun %>% left_join(base, by = c("code_muni" = "Código do Município"))



# Aprovação

## Anos Iniciais

aprovacao_anos_iniciais <- mun %>% ggplot() +
  geom_sf(aes(fill = Aprovacao_Anos_Iniciais, label = name_muni), color = "#000000") +
  labs(title = "Taxa de Aprovação dos municípios do Ceará",
       size = 8,
       subtitle = "anos iniciais das escolas públicas em 2017") +
  scale_fill_distiller(palette = "Greens", name="") +
  theme_void()


ggplotly(aprovacao_anos_iniciais, tooltip = c("name_muni", "Aprovacao_Anos_Iniciais"))


## Anos Finais

aprovacao_anos_finais <- mun %>% ggplot() +
  geom_sf(aes(fill = Aprovacao_Anos_Finais, label = name_muni), color = "#000000") +
  labs(title = "Taxa de Aprovação dos municípios do Ceará",
       size = 8,
       subtitle = "anos finais das escolas públicas em 2017") +
  scale_fill_distiller(palette = "Greens", name="") +
  theme_void()



ggplotly(aprovacao_anos_finais, tooltip = c("name_muni", "Aprovacao_Anos_Finais"))


## 5o Ano

aprovacao_anos_5o <- mun %>% ggplot() +
  geom_sf(aes(fill = `Aprovacao_ 5º Ano`, label = name_muni), color = "#000000") +
  labs(title = "Taxa de Aprovação dos municípios do Ceará",
       size = 8,
       subtitle = "5º ano das escolas públicas em 2017") +
  scale_fill_distiller(palette = "Greens", name="") +
  theme_void()



ggplotly(aprovacao_anos_5o, tooltip = c("name_muni", "Aprovacao_ 5º Ano"))


## 9o Ano

aprovacao_anos_9o <- mun %>% ggplot() +
  geom_sf(aes(fill = `Aprovacao_9º Ano`, label = name_muni), color = "#000000") +
  labs(title = "Taxa de Aprovação dos municípios do Ceará",
       size = 8,
       subtitle = "9º ano das escolas públicas em 2017") +
  scale_fill_distiller(palette = "Greens", name="") +
  theme_void()


ggplotly(aprovacao_anos_9o, tooltip = c("name_muni", "Aprovacao_9º Ano"))

#####

aprovacao_anos_iniciais

aprovacao_anos_finais

aprovacao_anos_5o

aprovacao_anos_9o


#####


# Reprovação

## Anos Iniciais

reprovacao_anos_iniciais <- mun %>% ggplot() +
  geom_sf(aes(fill = Reprovacao_Anos_Iniciais, label = name_muni), color = "#000000") +
  labs(title = "Taxa de Reprovação dos municípios do Ceará",
       size = 8,
       subtitle = "anos iniciais das escolas públicas em 2017") +
  scale_fill_distiller(palette = "Blues", name="") +
  theme_void()


ggplotly(reprovacao_anos_iniciais, tooltip = c("name_muni", "Reprovacao_Anos_Iniciais"))


## Anos Finais

reprovacao_anos_finais <- mun %>% ggplot() +
  geom_sf(aes(fill = Reprovacao_Anos_Finais, label = name_muni), color = "#000000") +
  labs(title = "Taxa de Reprovação dos municípios do Ceará",
       size = 8,
       subtitle = "anos finais das escolas públicas em 2017") +
  scale_fill_distiller(palette = "Blues", name="") +
  theme_void()



ggplotly(reprovacao_anos_finais, tooltip = c("name_muni", "Reprovacao_Anos_Finais"))


## 5o Ano

reprovacao_anos_5o <- mun %>% ggplot() +
  geom_sf(aes(fill = `Reprovacao_5º Ano`, label = name_muni), color = "#000000") +
  labs(title = "Taxa de Reprovação dos municípios do Ceará",
       size = 8,
       subtitle = "5º ano das escolas públicas em 2017") +
  scale_fill_distiller(palette = "Blues", name="") +
  theme_void()



ggplotly(reprovacao_anos_5o, tooltip = c("name_muni", "Reprovacao_5º Ano"))


## 9o Ano

reprovacao_anos_9o <- mun %>% ggplot() +
  geom_sf(aes(fill = `Reprovacao_9º Ano`, label = name_muni), color = "#000000") +
  labs(title = "Taxa de Reprovação dos municípios do Ceará",
       size = 8,
       subtitle = "9º ano das escolas públicas em 2017") +
  scale_fill_distiller(palette = "Blues", name="") +
  theme_void()


ggplotly(reprovacao_anos_9o, tooltip = c("name_muni", "Reprovacao_9º Finais"))


#####

reprovacao_anos_iniciais

reprovacao_anos_finais

reprovacao_anos_5o

reprovacao_anos_9o


#####


# Abandono

## Anos Iniciais

abandono_anos_iniciais <- mun %>% ggplot() +
  geom_sf(aes(fill = Abandono_Anos_Iniciais, label = name_muni), color = "#000000") +
  labs(title = "Taxa de Abandono dos municípios do Ceará",
       size = 8,
       subtitle = "anos iniciais das escolas públicas em 2017") +
  scale_fill_distiller(palette = "Oranges", name="") +
  theme_void()


ggplotly(abandono_anos_iniciais, tooltip = c("name_muni", "Abandono_Anos_Iniciais"))


## Anos Finais

abandono_anos_finais <- mun %>% ggplot() +
  geom_sf(aes(fill = Abandono_Anos_Finais, label = name_muni), color = "#000000") +
  labs(title = "Taxa de Abandono dos municípios do Ceará",
       size = 8,
       subtitle = "anos finais das escolas públicas em 2017") +
  scale_fill_distiller(palette = "Oranges", name="") +
  theme_void()



ggplotly(abandono_anos_finais, tooltip = c("name_muni", "Abandono_Anos_Finais"))


## 5o Ano

abandono_anos_5o <- mun %>% ggplot() +
  geom_sf(aes(fill = `Abandono_5º Ano`, label = name_muni), color = "#000000") +
  labs(title = "Taxa de Abandono dos municípios do Ceará",
       size = 8,
       subtitle = "5º ano das escolas públicas em 2017") +
  scale_fill_distiller(palette = "Oranges", name="") +
  theme_void()



ggplotly(abandono_anos_5o, tooltip = c("name_muni", "Abandono_5º Ano"))


## 9o Ano

abandono_anos_9o <- mun %>% ggplot() +
  geom_sf(aes(fill = `Abandono_9º Ano`, label = name_muni), color = "#000000") +
  labs(title = "Taxa de Abandono dos municípios do Ceará",
       size = 8,
       subtitle = "9º ano das escolas públicas em 2017") +
  scale_fill_distiller(palette = "Oranges", name="") +
  theme_void()


ggplotly(abandono_anos_9o, tooltip = c("name_muni", "Abandono_9º Finais"))



#####

abandono_anos_iniciais

abandono_anos_finais

abandono_anos_5o

abandono_anos_9o


#####


############################
##   MAPAS DO LUCHESÃO    ##
############################



# Municípios Cactus
library(readxl)
Cactus <- read_excel("Cactus _ Insper Data.xlsx")
Cactus <- Cactus %>% 
  filter(Estado == "Ceará") %>% 
  mutate(Cactus = 1)

# Puxando os dados de geolocalização dos municípios do Ceará
mun <- read_municipality(code_muni = 23, year = 2017)

# Agrupando a base dos alunos do SAEB
alunos_mun <- ceara_5ano %>% 
  filter(ID_MUNICIPIO < 2400000) %>% 
  group_by(ID_MUNICIPIO) %>% 
  summarise(n = n())


#Agrupando por escolas
alunos_esc <- ceara_5ano %>% 
  filter(ID_MUNICIPIO < 2400000) %>% 
  group_by(ID_ESCOLA, ID_MUNICIPIO) %>% 
  summarise(n = n()) %>% 
  group_by(ID_MUNICIPIO) %>% 
  summarise(n = n())

alunos_mun <- alunos_mun %>% 
  left_join(alunos_esc, by = c("ID_MUNICIPIO" = "ID_MUNICIPIO")) %>% 
  rename(alunos = n.x,
         escolas = n.y) %>% 
  mutate(escola_por_aluno_5ano = escolas/alunos)

mun_local <- mun %>% left_join(alunos_mun, by = c("code_muni" = "ID_MUNICIPIO"))

# Identificando municípios Cactus

mun_local <- mun_local %>% 
  left_join(Cactus, by = c("name_muni" = "Municipio")) %>% 
  select(-c(Estado, `Ano de entrada`)) %>% 
  mutate(Cactus = ifelse(is.na(Cactus), 0, 1))

mun_local <- mun_local %>% 
  mutate(Cactus = as.factor(Cactus))
sum(mun_local$Cactus)# 13, está certo

# Mapa Cactus
ggplot(data = mun_local) +
  geom_sf(aes(fill = Cactus), 
          color = "black") +
  scale_fill_manual(values = c("#CCFFCC", "#33CC00")) +
  theme_void() +
  labs(title = "Municípios Cactus")

# gráfico do número de alunos por município
ggplot(data = mun_local) +
  geom_sf(aes(fill = alunos)) +
  scale_fill_continuous(high = "red", low = "lightgreen") + 
  theme_void() +
  labs(title = "Número de alunos por município")

# gráfico do número de escolas por município
ggplot(data = mun_local) +
  geom_sf(aes(fill = escolas)) +
  scale_fill_continuous(high = "red", low = "lightgreen") + 
  theme_void() +
  labs(title = "Número de escolas por município")

# gráfico do log número de alunos por município
ggplot(data = mun_local) +
  geom_sf(aes(fill = log(alunos))) +
  scale_fill_continuous(high = "red", low = "lightgreen") + 
  theme_void() +
  labs(title = "Número de alunos por município")

# gráfico do número de escolas por alunos por município
ggplot(data = mun_local) +
  geom_sf(aes(fill = escola_por_aluno_5ano)) +
  scale_fill_continuous(high = "green", low = "red") + 
  theme_void() +
  labs(title = "Número de escolas por aluno por município")

# média de proficiência
media_mun <- ceara_5ano %>% 
  filter(ID_MUNICIPIO < 2400000) %>% 
  filter(!is.na(PROFICIENCIA_LP),
         !is.na(PROFICIENCIA_MT),
         !is.na(PROFICIENCIA_MT_SAEB),
         !is.na(PROFICIENCIA_LP_SAEB)) %>% 
  group_by(ID_MUNICIPIO) %>% 
  summarise(MEDIA_LP_SAEB = mean(PROFICIENCIA_LP_SAEB),
            MAX_LP_SAEB = max(PROFICIENCIA_LP_SAEB),
            MIN_LP_SAEB = min(PROFICIENCIA_LP_SAEB), 
            DP_LP_SAEB = sd(PROFICIENCIA_LP_SAEB),
            MEDIA_MT_SAEB = mean(PROFICIENCIA_MT_SAEB),
            MAX_MT_SAEB = max(PROFICIENCIA_MT_SAEB),
            MIN_MT_SAEB = min(PROFICIENCIA_MT_SAEB),
            DP_MT_SAEB = sd(PROFICIENCIA_MT_SAEB))

media_local <- mun %>% left_join(media_mun, by = c("code_muni" = "ID_MUNICIPIO"))

# gráfico de médias por município
ggplot(data = media_local) +
  geom_sf(aes(fill = MEDIA_LP_SAEB)) +
  scale_fill_continuous(high = "blue", low = "lightblue") + 
  theme_void() +
  labs(title = "Proficiência média de LP por município")

ggplot(data = media_local) +
  geom_sf(aes(fill = MEDIA_MT_SAEB)) +
  scale_fill_continuous(high = "blue", low = "lightblue") + 
  theme_void() +
  labs(title = "Proficiência média de MT por município")

