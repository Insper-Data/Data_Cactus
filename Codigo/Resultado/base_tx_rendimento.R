
library(tidyverse)
library(readxl)
library(geobr)
library(sf)
library(magrittr)
library(plotly)



base <- read_xlsx("taxas rendimento ceara publico 2017.xlsx") %>% 
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
