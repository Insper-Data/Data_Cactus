
library(tidyverse)
library(readxl)
library(writexl)

#### 2005 ####

SAEB_IDEB_ESCOLAS_5o <- read_excel("C:/Users/Bruno Bregola/Desktop/Projeto Cactus/SAEB_IDEB_ESCOLAS_5o.xlsx") %>% 
  mutate(across(7:22, as.numeric))



x <- SAEB_IDEB_ESCOLAS_5o %>%
  pivot_longer(-c(`Sigla da UF`:Rede), names_to = "ano", values_to = "MT_5o") %>% 
  mutate(ano = str_remove_all(ano, "MT_5o_"))

x1 <- x %>% 
  mutate(IDEB_5o = ifelse(str_length(ano) > 4, ano, NA))

x <- x1 %>% 
  filter(is.na(IDEB_5o)) %>% 
  select(-9) %>% 
  mutate(ano = as.numeric(ano))

x2 <- x1 %>% 
  filter(!is.na(IDEB_5o)) %>% 
  mutate(ano = str_remove_all(ano, "IDEB_5o_"),
         ano = as.numeric(ano),
         IDEB_5o = MT_5o) %>% 
  select(c(4,7,9))

base_tidy_5o <- x %>% 
  left_join(x2, by = c("C贸digo da Escola", "ano"))

#### 2009 ####

SAEB_IDEB_ESCOLAS_9o <- read_excel("C:/Users/Bruno Bregola/Desktop/Projeto Cactus/SAEB_IDEB_ESCOLAS_9o.xlsx") %>% 
  mutate(across(7:22, as.numeric))


y <- SAEB_IDEB_ESCOLAS_9o %>%
  pivot_longer(-c(`Sigla da UF`:Rede), names_to = "ano", values_to = "MT_9o") %>% 
  mutate(ano = str_remove_all(ano, "MT_9o_"))


y1 <- y %>% 
  mutate(IDEB_9o = ifelse(str_length(ano) > 4, ano, NA))


y <- y1 %>% 
  filter(is.na(IDEB_9o)) %>% 
  select(-9) %>% 
  mutate(ano = as.numeric(ano))


y2 <- y1 %>% 
  filter(!is.na(IDEB_9o)) %>% 
  mutate(ano = str_remove_all(ano, "IDEB_9o_"),
         ano = as.numeric(ano),
         IDEB_9o = MT_9o) %>% 
  select(c(4,7,9))

base_tidy_9o <- y %>% 
  left_join(y2, by = c("C贸digo da Escola", "ano"))

#### juntando ####

base <- base_tidy_5o %>% left_join(base_tidy_9o)


write_xlsx(base, "SAEB_IDEB_5o_9o.xlsx")


#### base pareada ####

base_pareada <- base %>% 
  filter(!is.na(MT_5o),
         !is.na(MT_9o),
         !is.na(IDEB_5o),
         !is.na(IDEB_9o)) %>%
  mutate(filtro = case_when(ano == 2017 ~ 1,
                            ano == 2019 ~ 1,
                            TRUE ~ 0)) %>%
  filter(filtro == 1) %>% 
  select(-filtro) %>% 
  group_by(`C贸digo da Escola`) %>% 
  filter(n() == 2) %>% 
  ungroup()


base_pareada %>% count(`C贸digo da Escola`) %>% view


write_xlsx(base_pareada, "base_pareada.xlsx")

###########################




#### adicionando o SPAECE ####



#############################
# RODAR ESSA PARTE SEPARADA #
#############################



spaece <- read_xlsx("SPAECE_9o_MT.xlsx") %>% 
  select(-c(Rede, `Regional Fortaleza`, `Munic铆pio`, `Distrito Fortaleza`))


base <- read_csv("SAEB_IDEB_ESCOLAS_5o_9o_2019_cactus.csv") %>% 
  filter(ano == c(2017, 2019)) %>% 
  select(-c(X1, `Sigla da UF`, `Nome do Munic铆pio_y`))


x <- left_join(base, spaece, by = c("C贸digo do Munic铆pio", "C贸digo da Escola", "ano" = "Edi莽茫o"))


write_csv(x, "SAEB_IDEB_SPAECE.csv")



#############################

evasao9_2017 <- read_xlsx("evasao_ceara_9o_2017.xlsx")
evasao9 <- read_xlsx("evasao_ceara_9o_2019.xlsx")

evasao9 <- 
  evasao9 %>% 
  mutate(across(6:11, ~ str_replace_all(.x, "NA", NA_character_)))


evasao9 <- 
  evasao9 %>% 
  mutate(across(6:11, ~ as.numeric(.x)))


evasao9_2017 <- 
  evasao9_2017 %>% 
  mutate(across(6:11, ~ as.numeric(.x)))

evasao9_2017 <- 
  evasao9_2017 %>% 
  mutate(across(6:11, ~ str_replace_all(.x, "NA", NA_character_)))

evasao_total <- rbind(evasao9_2017, evasao9)

ideb_saeb_spaece <- read_csv("SAEB_IDEB_SPAECE.csv")

df_regressao <- left_join(ideb_saeb_spaece, evasao_total, by = c("C骴igo do Munic韕io", "C骴igo da Escola", "ano" = "Ano"))

df_regressao <- df_regressao%>% rename(Nome_Escola = `Nome da Escola.x`)

library(writexl)
write_xlsx(df_regressao, "base_final.xlsx")
  
  
  
