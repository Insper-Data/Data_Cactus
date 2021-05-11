library(tidyverse)

library(idebr)

options(digits = 9)

db <- ideb_fundamental_finais_escolas %>% 
  filter(sigla_uf == "CE",
         rede == "Municipal",
         ano %in% c(2013, 2015, 2017, 2019)) %>% 
  select(c(4, 5, 7, 14, 17)) %>% 
  filter(ideb != "") %>% 
  mutate(id_escola = as.numeric(id_escola),
         ano = as.numeric(ano),
         nota_matematica = str_replace_all(nota_matematica, "\\,", "\\."),
         nota_matematica = str_replace_all(nota_matematica, "(\\d{3}(?!.))", "\\1.00"),
         nota_matematica = str_replace_all(nota_matematica, "(\\d{3}\\.\\d(?!.))", "\\10"),
         nota_matematica = as.numeric(nota_matematica),
         ideb = as.numeric(gsub(",", ".", ideb)))


controles <- read_csv("controles.csv")


painel <- left_join(db, controles, by = c("id_escola" = "ID_ESCOLA", "ano" = "ano"))


write_csv(painel, "painel.csv")
