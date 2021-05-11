library(tidyverse)

library(idebr)

db <- ideb_fundamental_finais_escolas %>% 
  filter(sigla_uf == "CE",
         rede == "Municipal",
         ano %in% c(2013, 2015, 2017, 2019)) %>% 
  select(c(4, 5, 7, 14, 17))


# controles <- read_csv("controles.csv")


# painel <- left_join(db, controles, by = c("id_escola" = "ID_ESCOLA", "ano" = "ano"))


# write_csv(painel, "painel.csv")