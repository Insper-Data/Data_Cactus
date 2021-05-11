
library(tidyverse)

c13 <- read_csv("controles_13.csv") 

c13 <- c13 %>% mutate(ano = 2013)

c15 <- read_csv("controles_15.csv")

c15 <- c15 %>% mutate(ano = 2015)

c17 <- read_csv("controles_17.csv")

c17 <- c17 %>% mutate(ano = 2017)

c19 <- read_csv("controles_19.csv")


controles <- rbind(c13, c15, c17, c19)


write_csv(controles, "controles.csv")
