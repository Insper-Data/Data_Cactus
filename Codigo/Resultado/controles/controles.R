
library(tidyverse)

c13 <- read_csv("controles_13.csv")

c15 <- read_csv("controles_15.csv")

c17 <- read_csv("controles_17.csv")

c19 <- read_csv("controles_19.csv")


rbind(c13, c15, c17)
