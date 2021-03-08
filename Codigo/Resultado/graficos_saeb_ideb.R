
library(tidyverse)
library(patchwork)

base <- read_csv("SAEB_IDEB_SOCIOECONOMICOS_cactus.csv") %>% 
  select(-c(X1, ID_PROVA_BRASIL, MEDIA_5EF_MT, MEDIA_9EF_MT)) %>% 
  mutate(cactus = str_remove_all(cactus, "_5_9"))

base %>% count(cactus)


base_agrupada <- base %>% group_by(cactus, ano) %>% 
  summarise(MT_5o = mean(MT_5o, na.rm = TRUE),
            MT_9o = mean(MT_9o, na.rm = TRUE),
            IDEB_5o = mean(IDEB_5o, na.rm = TRUE),
            IDEB_9o = mean(IDEB_9o, na.rm = TRUE))


b <- base_agrupada %>% 
  ggplot(aes(ano, MT_9o, color = cactus)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  scale_x_continuous(breaks = seq(-1, 2100, 2)) +
  labs(y = "Matemática SAEB 9o ano")
  
a <- base_agrupada %>% 
  ggplot(aes(ano, MT_5o, color = cactus)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  scale_x_continuous(breaks = seq(-1, 2100, 2)) +
  labs(y = "Matemática SAEB 5o ano")


c <- base_agrupada %>% 
  ggplot(aes(ano, IDEB_5o, color = cactus)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  scale_x_continuous(breaks = seq(-1, 2100, 2)) +
  #ylim(0, 10) +
  labs(y = "IDEB 5o ano")



d <- base_agrupada %>% 
  ggplot(aes(ano, IDEB_9o, color = cactus)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  scale_x_continuous(breaks = seq(-1, 2100, 2)) +
  #ylim(0, 10) +
  labs(y = "IDEB 9o ano")



base_agrupada %>%
  filter(ano == c(2017, 2019)) %>% 
  ggplot(aes(ano, MT_9o, color = cactus)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  scale_x_continuous(breaks = seq(-1, 2100, 2)) +
  labs(y = "Matemática SAEB 9o ano")



(a + b) / (c + d) + 
  plot_annotation(title = "Notas SAEB e IDEB",
                  subtitle = "por grupo de acordo com a entrada da Cactus na escola") + 
  plot_layout(guides = "collect")




##### Socioeconomicas ####

base %>% 
  count(NIVEL_SOCIO_ECONOMICO)


base %>% 
  filter(ano == 2017) %>% 
  group_by(cactus) %>% 
  filter(!is.na(NIVEL_SOCIO_ECONOMICO)) %>% 
  count(NIVEL_SOCIO_ECONOMICO) %>% 
  filter(cactus != "não cactus") %>% 
  ggplot(aes(cactus, n, fill = NIVEL_SOCIO_ECONOMICO)) +
  geom_col() +
  theme_classic() +
  theme(legend.title = element_blank()) +
  labs(title = "Número de escolas Cactus por nível socioeconômico",
       x = "Grupo",
       y = "#")
  


base %>% 
  filter(ano == 2017) %>% 
  group_by(cactus) %>% 
  filter(!is.na(NIVEL_SOCIO_ECONOMICO)) %>% 
  count(NIVEL_SOCIO_ECONOMICO) %>% 
  filter(cactus == "não cactus") %>% 
  ggplot(aes(cactus, n, fill = NIVEL_SOCIO_ECONOMICO)) +
  geom_col() +
  theme_classic() +
  theme(legend.title = element_blank()) +
  labs(title = "Número de escolas não Cactus por nível socioeconômico",
       x = "",
       y = "#")



