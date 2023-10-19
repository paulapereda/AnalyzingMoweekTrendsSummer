pacman::p_load(tidyverse, janitor, here)
source(here("src", "estilo_paula.R"))


clothes <- read_rds(here("data", "product_vestimenta.rds")) %>%
  filter(category %in% c("Blazers y chaquetas", "Blusas y tops", "Buzos y sacos", "Camisas",            
                         "Camperas y tapados",  "Capas y ponchos", "Chalecos y kimonos", "Conjuntos",          
                         "Jeans", "Monos", "Pantalones", "Polleras", "Remeras", "Ruanas y chales", 
                         "Shorts y bermudas", "Vestidos", "Trajes de baño"))

# DATA BUT MAKE IT FASHION - PARTE 3
## What about sizes?

sizes <- clothes %>% 
  pivot_longer(size_1:size_11, names_to = "size", values_to = "size_value") %>% 
  filter(!is.na(size_value)) %>% 
  group_by(category, brand, size_value) %>% 
  summarise(size = n()) %>% 
  arrange(category, brand, desc(size)) %>% 
  ungroup() 
  
### Jeans, Pantalones, Shorts & Bermudas

aux <- tibble(size_value = c("U", 
                             "0", "1", "2", "3", "4", "5", "6",
                             "S/M", "M/L", "L/XL",
                             "XXS", "XS", "S", "M", "L", "XL", "XXL", "XXXL", "4XL",
                             "22", "23", "24", "25", "26", "27", 
                             "28", "29", "30", "31", "32", "33", 
                             "34", "36", "38", "40", "42", "44"),
              x = lag(cumsum(c(2L, 
                               2L, 2L, 2L, 2L, 2L, 2L, 2L,
                               3L, 3L, 3L, 
                               3L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 
                               2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                               2L, 2L, 2L, 2L, 2L, 2L, 1L)), default = 0))


(lowerwear <- sizes %>% 
    filter(category %in% c("Jeans", "Pantalones", "Shorts y bermudas")) %>% 
    select( - category, - size) %>% 
    mutate(escala_talle = case_when(
             size_value == "U" ~ "Talle Único",
             size_value %in% c("0", "1", "2", "3", "4", "5", "6") ~ "Talles 1, 2, 3, 4, 5 & 6",
             size_value %in% c("S/M", "M/L", "L/XL") ~ "Talles S/M, M/L & L/XL",
             size_value %in% c("XXS", "XS", "S", "M", "L", "XL", "XXL", "XXXL", "4XL") ~ "Talles XXS, XS, S, M, L, XL, XXL, XXXL & 4XL",
             size_value %in% c("22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "36", "38", "40", "42", "44") ~ "Talles 22 al 44")) %>%
    left_join(aux, by = "size_value") %>%
    mutate(size_value = factor(size_value, levels = aux$size_value)) %>%
    arrange(brand, desc(size_value)) %>%
    ggplot(aes(x, brand, group = 1)) +
    geom_line(aes(group = interaction(escala_talle, brand)), color = "#988880") +
    geom_point(aes(color = size_value), alpha = .8, size = 3) +
    scale_color_manual(values = rep(c("#8E6151", "#988880"), nrow(aux))) + 
    scale_x_continuous(breaks = aux$x, labels = aux$size_value,
                       expand = c(.01, .01)) +
    scale_y_discrete(expand = c(.01, .01)) +
    theme(legend.position = "none",
          panel.border = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = "Escala de talles disponibles",
         y = "Marca"))

ggsave(here("plots", "01_lowerwear_talles.png"), dpi = 300, width = 16, height = 12)


### Blusas y tops, Camisas & Remeras

aux <- tibble(size_value = c("U", 
                             "0", "1", "2", "3", "4", 
                             "XS/S", "S/M", "M/L", "L/XL",
                             "XXS", "XS", "S", "M", "L", "XL", "XXL", "XXXL"),
              x = lag(cumsum(c(2L, 
                               1L, 1L, 1L, 1L, 2L,
                               2L, 2L, 2L, 3L, 
                               2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L)), default = 0))
(topwear <- sizes %>% 
    filter(category %in% c("Blusas y tops", "Camisas", "Remeras")) %>% 
    filter(!(size_value %in% c(90, 95, 100, 105, 110, 115))) %>% 
    select( - category, - size) %>% 
    mutate(escala_talle = case_when(
      size_value == "U" ~ "Talle Único",
      size_value %in% c("0", "1", "2", "3", "4") ~ "Talles 1, 2, 3 & 4",
      size_value %in% c("XS/S", "S/M", "M/L", "L/XL") ~ "Talles XS/S, S/M, M/L & L/XL",
      size_value %in% c("XXS", "XS", "S", "M", "L", "XL", "XXL", "XXXL") ~ "Talles XXS, XS, S, M, L, XL, XXL & XXXL")) %>%
    left_join(aux, by = "size_value") %>%
    mutate(size_value = factor(size_value, levels = aux$size_value)) %>%
    arrange(brand, desc(size_value)) %>%
    ggplot(aes(size_value, brand)) +
    geom_line(aes(group = interaction(escala_talle, brand)), color = "#988880") +
    geom_point(aes(color = size_value), alpha = .8, size = 3) +
    scale_color_manual(values = rep(c("#8E6151", "#988880"), nrow(aux))) +
    scale_y_discrete(expand = c(.01, .01)) +
    theme(legend.position = "none",
          panel.border = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = "Escala de talles disponibles",
         y = "Marca"))

ggsave(here("plots", "02_topwear_talles.png"), dpi = 300, width = 15, height = 12)

## Blazers & Chaquetas, Camperas y tapados

aux <- tibble(size_value = c("U", 
                             "0", "1", "2", "3", "4", 
                             "XS/S", "S/M", "M/L", "L/XL",
                             "XXS", "XS", "S", "M", "L", "XL", "XXL"),
              x = lag(cumsum(c(2L, 
                               1L, 1L, 1L, 1L, 2L,
                               2L, 2L, 2L, 3L, 
                               2L, 2L, 2L, 2L, 2L, 2L, 2L)), default = 0))
(outwear_1 <- sizes %>% 
    filter(category %in% c("Blazers y chaquetas", "Camperas y tapados")) %>% 
    select( - category, - size) %>% 
    mutate(escala_talle = case_when(
      size_value == "U" ~ "Talle Único",
      size_value %in% c("0", "1", "2", "3", "4") ~ "Talles 1, 2, 3 & 4",
      size_value %in% c("XS/S", "S/M", "M/L", "L/XL") ~ "Talles XS/S, S/M, M/L & L/XL",
      size_value %in% c("XXS", "XS", "S", "M", "L", "XL", "XXL") ~ "Talles XXS, XS, S, M, L, XL & XXL")) %>%
    left_join(aux, by = "size_value") %>%
    mutate(size_value = factor(size_value, levels = aux$size_value)) %>%
    arrange(brand, desc(size_value)) %>%
    ggplot(aes(size_value, brand)) +
    geom_line(aes(group = interaction(escala_talle, brand)), color = "#988880") +
    geom_point(aes(color = size_value), alpha = .8, size = 3) +
    scale_color_manual(values = rep(c("#8E6151", "#988880"), nrow(aux))) +
    scale_y_discrete(expand = c(.01, .01)) +
    theme(legend.position = "none",
          panel.border = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = "Escala de talles disponibles",
         y = "Marca"))

ggsave(here("plots", "03_outwear_1_talles.png"), dpi = 300, width = 15, height = 12)

### Capas, ponchos, ruanas y chales

aux <- tibble(size_value = c("U", 
                             "0", "1", "2", 
                             "S", "M"),
              x = lag(cumsum(c(2L, 
                               1L, 1L, 2L,
                               2L, 3L)), default = 0))
(outwear_2 <- sizes %>% 
    filter(category %in% c("Capas y ponchos", "Ruanas y chales")) %>% 
    select( - category, - size) %>% 
    mutate(escala_talle = case_when(
      size_value == "U" ~ "Talle Único",
      size_value %in% c("0", "1", "2") ~ "Talles 0, 1 & 2",
      size_value %in% c("S", "M") ~ "Talles S & M")) %>%
    left_join(aux, by = "size_value") %>%
    mutate(size_value = factor(size_value, levels = aux$size_value)) %>%
    arrange(brand, desc(size_value)) %>%
    ggplot(aes(size_value, brand)) +
    geom_line(aes(group = interaction(escala_talle, brand)), color = "#988880") +
    geom_point(aes(color = size_value), alpha = .8, size = 3) +
    scale_color_manual(values = rep(c("#8E6151", "#988880"), nrow(aux))) +
    scale_y_discrete(expand = c(.01, .01)) +
    theme(legend.position = "none",
          panel.border = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = "Escala de talles disponibles",
         y = "Marca"))

ggsave(here("plots", "04_outwear_2_talles.png"), dpi = 300, width = 15, height = 12)

## Buzos y sacos

aux <- tibble(size_value = c("U", 
                             "0", "1", "2", "3", "4", "6", "8", "10",   
                             "XS/S", "S/M", "M/L", "L/XL",
                             "XS", "S", "M", "L", "XL", "XXL", "XXXL",
                             "35", "36", "37", "38", "39", "40"),
              x = lag(cumsum(c(2L, 
                               1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L,
                               2L, 2L, 2L, 3L, 
                               2L, 2L, 2L, 2L, 3L, 5L, 3L,
                               1L, 1L, 1L, 1L, 1L, 1L)), default = 0))
(buzos_sacos <- sizes %>% 
    filter(category %in% c("Buzos y sacos")) %>% 
    select( - category, - size) %>% 
    mutate(escala_talle = case_when(
      size_value == "U" ~ "Talle Único",
      size_value %in% c("0", "1", "2", "3", "4", "6", "8", "10") ~ "Talles 1, 2, 3 & 4",
      size_value %in% c("XS/S", "S/M", "M/L", "L/XL") ~ "Talles XS/S, S/M, M/L & L/XL",
      size_value %in% c("XS", "S", "M", "L", "XL", "XXL", "XXXL") ~ "Talles XS, S, M, L, XL & XXL",
      size_value %in% c("35", "36", "37", "38", "39", "40") ~ "Talles 35, 36, 37, 38, 39 y 40")) %>%
    left_join(aux, by = "size_value") %>%
    mutate(size_value = factor(size_value, levels = aux$size_value)) %>%
    arrange(brand, desc(size_value)) %>%
    ggplot(aes(size_value, brand)) +
    geom_line(aes(group = interaction(escala_talle, brand)), color = "#988880") +
    geom_point(aes(color = size_value), alpha = .8, size = 3) +
    scale_color_manual(values = rep(c("#8E6151", "#988880"), nrow(aux))) +
    scale_y_discrete(expand = c(.01, .01)) +
    theme(legend.position = "none",
          panel.border = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = "Escala de talles disponibles",
         y = "Marca"))

ggsave(here("plots", "05_buzos_sacos_talles.png"), dpi = 300, width = 15, height = 12)

### Vestido & Monos

aux <- tibble(size_value = c("U", 
                             "1", "2", "3", "4", 
                             "XS/S", "S/M", "M/L", "L/XL", 
                             "XXS", "XS", "S", "M", "L", "XL"),
              x = lag(cumsum(c(2L, 
                               1L, 1L, 1L, 2L,
                               2L, 2L, 2L, 3L,
                               2L, 2L, 2L, 2L, 2L, 3L)), default = 0))
(vestidos_monos <- sizes %>% 
    filter(category %in% c("Vestidos", "Monos")) %>% 
    select( - category, - size) %>% 
    mutate(escala_talle = case_when(
      size_value == "U" ~ "Talle Único",
      size_value %in% c("1", "2", "3", "4") ~ "Talles 1, 2, 3 & 4",
      size_value %in% c("XS/S", "S/M", "M/L", "L/XL") ~ "Talles XS/S, S/M, M/L & L/XL",
      size_value %in% c("XXS", "XS", "S", "M", "L", "XL") ~ "Talles XXS, XS, S, M, L & XL")) %>%
    left_join(aux, by = "size_value") %>%
    mutate(size_value = factor(size_value, levels = aux$size_value)) %>%
    arrange(brand, desc(size_value)) %>%
    ggplot(aes(size_value, brand)) +
    geom_line(aes(group = interaction(escala_talle, brand)), color = "#988880") +
    geom_point(aes(color = size_value), alpha = .8, size = 3) +
    scale_color_manual(values = rep(c("#8E6151", "#988880"), nrow(aux))) +
    scale_y_discrete(expand = c(.01, .01)) +
    theme(legend.position = "none",
          panel.border = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(x = "Escala de talles disponibles",
         y = "Marca"))

ggsave(here("plots", "06_vestidos_monos_talles.png"), dpi = 300, width = 15, height = 12)
