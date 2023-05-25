pacman::p_load(tidyverse, here)
source(here("src", "estilo_paula.R"))

# Nacional: este producto fue producido en Uruguay.
# Sustentable: este producto fue producido con materiales sostenibles.
              
clothes <- read_rds(here("data", "product_vestimenta.rds")) %>% 
  filter(category %in% c("Blazers y chaquetas", "Blusas y tops", "Buzos y sacos", "Camisas",            
                         "Camperas y tapados",  "Capas y ponchos", "Chalecos y kimonos", "Conjuntos",          
                         "Jeans", "Monos", "Pantalones", "Polleras", "Remeras","Ruanas y chales", 
                         "Shorts y bermudas", "Vestidos"))

# DATA BUT MAKE IT FASHION - Parte 1
## Price Median (many outliers)

prices_category <- clothes %>% 
  group_by(category) %>% 
  summarise(mean_price = round(median(price)))

clothes %>% 
  ggplot(aes(fct_reorder(category, price, .desc = F), price)) +
  geom_point(aes(color = category), alpha = .6, size = 3) +
  scale_color_manual(values = rep(c("#8E6151", "#988880"), 30)) +
  scale_y_continuous(limits = c(0, 40000),
                     breaks = seq(0, 40000, by = 5000),
                     labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                     expand = c(.02, .002)) + 
  theme(legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  coord_flip() +
  labs(x = "Categoría",
       y = "Precio",
       caption = expression(paste(italic("Nota: El punto verde representa la mediana por categoría.")))) +
  stat_summary(fun = "median", geom = "point", shape = 16, alpha = .8, size = 6, color = "#2F5153", 
               show.legend = TRUE) 

ggsave(here("plots", "dispersion_categoria.png"), dpi = 300, width = 13, height = 7)
  
# Sustanaibility brands

sustainable <- clothes %>% 
  group_by(brand) %>% 
  summarise(sustainable = sum(sustainable)/n())

sustainable %>% 
  ggplot(aes(fct_reorder(brand, sustainable, .desc = F), sustainable)) +
  geom_point(aes(color = brand), alpha = .8, size = 5) +
  scale_color_manual(values = rep(c("#8E6151", "#988880"), 31)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = .1),
                     labels = scales::percent_format(),
                     expand = c(.02, .02)) + 
  scale_x_discrete(expand = c(.02, .02)) + 
  theme(legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(margin = margin(r = 12))) +
  coord_flip() +
  labs(x = "Marca",
       y = "% de prendas sustentables dentro de la colección")

ggsave(here("plots", "sustainable_brands.png"), dpi = 300, width = 13, height = 12)

## National brands

national <- clothes %>% 
  group_by(brand) %>% 
  summarise(national = sum(national)/n())

national %>% 
  ggplot(aes(fct_reorder(brand, national, .desc = F), national)) +
  geom_point(aes(color = brand), alpha = .8, size = 5) +
  scale_color_manual(values = rep(c("#8E6151", "#988880"), 31)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = .1),
                     labels = scales::percent_format(),
                     expand = c(.02, .02)) + 
  scale_x_discrete(expand = c(.02, .02)) + 
  theme(legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(margin = margin(r = 12))) +
  coord_flip() +
  labs(x = "Marca",
       y = "% de prendas nacionales dentro de la colección")

ggsave(here("plots", "national_brands.png"), dpi = 300, width = 13, height = 12)

## Sustanaibility & national brands

sust_nat <- clothes %>% 
  group_by(brand) %>% 
  summarise(sustainable = sum(sustainable),
            national = sum(national),
            n = n()) %>% 
  ungroup() %>% 
  mutate(sustainable_national = round((((sustainable*.5 + national*.5))/n)*100, 2)) %>% 
  select( - sustainable, - national, - n)

# DATA BUT MAKE IT FASHION - Parte 2
## Color Trends

n <- clothes %>% 
  pivot_longer(color_1:color_13, names_to = "color", values_to = "color_value") %>% 
  filter(!is.na(color_value)) %>% 
  group_by(category) %>% 
  summarise(color = n())

pre_colors <- clothes %>% 
  pivot_longer(color_1:color_13, names_to = "color", values_to = "color_value") %>% 
  filter(!is.na(color_value)) %>% 
  group_by(color_value) %>% 
  summarise(color = n())


colors <- clothes %>% 
  pivot_longer(color_1:color_13, names_to = "color", values_to = "color_value") %>% 
  filter(!is.na(color_value)) %>% 
  group_by(category, color_value) %>% 
  summarise(color = n()) %>% 
  arrange(category, desc(color)) %>% 
  filter(row_number() %in% c(1, 2, 3, 4, 5, 6, 7 ,8 , 9, 10, 11, 12, 13, 14, 15)) %>% 
  mutate(color = case_when(
    category == "Blazers y chaquetas" ~ color/325,
    category == "Blusas y tops" ~ color/549,
    category == "Buzos y sacos" ~ color/1742,
    category == "Camisas" ~ color/453,
    category == "Camperas y tapados" ~ color/510,
    category == "Capas y ponchos" ~ color/143,
    category == "Chalecos y kimonos" ~ color/238,
    category == "Conjuntos" ~ color/61,
    category == "Jeans" ~ color/280,
    category == "Monos" ~ color/46,
    category == "Pantalones" ~ color/875,
    category == "Polleras" ~ color/211,
    category == "Remeras" ~ color/313,
    category == "Ruanas y chales" ~ color/284,
    category == "Shorts y bermudas" ~ color/66,
    category == "Vestidos" ~ color/494), 
    color = round(color*100, 2))

colors %>% 
  ggplot(aes(fct_reorder(color_value, color, .desc = F), color)) +
  geom_point(aes(color = color_value), alpha = .6, size = 2) +
  scale_color_manual(values = rep(c("#8E6151", "#988880"), 30)) +
  theme(legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(margin = margin(r = 5))) +
  labs(x = "Categoría",
       y = "10 colores predominantes") +
  coord_flip() +
  facet_wrap(~ category, scales = "free_x")

# DATA BUT MAKE IT FASHION - PARTE 3
## What about sizes?

sizes <- clothes %>% 
  pivot_longer(size_1:size_11, names_to = "size", values_to = "size_value") %>% 
  filter(!is.na(size_value)) %>% 
  group_by(category, brand, size_value) %>% 
  summarise(size = n()) %>% 
  arrange(category, desc(size))


