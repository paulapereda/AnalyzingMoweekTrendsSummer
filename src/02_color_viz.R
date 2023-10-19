pacman::p_load(tidyverse, here)
source(here("src", "estilo_paula.R"))

# My dataset
color_palette <- read_rds("data-raw/color_palette_by_category.rds")

# Convert the data to a long format
color_palette_long <- color_palette %>%
  pivot_longer(cols = -Subfolder, names_to = "Distance", values_to = "Color") %>% 
  mutate(Subfolder = str_remove(Subfolder, "images/"),
         category = case_when(
           Subfolder == "blusas-y-tops" ~ "Blusas & tops",
           Subfolder == "remeras" ~ "Remeras",             
           Subfolder == "trajes-de-bano" ~ "Trajes de baño",     
           Subfolder == "camperas-y-tapados" ~ "Camperas & tapados", 
           Subfolder == "buzos-y-sacos" ~ "Buzos & sacos",
           Subfolder == "camisas" ~ "Camisas",            
           Subfolder == "vestidos" ~ "Vestidos",
           Subfolder == "pantalones" ~ "Pantalones",
           Subfolder == "capas-y-ponchos" ~ "Capas & ponchos",
           Subfolder == "blazers-y-chaquetas" ~ "Blazers & chaquetas",
           Subfolder == "chalecos-y-kimonos" ~ "Chalecos & kimonos", 
           Subfolder == "polleras" ~ "Polleras",           
           Subfolder == "jeans" ~ "Jeans",
           Subfolder == "shorts-y-bermudas" ~ "Shorts & bermudas",  
           Subfolder == "conjuntos" ~ "Conjuntos",        
           Subfolder == "monos" ~ "Monos"))

# Color distance calculation function
color_distance <- function(color1, color2) {
  col1 <- as.numeric(col2rgb(color1))
  col2 <- as.numeric(col2rgb(color2))
  sqrt(sum((col1 - col2)^2))
}

# Calculate color distances
color_palette_long <- color_palette_long %>%
  rowwise() %>% 
  mutate(Distance = color_distance(Color, "#FFFFFF"))

# Sort the data by color distance
color_palette_long <- color_palette_long %>%
  group_by(Subfolder) %>% 
  arrange(Distance) %>% 
  ungroup()

# Create the ggplot

# [1]  "blusas-y-tops"       "remeras"             "trajes-de-bano"      "camperas-y-tapados"  "buzos-y-sacos"       "camisas"            
# [7]  "vestidos"            "pantalones"          "capas-y-ponchos"     "blazers-y-chaquetas" "chalecos-y-kimonos"  "polleras"           
# [13] "jeans"               "shorts-y-bermudas"   "conjuntos"           "monos"

color_palette_long %>% 
  filter(Subfolder %in% c("blusas-y-tops", "remeras", "camisas")) %>% 
  ggplot(aes(x = reorder(category, Distance), y = 1, fill = Color)) +
    geom_col(position = "fill") +
    scale_fill_identity() +
    labs(x = NULL, y = NULL) +
  theme_void() +
  theme(axis.text.x = element_text(size = 15),
        text = element_text(family = "Belleza-Regular"))

ggsave(here("plots", "upperwear.png"), dpi = 300, width = 13, height = 7)

color_palette_long %>% 
  filter(Subfolder %in% c("pantalones", "polleras", "jeans")) %>% 
  ggplot(aes(x = reorder(category, Distance), y = 1, fill = Color)) +
  geom_col(position = "fill") +
  scale_fill_identity() +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(axis.text.x = element_text(size = 15),
        text = element_text(family = "Belleza-Regular"))

ggsave(here("plots", "bottomwear.png"), dpi = 300, width = 13, height = 7)

