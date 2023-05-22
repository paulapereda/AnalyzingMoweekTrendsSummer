pacman::p_load(tidyverse, here)

las_marcas <- read_rds(here("data-raw", "product_clean_vestimenta.rds")) %>% 
  filter(brand == "LAS MARCAS")

clothes_clean <- read_rds(here("data-raw", "product_clean_vestimenta_LAST.rds")) %>%
  bind_rows(las_marcas) %>% 
  mutate(national = as.numeric(grepl("Producto nacional", characteristics)),
         sustainable = as.numeric(grepl("Sustentable", characteristics)),
         colors = gsub("[[:space:]]+", " ", colors),
         colors = str_replace_all(colors, "Metalizado", "metalizado"),
         sizes = gsub("[[:space:]]+", " ", sizes),
         category = str_to_sentence(category)) %>% 
  separate(colors, into = paste0("color_", 1:13), sep = "(?<=[A-Z])\\s+(?=[A-Z][^a-z])|\\s+(?=[A-Z])", extra = "merge") %>%
  separate(sizes, into = paste0("size_", 1:11), sep = "\\s+") %>%
  separate_rows(starts_with("size_"), sep = " ") %>% 
  select(- contains("URL")) %>% 
  unique()

write_rds(clothes_clean, here("data", "product_vestimenta.rds"))
