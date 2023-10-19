pacman::p_load(tidyverse, here)

clothes_clean <- read_rds(here("data-raw", "product_vestimenta_clean.rds")) %>%
  group_by(name, brand) %>% 
  summarize(
  characteristics = paste(characteristics, collapse = " "),
  price = first(price),
  bank_price = first(bank_price),  
  category = first(category),
  colors = first(colors), 
  sizes = first(sizes),
  description = first(description)) %>% 
  ungroup() %>% 
  mutate(national = as.numeric(grepl("Producto nacional", characteristics)),
         sustainable = as.numeric(grepl("Sustentable", characteristics)),
         colors = gsub("[[:space:]]+", " ", colors),
         colors = str_replace_all(colors, "Metalizado", "metalizado"),
         sizes = gsub("[[:space:]]+", " ", sizes),
         category = str_to_sentence(category)) %>% 
  separate(colors, into = paste0("color_", 1:13), sep = "(?<=[A-Z])\\s+(?=[A-Z][^a-z])|\\s+(?=[A-Z])", extra = "merge") %>%
  separate(sizes, into = paste0("size_", 1:11), sep = "\\s+") %>%
  separate_rows(starts_with("size_"), sep = " ")

write_rds(clothes_clean, here("data", "product_vestimenta.rds"))
