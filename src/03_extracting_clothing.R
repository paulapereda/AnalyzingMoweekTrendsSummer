start_time <- Sys.time()
pacman::p_load(tidyverse, rvest, httr)

url <- "https://moweek.com.uy"
html_content <- GET(url)
webpage <- read_html(content(html_content, as = "text"))

category_nodes <- html_nodes(webpage, ".expandedCategory")

category_urls <- lapply(category_nodes, function(node) html_nodes(node, "a") %>%
                          html_attr("href")) %>%
  unlist() %>%
  str_subset("/vestimenta/")  %>%
  str_replace_all("1", "60")

category_urls <- category_urls[!(category_urls %in% c("/vestimenta/60",          "/vestimenta/activewear/60",
                                                      "/vestimenta/bodywear/60", "/vestimenta/leggings-y-bikers/60",
                                                      "/vestimenta/lenceria/60", "/vestimenta/hombre/60",
                                                      "/vestimenta/ninos/60",    "/vestimenta/pijamas-y-camisones/60",
                                                      "https://moweek.com.uy/vestimenta/vestidos/60",
                                                      "https://moweek.com.uy/vestimenta/trajes-de-bano/60"))]

category_urls <- category_urls[(category_urls %in% c("/vestimenta/5"))]

product_data <- tibble()
for (url in category_urls) {
  
  cat_url <- paste0("https://moweek.com.uy", url)
  cat_content <- RETRY("GET", cat_url)
  cat_page <- read_html(content(cat_content, as = "text"))
  
  # Extract the category name and the subcategory name from the page title
  cat_name <- html_text(html_node(cat_page, "title")) %>%
    str_to_title() %>% 
    str_remove(" - Moweek - Encontrá Lo Mejor De La Moda Local")
  
  image_tags <- html_nodes(cat_page, ".productViewContainer")
  
  for (tag in image_tags) {
    
    skip_to_next <- FALSE
    
    tryCatch({ 
      
      # Extract product information from the image tag
      name <- html_text(html_node(tag, ".productViewName"))
      price <- html_text(html_node(tag, ".productViewPrice"))
      
      product_url <- html_attr(tag, "href")
      
      # Visit the product page and extract additional information
      product_content <- RETRY("GET", paste0("https://moweek.com.uy", product_url))
      product_page <- read_html(content(product_content, as = "text"))
      
      brand <- html_text(html_node(product_page, ".productInfoTitle.brandName"))
      characteristics <- html_text(html_elements(product_page, "#cocardasContainer div[class= 'filterCocarda shown']"))
      img_url <- html_attr(html_node(product_page, ".navImgProduct"), "src")
      sizes <- html_text(html_nodes(product_page, ".specGroupTitle:contains('Talle') + .specs"))
      color <- html_text(html_nodes(product_page, ".specGroupTitle:contains('Color') + .specs"))
      description <- html_text(html_node(product_page, ".productGroupInfo"))
      
      # Combine all the information into a single row and add it to the product_data data frame
      product_info <- tibble(name = name,
                             price = price,
                             brand = brand,
                             type = url,
                             category = cat_name,
                             characteristics = characteristics,
                             sizes = sizes,
                             colors = color,
                             description = description)
      
    },
    
    error = function(e) { skip_to_next <<- TRUE})
    
    if(skip_to_next) { next } 
    
    product_data <- product_data %>%
      bind_rows(product_info)
  }
}

write_rds(product_data, "data-raw/product_vestimenta.rds")

# Clean data

product_data_clean <- product_data %>%
  transmute(name = str_trim(name),
            price = str_trim(price),
            bank_price = str_trim(price),
            brand = str_trim(brand),
            category = category,
            characteristics = str_trim(characteristics),
            sizes = str_trim(sizes),
            colors = str_trim(colors),
            description = str_trim(description)) %>% 
  mutate(price = as.numeric(str_replace_all(price, "[\\$\\s.]", "")),
         bank_price = price*.75,
         brand = str_remove(brand, "by \n                                "),
         type = "Vestimenta")

write_rds(product_data_clean, here("data-raw", "product_vestimenta_clean.rds"))

end_time <- Sys.time()
end_time - start_time

# Time difference of 1.867295 hours
