pacman::p_load(tidyverse, rvest, httr)

url <- "https://moweek.com.uy/"
html_content <- GET(url)
webpage <- read_html(content(html_content, as = "text"))

category_nodes <- html_nodes(webpage, ".expandedCategory")

category_urls <- lapply(category_nodes, function(node) html_nodes(node, "a") %>%
                          html_attr("href")) %>%
  unlist() %>%
  str_subset("/vestimenta/|/calzado/|/accesorios/") %>% 
  str_replace_all("1", "10") 

category_urls <- category_urls[!(category_urls %in% c("/vestimenta/10", "/calzado/10", 
                                                      "/accesorios/10"))]

product_data <- tibble()
for (url in category_urls) {
  
  cat_url <- paste0("https://moweek.com.uy", url)
  cat_content <- RETRY("GET", cat_url)
  cat_page <- read_html(content(cat_content, as = "text"))
  
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
    characteristics <- html_text(html_node(product_page, "#cocardasContainer"))
    img_url <- html_attr(html_node(product_page, ".navImgProduct"), "src")
    sizes <- html_text(html_nodes(product_page, ".specGroupTitle:contains('Talle') + .specs"))
    color <- html_text(html_nodes(product_page, ".specGroupTitle:contains('Color') + .specs"))
    description <- html_text(html_node(product_page, ".productGroupInfo"))
    
    # Combine all the information into a single row and add it to the product_data data frame
    product_info <- tibble(
      name = name,
      price = price,
      brand = brand,
      bharacteristics = characteristics,
      sizes = sizes,
      colors = color,
      description = description,
      image_URL = img_url,
      product_URL = product_url)
    
    },
    
    error = function(e) { skip_to_next <<- TRUE})
    
    if(skip_to_next) { next } 
    
    product_data <- product_data %>%
      bind_rows(product_info)
  }
}

# Clean data
product_data <- product_data %>%
  mutate(Name = str_trim(Name),
         Price = str_trim(Price),
         Brand = str_trim(Brand),
         Characteristics = str_trim(Characteristics),
         Sizes = str_trim(Sizes),
         Color = str_trim(Color),
         Description = str_trim(Description))
# %>% filter(Image_URL != "/files/empty.png")