pacman::p_load(tidyverse, rvest, httr)

url <- "https://moweek.com.uy/"
html_content <- GET(url)
webpage <- read_html(content(html_content, as = "text"))

category_nodes <- html_nodes(webpage, ".expandedCategory")

category_urls <- lapply(category_nodes, function(node) html_nodes(node, "a") %>%
                          html_attr("href")) %>%
  unlist() %>%
  str_subset("/vestimenta/|/calzado/|/accesorios/")
# 
# product_data <- tibble()
# for (url in image_data$Product_URL) {
#   # Because we are sending so many requests, some are likely to fail
#   product_content <- RETRY("GET", url)
#   product_page <- read_html(content(product_content, as = "text"))
#   
#   # Extract the information from the page and store it in a data frame
#   name <- html_text(html_node(product_page, ".productDetailName"))
#   by <- html_text(html_node(product_page, ".productDetailBrand"))
#   price <- html_text(html_node(product_page, ".productDetailPrice"))
#   express_shipping <- ifelse(grepl("Envio express", html_text(html_node(product_page, ".productDetailShipments"))), "Yes", "No")
#   national_product <- ifelse(grepl("Producto nacional", html_text(html_node(product_page, ".productDetailShipments"))), "Yes", "No")
#   sizes <- html_text(html_node(product_page, ".productDetailSize")) %>% str_remove("Talle: ") %>% str_trim()
#   colors <- html_text(html_node(product_page, ".productDetailColor")) %>% str_remove("Color: ") %>% str_trim()
#   description <- html_text(html_node(product_page, ".productDetailDescription"))
#   
#   product_info <- tibble(
#     Product_URL = url,
#     Name = name,
#     By = by,
#     Price = price,
#     Express_Shipping = express_shipping,
#     National_Product = national_product,
#     Sizes = sizes,
#     Colors = colors,
#     Description = description
#   )
#   product_data <- product_data %>% 
#     bind_rows(product_info)
# }

product_data <- tibble()
for (url in category_urls) {
  cat_url <- paste0("https://moweek.com.uy", url)
  cat_content <- RETRY("GET", cat_url)
  cat_page <- read_html(content(cat_content, as = "text"))
  
  image_tags <- html_nodes(cat_page, ".productViewContainer")
  
  for (tag in image_tags) {
    # Extract product information from the image tag
    name <- html_text(html_node(tag, ".productViewName"))
    price <- html_text(html_node(tag, ".productViewPrice"))
    product_url <- html_attr(html_node(tag, "a"), "href")
    img_url <- html_attr(html_node(tag, ".productViewTop"), "data-hover-image")
    
    # Visit the product page and extract additional information
    product_content <- RETRY("GET", paste0("https://moweek.com.uy", product_url))
    product_page <- read_html(content(product_content, as = "text"))
    
    brand <- html_text(html_node(product_page, ".productInfoTitle .brandName a em"))
    characteristics <- html_text(html_node(product_page, "#cocardasContainer"))
    prices <- html_text(html_nodes(product_page, "#productPricesContainer .productPrice"))
    sizes <- html_text(html_nodes(product_page, ".specGroupTitle:contains('Talle') + .specGroupContent a"))
    color <- html_text(html_nodes(product_page, ".specGroupTitle:contains('Color') + .specGroupContent"))
    description <- html_text(html_node(product_page, "#productInfoDescription .moreInfoText"))
    
    # Combine all the information into a single row and add it to the product_data data frame
    product_info <- tibble(
      Category = cat_name,
      Subcategory = subcat_name,
      Name = name,
      Price = price,
      Brand = brand,
      Characteristics = characteristics,
      Prices = prices,
      Sizes = sizes,
      Color = color,
      Description = description,
      Image_URL = img_url,
      Product_URL = product_url
    )
    product_data <- product_data %>%
      add_row(product_info)
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
         Description = str_trim(Description)) %>% 
  filter(Image_URL != "/files/empty.png")
