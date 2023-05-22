pacman::p_load(tidyverse, rvest, httr)

url <- "https://moweek.com.uy/"
html_content <- GET(url)
webpage <- read_html(content(html_content, as = "text"))

category_nodes <- html_nodes(webpage, ".expandedCategory")

category_urls <- lapply(category_nodes, function(node) html_nodes(node, "a") %>% 
                          html_attr("href")) %>% 
  unlist() %>% 
  str_subset("/vestimenta/|/calzado/|/accesorios/")

image_data <- tibble()
for (url in category_urls) {
  cat_url <- paste0("https://moweek.com.uy", url)
  # Because we are sending so many requests, some are likely to fail
  cat_content <- RETRY("GET", cat_url)
  cat_page <- read_html(content(cat_content, as = "text"))
  
  # Extract the category name and the subcategory name from the page title
  cat_name <- html_text(html_node(cat_page, "title")) %>%
    str_replace("- MoWeek", "") %>%
    str_to_title()
  subcat_name <- html_text(html_node(cat_page, ".categoryLevelTwoTitle")) %>%
    str_to_title()
  
  # Extract the image information from the page and store it in a data frame
  image_tags <- html_nodes(cat_page, ".productViewContainer")
  # Initialise columns so bind_rows() works
  image_info <- tibble(
    Category = character(),
    Subcategory = character(),
    Name = character(),
    Price = character(),
    Image_URL = character()
  )
  for (tag in image_tags) {
    name <- html_text(html_node(tag, ".productViewName"))
    price <- html_text(html_node(tag, ".productViewPrice"))
    img_url <- html_attr(html_node(tag, ".productViewTop"), "data-hover-image")
    image_info <- image_info %>%
      add_row(Category = cat_name,
              Subcategory = subcat_name,
              Name = name,
              Price = price,
              Image_URL = img_url)
  }
  image_data <- image_data %>% 
    bind_rows(image_info)
}

# Clean data
image_data <- image_data %>%
  mutate(Name = str_trim(Name),
         Price = str_trim(Price)) %>% 
  filter(Image_URL != "/files/empty.png")

# Get the file name of each image

file_names <- str_match(image_data$Image_URL, ".+(/.*?$)")[, 2] %>%
  str_sub(start = 2L)

for (i in seq_len(nrow(image_data))) {
  url <- image_data$Image_URL[i]
  category <- image_data$Category[i]
  subcategory <- image_data$Subcategory[i]
  category_folder <- gsub(" ", "_", category)
  subcategory_folder <- gsub(" ", "_", subcategory)
  
  if (!dir.exists(category_folder)) {
    dir.create(category_folder)
  }
  if (!dir.exists(paste0(category_folder, "/", subcategory_folder))) {
    dir.create(paste0(category_folder, "/", subcategory_folder))
  }
  
  # Download and store image in correct directory
  dir_name <- paste0(category_folder, "/", subcategory_folder, "/")
  file_name <- file_names[i]
  download.file(url, paste0(dir_name, file_name))
}
