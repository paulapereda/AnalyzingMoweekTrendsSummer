pacman::p_load(tidyverse, rvest, httr)

url <- "https://moweek.com.uy"
webpage <- httr::GET(url)

webpage <- read_html(content(webpage, as = "text"))

category_nodes <- html_nodes(webpage, ".expandedCategory")

category_urls <- lapply(category_nodes, function(node) html_nodes(node, "a") %>%
                          html_attr("href")) %>%
  unlist() %>%
  str_subset("vestimenta") %>%
  str_replace_all("1", "30")

category_urls <- category_urls[!(category_urls %in% c("/vestimenta/30",          "/vestimenta/activewear/30",
                                                      "/vestimenta/bodywear/30", "/vestimenta/leggings-y-bikers/30",
                                                      "/vestimenta/lenceria/30", "/vestimenta/hombre/30",
                                                      "/vestimenta/ninos/30",    "/vestimenta/pijamas-y-camisones/30",
                                                      "https://moweek.com.uy/vestimenta/vestidos/30",
                                                      "https://moweek.com.uy/vestimenta/trajes-de-bano/30"))]

dir.create("images3", showWarnings = FALSE)

# Function to limit folder name length
limit_folder_name_length <- function(category_url, max_length = 50) {
  # Extract the subcategory name from the category_url
  subcategory_name <- gsub(".*/vestimenta/(.*?)/30", "\\1", category_url)
  
  if (nchar(subcategory_name) > max_length) {
    subcategory_name <- substr(subcategory_name, 1, max_length)
  }
  return(subcategory_name)
}

# Loop through subcategories and download images
for (category_url in category_urls) {
  
  subcategory_name <- gsub(".*/vestimenta/(.*?)/30", "\\1", category_url)
  subfolder_name <- limit_folder_name_length(gsub(" ", "_", tolower(subcategory_name)))
  subfolder_path <- file.path("images3", subfolder_name)
  dir.create(subfolder_path, showWarnings = FALSE, recursive = TRUE)
  
  subcategory_url <- paste0(url, gsub("/+", "/", category_url))
  
  # Sleep for a few seconds to avoid overloading the server
  Sys.sleep(3)
  
  cat_content <- RETRY("GET", subcategory_url)
  cat_page <- read_html(content(cat_content, as = "text"))
  
  image_urls <- cat_page %>%
    html_nodes(".productViewTopImage") %>%
    html_attr("src")
  
  image_urls <- ifelse(substr(image_urls, 1, 8) != "https://", paste0(url, image_urls), image_urls)
  
  for (image_url in image_urls) {
    if (!is.na(image_url) && image_url != "") {
     
      image_name <- basename(image_url)
      image_path <- file.path(subfolder_path, image_name)
      
      # Download and save the image to the subfolder
      download.file(image_url, image_path, mode = "wb")
      
    }
  }
}
