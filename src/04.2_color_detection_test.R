pacman::p_load(tidyverse, colormap, magick, here)

# Define a function to extract the dominant colors
extract_colors <- function(file_path) {
  img <- tryCatch({
    image_read(file_path)  # Read the image
  }, error = function(e) {
    return(NULL)  # Return NULL if the image cannot be read
  })
  
  if (!is.null(img)) {
    img_colors <- image_quantize(img, colorspace = "sRGB")  # Quantize colors
    return(img_colors)
  } else {
    return(NULL)
  }
}

# Get the list of image file paths
folder_path <- here("imgs","Blazers_Y_Chaquetas_-_Moweek_-_Encontrá_Lo_Mejor_De_La_Moda_Local", "Ver_Todo")
file_paths <- list.files(path = folder_path, pattern = "\\.jpg$|\\.jpeg$|\\.png|\\.JPG|\\.JPEG|\\.PNG$", full.names = TRUE)

# Extract dominant colors for valid images
all_colors <- lapply(file_paths, extract_colors)
all_colors <- Filter(Negate(is.null), all_colors)  # Remove NULL values
if (length(all_colors) == 0) {
  stop("No valid images found in the folder.")
}

# Function to extract RGB values from color
get_rgb <- function(color) {
  rgb_val <- col2rgb(color)
  rgb(rgb_val[1, 1], rgb_val[2, 1], rgb_val[3, 1], maxColorValue = 255)
}

# Perform k-means clustering to find the dominant colors
num_colors <- 5
dominant_colors <- sapply(all_colors, function(colors) {
  rgb_values <- sapply(colors, function(color) {
    get_rgb(color)
  })
  kmeans(rgb_values, centers = num_colors)$centers
})

# Print the dominant colors
print(dominant_colors)
