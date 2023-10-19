pacman::p_load(tidyverse, EBImage, here)

source(here("src", "extract_colors.R"))

# Function to process a subfolder and extract colors for all images
process_subfolder <- function(subfolder_path, num_col = 5, main_folder) {
  image_files <- list.files(subfolder_path, full.names = TRUE, pattern = "\\.(jpg|jpeg|png|gif)$", recursive = TRUE)
  if (length(image_files) == 0) {
    return(NULL)
  }
  
  colors_data <- data.frame(Subfolder = character(0), Colors = character(0))
  
  for (image_file in image_files) {
    tryCatch({
      colors <- extract_colours(image_file, num_col)
      colors_data <- rbind(colors_data, data.frame(Subfolder = subfolder_path, Colors = paste(colors, collapse = ", ")))
    }, error = function(e) {
      # Log the error and continue to the next image
      cat("Error processing image:", image_file, " - Error: ", conditionMessage(e), "\n")
    })
  }
  
  # Check if the subfolder is not the same as the main folder
  if (subfolder_path != main_folder) {
    return(colors_data)
  } else {
    return(NULL)
  }
}

# Main function to process all subfolders
process_main_folder <- function(main_folder, num_col = 5) {
  subfolder_paths <- list.dirs(main_folder, full.names = TRUE, recursive = TRUE)
  all_colors_data <- data.frame(Subfolder = character(0), Colors = character(0))
  
  for (subfolder_path in subfolder_paths) {
    subfolder_colors_data <- process_subfolder(subfolder_path, num_col, main_folder)
    if (!is.null(subfolder_colors_data)) {
      all_colors_data <- rbind(all_colors_data, subfolder_colors_data)
    }
  }
  
  return(all_colors_data)
}

main_folder <- "images"
result <- process_main_folder(main_folder) %>% 
  separate(Colors, into = c("Color1", "Color2", "Color3", "Color4", "Color5"), sep = ", ")

# Save the result to a rds file
write_rds(result, "data-raw/color_palette_by_category.rds")
