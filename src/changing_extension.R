# Load the 'fs' package for file system operations
library(fs)

# Function to rename files with a specific extension in a directory and its subdirectories
rename_files_in_directory <- function(directory_path, old_extension, new_extension) {
  # List all files and directories in the current directory
  dir_contents <- list.files(directory_path, full.names = TRUE)
  
  # Loop through each item in the directory
  for (item in dir_contents) {
    if (file.info(item)$isdir) {
      # If the item is a subdirectory, recursively call the function
      rename_files_in_directory(item, old_extension, new_extension)
    } else if (grepl(paste0(old_extension), item)) {
      # If the item is a file with the old extension, rename it
      new_name <- gsub(paste0(old_extension), paste0(new_extension), item)
      file.rename(item, new_name)
    }
  }
}

# Specify the root directory where you want to start the renaming
root_directory <- "images"
old_extension <- ".jpg"  # Change this to the old file extension
new_extension <- ".png"  # New file extension

# Call the function to rename files in the root directory and its subdirectories
rename_files_in_directory(root_directory, old_extension, new_extension)
