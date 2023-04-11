pacman::p_load(jpeg, tidyverse, colourspace)

detect_dominant_colors <- function(folder_path, n = 5) {
  img_list <- list.files(folder_path, pattern = ".jpg|.jpeg|.png", full.names = TRUE)
  color_df <- purrr::map_df(img_list, function(filename) {
    img <- jpeg::readJPEG(filename)
    img_df <- data.frame(colourspace::RGB2LCH(colourspace::RGB(img)))
    img_df <- img_df %>%
      mutate(color = paste(round(L), round(C), round(H), sep = "_")) %>%
      group_by(color) %>%
      summarize(freq = n()) %>%
      ungroup() %>%
      arrange(desc(freq)) %>%
      top_n(n, freq)
    img_df$filename <- filename
    return(img_df)
  })
  color_summary <- color_df %>%
    group_by(color) %>%
    summarize(mean_freq = mean(freq))
  return(list(color_df = color_df, color_summary = color_summary))
}

result <- detect_dominant_colors("path/to/folder/with/images") # acá puedo usar la función :)

# Extract the color_df and color_summary data frames from the result list.

color_df <- result$color_df
color_summary <- result$color_summary

# View the color_summary data frame to see the mean frequency of each dominant color across all images in the folder.

color_summary

# Visualize the dominant colors (in a folder x) using a bar chart.

ggplot(color_summary, aes(x = color, y = mean_freq, fill = color)) +
  geom_col() +
  scale_fill_manual(values = color_summary$color) +
  theme(axis.text.x = element_blank()) +
  labs(x = NULL, y = "Mean Frequency", title = "Dominant Colors Across Images")


