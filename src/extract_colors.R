extract_colors <- function(
    url_img = "http://developer.r-project.org/Logo/Rlogo-1.png", num_col = 5, rsize = 100) {
  
  ## Read Image
  if (class(url_img) != "Image") {
    img <- readImage(url_img) # local file or url
  } else {
    img <- url_img # is already a loaded "Image"
  }
  
  ## Resize Image (make it smaller so the remaining tasks run faster)  
  if (max(dim(img)[1:2]) > rsize) {
    if (dim(img)[1] > dim(img)[2]) {
      img <- resize(img, w = rsize)
    } else {
      img <- resize(img, h = rsize)
    }
  }
  
  ## Melt
  img_melt <- reshape2::melt(img)
  
  ## Reshape
  img_rgb <- reshape(img_melt, timevar = "Var3", idvar = c("Var1", "Var2"), direction = "wide")
  img_rgb$Var1 <- - img_rgb$Var1
  
  ## Detect dominant colours with kmeans (multiple starts)
  col_dom <- kmeans(img_rgb[, 3:5], centers = num_col, nstart = 3, iter.max = 100)
  
  ## Return k-means centers as RGB colours
  cus_pal <- sort(rgb(col_dom$centers))
  return(as.character(cus_pal))
  
}