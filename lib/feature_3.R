
#############################################################
### Construct features out of images for training/testing ###
#############################################################

# CONSTRUCTS HARMONIC COEFFICIENTS FROM ELLIPTICAL FOURIER OUTLINE ANALYSIS

source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(EBImage)
install.packages("devtools")
devtools::install_github("vbonhomme/Momocs")
library(Momocs)

feature <- function(img_dir, data_dir) {
  
  ### Constructs features out of images for training/testing
  
  ### img_dir: class "character", path to directory of images to be processed
  ### data_dir: class "character", path to directory to place feature data files in
  ### Output: .rds files, one for each image, containing the features for that image
  
  ##### CURRENT STATUS (2016/03/18 19:30): 
  ##### This function constructs only harmonic coefficient features.
  ##### WARNING: This function also writes a new processed image file per image.
  #####          This will thus double the number of images in your image directory.
  ##### Maybe a separate directory for the processed files should be created.
  ##### Running time on Arnold's computer:
  ##### user: 2655.92 system: 43.69 elapsed 2824.62 (approx 47 minutes)
  
  file_names <- list.files(img_dir, pattern = "[[:digit:]].jpg") # THIS IS NOT A GOOD SOLUTION
  file_names <- sort(file_names)
  file_paths <- rep(NA_character_, length(file_names))
  for (i in 1:length(file_names)) {
    file_paths[i] <- paste(img_dir, file_names[i], sep = "/")
  }
  file_paths <- sort(file_paths)
  
  # Construct harmonic coefficient features from image outline
  # Note: Some images may be invalid; features will not be constructed for those images
  # Note: Some images may result in outlines with too little detail; features will not be
  #       constructed for those images
  for (i in 1:length(file_paths)) {
    tryCatch({
      img <- readImage(file_paths[i])
      img_bin <- channel(img, mode = "gray") # convert image to greyscale
      img_bin <- gblur(img_bin, sigma = 5) # smooth image with a low-pass filter
      threshold <- otsu(img_bin)
      img_bin <- img_bin > threshold # create binary black/white image using Otsu threshold
      writeImage(img_bin, paste(img_dir, "/", unlist(strsplit(file_names[i], split = "[.]"))[1], 
                                "_bin.jpg", sep = ""), type = "jpeg")
      momocs_out <- import_jpg1(jpg.path = paste(img_dir, "/", 
                                                 unlist(strsplit(file_names[i], split = "[.]"))[1], 
                                                 "_bin.jpg", sep = ""),
                                threshold = threshold)
      momocs_out <- Out(list(momocs_out))
      momocs_out <- coo_smooth(momocs_out, 5) %>% coo_scale() %>% coo_center()
      momocs_ef <- efourier(momocs_out, nb.h = 10)
      momocs_coeff <- momocs_ef$coe[1, ]
      if(length(momocs_coeff) != 40) {
        next
      }
      
      mat <- imageData(img)
      nR <- 5
      nG <- 5
      nB <- 5
      rBin <- seq(0, 1, length.out=nR)
      gBin <- seq(0, 1, length.out=nG)
      bBin <- seq(0, 1, length.out=nB)
      freq_rgb <- as.data.frame(table(factor(findInterval(mat[,,1], rBin), levels=1:nR), 
                                      factor(findInterval(mat[,,2], gBin), levels=1:nG), 
                                      factor(findInterval(mat[,,3], bBin), levels=1:nB)))
      rgb_feature <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
      
      final <- c(rgb_feature, momocs_coeff)
      saveRDS(final,
              file = paste(data_dir, "/", unlist(strsplit(file_names[i], "[.]"))[1], ".rds", sep = ""))

    }, 
    error = function(c) "invalid or corrupt JPEG file")
  }
}

feature_mat <- function(data_dir) {
  
  ### Create a matrix with features for all observations
  
  ### data_dir: class "character", path to directory where feature data files are placed
  ### Output: matrix with features for all observations
  
  feature_file_names <- Sys.glob(paste(data_dir, "/*.rds", sep = ""))
  feature_file_names <- sort(feature_file_names)
  
  # create feature matrix
  catdog <- do.call('rbind', lapply(feature_file_names, readRDS))
  return(catdog)
}

label_vec <- function(data_dir) {
  
  ### Create a vector with labels for all observations
  
  ### data_dir: class "character", path to directory where feature data files are placed
  ### Output: vector with labels for all observations
  
  feature_names <- list.files(data_dir, pattern = "rds")
  feature_names <- sort(feature_names)
  
  # extract vector of labels (cat = 1, dog = 0)
  breed_name <- rep(NA, length(feature_names))
  for(i in 1:length(feature_names)){
    tt <- unlist(strsplit(feature_names[i], "_"))
    tt <- tt[-length(tt)]
    breed_name[i] = paste(tt, collapse="_", sep="")
  }
  cat_breed <- c("Abyssinian", "Bengal", "Birman", "Bombay", "British_Shorthair", "Egyptian_Mau",
                 "Maine_Coon", "Persian", "Ragdoll", "Russian_Blue", "Siamese", "Sphynx")
  iscat <- breed_name %in% cat_breed
  y_cat <- as.numeric(iscat)
  return(y_cat)
  
}