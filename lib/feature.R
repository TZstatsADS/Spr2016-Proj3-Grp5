
#############################################################
### Construct features out of images for training/testing ###
#############################################################

library(EBImage)

feature <- function(img_dir, data_dir) {
  
  ### Constructs features out of images for training/testing
  
  ### img_dir: class "character", path to directory of images to be processed
  ### data_dir: class "character", path to directory to place feature data files in
  ### Output: .rds files, one for each image, containing the features for that image
  
  ##### CURRENT STATUS (2016/03/05 19:00): 
  ##### This function constructs only color histogram features.
  
  file_names <- list.files(img_dir, pattern = "jpg")
  file_names <- sort(file_names)
  file_paths <- Sys.glob(paste(img_dir, "/*.jpg", sep = ""))
  file_paths <- sort(file_paths)
  
  # Construct color (RGB) histogram features
  # Note: Some images may be invalid; features will not be constructed for those images
  for (i in 1:length(file_paths)) {
    tryCatch({
      img <- readImage(file_paths[i])
      img <- resize(img, 256, 256) # resize image for faster feature construction
      mat <- imageData(img)
      # Tuning parameters: number of red bins nR, number of green bins nG, number of blue bins nB
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
      saveRDS(rgb_feature,
              file = paste(data_dir, "/", unlist(strsplit(file_names[i], "[.]"))[1], ".rds", sep = ""))
    }, 
    error = function(c) "invalid or corrupt JPEG file, or no RGB values present")
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