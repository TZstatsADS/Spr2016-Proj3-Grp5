library(EBImage)

feature <- function(img_dir, data_dir){
  file_names <- list.files(img_dir, pattern = "jpg")
  file_names <- sort(file_names)
  file_paths <- Sys.glob(paste(img_dir, "/*.jpg", sep = ""))
  file_paths <- sort(file_paths)
  
  construct_rgb_feature <- function(X){
    freq_rgb <- as.data.frame(table(factor(findInterval(X[,,1], rBin), levels=1:nR), 
                                    factor(findInterval(X[,,2], gBin), levels=1:nG), 
                                    factor(findInterval(X[,,3], bBin), levels=1:nB)))
    rgb_feature <- as.numeric(freq_rgb$Freq)/(ncol(X)*nrow(X))
    return(rgb_feature)
  }
  
  for (k in 1:length(file_paths)) {
    tryCatch({
      img <- readImage(file_paths[k])
      img <- resize(img, 256, 256)
      mat <- imageData(img)
      
      nR <- 10
      nG <- 8
      nB <- 10
      rBin <- seq(0, 1, length.out=nR)
      gBin <- seq(0, 1, length.out=nG)
      bBin <- seq(0, 1, length.out=nB)
      
      N <- 3 # number of bins in x-axis
      M <- 5 # number of bins in y-axis
      p_x <- p_y <- 250
      img_s <- resize(img, p_x, p_y)
      xbin <- floor(seq(0, p_x, length.out= N+1))
      ybin <- seq(0, p_y, length.out=M+1)
      
      ff <- rep(NA, N*M*nR*nG*nB)
      for(i in 1:N){
        for(j in 1:M){
          tmp <- img_s[(xbin[i]+1):xbin[i+1], (ybin[j]+1):ybin[j+1], ]
          ff[((M*(i-1)+j-1)*nR*nG*nB+1):((M*(i-1)+j)*nR*nG*nB)] <- construct_rgb_feature(tmp) 
        }
      }
      saveRDS(ff,file = paste(data_dir, "/", unlist(strsplit(file_names[k], "[.]"))[1], ".rds", sep = ""))
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
  
  