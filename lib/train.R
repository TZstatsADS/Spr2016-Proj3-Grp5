# INSERT ALL PACKAGES NEEDED FOR MODELS HERE, INCLUDING INSTALLATION LINES

install.packages("e1071")
install.packages("ada")
library(e1071)
library(ada)

train <- function(dat_train, lab_train) {
  
  ### Train a linear support vector machine with color histogram features AND
  ### train the final model (NOT YET IMPLEMENTED) with additional features
  
  ### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ### !!!THIS ASSUMES THAT THE COLOR HISTOGRAM FEATURES WILL BE!!! 
  ### !!!     THE FIRST 125 COLUMNS OF THE FEATURE MATRIX      !!!
  ### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  ### dat_train: constructed features for training data
  ### lab_train: class labels for training data
  ### Output: a list of 2; first element is the fitted BASELINE model, second is the fitted FINAL model
  
  ##### CURRENT STATUS (2016/03/05 23:00): 
  ##### Final model has not yet been implemented
  
  baseline <- svm(x = dat_train[, 1:125], y = lab_train, 
              type = "C-classification", kernel = "linear", 
              cost = 1)
  final <- ada(x = dat_train[, 126:1290], y = lab_train,
               type = "discrete", nu = 0.1)
  return(list(baseline, final))
  
}


