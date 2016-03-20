library(e1071)

train_baseline <- function(dat_train, lab_train, cost = 100) {
  
  ### Train a linear support vector machine using processed features from training images
  
  ### dat_train: training features
  ### lab_train: training labels
  ### cost: cost of constraints violation parameter for linear SVM
  ### Output: training model specification
  
  ##### CURRENT STATUS (2016/03/05 23:00): 
  ##### No cross-validation has been conducted yet
  
  fit1 <- svm(x = dat_train, y = lab_train, 
              type = "C-classification", kernel = "linear", 
              cost = 100)
  return(fit1)
  
}


