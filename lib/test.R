library(e1071)

test_baseline <- function(fit_baseline, dat_test) {
  
  ### Fit the linear SVM classification model onto testing data
  
  ### fit_baseline: model object
  ### dat_test: testing data
  ### Output: confusion matrix of predicted labels vs. actual labels
  
  pred <- predict(fit_baseline, newdata = dat_test)
  return(table(pred, dat_test$y_cat))
}