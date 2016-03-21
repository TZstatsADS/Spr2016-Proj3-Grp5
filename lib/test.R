library(e1071)

test <- function(fit_train, dat_test) {
  
  ### Fit the linear SVM classification model onto testing data
  
  ### fit_train: a list of 2; first element is the fitted BASELINE model, second is the fitted FINAL model
  ### dat_test: constructed features for testing data
  ### Output: a list of 2; first element is a vector of class label predictions for the BASELINE model,
  ###                      second element is a vector of class label predictions for the FINAL model
  
  pred_baseline <- predict(fit_train[[1]], newdata = dat_test)
  if (!is.na(c[[2]])) {
    pred_final <- predict(fit_train[[2]], newdata = dat_test)
  } else {pred_final <- NA}
  
  output <- list(pred_baseline, pred_final)
  names(output) <- c("baseline", "adv")
  return(output)
}