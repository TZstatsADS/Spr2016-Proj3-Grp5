source("./lib/feature_sc.R") # read in feature.R which contains the functions 
# feature(), feature_mat() and label_vec()

img_dir <- "./images"
data_dir <- "./output" 

# this will write a new feature file for each image; only run if new features are going to be created
feature(img_dir, data_dir)

catdog <- feature_mat(data_dir)
saveRDS(catdog, "./output/feature.rds")
# create vector of class labels
y_cat <- label_vec(data_dir)
saveRDS(y_cat, "./output/label.rds")


source("./lib/split.R")
dat_train <- catdog[ttsplit, ]
dat_test <- catdog[!1:nrow(catdog) %in% ttsplit, ]
saveRDS(dat_train, file = "./output/train.rds")
saveRDS(dat_test, file = "./output/test.rds")
lab_train <- y_cat[ttsplit]
lab_test <- y_cat[!1:nrow(catdog) %in% ttsplit]
saveRDS(lab_train, file = "./output/train_lab.rds")
saveRDS(lab_test, file = "./output/test_lab.rds")



# fit baseline on training data 
source("./lib/train_sc.R")
fit_train_baseline <- train_baseline(dat_train, lab_train, cost = 100)
saveRDS(fit_train_baseline, file = "./output/fit_train_baseline.rds")

# predict baseline performance on testing data
source("./lib/test_sc.R")
pred_test_baseline <- test_baseline(fit_train_baseline, dat_test, lab_test)
saveRDS(pred_test_baseline, file = "./output/pred_test_baseline.rds")









