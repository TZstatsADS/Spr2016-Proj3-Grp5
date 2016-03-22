#############################
### Main execution script ###
#############################

# Working directory should be cycle3cvd-team5

# NOTE: Place your images in a folder outside the repository (too large for Github.)

source("./lib/feature.R") # read in feature.R which contains the functions 
                          # feature(), feature_mat() and label_vec()

img_dir <- "../data/images.tar/images" # this should be consistent on everyone's computers
data_dir <- "./output/features" # this should be consistent on everyone's computers

# this will write a new feature file for each image; only run if new features are going to be created
system.time(feature(img_dir, data_dir))

# create matrix with features for all observations
catdog <- feature_mat(data_dir)
saveRDS(catdog, "./output/feature.rds")
# create vector of class labels
y_cat <- label_vec(data_dir)
saveRDS(y_cat, "./output/label.rds")

# split into training and testing data
source("./lib/split.R")
dat_train <- catdog[ttsplit, ]
dat_test <- catdog[!1:nrow(catdog) %in% ttsplit, ]
saveRDS(dat_train, file = "./output/train.rds")
saveRDS(dat_test, file = "./output/test.rds")
lab_train <- y_cat[ttsplit]
lab_train <- as.factor(lab_train)
lab_test <- y_cat[!1:nrow(catdog) %in% ttsplit]
lab_test <- as.factor(lab_test)
saveRDS(lab_train, file = "./output/train_lab.rds")
saveRDS(lab_test, file = "./output/test_lab.rds")

# load training and testing data (skip all the above lines)
dat_train <- readRDS("./output/train.rds")
dat_test <- readRDS("./output/test.rds")
lab_train <- readRDS("./output/train_lab.rds")
lab_test <- readRDS("./output/test_lab.rds")

# fit baseline and final models to training data 
source("./lib/train.R")
system.time(fit_train <- train(dat_train, lab_train))
saveRDS(fit_train, file = "./output/fit_train.rds")

# predict baseline performance on testing data
source("./lib/test.R")
pred_test <- test(fit_train, dat_test)
saveRDS(pred_test, file = "./output/pred_test.rds")
