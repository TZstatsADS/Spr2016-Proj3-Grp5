#############################
### Main execution script ###
#############################

# Working directory should be cycle3cvd-team5

# NOTE: Place your images in a folder outside the repository (too large for Github.)

source("./lib/feature.R") # read in feature.R which contains the functions 
                          # feature(), feature_mat() and label_vec()

img_dir <- "../data/images.tar/images" # this should be consistent on everyone's computers
data_dir <- "./output/features" # this should be consisted on everyone's computers

# this will write a new feature file for each image; only run if new features are going to be created
feature(img_dir, data_dir)

# create matrix with features for all observations
catdog <- feature_mat(data_dir)
# create vector of class labels
y_cat <- label_vec(data_dir)

# split into training and testing data
source("./lib/split.R")
dat_train <- catdog[ttsplit, ]
dat_test <- catdog[!1:nrow(catdog) %in% ttsplit, ]
saveRDS(dat_train, file = "./output/train.rds")
saveRDS(dat_test, file = "./output/test.rds")

# fit baseline on training data 
source("./lib/train.R")
fit_train_baseline <- train_baseline(dat_train, cost = 100)
saveRDS(fit_train_baseline, file = "./output/fit_train_baseline.rds")

# predict baseline performance on testing data
source("./lib/test.R")
pred_test_baseline <- test_baseline(fit_train_baseline, dat_test)
saveRDS(pred_test_baseline, file = "./output/pred_test_baseline.rds")
