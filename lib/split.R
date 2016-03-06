set.seed(88882)

data_dir <- "./output/features"
feature_names <- list.files(data_dir, pattern = "rds")

ttsplit <- sample.int(length(feature_names), 5000)
