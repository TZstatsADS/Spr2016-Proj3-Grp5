img_dir <- "../data/images.tar/images" # this should be consistent on everyone's computers
data_dir <- "./output/features" # this should be consisted on everyone's computers

file_names <- list.files(img_dir, pattern = "jpg")
file_names <- sort(file_names)
file_paths <- Sys.glob(paste(img_dir, "/*.jpg", sep = ""))
file_paths <- sort(file_paths)

img <- readImage(file_paths[2242])
img <- resize(img, 256, 256)
img <- channel(img, mode = "gray")
display(img)
img <- thresh(img)
img <- dilate(img)
img <- fillHull(img)
oc <- ocontour(bwlabel(img))
plot(oc[[1]], type = "l")
points(oc[[1]], col = 4)

