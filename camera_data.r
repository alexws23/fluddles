library(magick)
library(tesseract)
library(exif)
library(tidyverse)

img <- image_read("imgs/Camera1_Unprocessed/04150004.JPG")
img <- image_read("imgs/Camera1_Unprocessed/04150234.JPG")
img <- image_read("imgs/Camera1_Unprocessed/04150001.JPG")


# crop where the "T/M" is (you'll need to tune this)
crop <- image_crop(img, "35x47+342+2087")
crop <- image_crop(img, "75x94+670+4104")

image_write(crop, path = "imgs/test.png")

text <- ocr(crop)

print(text)

## Need to:
# Annotate this code
# Create a for-loop to quickly apply to all images. 
# Find a way to add the code T/M to the file name or sort files into separate folders. Some way to easily sort between the two types.

#LRFP2
images <- list.files(path = "R:/Fluddle/Data/Camera Trap Data/Raw Data/LRFP2/20260422-20260506/")

for (i in images) {
  file <- i
  
  img <- image_read(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/LRFP2/20260422-20260506/", file, sep = ""))
  
  exif <- read_exif(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/LRFP2/20260422-20260506/", i, sep = "")) %>% 
    select(c("image_width"))
  timestamp <- read_exif(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/LRFP2/20260422-20260506/", i, sep = "")) %>% 
    select(c("timestamp"))
  timestamp$timestamp <- gsub(" ", "", timestamp$timestamp)
  timestamp$timestamp <- gsub(":", "", timestamp$timestamp)
  
  if (exif$image_width == 7552) {
    crop <- image_crop(img, "75x94+667+4103")
  }else{crop <- image_crop(img, "35x47+342+2087")}
  
  text <- ocr(crop)
  
  text <- gsub("\n", "", text)
  
  file <- gsub(".JPG", "", file)
  
  file <- paste(file, text, sep="_")
  
  if (endsWith(file, "M")) {
    image_write(img, path = paste("R:/Fluddle/Data/Camera Trap Data/Processed Data/LRFP2/Motion/", file,timestamp$timestamp, ".JPG", sep = ""))
  } else {image_write(img, path = paste("R:/Fluddle/Data/Camera Trap Data/Processed Data/LRFP2/Timelapse/", file,timestamp$timestamp, ".JPG", sep = ""))}
}

#BPFP1
images <- list.files(path = "R:/Fluddle/Data/Camera Trap Data/Raw Data/BPFP1/20260422-20260506/")

for (i in images) {
  file <- i
  
  img <- image_read(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/BPFP1/20260422-20260506/", file, sep = ""))
  
  exif <- read_exif(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/BPFP1/20260422-20260506/", i, sep = "")) %>% 
    select(c("image_width"))
  timestamp <- read_exif(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/BPFP1/20260422-20260506/", i, sep = "")) %>% 
    select(c("timestamp"))
  
  if (exif$image_width == 7552) {
    crop <- image_crop(img, "75x94+667+4103")
  }else{crop <- image_crop(img, "35x47+342+2087")}
  
  text <- ocr(crop)
  
  text <- gsub("\n", "", text)
  
  file <- gsub(".JPG", "", file)
  
  file <- paste(file, text, sep="_")
  
  if (endsWith(file, "M")) {
    image_write(img, path = paste("R:/Fluddle/Data/Camera Trap Data/Processed Data/BPFP1/Motion/", file, timestamp$timestamp, ".JPG", sep = ""))
  } else {image_write(img, path = paste("R:/Fluddle/Data/Camera Trap Data/Processed Data/BPFP1/Timelapse/", file, timestamp$timestamp, ".JPG", sep = ""))}
}

#BPFP2
images <- list.files(path = "R:/Fluddle/Data/Camera Trap Data/Raw Data/BPFP2/20260422-20260506/")

for (i in images) {
  file <- i
  
  img <- image_read(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/BPFP2/20260422-20260506/", file, sep = ""))
  
  exif <- read_exif(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/BPFP2/20260422-20260506/", i, sep = "")) %>% 
    select(c("image_width"))
  timestamp <- read_exif(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/BPFP2/20260422-20260506/", i, sep = "")) %>% 
    select(c("timestamp"))
  timestamp$timestamp <- gsub(" ", "", timestamp$timestamp)
  timestamp$timestamp <- gsub(":", "", timestamp$timestamp)
  
  if (exif$image_width == 7552) {
    crop <- image_crop(img, "75x94+667+4103")
  }else{crop <- image_crop(img, "35x47+342+2087")}
  
  text <- ocr(crop)
  
  text <- gsub("\n", "", text)
  
  file <- gsub(".JPG", "", file)
  
  file <- paste(file, text, sep="_")
  
  if (endsWith(file, "M")) {
    image_write(img, path = paste("R:/Fluddle/Data/Camera Trap Data/Processed Data/BPFP2/Motion/", file,timestamp$timestamp, ".JPG", sep = ""))
  } else {image_write(img, path = paste("R:/Fluddle/Data/Camera Trap Data/Processed Data/BPFP2/Timelapse/", file,timestamp$timestamp, ".JPG", sep = ""))}
}

#LRFP1
images <- list.files(path = "R:/Fluddle/Data/Camera Trap Data/Raw Data/LRFP1/20260422-20260506/")

for (i in images) {
  file <- i
  
  img <- image_read(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/LRFP1/20260422-20260506/", file, sep = ""))
  
  exif <- read_exif(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/LRFP1/20260422-20260506/", i, sep = "")) %>% 
    select(c("image_width"))
  timestamp <- read_exif(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/LRFP1/20260422-20260506/", i, sep = "")) %>% 
    select(c("timestamp"))
  timestamp$timestamp <- gsub(" ", "", timestamp$timestamp)
  timestamp$timestamp <- gsub(":", "", timestamp$timestamp)
  
  if (exif$image_width == 7552) {
    crop <- image_crop(img, "75x94+667+4103")
  }else{crop <- image_crop(img, "35x47+342+2087")}
  
  text <- ocr(crop)
  
  text <- gsub("\n", "", text)
  
  file <- gsub(".JPG", "", file)
  
  file <- paste(file, text, sep="_")
  
  if (endsWith(file, c("M"))) {
    image_write(img, path = paste("R:/Fluddle/Data/Camera Trap Data/Processed Data/LRFP1/Motion/", file,timestamp$timestamp, ".JPG", sep = ""))
  } else {image_write(img, path = paste("R:/Fluddle/Data/Camera Trap Data/Processed Data/LRFP1/Timelapse/", file,timestamp$timestamp, ".JPG", sep = ""))}
}

#DYFP1
images <- list.files(path = "R:/Fluddle/Data/Camera Trap Data/Raw Data/DYFP1/20260422-20260506/")

for (i in images) {
  file <- i
  
  img <- image_read(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/DYFP1/20260422-20260506/", file, sep = ""))
  
  exif <- read_exif(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/DYFP1/20260422-20260506/", i, sep = "")) %>% 
    select(c("image_width"))
  timestamp <- read_exif(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/DYFP1/20260422-20260506/", i, sep = "")) %>% 
    select(c("timestamp"))
  timestamp$timestamp <- gsub(" ", "", timestamp$timestamp)
  timestamp$timestamp <- gsub(":", "", timestamp$timestamp)
  
  if (exif$image_width == 7552) {
    crop <- image_crop(img, "75x94+667+4103")
  }else{crop <- image_crop(img, "35x47+342+2087")}
  
  text <- ocr(crop)
  
  text <- gsub("\n", "", text)
  
  file <- gsub(".JPG", "", file)
  
  file <- paste(file, text, sep="_")
  
  if (endsWith(file, "M")) {
    image_write(img, path = paste("R:/Fluddle/Data/Camera Trap Data/Processed Data/DYFP1/Motion/", file,timestamp$timestamp, ".JPG", sep = ""))
  } else {image_write(img, path = paste("R:/Fluddle/Data/Camera Trap Data/Processed Data/DYFP1/Timelapse/", file,timestamp$timestamp, ".JPG", sep = ""))}
}

#DYFP2
images <- list.files(path = "R:/Fluddle/Data/Camera Trap Data/Raw Data/DYFP2/20260422-20260506/")

for (i in images) {
  file <- i
  
  img <- image_read(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/DYFP2/20260422-20260506/", file, sep = ""))
  
  exif <- read_exif(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/DYFP2/20260422-20260506/", i, sep = "")) %>% 
    select(c("image_width"))
  timestamp <- read_exif(paste("R:/Fluddle/Data/Camera Trap Data/Raw Data/DYFP2/20260422-20260506/", i, sep = "")) %>% 
    select(c("timestamp"))
  timestamp$timestamp <- gsub(" ", "", timestamp$timestamp)
  timestamp$timestamp <- gsub(":", "", timestamp$timestamp)
  
  if (exif$image_width == 7552) {
    crop <- image_crop(img, "75x94+667+4103")
  }else{crop <- image_crop(img, "35x47+342+2087")}
  
  text <- ocr(crop)
  
  text <- gsub("\n", "", text)
  
  file <- gsub(".JPG", "", file)
  
  file <- paste(file, text, sep="_")
  
  if (endsWith(file, "M")) {
    image_write(img, path = paste("R:/Fluddle/Data/Camera Trap Data/Processed Data/DYFP2/Motion/", file,timestamp$timestamp, ".JPG", sep = ""))
  } else {image_write(img, path = paste("R:/Fluddle/Data/Camera Trap Data/Processed Data/DYFP2/Timelapse/", file,timestamp$timestamp, ".JPG", sep = ""))}
}
