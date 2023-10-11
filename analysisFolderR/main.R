#function that passes files from 
read <- function(data_folder){
  full <- 0
  for (file in data_folder) {
    #read in data
    individual <- read.csv(file = file)
    full <- rbind(full, individual)
  }
  fixed <- full[-1,]
  return(fixed)
}

library(tidyverse)
getwd()

#sceneShapeSearchSub055Run03 <- read.csv("../output/bxData/sceneShapeSearchSub003Run02.csv")

bx_files <- dir(path = "../output/bxData/", full.names = TRUE)
eye_files <- dir(path = "../output/eyeData/fixationData", full.names = TRUE)



all_bx_files <- read(bx_files)
all_fixation_files <- read(eye_files)

