library(tidyverse)
getwd()

sceneShapeSearchSub055Run03 <- read.csv("../output/bxData/sceneShapeSearchSub055Run03.csv")

#function that passes files from 
read <- function(data){
  full <- 0
  for (file in data) {
    #get condition of either organized or cluttered
    condition = str_sub(file,-8, -8)
    #get subject number
    subn = str_sub(file,-6, -5)
    #read in data
    individual <- read.table(file = file)
    #create new variables
    individual$condition <- condition
    individual$subn <- subn
    full <- rbind(full, individual)
  }
  fixed <- full[-c(1),]
  return(fixed)
}
