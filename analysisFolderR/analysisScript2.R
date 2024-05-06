library(tidyverse)

sceneShapeSearchSub001Run02 <- read.csv("~/MATLAB/repos/shapeSearch/output/bxData/sceneShapeSearchSub001Run02.csv")

read <- function(data_folder, get_subj_info = FALSE){
  full <- 0
  for (file in data_folder) {
    #read in data
    individual <- read.csv(file = file)
    if (get_subj_info == TRUE){
      run_num = str_sub(file,-6, -4)
      sub_num = str_sub(file,-8, -6)
      
      individual$run_num <- run_num
      individual$sub_num <- sub_num
    }
    full <- rbind(full, individual)
  }
  fixed <- full[-1,]
  return(fixed)
}

bx_files <- dir(path = "../output/bxData/", full.names = TRUE)
all_imported_bx_files <- read(bx_files)
all_fixation_files$run_num <- gsub("[^0-9]", "", all_fixation_files$run_num)

all_bx_files <- all_imported_bx_files %>%
  filter(accuracy == 1,
         run_num != 1) %>%
  mutate(sub_num = as.factor(sub_num),
         rt = ifelse(rt <= 200, NA, rt),
         rt = ifelse(rt > mean(rt, na.rm=TRUE)+2*sd(rt, na.rm = TRUE), NA, rt),
         rt = ifelse(rt < mean(rt, na.rm=TRUE)-2*sd(rt, na.rm = TRUE), NA, rt))

bx_rt_summary <- all_bx_files  %>% 
  group_by(sub_num, trialTypeValid0Invalid1, trialTypeExtraTarget1NoExtraTarget0) %>% 
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>% 
  mutate(Validity = factor(trialTypeValid0Invalid1, levels = c(0, 1), labels = c("Valid", "Invalid")),
                           additionalTargetDistractor  = factor(trialTypeExtraTarget1NoExtraTarget0, levels = c(0, 1), labels = c("No distractor present", "Distractor present")))

aov_RT <- aov(meanRT ~ Validity*additionalTargetDistractor + Error(sub_num/(Validity*additionalTargetDistractor)), 
              data = bx_rt_summary)

summary(aov_RT)

write.csv(bx_rt_summary, "summary.csv")

