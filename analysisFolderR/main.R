library(tidyverse)
library(lsmeans)
getwd()

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
eye_files <- dir(path = "../output/eyeData/fixationData", full.names = TRUE)

all_bx_files1 <- read(bx_files)
all_fixation_files <- read(eye_files, get_subj_info = TRUE)

all_fixation_files$run_num <- gsub("[^0-9]", "", all_fixation_files$run_num)
all_fixation_files$sub_num <- gsub("[^0-9]", "", all_fixation_files$sub_num)

all_bx_files_no_practice <- subset(all_bx_files, all_bx_files$run_num!=1)
# Behavioral Data Cleanup
all_bx_files <- all_bx_files1 %>%
  filter(accuracy == 1,
         run_num != 1) %>% 
  mutate(sub_num = as.factor(sub_num),
         run_num = as.factor(run_num),
         rt = ifelse(rt <= 200, NA, rt),
         rt = ifelse(rt > mean(rt, na.rm=TRUE)+3*sd(rt, na.rm = TRUE), NA, rt),
         rt = ifelse(rt < mean(rt, na.rm=TRUE)-3*sd(rt, na.rm = TRUE), NA, rt),
         rt = ifelse(accuracy==2, NA, rt))

all_bx_files <- all_bx_files1 %>%
  filter(accuracy == 1,
         run_num != 1) %>% 
  mutate(sub_num = as.factor(sub_num),
         run_num = as.factor(run_num)) %>% 
  group_by(sub_num, 
           trialTypeValid0Invalid1, 
           trialTypeExtraTarget1NoExtraTarget0) %>% 
  mutate(rt = ifelse(rt <= 200, NA, rt),
         rt = ifelse(rt > mean(rt, na.rm=TRUE)+3*sd(rt, na.rm = TRUE), NA, rt),
         rt = ifelse(rt < mean(rt, na.rm=TRUE)-3*sd(rt, na.rm = TRUE), NA, rt)) %>% 
  ungroup()


#add section later that removes participants without full runs

bx_rt_summary <- all_bx_files %>% 
  filter(accuracy == 1) %>% 
  group_by(sub_num, trialTypeValid0Invalid1, trialTypeExtraTarget1NoExtraTarget0) %>% 
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>% 
  mutate(Validity = as.factor(trialTypeValid0Invalid1),
         additionalTargetDistractor = as.factor(trialTypeExtraTarget1NoExtraTarget0))

bx_rt_summary$Validity <- recode_factor(bx_rt_summary$Validity, 
                                        '0' = "Valid", 
                                        '1' = "Invalid")

bx_rt_summary$additionalTargetDistractor <- recode_factor(bx_rt_summary$additionalTargetDistractor, 
                                        '0' = "No distractor present", 
                                        '1' = "Distractor Present")

bx_rt_summary %>% 
  ggplot(aes(y=meanRT, x=Validity, color = additionalTargetDistractor))+
  geom_violin()

aov_RT <- aov(meanRT ~ Validity*additionalTargetDistractor + Error(sub_num/(Validity*additionalTargetDistractor)), 
              data = bx_rt_summary)

summary(aov_RT)

# Perform pairwise tests
lsd_results <- lsmeans(aov_RT, pairwise ~ Validity * additionalTargetDistractor, adjust = "none")
summary(lsd_results)

hsd_results <- TukeyHSD(aov_RT)
summary(hsd_results)

#fixation analysis
all_fixation_files <- all_fixation_files %>%
  mutate(sub_num = as.numeric(sub_num), #this line is only to remove the leading 0 which caused an issue with a left join
         sub_num = as.factor(sub_num),
         run_num = as.numeric(run_num), #this line is only to remove the leading 0 which caused an issue with a left join
         run_num = as.factor(run_num),
         correctTarget = ifelse(previousFixationRect == targetPositionInds, 1, 0),
         trial_num = trialNum)

#join the fixation data with the bx_data. This gives us access to accuracy because later analysis will probably be done on
#only accurate trials
joined_fixation_data <- left_join(all_fixation_files, 
                                  all_bx_files, 
                                  by=c('sub_num'='sub_num', 
                                       'trial_num'='trial_num', 
                                       'run_num'='run_num'))

#add accuracy from the joined df to the main df
all_fixation_files$accuracy <- joined_fixation_data$accuracy

#added fixation count (this is a count of how many fixations were made each trial and what order)
all_fixation_files <- all_fixation_files %>%
  group_by(sub_num, run_num, trial_num) %>%
  mutate(
    fixation_count = row_number(),
    first_correct_fixation = ifelse(correctTarget == 1 & cumsum(correctTarget == 1) == 1, 1, 0),
    first_fixation_number = ifelse(first_correct_fixation == 1, fixation_count, NA))

all_first_fixation <- all_fixation_files %>% 
  filter(fixation_count == 1,
         run_num != 1,
         accuracy == 1)

all_first_fixation_summary <- all_first_fixation %>% 
  group_by(sub_num, thisTrialExtraTarget, thisTrialIncorrectTargetLocation) %>% 
  summarise(percent_first_fixation = mean(correctTarget, na.rm = TRUE)) %>% 
  mutate(Validity = as.factor(thisTrialIncorrectTargetLocation),
         additionalTargetDistractor = as.factor(thisTrialExtraTarget))
  
all_first_fixation_summary$Validity <- recode_factor(all_first_fixation_summary$Validity, 
                                        '0' = "Valid", '1' = "Invalid")

all_first_fixation_summary$additionalTargetDistractor <- recode_factor(all_first_fixation_summary$additionalTargetDistractor, 
                                                          '0' = "No distractor present", '1' = "Distractor Present")

aov_first_fixation <- aov(percent_first_fixation ~ Validity*additionalTargetDistractor + Error(sub_num/(Validity*additionalTargetDistractor)), 
                          data = all_first_fixation_summary)
summary(aov_first_fixation)

all_first_fixation_summary %>% 
  ggplot(aes(y=percent_first_fixation, 
             x=Validity, 
             fill = additionalTargetDistractor))+
  geom_violin()+
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 18, 
               size = 3,
               position = position_dodge(width = .9))

