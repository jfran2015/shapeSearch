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

all_imported_bx_files <- read(bx_files)
all_fixation_files <- read(eye_files, get_subj_info = TRUE)

all_fixation_files$run_num <- gsub("[^0-9]", "", all_fixation_files$run_num)
all_fixation_files$sub_num <- gsub("[^0-9]", "", all_fixation_files$sub_num)

#all_bx_files_no_practice <- subset(all_imported_bx_files, all_bx_files$run_num!=1)
# Behavioral Data Cleanup

sub_run_counts <- all_imported_bx_files %>%
  group_by(sub_num) %>%
  summarize(num_runs = n_distinct(run_num))

all_imported_bx_files <- all_imported_bx_files %>% 
  mutate(sub_num = as.factor(sub_num),
         run_num = as.factor(run_num))

all_bx_files <- all_imported_bx_files %>%
  filter(accuracy == 1,
         run_num != 1,
         sub_num != 20) %>%
  group_by(sub_num, 
           trialTypeValid0Invalid1, 
           trialTypeExtraTarget1NoExtraTarget0) %>% 
  mutate(rt = ifelse(rt <= 200, NA, rt),
         rt = ifelse(rt > mean(rt, na.rm=TRUE)+3*sd(rt, na.rm = TRUE), NA, rt),
         rt = ifelse(rt < mean(rt, na.rm=TRUE)-3*sd(rt, na.rm = TRUE), NA, rt)) %>% 
  ungroup()


#add section later that removes participants without full runs

bx_rt_summary <- all_bx_files  %>% 
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
  ggplot(aes(y=meanRT, x=Validity, fill = additionalTargetDistractor))+
  geom_boxplot()+
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 18, 
               size = 3,
               position = position_dodge(width = .9))

aov_RT <- aov(meanRT ~ Validity*additionalTargetDistractor + Error(sub_num/(Validity*additionalTargetDistractor)), 
              data = bx_rt_summary)

summary(aov_RT)

#accuracy analysis
all_bx_files_accuracy <- all_imported_bx_files %>%
  filter(run_num != 1,
         accuracy != 2,
         sub_num != 20) %>% 
  mutate(sub_num = as.factor(sub_num),
         run_num = as.factor(run_num)) %>% 
  group_by(sub_num, 
           trialTypeValid0Invalid1, 
           trialTypeExtraTarget1NoExtraTarget0) %>% 
  mutate(accuracy = ifelse(rt <= 200, NA, accuracy),
         accuracy = ifelse(rt > mean(rt, na.rm=TRUE)+3*sd(rt, na.rm = TRUE), NA, accuracy),
         accuracy = ifelse(rt < mean(rt, na.rm=TRUE)-3*sd(rt, na.rm = TRUE), NA, accuracy)) %>% 
  ungroup()

bx_accuracy_summary <- all_bx_files_accuracy %>% 
  group_by(sub_num, trialTypeValid0Invalid1, trialTypeExtraTarget1NoExtraTarget0) %>% 
  summarise(meanAccuracy = mean(accuracy, na.rm = TRUE)) %>% 
  mutate(Validity = as.factor(trialTypeValid0Invalid1),
         additionalTargetDistractor = as.factor(trialTypeExtraTarget1NoExtraTarget0))

bx_accuracy_summary$Validity <- recode_factor(bx_accuracy_summary$Validity, 
                                        '0' = "Valid", 
                                        '1' = "Invalid")

bx_accuracy_summary$additionalTargetDistractor <- recode_factor(bx_accuracy_summary$additionalTargetDistractor, 
                                                          '0' = "No distractor present", 
                                                          '1' = "Distractor Present")

aov_Accuracy <- aov(meanAccuracy ~ Validity*additionalTargetDistractor + Error(sub_num/(Validity*additionalTargetDistractor)), 
              data = bx_accuracy_summary)

summary(aov_Accuracy)

bx_accuracy_summary %>% 
  ggplot(aes(x = Validity, y = meanAccuracy, fill = additionalTargetDistractor))+
  geom_boxplot()+
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 18, 
               size = 3,
               position = position_dodge(width = .9))

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
                                  all_imported_bx_files, 
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

all_fixation_count <- all_fixation_files %>% 
  filter(run_num != 1,
         accuracy == 1)

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

# Analysis of what number they looked at the fixation first
all_fixation_count_summary <- all_fixation_count %>% 
  group_by(sub_num, thisTrialExtraTarget, thisTrialIncorrectTargetLocation) %>% 
  summarise(avg_count = mean(first_fixation_number, na.rm = TRUE)) %>% 
  mutate(Validity = as.factor(thisTrialIncorrectTargetLocation),
         additionalTargetDistractor = as.factor(thisTrialExtraTarget))

all_fixation_count_summary$Validity <- recode_factor(all_fixation_count_summary$Validity, 
                                                     '0' = "Valid", '1' = "Invalid")

all_fixation_count_summary$additionalTargetDistractor <- recode_factor(all_fixation_count_summary$additionalTargetDistractor, 
                                                                       '0' = "No distractor present", '1' = "Distractor Present")


aov_fixation_count <- aov(avg_count ~ Validity*additionalTargetDistractor + Error(sub_num/(Validity*additionalTargetDistractor)), 
                          data = all_fixation_count_summary)
summary(aov_fixation_count)

all_fixation_count_summary %>% 
  ggplot(aes(x = Validity, y = avg_count, fill = additionalTargetDistractor))+
  geom_violin()+
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 18, 
               size = 3,
               position = position_dodge(width = .9))

# distractor analysis

distractor_df <- joined_fixation_data %>% 
  mutate(correctFixTarget = ifelse(previousFixationRect == targetPositionInds, 1, 0),
         currentFixETDistractor = ifelse(previousFixationRect == extraTargetShapePosition, 1, 0),
         currentFixNonETDistractor = ifelse(previousFixationRect != extraTargetShapePosition & previousFixationRect != targetPositionInds, 1, 0))

distractor_info <- distractor_df %>% 
  filter(trialTypeValid0Invalid1 == 0) %>% 
  select(sub_num, target_number, target_location_type) %>% 
  distinct(sub_num, target_number, target_location_type)

distractor_df_joined <- left_join(distractor_df, distractor_info, by =c('sub_num'='sub_num',
                                                                        'extraTargetShapeNumber' = 'target_number'))
distractor_df_joined <- distractor_df_joined %>%     
  mutate(validDistractorPositionValid0Invalid1 = ifelse(extraTargetShapeType == target_location_type.y, 0, 1)) %>% 
  group_by(sub_num, run_num, trial_num) %>%
  mutate(
    fixation_count = row_number(),
    first_et_distractor_fixation = ifelse(currentFixETDistractor == 1 & cumsum(currentFixETDistractor == 1) == 1, 1, 0),
    first_et_distractor_fixation_number = ifelse(first_et_distractor_fixation == 1, fixation_count, NA),
    first_non_et_distractor_fixation = ifelse(currentFixNonETDistractor == 1 & cumsum(currentFixNonETDistractor == 1) == 1, 1, 0),
    first_non_et_distractor_fixation_number = ifelse(first_non_et_distractor_fixation == 1, fixation_count, NA)) %>% 
  filter(run_num != 1,
         sub_num != 20,
         fixation_count == 1)

first_fixation_extra_target_summary <- distractor_df_joined %>%
  filter(thisTrialExtraTarget == 1) %>% 
  group_by(sub_num, thisTrialIncorrectTargetLocation, validDistractorPositionValid0Invalid1) %>% 
  summarise(percent_first_fixation = mean(currentFixETDistractor, na.rm = TRUE)) %>% 
  mutate(Validity = as.factor(thisTrialIncorrectTargetLocation),
         DistractorPositionValidity = as.factor(validDistractorPositionValid0Invalid1))

first_fixation_extra_target_summary$Validity <- recode_factor(first_fixation_extra_target_summary$Validity, 
                                                     '0' = "Valid", '1' = "Invalid")

first_fixation_extra_target_summary$DistractorPositionValidity <- recode_factor(first_fixation_extra_target_summary$DistractorPositionValidity, 
                                                                       '0' = "Valid", '1' = "Invalid")


extra_target_first_fix <- aov(percent_first_fixation ~ Validity*DistractorPositionValidity + Error(sub_num/(Validity*DistractorPositionValidity)), 
                          data = first_fixation_extra_target_summary)
summary(extra_target_first_fix)

first_fixation_extra_target_summary %>% 
  ggplot(aes(x = Validity, y = percent_first_fixation, fill = DistractorPositionValidity))+
  geom_violin()+
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 18, 
               size = 3,
               position = position_dodge(width = .9))
  

                                
                                
                                
                                
                                
                                