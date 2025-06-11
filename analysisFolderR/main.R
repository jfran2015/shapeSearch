# ======= LIBRARIES & DATA IMPORT =====================
library(tidyverse)
library(lsmeans)
library(ggpubr)
library(wesanderson)
library(rstatix)
library(R.matlab)
library(readxl)
library(effectsize)

# Import demographic data
JustinsDemographics <- read_excel("JustinsDemographics.xlsx", sheet = "ShapeSearch")
JustinsDemographics = JustinsDemographics[-1:-2, ]  # Remove first two rows (headers/notes)
JustinsDemographics <- JustinsDemographics %>% 
  select(sub_num=Q1, Handedness, Age, Vision, Contacts, Colorblind, Gender, Ethnicity, Race)

getwd()

# ======= DATA READING FUNCTION =====================
# Reads all files in a folder and optionally adds subject/run info
read <- function(data_folder, get_subj_info = FALSE) {
  full <- data.frame()
  for (file in data_folder) {
    individual <- read.csv(file = file)
    if (get_subj_info == TRUE){
      run_num = str_sub(file,-6, -4)
      sub_num = str_sub(file,-8, -6)
      individual$run_num <- run_num
      individual$sub_num <- sub_num
    }
    full <- rbind(full, individual)
  }
  if (nrow(full) > 0 && all(is.na(full[1, ]))) {
    fixed <- full[-1, ]
  } else {
    fixed <- full
  }
  return(fixed)
}

# ======= IMPORT BEHAVIORAL & EYE DATA =====================
bx_files <- dir(path = "../output/bxData/", full.names = TRUE)
eye_files <- dir(path = "../output/eyeData/fixationData", full.names = TRUE)

all_imported_bx_files <- read(bx_files)
all_fixation_files <- read(eye_files, get_subj_info = TRUE)


# Clean up subject/run numbers
all_fixation_files$run_num <- gsub("[^0-9]", "", all_fixation_files$run_num)
all_fixation_files$sub_num <- gsub("[^0-9]", "", all_fixation_files$sub_num)

# ======= BEHAVIORAL DATA CLEANUP =======
all_imported_bx_files <- all_imported_bx_files %>% 
  mutate(sub_num = as.factor(sub_num),
         run_num = as.factor(run_num))

# Summarize unique runs and accuracy per subject
unique_run_summary <- all_imported_bx_files  %>% 
  group_by(sub_num) %>% 
  summarise(unique_runs = n_distinct(run_num),
            overall_accuracy = mean(accuracy, na.rm = TRUE))

# Merge demographics and run summary
demographics_for_participants <- left_join(unique_run_summary, JustinsDemographics, by = "sub_num")
all_imported_bx_files <- left_join(all_imported_bx_files, unique_run_summary, by = "sub_num")

# Filter and clean RT data
all_bx_files <- all_imported_bx_files %>%
  filter(accuracy == 1,
         run_num != 1,
         sub_num != 20,
         unique_runs == 7,
         overall_accuracy > 0.80) %>%
  group_by(sub_num, 
           trialTypeValid0Invalid1, 
           trialTypeExtraTarget1NoExtraTarget0) %>% 
  mutate(rt = ifelse(rt <= 200, NA, rt),
         rt = ifelse(rt > mean(rt, na.rm=TRUE)+3*sd(rt, na.rm = TRUE), NA, rt),
         rt = ifelse(rt < mean(rt, na.rm=TRUE)-3*sd(rt, na.rm = TRUE), NA, rt),
         Validity = factor(trialTypeValid0Invalid1, levels = c(0, 1), labels = c("Valid", "Invalid")),
         additionalTargetDistractor  = factor(trialTypeExtraTarget1NoExtraTarget0, levels = c(0, 1), labels = c("Distractor absent", "Distractor present"))) %>% 
  ungroup()

# ======= SCENE & OVERLAP INFO JOIN =====================
scene_info <- read.csv("../output/sceneInfo.csv")
colnames(scene_info)<- c("sub_num","run_num","trial_num", "scene_ind", "position_ind")
scene_info <- scene_info %>% 
  mutate(sub_num = as.factor(sub_num),
         run_num = as.factor(run_num))

all_bx_files <- left_join(all_bx_files, scene_info, by= c("sub_num","run_num","trial_num"))

overlap_info <- read.csv("../output/overlap_info.csv", header = FALSE)
colnames(overlap_info)<- c("scene_ind","1","2", "3", "4")
overlap_info_long <- overlap_info %>% 
  pivot_longer(!scene_ind, names_to = "position_ind", values_to = "overlapYes1No0") %>% 
  mutate(position_ind = as.numeric(position_ind))

all_bx_files <- left_join(all_bx_files, overlap_info_long, by = c("scene_ind", "position_ind"))

# Summary of only overlaping positions
overlap_summary <- all_bx_files  %>%
  filter(overlapYes1No0 == 1) %>% 
  group_by(sub_num, trialTypeValid0Invalid1, trialTypeExtraTarget1NoExtraTarget0) %>% 
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>% 
  mutate(Validity = factor(trialTypeValid0Invalid1, levels = c(0, 1), labels = c("Valid", "Invalid")),
         additionalTargetDistractor  = factor(trialTypeExtraTarget1NoExtraTarget0, levels = c(0, 1), labels = c("Distractor absent", "Distractor present")))

overlap_aov <- aov(meanRT ~ Validity*additionalTargetDistractor + Error(sub_num/(Validity*additionalTargetDistractor)), 
                data = overlap_summary)
summary(overlap_aov)
eta_squared(overlap_aov, partial = TRUE, ci = 0.95)

# RT summary stats by condition
overlap_summary %>%
  group_by(Validity, additionalTargetDistractor) %>%
  summarise(
    mean_RT = mean(meanRT),
    sd_RT = sd(meanRT),
    n = n() / 2,
    se = sd_RT / sqrt(n)
  )

# By distractor
overlap_summary %>%
  group_by(additionalTargetDistractor) %>%
  summarise(
    mean_RT = mean(meanRT),
    sd_RT = sd(meanRT),
    n = n() / 2,
    se = sd_RT / sqrt(n)
  )

# By validity
overlap_summary %>%
  group_by(Validity) %>%
  summarise(
    mean_RT = mean(meanRT),
    sd_RT = sd(meanRT),
    n = n() / 2,
    se = sd_RT / sqrt(n)
  )

library(lme4)
library(lmerTest)  # for p-values
normal_model <- lmer(rt ~ Validity * additionalTargetDistractor + (1|sub_num), data = all_bx_files)
summary(normal_model)

y_model <- lmer(rt ~ Validity * additionalTargetDistractor * target_position_2 + (1|sub_num), 
                data = all_bx_files %>% filter(overlapYes1No0 == 1))
summary(y_model)
# Model
model <- lmer(rt ~ Validity * additionalTargetDistractor + target_position_2 + (1|sub_num), 
              data = all_bx_files %>% filter(overlapYes1No0 == 1))
# Model
model <- lmer(rt ~ Validity * additionalTargetDistractor * target_position_2 + (1|sub_num), 
              data = all_bx_files %>% filter(overlapYes1No0 == 1))

all_bx_files %>% 
ggplot(aes(x = target_position_2, y = rt, color = Validity)) +
  stat_smooth(method = "lm", se = TRUE) +  # linear trend lines by Validity
  labs(x = "Target Position", y = "Reaction Time", color = "Validity") +
  theme_minimal()


# ======= RT SUMMARY ==============
bx_rt_summary <- all_bx_files  %>%
  group_by(sub_num, trialTypeValid0Invalid1, trialTypeExtraTarget1NoExtraTarget0) %>% 
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>% 
  mutate(Validity = factor(trialTypeValid0Invalid1, levels = c(0, 1), labels = c("Valid", "Invalid")),
         additionalTargetDistractor  = factor(trialTypeExtraTarget1NoExtraTarget0, levels = c(0, 1), labels = c("Distractor absent", "Distractor present")))

# ======= RT ANOVA & SUMMARY ==============
aov_RT <- aov(meanRT ~ Validity*additionalTargetDistractor + Error(sub_num/(Validity*additionalTargetDistractor)), 
              data = bx_rt_summary)
summary(aov_RT)
model.tables(aov_RT, "means")
eta_squared(aov_RT, partial = TRUE, ci = 0.95)


# RT summary stats by condition
bx_rt_summary %>%
  group_by(Validity, additionalTargetDistractor) %>%
  summarise(
    mean_RT = mean(meanRT),
    sd_RT = sd(meanRT),
    n = n() / 2,
    se = sd_RT / sqrt(n)
  )

# By distractor
bx_rt_summary %>%
  group_by(additionalTargetDistractor) %>%
  summarise(
    mean_RT = mean(meanRT),
    sd_RT = sd(meanRT),
    n = n() / 2,
    se = sd_RT / sqrt(n)
  )

# By validity
bx_rt_summary %>%
  group_by(Validity) %>%
  summarise(
    mean_RT = mean(meanRT),
    sd_RT = sd(meanRT),
    n = n() / 2,
    se = sd_RT / sqrt(n)
  )

# ======= EPOCH (RUN) ANALYSIS =======
bx_rt_epoch_summary <- all_bx_files  %>% 
  group_by(sub_num, trialTypeValid0Invalid1, trialTypeExtraTarget1NoExtraTarget0, run_num) %>% 
  summarise(meanRT = mean(rt, na.rm = TRUE)) %>% 
  mutate(Validity = factor(trialTypeValid0Invalid1, levels = c(0, 1), labels = c("Valid", "Invalid")),
         additionalTargetDistractor  = factor(trialTypeExtraTarget1NoExtraTarget0, levels = c(0, 1), labels = c("Distractor absent", "Distractor present")))

aov_epoch_RT <- aov(meanRT ~ Validity*additionalTargetDistractor*run_num + Error(sub_num/(Validity*additionalTargetDistractor*run_num)), 
                    data = bx_rt_epoch_summary)
summary(aov_epoch_RT)
eta_squared(aov_epoch_RT, partial = TRUE, ci = 0.95)


# Pairwise tests
lsd_results <- lsmeans(aov_epoch_RT, pairwise ~ Validity * run_num, adjust = "none")
summary(lsd_results)

bx_rt_epoch_summary %>% 
lsd_results <- emmeans(aov_epoch_RT, pairwise ~ Validity * run_num, adjust = "none")
  geom_violin()+
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 18, 
               size = 3,
               position = position_dodge(width = .9))+
  labs(title="Mean response time across validity\nand distractor presence",
       x ="Validity", 
       y = "Respnse Time (ms)",
       fill = "Distractor Presence")+
  theme_classic()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=19),
        plot.title=element_text(size=23),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14))+
  scale_y_continuous(limits = c(700, 2000),
                     breaks = seq(700,2000, by = 100))+
  scale_fill_brewer(palette="Set3")

# ======= ACCURACY ANALYSIS =======
all_bx_files_accuracy <- all_imported_bx_files %>%
  filter(accuracy != 2,
         run_num != 1,
         unique_runs == 7,
         overall_accuracy > 0.80) %>% 
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

aov_Accuracy <- aov(meanAccuracy ~ Validity*additionalTargetDistractor + Error(sub_num/(Validity*additionalTargetDistractor)), 
              data = bx_accuracy_summary)
summary(aov_Accuracy)
model.tables(aov_Accuracy, "means")
eta_squared(aov_Accuracy, partial = TRUE, ci = 0.95)

# Accuracy summary stats
bx_accuracy_summary %>%
  group_by(Validity, additionalTargetDistractor) %>%
  summarise(
    mean_accuracy = mean(meanAccuracy),
    sd_accuracy = sd(meanAccuracy),
    n = n() / 2,
    se = sd_accuracy / sqrt(n)
  )

bx_accuracy_summary %>%
  group_by(additionalTargetDistractor) %>%
  summarise(
    mean_accuracy = mean(meanAccuracy),
    sd_accuracy = sd(meanAccuracy),
    n = n() / 2,
    se = sd_accuracy / sqrt(n)
  )

bx_accuracy_summary %>%
  group_by(Validity) %>%
  summarise(
    mean_accuracy = mean(meanAccuracy),
    sd_accuracy = sd(meanAccuracy),
    n = n() / 2,
    se = sd_accuracy / sqrt(n)
  )

# Accuracy boxplot
bx_accuracy_summary %>% 
  ggplot(aes(x = Validity, y = meanAccuracy, fill = additionalTargetDistractor))+
  geom_boxplot()+
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 18, 
               size = 3,
               position = position_dodge(width = .75))

# ======= FIXATION ANALYSIS =======
# Clean up fixation data and join with behavioral data
all_fixation_files <- all_fixation_files %>%
  mutate(sub_num = as.numeric(sub_num), # Remove leading 0s
         sub_num = as.factor(sub_num),
         run_num = as.numeric(run_num),
         run_num = as.factor(run_num),
         correctTarget = ifelse(previousFixationRect == targetPositionInds, 1, 0),
         trial_num = trialNum)

# Join fixation and behavioral data for accuracy info
joined_fixation_data <- left_join(all_fixation_files, 
                                  all_imported_bx_files, 
                                  by=c('sub_num'='sub_num', 
                                       'trial_num'='trial_num', 
                                       'run_num'='run_num'))

# Add accuracy columns to fixation data
all_fixation_files$accuracy <- joined_fixation_data$accuracy
all_fixation_files$overall_accuracy <- joined_fixation_data$overall_accuracy
all_fixation_files$unique_runs <- joined_fixation_data$unique_runs

# Add fixation count/order columns
all_fixation_files <- all_fixation_files %>%
  group_by(sub_num, run_num, trial_num) %>%
  mutate(
    fixation_count = row_number(),
    first_correct_fixation = ifelse(correctTarget == 1 & cumsum(correctTarget == 1) == 1, 1, 0),
    first_fixation_number = ifelse(first_correct_fixation == 1, fixation_count, NA))

# ======= FIRST FIXATION ANALYSIS =====================
all_first_fixation <- all_fixation_files %>% 
  filter(fixation_count == 1,
         run_num != 1,
         accuracy == 1,
         unique_runs == 7,
         overall_accuracy > .80)

all_first_fixation_summary <- all_first_fixation %>% 
  group_by(sub_num, thisTrialExtraTarget, thisTrialIncorrectTargetLocation) %>% 
  summarise(percent_first_fixation = mean(correctTarget, na.rm = TRUE)) %>% 
  mutate(Validity = factor(thisTrialIncorrectTargetLocation, levels = c(0, 1), labels = c("Valid", "Invalid")),
         additionalTargetDistractor  = factor(thisTrialExtraTarget, levels = c(0, 1), labels = c("Distractor absent", "Distractor present")))

aov_first_fixation <- aov(percent_first_fixation ~ Validity*additionalTargetDistractor + Error(sub_num/(Validity*additionalTargetDistractor)), 
                          data = all_first_fixation_summary)
summary(aov_first_fixation)
model.tables(aov_first_fixation, "means")

# First fixation summary stats
all_first_fixation_summary %>%
  group_by(thisTrialIncorrectTargetLocation, thisTrialExtraTarget) %>%
  summarise(
    mean_percent_first_fixation = mean(percent_first_fixation),
    sd_percent_first_fixation = sd(percent_first_fixation),
    n = n() / 2,
    se = sd_percent_first_fixation / sqrt(n)
  )

all_first_fixation_summary %>%
  group_by(thisTrialExtraTarget) %>%
  summarise(
    mean_percent_first_fixation = mean(percent_first_fixation),
    sd_percent_first_fixation = sd(percent_first_fixation),
    n = n() / 2,
    se = sd_percent_first_fixation / sqrt(n)
  )

all_first_fixation_summary %>%
  group_by(thisTrialIncorrectTargetLocation) %>%
  summarise(
    mean_percent_first_fixation = mean(percent_first_fixation),
    sd_percent_first_fixation = sd(percent_first_fixation),
    n = n() / 2,
    se = sd_percent_first_fixation / sqrt(n)
  )

all_fixation_count <- all_fixation_files %>% 
  filter(run_num != 1,
         accuracy == 1)

# ======= FIXATION COUNT ANALYSIS =====================
all_fixation_count_summary <- all_fixation_count %>% 
  group_by(sub_num, thisTrialExtraTarget, thisTrialIncorrectTargetLocation) %>% 
  summarise(avg_count = mean(first_fixation_number, na.rm = TRUE)) %>% 
  mutate(Validity = factor(thisTrialIncorrectTargetLocation, levels = c(0, 1), labels = c("Valid", "Invalid")),
         additionalTargetDistractor  = factor(thisTrialExtraTarget, levels = c(0, 1), labels = c("Distractor absent", "Distractor present")))

aov_fixation_count <- aov(avg_count ~ Validity*additionalTargetDistractor + Error(sub_num/(Validity*additionalTargetDistractor)), 
                          data = all_fixation_count_summary)
summary(aov_fixation_count)

# Fixation count violin plot
all_fixation_count_summary %>% 
  ggplot(aes(x = Validity, y = avg_count, fill = additionalTargetDistractor))+
  geom_violin()+
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 18, 
               size = 3,
               position = position_dodge(width = .9))

# ======= FIRST FIXATION & FIXATION COUNT ANALYSIS  FOR OVERLAPING REGIONS =====================
joined_fixation_data <- left_join(joined_fixation_data, 
                                  scene_info, 
                                  by= c("sub_num",
                                        "run_num",
                                        "trial_num"))

joined_fixation_data <- joined_fixation_data %>%
  mutate(tiral_num = trialNum)

joined_fixation_data <- left_join(joined_fixation_data, overlap_info_long, by = c("scene_ind", "position_ind"))

all_first_fixation_overlap <- all_fixation_files %>% 
  filter(fixation_count == 1,
         run_num != 1,
         accuracy == 1,
         unique_runs == 7,
         overall_accuracy > .80,
         overlapYes1No0 == 1)

all_first_fixation_summary <- all_first_fixation %>% 
  group_by(sub_num, thisTrialExtraTarget, thisTrialIncorrectTargetLocation) %>% 
  summarise(percent_first_fixation = mean(correctTarget, na.rm = TRUE)) %>% 
  mutate(Validity = factor(thisTrialIncorrectTargetLocation, levels = c(0, 1), labels = c("Valid", "Invalid")),
         additionalTargetDistractor  = factor(thisTrialExtraTarget, levels = c(0, 1), labels = c("Distractor absent", "Distractor present")))

aov_first_fixation <- aov(percent_first_fixation ~ Validity*additionalTargetDistractor + Error(sub_num/(Validity*additionalTargetDistractor)), 
                          data = all_first_fixation_summary)
summary(aov_first_fixation)
model.tables(aov_first_fixation, "means")

# First fixation summary stats
all_first_fixation_summary %>%
  group_by(thisTrialIncorrectTargetLocation, thisTrialExtraTarget) %>%
  summarise(
    mean_percent_first_fixation = mean(percent_first_fixation),
    sd_percent_first_fixation = sd(percent_first_fixation),
    n = n() / 2,
    se = sd_percent_first_fixation / sqrt(n)
  )

all_first_fixation_summary %>%
  group_by(thisTrialExtraTarget) %>%
  summarise(
    mean_percent_first_fixation = mean(percent_first_fixation),
    sd_percent_first_fixation = sd(percent_first_fixation),
    n = n() / 2,
    se = sd_percent_first_fixation / sqrt(n)
  )

all_first_fixation_summary %>%
  group_by(thisTrialIncorrectTargetLocation) %>%
  summarise(
    mean_percent_first_fixation = mean(percent_first_fixation),
    sd_percent_first_fixation = sd(percent_first_fixation),
    n = n() / 2,
    se = sd_percent_first_fixation / sqrt(n)
  )

all_fixation_count <- all_fixation_files %>% 
  filter(run_num != 1,
         accuracy == 1)

# ======= DISTRACTOR ANALYSIS =====================
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
  filter(accuracy != 2,
         run_num != 1,
         unique_runs == 7,
         overall_accuracy > 0.80,
         fixation_count == 1)

first_fixation_extra_target_summary <- distractor_df_joined %>%
  filter(thisTrialExtraTarget == 1) %>% 
  group_by(sub_num, thisTrialIncorrectTargetLocation, validDistractorPositionValid0Invalid1) %>% 
  summarise(percent_first_fixation = mean(currentFixETDistractor, na.rm = TRUE)) %>% 
  mutate(Validity = as.factor(thisTrialIncorrectTargetLocation),
         DistractorPositionValidity = as.factor(validDistractorPositionValid0Invalid1))

first_fixation_extra_target_summary <- first_fixation_extra_target_summary %>%
  mutate(
    Validity = recode_factor(Validity, '0' = "Valid", '1' = "Invalid"),
    DistractorPositionValidity = recode_factor(DistractorPositionValidity, '0' = "Valid", '1' = "Invalid")
  )

extra_target_first_fix <- aov(percent_first_fixation ~ Validity*DistractorPositionValidity + Error(sub_num/(Validity*DistractorPositionValidity)), 
                          data = first_fixation_extra_target_summary)
summary(extra_target_first_fix)

# Distractor violin plot
first_fixation_extra_target_summary %>% 
  ggplot(aes(x = Validity, y = percent_first_fixation, fill = DistractorPositionValidity))+
  geom_violin()+
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 18, 
               size = 3,
               position = position_dodge(width = .9))
# ======= END OF SCRIPT =====================


