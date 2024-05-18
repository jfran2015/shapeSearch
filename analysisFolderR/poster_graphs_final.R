library(tidyverse)
library(RColorBrewer)
library(svglite)

response_time_violin <- bx_rt_summary %>% 
  ggplot(aes(y=meanRT, x = Validity, fill = additionalTargetDistractor))+
  geom_violin()+
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 18, 
               size = 4, 
               color = "black")+
  stat_summary(fun.data = mean_cl_normal,#get 95% Confidence Intervals; you can also set other kinds of error bars  
               geom = "errorbar", #graph error bars
               na.rm = T,
               width = .1)+
  
  ylab("Response Time (ms)")+
  facet_grid(~additionalTargetDistractor)+
  scale_fill_brewer(type = "qual", palette = palatte_num)+
  theme_classic()+
  scale_y_continuous(limits = nicelimits,
                     breaks = seq(800,2000, by = 100))
response_time_violin

fixation_proportion_violin <- all_first_fixation_summary %>% 
  ggplot(aes(y=percent_first_fixation, x = Validity, fill = additionalTargetDistractor))+
  geom_violin()+
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 18, 
               size = 4, 
               color = "black")+
  stat_summary(fun.data = mean_cl_normal,#get 95% Confidence Intervals; you can also set other kinds of error bars  
               geom = "errorbar", #graph error bars
               na.rm = T,
               width = .1)+
  
  ylab("Proportion of first fixation")+
  facet_grid(~additionalTargetDistractor)+
  scale_fill_brewer(type = "qual", palette = palatte_num)+
  theme_classic()+
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(.10,.80, by = .05))

fixation_proportion_violin

ggsave("~/Documents/posters/response_time_violin.svg", response_time_violin, dpi = 300, width = 10, height = 8, units = "in")
ggsave("~/Documents/posters/fixation_proportion_violin.svg", fixation_proportion_violin, dpi = 300, width = 10, height = 8, units = "in")
