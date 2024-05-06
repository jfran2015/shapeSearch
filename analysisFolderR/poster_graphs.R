library(tidyverse)
library(RColorBrewer)

dodge_width = 0.1

lineSize = .71
dodge_width = 0
palatte_num = 6

nicelimits <- function(x) {
  range(scales::extended_breaks(only.loose = TRUE)(x))
}

bx_plot <- bx_rt_summary %>% 
  ggplot(aes(y = meanRT, x = additionalTargetDistractor, fill = Validity))+
  geom_boxplot(outliers = FALSE)+
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = 0.75), size = 3, aes(shape =additionalTargetDistractor))+
  theme_classic()+
  scale_y_continuous(limits = nicelimits,
                     breaks = seq(900,1500, by = 100))+
  scale_fill_brewer(type = "qual", palette = palatte_num)+
  labs(x ="Validity", 
       y = "Response Time (ms)",
       fill = "Distractor Presence")+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'),
    axis.text=element_text(size=30),
    axis.title=element_text(size=33),
    plot.title=element_text(size=33),
    legend.text=element_text(size=30),
    legend.title=element_text(size=33))+ #transparent legend panel
  guides(shape = FALSE)
bx_plot

bx_rt_summary %>%
  ggplot(aes(y = meanRT, x = additionalTargetDistractor, color = Validity))+
  stat_summary(fun = mean,      #get means
               geom = "point",  #graph points
               position = position_dodge(width = .15),
               na.rm = TRUE)+      #NAs are removed (not taken into computation) if there's any
  stat_summary(fun = mean,
               geom = "line",   #graph lines
               na.rm = T,
               position = position_dodge(width = .15),
               aes(group = additionalTargetDistractor))+
  stat_summary(fun.data = mean_cl_normal,#get 95% Confidence Intervals; you can also set other kinds of error bars  
               geom = "errorbar", #graph error bars
               na.rm = T,
               position = position_dodge(width = .15),
               width = .1)+
  labs(title = "Flanker task response time")
  
  #List of Qualitative palettes includes Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3 >> display.brewer.all(type="qual")
  
  # Assuming you have a data frame named 'data' with columns 'additionalTargetDistractor' and the variable you want to summarize
  
  fixation_plot <- all_first_fixation_summary %>% 
  ggplot(aes(x = Validity, y = percent_first_fixation, fill = additionalTargetDistractor))+
  geom_boxplot(outliers = FALSE, notch = TRUE)+
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = 0.75), size = 3, aes(shape =additionalTargetDistractor))+
  theme_classic()+
  scale_y_continuous(limits = nicelimits,
                     labels = scales::percent)+
  scale_fill_brewer(type = "qual", palette = palatte_num)+
  labs(x ="Validity", 
       y = "Proportion of first fixation",
       shape = "Additional Target\nDistractor Presence")+
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'),
    axis.text=element_text(size=30),
    axis.title=element_text(size=33),
    plot.title=element_text(size=33),
    legend.text=element_text(size=30),
    legend.title=element_text(size=33))+ #transparent legend panel
  guides(shape = guide_legend(override.aes = list(fill = "white", colour = "black")),
         fill = FALSE)
fixation_plot

ggsave("bxplottransparent.png", bx_plot, bg="transparent", dpi = 600, width = 20, height = 16, units = "in")
ggsave("fixplottransparent.png", fixation_plot, bg="transparent", dpi = 600, width = 20, height = 16, units = "in")

