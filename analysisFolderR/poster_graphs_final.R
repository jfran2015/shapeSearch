library(tidyverse)
library(RColorBrewer)
library(svglite)
library(Rmisc)

nicelimits <- function(x) {
  range(scales::extended_breaks(only.loose = TRUE)(x))
}

# Compute within-subjects summary stats
rt_summary_within <- summarySEwithin(
  data = bx_rt_summary, 
  measurevar = "meanRT", 
  withinvars = c("Validity", "additionalTargetDistractor"), 
  idvar = "sub_num", # your subject column
  na.rm = TRUE
)

# Plot
response_time_violin <- ggplot(rt_summary_within, 
                               aes(y = meanRT,
                                   x = Validity, 
                                   fill = additionalTargetDistractor)) +
  geom_violin(data = bx_rt_summary, 
              alpha = 1) + # raw violins
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 18, 
               size = 3, 
               color = "black") +
  geom_errorbar(aes(ymin = meanRT - ci, 
                    ymax = meanRT + ci), 
                width = .1, color = "black") +
  ylab("Response Time (ms)") +
  facet_grid(~additionalTargetDistractor) +
  scale_fill_brewer(type = "qual", palette = palatte_num) +
  theme_classic() +
  theme(legend.position = "none")+
  scale_y_continuous(limits = nicelimits,
                     breaks = seq(800,2000, by = 100))

response_time_violin

# Compute within-subject summary stats
fix_within_summary <- summarySEwithin(
  data = all_first_fixation_summary,
  measurevar = "percent_first_fixation",
  withinvars = c("Validity", "additionalTargetDistractor"),
  idvar = "sub_num" # replace with your subject column
)

# Plot with within-subject error bars
fixation_proportion_violin <- all_first_fixation_summary %>% 
  ggplot(aes(y = percent_first_fixation, 
             x = Validity, 
             fill = additionalTargetDistractor)) +
  geom_violin() +
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 18, 
               size = 4, 
               color = "black") +
  geom_errorbar(data = fix_within_summary,
                aes(y = percent_first_fixation,
                    ymin = percent_first_fixation - ci,
                    ymax = percent_first_fixation + ci),
                width = .1,
                color = "black") +
  ylab("Proportion of first fixation") +
  facet_grid(~additionalTargetDistractor) +
  scale_fill_brewer(type = "qual", palette = palatte_num) +
  theme_classic() +
  theme(legend.position = "none")+
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(.10,.80, by = .05))

fixation_proportion_violin


ggsave("~/Documents/posters/response_time_violin_E1_wseb.pdf", 
       response_time_violin, 
       dpi = 300, 
       width = 10, 
       height = 9, 
       units = "in")

ggsave("~/Documents/posters/fixation_proportion_violin_E1_wseb.pdf", 
       fixation_proportion_violin, 
       dpi = 300, 
       width = 10, 
       height = 9, 
       units = "in")
