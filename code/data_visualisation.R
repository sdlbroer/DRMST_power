# clear environment
rm(list = ls())

# load data
load('C:/Users/rmjwlsd/OneDrive - University College London/analyses/NI_outcomes.RData')

# load necessary libraries
library(dplyr) # data wrangling
library(ggplot2) # plots
library(ggmagnify) # magnify part of plot
library(readxl) # read in data
library(survival) # estimate KM
library(GGally) # plot survival function

# clean environment
rm(list = setdiff(ls(), c('flex_NI_margins', 'conclusions', 
                          'pvalues', 'flex_emp_power', 'flex_average_p',
                          'sign_levels', 'time_hors', 'reconstructed')))

# dataframe with empirical power and average p for each analysis + summary measure
power_p <- left_join(data.frame('margin' = names(flex_emp_power), flex_emp_power),
                     data.frame('margin' = names(flex_average_p), flex_average_p)) %>%
  mutate(margin = substr(margin, 8, 100),
         # tau
         tau = ifelse(margin == 'HR', 'clinical', NA),
         tau = ifelse(grepl('clin', margin), 'clinical', tau),
         tau = ifelse(grepl('max', margin), 'maximum', tau),
         # analysis method
         analysis = ifelse(margin == 'HR', 'Cox', NA),
         analysis = ifelse(grepl('_', margin), 'flexible_parametric', analysis),
         analysis = ifelse(grepl('KM', margin), 'KM', analysis),
         # summary measure
         margin = ifelse(margin == 'DS_clin', 'DS', margin),
         margin = ifelse(margin == 'DRMST_clin', 'DRMST', margin),
         margin = ifelse(margin == 'DS_max', 'DS', margin),
         margin = ifelse(margin == 'DRMST_max', 'DRMST', margin),
         margin = ifelse(margin == 'DS_clin_flex_KM', 'DS', margin),
         margin = ifelse(margin == 'DS_max_flex_KM', 'DS', margin),
         margin = ifelse(margin == 'DRMST_clin_flex_KM', 'DRMST', margin),
         margin = ifelse(margin == 'DRMST_max_flex_KM', 'DRMST', margin))

# plot p-values on log scale ---------------------------------------------------
ggplot(data = rbind(mutate(pvalues, agreed_HRandDRMST = as.character(agreed_HRandDRMST)), 
                    c(NA, 1e-36, 1e-33, 1e-333, 'a', 'a', 'a')), # such that the plot is square
       mapping = aes(x = log2(as.numeric(margin_DRMST_clin)), y = log2(as.numeric(margin_HR)), 
                     colour = as.factor(agreed_HRandDRMST), alpha = as.factor(agreed_HRandDRMST))) +
  geom_point(size = 2, shape = 20) +
  # equality line
  geom_abline(colour = 'gray55', linetype = 'dotdash') +
  # significance level 0.05
  geom_hline(yintercept = log2(0.05), col = "black") +
  geom_vline(xintercept = log2(0.05), col = "black") +
  #annotate("rect", xmin = -Inf, xmax = log2(0.05), ymin = log2(0.05), ymax = 0, alpha = 0.2, fill = "orange") +
  #annotate("rect", xmin = log2(0.05), xmax = 0, ymin = -Inf, ymax = log2(0.05), alpha = 0.2, fill = "orange") +
  # significance level 0.025
  #geom_hline(yintercept = log2(0.025), col = "gray") +
  #geom_vline(xintercept = log2(0.025), col = "gray") +
  #annotate("rect", xmin = -Inf, xmax = log2(0.025), ymin = log2(0.025), ymax = 0, alpha = 0.2, fill = "yellow") +
  #annotate("rect", xmin = log2(0.025), xmax = 0, ymin = -Inf, ymax = log2(0.025), alpha = 0.2, fill = "yellow") +
  labs(title = 'p-values by summary measure',
       x = 'p-value DRMST margin',
       y = 'p-value HR margin',
       color = 'Conclusions of \n the margins is') + 
  scale_alpha_manual(values = c('FALSE' = 1, 'TRUE' = 1, 'a' = 0),
                     guide = 'none') +
  scale_color_manual(values = c('FALSE' = '#784283', 'TRUE' = '#d3afe9', 'a' = 'black'),
                     breaks = c('FALSE', 'TRUE'),
                     labels = c('FALSE' = 'different', 'TRUE' = 'the same')) +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_text(colour = 'black', size = 10, face = 'bold'),
        legend.text = element_text(colour = 'black', size = 10)) + 
  geom_magnify(from = c(xmin = -23, xmax = -1, ymin = -17, ymax = -1), 
             to = c(-71, -11, -120, -70), 
               colour = 'gray35') +
  scale_y_continuous(labels = function(x) format(2^x, digits = 1, scientific = TRUE),
                     breaks = c(log2(1e-30), log2(1e-20), log2(1e-10), log2(1e-0))) +
  scale_x_continuous(labels = function(x) format(2^x, digits = 1, scientific = TRUE),
                     breaks = c(log2(1e-30), log2(1e-20), log2(1e-10), log2(1e-0))) 


# plot all KM curves -----------------------------------------------------------

trial_names <- read_excel('C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/NI_margins.xlsx', 
                          sheet = 'key', col_types = c('numeric', 'text', 'skip'))

for(i in intersect(1:35, unique(reconstructed$study_nr))){
  # temporary dataframe containing only that study
  df.temp <- reconstructed[reconstructed$study_nr == i,]
  
  # define x axis label
  if(i %in% c(4,8,12,14,29,30,36,45,47,52,55,57,61)) xlab = 'Time since baseline (days)'
  if(i %in% c(5,10,37,46,54,66,70)) xlab = 'Time since baseline (weeks)'
  if(i %in% c(1,3,6,9,11,15,16,20,23,24,26,27,31,32,33,34,35,38,40,41,42,43,44,51,56,58,59,60,62,63,65,67,68,69))  xlab = 'Time since baseline (months)'
  if(i %in% c(2,7,13,17,28,37,46,54,66,70,39,48,49,50,53,64)) xlab = 'Time since baseline (years)'
  
  # fit Kaplan-Meier
  KM.est <- survfit(Surv(time, surv) ~ arm, data = df.temp, type = 'kaplan-meier')
  
  # plot Kaplan-Meier
  ggsave(paste0('C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/reconstructed_KMs/', trial_names$trial_name[trial_names$id == i],'.jpg'), 
         plot = print(ggsurv(KM.est, lty.est=c(1,2), surv.col = 1,
                             plot.cens = F) + 
                        labs(x = xlab,
                             y = 'Survival probability',
                             title = trial_names$trial_name[trial_names$id == i]) +
                        theme_bw() + 
                        guides(colour = FALSE) + 
                        scale_linetype_discrete(name = '', breaks = c(1,2), 
                                                labels = c('Control arm', 'Treatment arm')) + 
                        theme(legend.position = 'none',
                              axis.text=element_text(size=10),
                              axis.title=element_text(size=10),
                              legend.title = element_text(colour = 'black', size = 8, face = 'bold'),
                              legend.text = element_text(colour = 'black', size = 8),
                              plot.title = element_text(size = 12, hjust = 0.5))))
  
}
