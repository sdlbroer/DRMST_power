# clear environment
rm(list = ls())

# load data
load('C:/Users/rmjwlsd/OneDrive - University College London/analyses/NI_outcomes_240523.RData')

# load necessary libraries
library(dplyr) # data wrangling
library(ggplot2) # plots
library(ggmagnify) # magnify part of plot 

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

# plot p-values on log scale
ggplot(data = rbind(mutate(pvalues, agreed_HRandDRMST = as.character(agreed_HRandDRMST)), 
                    c(NA, 1e-36, 1e-33, 1e-333, 'a', 'a', 'a')), # such that the plot is square
       mapping = aes(x = log2(as.numeric(margin_DRMST_clin)), y = log2(as.numeric(margin_HR)), 
                     colour = as.factor(agreed_HRandDRMST), alpha = as.factor(agreed_HRandDRMST))) +
  geom_point(size = 2, shape = 20) +
  # equality line
  geom_abline(colour = 'gray55', linetype = 'dotdash') +
  # significance level 0.05
  #geom_hline(yintercept = log2(0.05), col = "black") +
  #geom_vline(xintercept = log2(0.05), col = "black") +
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
