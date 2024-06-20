# clear environment
rm(list = ls())

# load data
load('NI_outcomes_240620.RData')

# load necessary libraries
library(dplyr) # data wrangling
library(ggplot2) # plots
library(ggmagnify) # magnify part of plot 

# clean environment
rm(list = setdiff(ls(), c('exp_NI_outcomes', 'exp_NI_concl', 
                          'exp_emp_power', 'exp_average_p')))

# preprocessing dataframes
conclusions <- exp_NI_concl[,c('id', 'margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]
pvalues <- exp_NI_outcomes[,c('id', 'margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]
emp_power <- exp_emp_power[c('margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]
average_p <- exp_average_p[c('margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]

conclusions$agreed_HRandDS <- conclusions$margin_HR == conclusions$margin_DS_clin
conclusions$agreed_HRandDRMST <- conclusions$margin_HR == conclusions$margin_DRMST_clin
conclusions$agreed_DSandDRMST <- conclusions$margin_DS_clin == conclusions$margin_DRMST_clin

pvalues <- left_join(pvalues, select(conclusions, id, agreed_HRandDS, agreed_HRandDRMST, agreed_DSandDRMST),
                     by = 'id')

# dataframe with empirical power and average p for each analysis + summary measure
exp_power_p <- left_join(data.frame('margin' = names(exp_emp_power), exp_emp_power),
                     data.frame('margin' = names(exp_average_p), exp_average_p)) %>%
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
         margin = ifelse(margin == 'DS_clin_exp_KM', 'DS', margin),
         margin = ifelse(margin == 'DS_max_exp_KM', 'DS', margin),
         margin = ifelse(margin == 'DRMST_clin_exp_KM', 'DRMST', margin),
         margin = ifelse(margin == 'DRMST_max_exp_KM', 'DRMST', margin))

# plot p-values on log scale
ggplot(data = mutate(pvalues[!(pvalues$id %in% c(37)),], agreed_HRandDRMST = as.character(agreed_HRandDRMST)), 
       mapping = aes(x = as.numeric(margin_DRMST_clin)^(1/10), y = as.numeric(margin_HR)^(1/10), 
                     colour = as.factor(agreed_HRandDRMST), alpha = as.factor(agreed_HRandDRMST))) +
  geom_point(size = 3, shape = 20) +
  # equality line
  geom_abline(colour = 'gray55', linetype = 'dotdash') +
  # significance level 0.05
  #geom_hline(yintercept = (0.05)^(1/10), col = "black") +
  #geom_vline(xintercept = (0.05)^(1/10), col = "black") +
  annotate("rect", xmin = -Inf, xmax = (0.05)^(1/10), ymin = (0.05)^(1/10), ymax = 1, alpha = 0.08, fill = "#00FF00") +
  annotate("rect", xmin = (0.05)^(1/10), xmax = 1, ymin = -Inf, ymax = (0.05)^(1/10), alpha = 0.08, fill = "#00FF00") +
  # significance level 0.025
  #geom_hline(yintercept = (0.025)^(1/10), col = "gray") +
  #geom_vline(xintercept = (0.025)^(1/10), col = "gray") +
  annotate("rect", xmin = -Inf, xmax = (0.025)^(1/10), ymin = (0.025)^(1/10), ymax = 1, alpha = 0.08, fill = "deepskyblue") +
  annotate("rect", xmin = (0.025)^(1/10), xmax = 1, ymin = -Inf, ymax = (0.025)^(1/10), alpha = 0.08, fill = "deepskyblue") +
  labs(#title = 'p-values by summary measure (Box-Cox transformation, λ = 1/10)',
    x = expression(paste(p^{1/10}, ' calculated using DRMST')), # expression(paste(p-value^{1/10},' DRMST margin')),
    y = expression(paste(p^{1/10}, ' calculated using HR')),
    color = 'Conclusions between the margins') + 
  scale_alpha_manual(values = c('FALSE' = 1, 'TRUE' = 1, 'a' = 0),
                     guide = 'none') +
  scale_color_manual(values = c('FALSE' = '#784283', 'TRUE' = '#d3afe9', 'a' = 'black'),
                     breaks = c('FALSE', 'TRUE'),
                     labels = c('FALSE' = 'disagree', 'TRUE' = 'agree')) +
  theme_bw() +
  theme(legend.position = 'none',
        legend.title = element_text(colour = 'black', size = 12, face = 'bold'),
        legend.text = element_text(colour = 'black', size = 12),
        axis.title = element_text(colour = 'black', size = 19, face = 'bold')) 
