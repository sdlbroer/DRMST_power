# clear environment
rm(list = ls())

# load data
load('C:/Users/rmjwlsd/OneDrive - University College London/analyses/NI_outcomes.RData')

# load necessary libraries
library(dplyr) # data wrangling
library(survival) # PH test 

# clean environment
rm(list = setdiff(ls(), c('flex_NI_concl', 'flex_NI_outcomes', 'KM_concl', 'KM_outcomes', 
                          'conclusions', 'pvalues', 'reconstructed')))

# PH assumption ----------------------------------------------------------------

# check PH assumption for each data set 
PH <- data.frame('id' = sort(unique(reconstructed$study_nr)),
                 'pvalue' = NA, 'PH_adhered' = NA)

for(i in unique(reconstructed$study_nr)){
  # temporary dataframe containing only that study
  df.temp <- reconstructed[reconstructed$study_nr == i,]
  
  # check PH assumption
  model.cox <- coxph(Surv(time, surv) ~ arm, data = df.temp)
  test.ph <- cox.zph(model.cox)
  
  # save values in dataframe
  PH[PH$id == i, 'pvalue'] <- test.ph$table[1,"p"]
  PH[PH$id == i, 'PH_adhered'] <- PH[PH$id == i, 'pvalue'] >= 0.05
}
PH[,'pvalue'] <- round(PH[,'pvalue'], 4)

# look further into the trials with evidence of non-proportionality
non_PH <- PH$id[PH$PH_adhered == FALSE]

for(i in non_PH){
  # temporary dataframe containing only that study
  df.temp <- reconstructed[reconstructed$study_nr == i,]
  
  # fit Kaplan-Meier
  KM.est <- survfit(Surv(time, surv) ~ arm, data = df.temp, type = "kaplan-meier")
  
  # plot Kaplan-Meier
  print(plot(KM.est,
             col = c('red', 'blue'),
             conf.int = FALSE,
             ylim = c(min(KM.est$surv) - 0.1, 1),
             main = as.character(i)))
}

# subgroup analysis ------------------------------------------------------------

# create dataframes that excludes non-PH trials
flex_NI_concl_PH <- flex_NI_concl[!(flex_NI_concl$id %in% non_PH),]
flex_NI_outcomes_PH <- flex_NI_outcomes[!(flex_NI_outcomes$id %in% non_PH),]
KM_concl_PH <- KM_concl[!(KM_concl$id %in% non_PH),]
KM_outcomes_PH <- KM_outcomes[!(KM_outcomes$id %in% non_PH),]

# calculate mean number of times NI was concluded
flex_emp_power_PH <- c(apply(flex_NI_concl_PH[,2:6], 2, function(x) mean(x, na.rm = T)),
                       apply(KM_concl_PH[,c(4:5, 8:9)], 2, function(x) mean(x, na.rm = T)))
flex_average_p_PH <- c(apply(flex_NI_outcomes_PH[,2:6], 2, function(x) mean(x, na.rm = T)),
                       apply(KM_outcomes_PH[,c(4:5, 8:9)], 2, function(x) mean(x, na.rm = T)))

# primary outcomes
conclusions_PH <- conclusions[!(conclusions$id %in% non_PH),]
pvalues_PH <- pvalues[!(pvalues$id %in% non_PH),]
emp_power_PH <- flex_emp_power_PH[c('margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]
average_p_PH <- flex_average_p_PH[c('margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]

# dataframe with empirical power and average p for each analysis + summary measure
power_p_PH <- left_join(data.frame('margin' = names(flex_emp_power_PH), flex_emp_power_PH),
                        data.frame('margin' = names(flex_average_p_PH), flex_average_p_PH)) %>%
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
