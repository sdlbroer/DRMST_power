# clear environment
rm(list = ls())

# load data
source('C:/Users/rmjwlsd/OneDrive - University College London/analyses/preprocessing_data.R')
load('C:/Users/rmjwlsd/OneDrive - University College London/analyses/NI_outcomes.RData')

# load necessary libraries
library(flexsurv) # fit flexible parametric surival model

# clean environment
rm(list = setdiff(ls(), c('flex_NI_concl', 'flex_NI_outcomes', 'KM_concl', 'KM_outcomes', 
                          'time_hors', 'conclusions', 'pvalues', 'reconstructed')))

# split trials by event rate ---------------------------------------------------

# create dataframe to save survival at tau
surv.tau <- data.frame(id = unique(reconstructed$study_nr),
                       surv.tau = NA)

# calculate survival at tau 
for(i in unique(reconstructed$study_nr)){
  # temporary dataframe containing only that study
  df.temp <- reconstructed[reconstructed$study_nr == i,]
  
  # define parameters
  tau <- time_hors$time_hor[time_hors$id == i]
  tau <- ifelse(tau > min(max(df.temp$time[df.temp$arm == 1]), max(df.temp$time[df.temp$arm == 2])), 
                min(max(df.temp$time[df.temp$arm == 1]), max(df.temp$time[df.temp$arm == 2])), tau)
  
  # fit flexible parametric model on control arm
  fit.flexsurvs <- try(flexsurvspline(Surv(time,surv) ~ 1, data = df.temp[df.temp$arm == 1,], k = 2), silent = T)
  if(i == 31) fit.flexsurvs <-  try(flexsurvspline(Surv(time,surv) ~ 1, data = df.temp[df.temp$arm == 1,], k = 2, knots = c(2.7, 3.2)))
  
  # predict survival at tau
  surv.tau$surv.tau[surv.tau$id == i] <- as.numeric(predict(fit.flexsurvs, type="survival", times=tau)$.pred_survival[1])
}

# split trials into two groups 
median.surv <- round(median(surv.tau$surv.tau), 2)
low_survival <- surv.tau$id[surv.tau$surv.tau < median.surv]
high_survival <- surv.tau$id[surv.tau$surv.tau >= median.surv]

# subgroup analysis ------------------------------------------------------------

# low survival -----------------------------------------------------------------
# create dataframes that include only low survival trials
flex_NI_concl_low <- flex_NI_concl[flex_NI_concl$id %in% low_survival,]
flex_NI_outcomes_low <- flex_NI_outcomes[flex_NI_outcomes$id %in% low_survival,]
KM_concl_low <- KM_concl[KM_concl$id %in% low_survival,]
KM_outcomes_low <- KM_outcomes[KM_outcomes$id %in% low_survival,]

# calculate mean number of times NI was concluded
flex_emp_power_low <- c(apply(flex_NI_concl_low[,2:6], 2, function(x) mean(x, na.rm = T)),
                        apply(KM_concl_low[,c(4:5, 8:9)], 2, function(x) mean(x, na.rm = T)))
flex_average_p_low <- c(apply(flex_NI_outcomes_low[,2:6], 2, function(x) mean(x, na.rm = T)),
                        apply(KM_outcomes_low[,c(4:5, 8:9)], 2, function(x) mean(x, na.rm = T)))

# primary outcomes
conclusions_low <- conclusions[conclusions$id %in% low_survival,]
pvalues_low <- pvalues[pvalues$id %in% low_survival,]
emp_power_low <- flex_emp_power_low[c('margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]
average_p_low <- flex_average_p_low[c('margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]

# dataframe with empirical power and average p for each analysis + summary measure
power_p_low <- left_join(data.frame('margin' = names(flex_emp_power_low), flex_emp_power_low),
                         data.frame('margin' = names(flex_average_p_low), flex_average_p_low)) %>%
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

# high survival ----------------------------------------------------------------
# create dataframes that include only high survival trials
flex_NI_concl_high <- flex_NI_concl[flex_NI_concl$id %in% high_survival,]
flex_NI_outcomes_high <- flex_NI_outcomes[flex_NI_outcomes$id %in% high_survival,]
KM_concl_high <- KM_concl[KM_concl$id %in% high_survival,]
KM_outcomes_high <- KM_outcomes[KM_outcomes$id %in% high_survival,]

# calculate mean number of times NI was concluded
flex_emp_power_high <- c(apply(flex_NI_concl_high[,2:6], 2, function(x) mean(x, na.rm = T)),
                         apply(KM_concl_high[,c(4:5, 8:9)], 2, function(x) mean(x, na.rm = T)))
flex_average_p_high <- c(apply(flex_NI_outcomes_high[,2:6], 2, function(x) mean(x, na.rm = T)),
                         apply(KM_outcomes_high[,c(4:5, 8:9)], 2, function(x) mean(x, na.rm = T)))

# primary outcomes
conclusions_high <- conclusions[conclusions$id %in% high_survival,]
pvalues_high <- pvalues[pvalues$id %in% high_survival,]
emp_power_high <- flex_emp_power_high[c('margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]
average_p_high <- flex_average_p_high[c('margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]

# dataframe with empirical power and average p for each analysis + summary measure
power_p_high <- left_join(data.frame('margin' = names(flex_emp_power_high), flex_emp_power_high),
                          data.frame('margin' = names(flex_average_p_high), flex_average_p_high)) %>%
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
