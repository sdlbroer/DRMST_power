# clear environment
rm(list = ls())

# load data
source('preprocessing_data.R')
load('NI_outcomes_240620.RData')

# load necessary libraries

# clean environment
rm(list = setdiff(ls(), c('flex_NI_concl', 'flex_NI_outcomes', 'KM_concl', 'KM_outcomes', 
                          'reconstructed', 'HR_margins', 'DS_margins')))

# preprocessing dataframes
conclusions <- flex_NI_concl[,c('id', 'margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]
pvalues <- flex_NI_outcomes[,c('id', 'margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]

conclusions$agreed_HRandDS <- conclusions$margin_HR == conclusions$margin_DS_clin
conclusions$agreed_HRandDRMST <- conclusions$margin_HR == conclusions$margin_DRMST_clin
conclusions$agreed_DSandDRMST <- conclusions$margin_DS_clin == conclusions$margin_DRMST_clin

pvalues <- left_join(pvalues, select(conclusions, id, agreed_HRandDS, agreed_HRandDRMST, agreed_DSandDRMST),
                     by = 'id')

# HR ---------------------------------------------------------------------------
# create dataframes with only HR margins
flex_NI_concl_HR <- flex_NI_concl[flex_NI_concl$id %in% HR_margins$id,]
flex_NI_outcomes_HR <- flex_NI_outcomes[flex_NI_outcomes$id %in% HR_margins$id,]
KM_concl_HR <- KM_concl[KM_concl$id %in% HR_margins$id,]
KM_outcomes_HR <- KM_outcomes[KM_outcomes$id %in% HR_margins$id,]

# calculate mean number of times NI was concluded
flex_emp_power_HR <- c(apply(flex_NI_concl_HR[,2:6], 2, function(x) mean(x, na.rm = T)),
                       apply(KM_concl_HR[,c(4:5, 8:9)], 2, function(x) mean(x, na.rm = T)))
flex_average_p_HR <- c(apply(flex_NI_outcomes_HR[,2:6], 2, function(x) mean(x, na.rm = T)),
                       apply(KM_outcomes_HR[,c(4:5, 8:9)], 2, function(x) mean(x, na.rm = T)))

# primary outcomes
conclusions_HR <- conclusions[conclusions$id %in% HR_margins$id,]
pvalues_HR <- pvalues[pvalues$id %in% HR_margins$id,]
emp_power_HR <- flex_emp_power_HR[c('margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]
average_p_HR <- flex_average_p_HR[c('margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]

# dataframe with empirical power and average p for each analysis + summary measure
power_p_HR <- left_join(data.frame('margin' = names(flex_emp_power_HR), flex_emp_power_HR),
                        data.frame('margin' = names(flex_average_p_HR), flex_average_p_HR)) %>%
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

# DS ---------------------------------------------------------------------------
# create dataframes with only DS margins
flex_NI_concl_DS <- flex_NI_concl[flex_NI_concl$id %in% DS_margins$id,]
flex_NI_outcomes_DS <- flex_NI_outcomes[flex_NI_outcomes$id %in% DS_margins$id,]
KM_concl_DS <- KM_concl[KM_concl$id %in% DS_margins$id,]
KM_outcomes_DS <- KM_outcomes[KM_outcomes$id %in% DS_margins$id,]

# calculate mean number of times NI was concluded
flex_emp_power_DS <- c(apply(flex_NI_concl_DS[,2:6], 2, function(x) mean(x, na.rm = T)),
                       apply(KM_concl_DS[,c(4:5, 8:9)], 2, function(x) mean(x, na.rm = T)))
flex_average_p_DS <- c(apply(flex_NI_outcomes_DS[,2:6], 2, function(x) mean(x, na.rm = T)),
                       apply(KM_outcomes_DS[,c(4:5, 8:9)], 2, function(x) mean(x, na.rm = T)))

# primary outcomes
conclusions_DS <- conclusions[conclusions$id %in% DS_margins$id,]
pvalues_DS <- pvalues[pvalues$id %in% DS_margins$id,]
emp_power_DS <- flex_emp_power_DS[c('margin_DS', 'margin_DS_clin', 'margin_DRMST_clin')]
average_p_DS <- flex_average_p_DS[c('margin_DS', 'margin_DS_clin', 'margin_DRMST_clin')]

# dataframe with empirical power and average p for each analysis + summary measure
power_p_DS <- left_join(data.frame('margin' = names(flex_emp_power_DS), flex_emp_power_DS),
                        data.frame('margin' = names(flex_average_p_DS), flex_average_p_DS)) %>%
  mutate(margin = substr(margin, 8, 100),
         # tau
         tau = ifelse(margin == 'DS', 'clinical', NA),
         tau = ifelse(grepl('clin', margin), 'clinical', tau),
         tau = ifelse(grepl('max', margin), 'maximum', tau),
         # analysis method
         analysis = ifelse(margin == 'DS', 'Cox', NA),
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
