# clear environment
rm(list = ls())

# load data
source('margin_conversion_flexsurv.R')

# load necessary libraries

# create dataframe with significance levels 
sign_levels <- rbind(select(HR_margins, id, onetwosided, CI_perc),
                     select(DS_margins, id, onetwosided, CI_perc),
                     select(DRMST_margins, id, onetwosided, CI_perc)) %>%
  mutate(alpha = ifelse(onetwosided == 'onesided', 1-CI_perc/100, (1-CI_perc/100)/2)) %>%
  arrange(id)

# create dataframe with time horizons
time_hors <- rbind(select(HR_margins, id, time_hor, max_time),
                   cbind(select(DS_margins, id, time_hor), max_time = NA),
                   cbind(select(DRMST_margins, id, time_hor), max_time = NA)) %>%
  arrange(id)

# clean environment
exp_NI_margins <- NI_margins_exp
flex_NI_margins <- NI_margins_flexsurv
rm(list = setdiff(ls(), c('exp_NI_margins', 'flex_NI_margins',
                          'reconstructed', 'sign_levels', 'time_hors')))

# dataframe to save all NI outcomes 
exp_NI_outcomes <- flex_NI_outcomes <- NI_estimates <- NI_CI <-
  data.frame('id' = sort(unique(exp_NI_margins$id)),
             'margin_HR' = NA, 
             'margin_DS_clin' = NA,
             'margin_DRMST_clin' = NA, 
             'margin_DS_max' = NA, 
             'margin_DRMST_max' = NA)
KM_outcomes <- data.frame('id' = sort(unique(exp_NI_margins$id)),
                          'margin_DS_clin_exp' = NA, 'margin_DS_max_exp' = NA,
                          'margin_DS_clin_flex' = NA, 'margin_DS_max_flex' = NA,
                          'margin_DRMST_clin_exp' = NA,'margin_DRMST_max_exp' = NA,
                          'margin_DRMST_clin_flex' = NA,  'margin_DRMST_max_flex' = NA)

# test for non-inferiority

## exponential distribution
for(i in exp_NI_margins$id){
  # temporary dataframe containing only that study
  df.temp <- reconstructed[reconstructed$study_nr == i,] %>%
    mutate(arm = revalue(as.character(arm), c('1' = 0, '2' = 1)))
  alpha <- sign_levels$alpha[sign_levels$id == i]
  tau <- time_hors$time_hor[time_hors$id == i]
  tau <- ifelse(tau > min(max(df.temp$time[df.temp$arm == 0]), max(df.temp$time[df.temp$arm == 1])), 
                min(max(df.temp$time[df.temp$arm == 0]), max(df.temp$time[df.temp$arm == 1])), tau)
  tau_max <- time_hors$max_time[time_hors$id == i]
  
  # margin = HR
  outcome <- test.NI.survival(
    time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
    NI.margin = as.numeric(exp_NI_margins[exp_NI_margins$id == i, 'margin_HR']), 
    sig.level = alpha, summary.measure = 'HR',
    unfavourable = TRUE,
    test.type = 'Cox.PH',
    print.out = FALSE)
  exp_NI_outcomes[exp_NI_outcomes$id == i, 'margin_HR'] <- outcome$p
  NI_estimates[NI_estimates$id == i, 'margin_HR'] <- outcome$estimate
  NI_CI[NI_CI$id == i, 'margin_HR'] <- paste0('(', round(outcome$CI[1], 3), ', ', round(outcome$CI[2], 3), ')')
  
  # margin = DS (clin)
  ## flexsurv method
  outcome <- test.NI.survival(
    time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
    NI.margin = as.numeric(exp_NI_margins[exp_NI_margins$id == i, 'margin_DS_clin']), 
    sig.level = alpha, summary.measure = 'DS',
    unfavourable = TRUE,
    test.type = 'flexsurv.PH.delta',
    tau = tau,
    print.out = FALSE)
  exp_NI_outcomes[exp_NI_outcomes$id == i, 'margin_DS_clin'] <- outcome$p
  NI_estimates[NI_estimates$id == i, 'margin_DS_clin'] <- outcome$estimate
  NI_CI[NI_CI$id == i, 'margin_DS_clin'] <- paste0('(', round(outcome$CI[1], 3), ', ', round(outcome$CI[2], 3), ')')
  ## KM method
  outcome <- test.NI.survival(
    time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
    NI.margin = as.numeric(exp_NI_margins[exp_NI_margins$id == i, 'margin_DS_clin']), 
    sig.level = alpha, summary.measure = 'DS',
    unfavourable = TRUE,
    test.type = 'KM',
    tau = tau,
    print.out = FALSE)
  KM_outcomes[KM_outcomes$id == i, 'margin_DS_clin_exp'] <- outcome$p
  
  # margin = DRMST (clin)
  ## flexsurv method
  outcome <- test.NI.survival(
    time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
    NI.margin = as.numeric(exp_NI_margins[exp_NI_margins$id == i, 'margin_DRMST_clin']), 
    sig.level = alpha, summary.measure = 'DRMST',
    unfavourable = TRUE,
    test.type = 'flexsurv.PH.delta',
    tau = tau,
    print.out = FALSE)
  exp_NI_outcomes[exp_NI_outcomes$id == i, 'margin_DRMST_clin'] <- outcome$p
  NI_estimates[NI_estimates$id == i, 'margin_DRMST_clin'] <- outcome$estimate
  NI_CI[NI_CI$id == i, 'margin_DRMST_clin'] <- paste0('(', round(outcome$CI[1], 3), ', ', round(outcome$CI[2], 3), ')')
  ## KM method
  outcome <- test.NI.survival(
    time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
    NI.margin = as.numeric(exp_NI_margins[exp_NI_margins$id == i, 'margin_DRMST_clin']), 
    sig.level = alpha, summary.measure = 'DRMST',
    unfavourable = TRUE,
    test.type = 'KM',
    tau = tau,
    print.out = FALSE)
  KM_outcomes[KM_outcomes$id == i, 'margin_DRMST_clin_exp'] <- outcome$p
  
  if(!is.na(tau_max)){
    tau_max <- min(max(df.temp$time[df.temp$arm == 0]), max(df.temp$time[df.temp$arm == 1]))
    # margin = DS (max)
    ## flexsurv method
    outcome <- test.NI.survival(
      time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
      NI.margin = as.numeric(exp_NI_margins[exp_NI_margins$id == i, 'margin_DS_max']), 
      sig.level = alpha, summary.measure = 'DS',
      unfavourable = TRUE,
      test.type = 'flexsurv.PH.delta',
      tau = tau_max,
      print.out = FALSE)
    exp_NI_outcomes[exp_NI_outcomes$id == i, 'margin_DS_max'] <- outcome$p
    NI_estimates[NI_estimates$id == i, 'margin_DS_max'] <- outcome$estimate
    NI_CI[NI_CI$id == i, 'margin_DS_max'] <- paste0('(', round(outcome$CI[1], 3), ', ', round(outcome$CI[2], 3), ')')
    ## KM method
    outcome <- test.NI.survival(
      time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
      NI.margin = as.numeric(exp_NI_margins[exp_NI_margins$id == i, 'margin_DS_max']), 
      sig.level = alpha, summary.measure = 'DS',
      unfavourable = TRUE,
      test.type = 'KM',
      tau = tau_max,
      print.out = FALSE)
    KM_outcomes[KM_outcomes$id == i, 'margin_DS_max_exp'] <- outcome$p
    
    # margin = DRMST (max)
    ## flexsurv method
    outcome <- test.NI.survival(
      time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
      NI.margin = as.numeric(exp_NI_margins[exp_NI_margins$id == i, 'margin_DRMST_max']), 
      sig.level = alpha, summary.measure = 'DRMST',
      unfavourable = TRUE,
      test.type = 'flexsurv.PH.delta',
      tau = tau_max,
      print.out = FALSE)
    exp_NI_outcomes[exp_NI_outcomes$id == i, 'margin_DRMST_max'] <- outcome$p
    NI_estimates[NI_estimates$id == i, 'margin_DRMST_max'] <- outcome$estimate
    NI_CI[NI_CI$id == i, 'margin_DRMST_max'] <- paste0('(', round(outcome$CI[1], 3), ', ', round(outcome$CI[2], 3), ')')
    ## KM method
    outcome <- test.NI.survival(
      time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
      NI.margin = as.numeric(exp_NI_margins[exp_NI_margins$id == i, 'margin_DRMST_max']), 
      sig.level = alpha, summary.measure = 'DRMST',
      unfavourable = TRUE,
      test.type = 'KM',
      tau = tau_max,
      print.out = FALSE)
    KM_outcomes[KM_outcomes$id == i, 'margin_DRMST_max_exp'] <- outcome$p
  }
}

## flexsurv 
for(i in flex_NI_margins$id){
  # temporary dataframe containing only that study
  df.temp <- reconstructed[reconstructed$study_nr == i,] %>%
    mutate(arm = revalue(as.character(arm), c('1' = 0, '2' = 1)))
  alpha <- sign_levels$alpha[sign_levels$id == i]
  tau <- time_hors$time_hor[time_hors$id == i]
  tau <- ifelse(tau > min(max(df.temp$time[df.temp$arm == 0]), max(df.temp$time[df.temp$arm == 1])), 
                min(max(df.temp$time[df.temp$arm == 0]), max(df.temp$time[df.temp$arm == 1])), tau)
  tau_max <- time_hors$max_time[time_hors$id == i]
  
  # margin = HR
  outcome <- test.NI.survival(
    time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
    NI.margin = as.numeric(flex_NI_margins[flex_NI_margins$id == i, 'margin_HR']), 
    sig.level = alpha, summary.measure = 'HR',
    unfavourable = TRUE,
    test.type = 'Cox.PH',
    print.out = FALSE)
  flex_NI_outcomes[flex_NI_outcomes$id == i, 'margin_HR'] <- outcome$p
  
  # margin = DS (clin)
  ## flexsurv method
  outcome <- test.NI.survival(
    time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
    NI.margin = as.numeric(flex_NI_margins[flex_NI_margins$id == i, 'margin_DS_clin']), 
    sig.level = alpha, summary.measure = 'DS',
    unfavourable = TRUE,
    test.type = 'flexsurv.PH.delta',
    tau = tau,
    print.out = FALSE)
  flex_NI_outcomes[flex_NI_outcomes$id == i, 'margin_DS_clin'] <- outcome$p
  ## KM method
  outcome <- test.NI.survival(
    time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
    NI.margin = as.numeric(flex_NI_margins[flex_NI_margins$id == i, 'margin_DS_clin']), 
    sig.level = alpha, summary.measure = 'DS',
    unfavourable = TRUE,
    test.type = 'KM',
    tau = tau,
    print.out = FALSE)
  KM_outcomes[KM_outcomes$id == i, 'margin_DS_clin_flex'] <- outcome$p
  
  # margin = DRMST (clin)
  ## flexsurv method
  outcome <- test.NI.survival(
    time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
    NI.margin = as.numeric(flex_NI_margins[flex_NI_margins$id == i, 'margin_DRMST_clin']), 
    sig.level = alpha, summary.measure = 'DRMST',
    unfavourable = TRUE,
    test.type = 'flexsurv.PH.delta',
    tau = tau,
    print.out = FALSE)
  flex_NI_outcomes[flex_NI_outcomes$id == i, 'margin_DRMST_clin'] <- outcome$p
  ## KM method
  outcome <- test.NI.survival(
    time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
    NI.margin = as.numeric(flex_NI_margins[flex_NI_margins$id == i, 'margin_DRMST_clin']), 
    sig.level = alpha, summary.measure = 'DRMST',
    unfavourable = TRUE,
    test.type = 'KM',
    tau = tau,
    print.out = FALSE)
  KM_outcomes[KM_outcomes$id == i, 'margin_DRMST_clin_flex'] <- outcome$p
  
  if(!is.na(tau_max)){
    tau_max <- min(max(df.temp$time[df.temp$arm == 0]), max(df.temp$time[df.temp$arm == 1]))
    # margin = DS (max)
    ## flexsurv method
    outcome <- test.NI.survival(
      time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
      NI.margin = as.numeric(flex_NI_margins[flex_NI_margins$id == i, 'margin_DS_max']), 
      sig.level = alpha, summary.measure = 'DS',
      unfavourable = TRUE,
      test.type = 'flexsurv.PH.delta',
      tau = tau_max,
      print.out = FALSE)
    flex_NI_outcomes[flex_NI_outcomes$id == i, 'margin_DS_max'] <- outcome$p
    ## KM method
    outcome <- test.NI.survival(
      time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
      NI.margin = as.numeric(flex_NI_margins[flex_NI_margins$id == i, 'margin_DS_max']), 
      sig.level = alpha, summary.measure = 'DS',
      unfavourable = TRUE,
      test.type = 'KM',
      tau = tau_max,
      print.out = FALSE)
    KM_outcomes[KM_outcomes$id == i, 'margin_DS_max_flex'] <- outcome$p
    
    # margin = DRMST (max)
    ## flexsurv method
    outcome <- test.NI.survival(
      time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
      NI.margin = as.numeric(flex_NI_margins[flex_NI_margins$id == i, 'margin_DRMST_max']), 
      sig.level = alpha, summary.measure = 'DRMST',
      unfavourable = TRUE,
      test.type = 'flexsurv.PH.delta',
      tau = tau_max,
      print.out = FALSE)
    flex_NI_outcomes[flex_NI_outcomes$id == i, 'margin_DRMST_max'] <- outcome$p
    # KM method
    outcome <- test.NI.survival(
      time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
      NI.margin = as.numeric(flex_NI_margins[flex_NI_margins$id == i, 'margin_DRMST_max']), 
      sig.level = alpha, summary.measure = 'DRMST',
      unfavourable = TRUE,
      test.type = 'KM',
      tau = tau_max,
      print.out = FALSE)
    KM_outcomes[KM_outcomes$id == i, 'margin_DRMST_max_flex'] <- outcome$p
  }
}

# clean up environment
rm(alpha, i, tau, tau_max, outcome, df.temp, mydata)

# conclusions ------------------------------------------------------------------
# fill in the empty columns of tau_max
exp_NI_outcomes <- exp_NI_outcomes %>%
  mutate(margin_DS_max = ifelse(is.na(margin_DS_max), margin_DS_clin, margin_DS_max),
         margin_DRMST_max = ifelse(is.na(margin_DRMST_max), margin_DRMST_clin, margin_DRMST_max))
flex_NI_outcomes <- flex_NI_outcomes %>%
  mutate(margin_DS_max = ifelse(is.na(margin_DS_max), margin_DS_clin, margin_DS_max),
         margin_DRMST_max = ifelse(is.na(margin_DRMST_max), margin_DRMST_clin, margin_DRMST_max))
KM_outcomes <- KM_outcomes %>%
  mutate(margin_DS_max_exp = ifelse(is.na(margin_DS_max_exp), margin_DS_clin_exp, margin_DS_max_exp),
         margin_DS_max_flex = ifelse(is.na(margin_DS_max_flex), margin_DS_clin_flex, margin_DS_max_flex),
         margin_DRMST_max_exp = ifelse(is.na(margin_DRMST_max_exp), margin_DRMST_clin_exp, margin_DRMST_max_exp),
         margin_DRMST_max_flex = ifelse(is.na(margin_DRMST_max_flex), margin_DRMST_clin_flex, margin_DRMST_max_flex))

# check if NI is concluded (based on the significance level of the study)
## exponential distribution
exp_NI_concl <- left_join(exp_NI_outcomes, select(sign_levels, id, alpha))
exp_NI_concl[,2:6] <- exp_NI_concl[,2:6] < exp_NI_concl$alpha
exp_NI_concl$alpha <- NULL
## flexsurv
flex_NI_concl <- left_join(flex_NI_outcomes, select(sign_levels, id, alpha))
flex_NI_concl[,2:6] <- flex_NI_concl[,2:6] < flex_NI_concl$alpha
flex_NI_concl$alpha <- NULL
## KM methods
KM_concl <- left_join(KM_outcomes, select(sign_levels, id, alpha))
KM_concl[,2:9] <- KM_concl[,2:9] < KM_concl$alpha
KM_concl$alpha <- NULL
names(KM_concl) <- names(KM_outcomes) <- 
  c('id', 'margin_DS_clin_exp_KM', 'margin_DS_max_exp_KM', 'margin_DS_clin_flex_KM', 'margin_DS_max_flex_KM', 
    'margin_DRMST_clin_exp_KM', 'margin_DRMST_max_exp_KM', 'margin_DRMST_clin_flex_KM', 'margin_DRMST_max_flex_KM')

# calculate mean number of times NI was concluded
## exponential distribution
exp_emp_power <- c(apply(exp_NI_concl[,2:6], 2, function(x) mean(x, na.rm = T)),
                   apply(KM_concl[,c(2:3, 6:7)], 2, function(x) mean(x, na.rm = T)))
exp_average_p <- c(apply(exp_NI_outcomes[,2:6], 2, function(x) mean(x, na.rm = T)),
                   apply(KM_outcomes[,c(2:3, 6:7)], 2, function(x) mean(x, na.rm = T)))
## flexsurv
flex_emp_power <- c(apply(flex_NI_concl[,2:6], 2, function(x) mean(x, na.rm = T)),
                    apply(KM_concl[,c(4:5, 8:9)], 2, function(x) mean(x, na.rm = T)))
flex_average_p <- c(apply(flex_NI_outcomes[,2:6], 2, function(x) mean(x, na.rm = T)),
                    apply(KM_outcomes[,c(4:5, 8:9)], 2, function(x) mean(x, na.rm = T)))

# save environment -------------------------------------------------------------
save.image(file = paste0('NI_outcomes_', 
                         format(Sys.Date(), '%y%m%d'), '.RData'))














# primary outcomes -------------------------------------------------------------
conclusions <- flex_NI_concl[,c('id', 'margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]
pvalues <- flex_NI_outcomes[,c('id', 'margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]
emp_power <- flex_emp_power[c('margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]
average_p <- flex_average_p[c('margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]

conclusions$agreed_HRandDS <- conclusions$margin_HR == conclusions$margin_DS_clin
conclusions$agreed_HRandDRMST <- conclusions$margin_HR == conclusions$margin_DRMST_clin
conclusions$agreed_DSandDRMST <- conclusions$margin_DS_clin == conclusions$margin_DRMST_clin

pvalues <- left_join(pvalues, select(conclusions, id, agreed_HRandDS, agreed_HRandDRMST, agreed_DSandDRMST),
                     by = 'id')

# primary outcomes, exponential ------------------------------------------------
exp_conclusions <- exp_NI_concl[,c('id', 'margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]
exp_pvalues <- exp_NI_outcomes[,c('id', 'margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]
exp_emp_power_subset <- exp_emp_power[c('margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]
exp_average_p_subset <- exp_average_p[c('margin_HR', 'margin_DS_clin', 'margin_DRMST_clin')]

exp_conclusions$agreed_HRandDS <- exp_conclusions$margin_HR == exp_conclusions$margin_DS_clin
exp_conclusions$agreed_HRandDRMST <- exp_conclusions$margin_HR == exp_conclusions$margin_DRMST_clin
exp_conclusions$agreed_DSandDRMST <- exp_conclusions$margin_DS_clin == exp_conclusions$margin_DRMST_clin

exp_pvalues <- left_join(exp_pvalues, select(exp_conclusions, id, agreed_HRandDS, agreed_HRandDRMST, agreed_DSandDRMST),
                         by = 'id')

# save environment -------------------------------------------------------------
save.image(file = paste0('C:/Users/rmjwlsd/OneDrive - University College London/analyses/NI_outcomes_', 
                         format(Sys.Date(), '%y%m%d'), '.RData'))
