# clear environment
rm(list = ls())

# load data
load('C:/Users/rmjwlsd/OneDrive - University College London/analyses/NI_outcomes.RData')

# load necessary libraries
library(plyr) # data wrangling 
library(dplyr) # data wrangling 
library(readxl) # read excel files
library(dani) # perform NI testing

# look further into trials with no evidence of NI
id_disagree <- conclusions$id[!conclusions$agreed_HRandDS | !conclusions$agreed_HRandDRMST |
                                !conclusions$agreed_DSandDRMST]

for(i in setdiff(id_disagree, 35:70)){
  print(paste0('Trial ', i, ' had the following properties'))
  print(paste0('  original summary measure: ', ifelse(i %in% c(33, 53, 57), 'HR', 'DS')))
  print(paste0('  significance level: ', sign_levels$alpha[sign_levels$id == i]))
  print(paste0('    HR p-value: ', round(flex_NI_outcomes$margin_HR[flex_NI_outcomes$id == i], 3)))
  print(paste0('    DS p-value: ', round(flex_NI_outcomes$margin_DS_clin[flex_NI_outcomes$id == i], 3)))
  print(paste0('    DRMST p-value: ', round(flex_NI_outcomes$margin_DRMST_clin[flex_NI_outcomes$id == i], 3)))
  print(paste0(' '))
}

# for Isabelle's trials: look at the margins she used
## margins
margins_Weir <- as.data.frame(rbind(c(36, 1.886, -8.395, -0.030),
                                    c(53, 1.700, -0.090, NA),
                                    c(57, 1.500, -30.295, NA),
                                    c(65, 2.473, -1.320, -0.100)))
names(margins_Weir) <- c('id', 'HR', 'DRMST', 'DS')

Weir_outcomes <- data.frame(id = intersect(id_disagree, 35:70), 
                            margin_HR = NA, margin_DS_clin = NA, margin_DRMST_clin = NA)

for(i in intersect(id_disagree, 35:70)){
  # temporary dataframe containing only that study
  df.temp <- reconstructed[reconstructed$study_nr == i,] %>%
    mutate(arm = revalue(as.character(arm), c('1' = 0, '2' = 1)))
  unfav <- ifelse(i == 16, FALSE, TRUE) # is this true for all studies?
  alpha <- sign_levels$alpha[sign_levels$id == i]
  tau <- time_hors$time_hor[time_hors$id == i]
  tau <- ifelse(tau > min(max(df.temp$time[df.temp$arm == 0]), max(df.temp$time[df.temp$arm == 1])), 
                min(max(df.temp$time[df.temp$arm == 0]), max(df.temp$time[df.temp$arm == 1])), tau)
  
  # margin = HR
  outcome <- test.NI.survival(
    time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
    NI.margin = as.numeric(margins_Weir[margins_Weir$id == i, 'HR']), 
    sig.level = alpha, summary.measure = 'HR',
    unfavourable = unfav,
    test.type = 'Cox.PH',
    print.out = FALSE)
  Weir_outcomes[Weir_outcomes$id == i, 'margin_HR'] <- outcome$p
  
  # margin = DS (clin)
  if(!is.na(margins_Weir[margins_Weir$id == i, 'DS'])){
    outcome <- test.NI.survival(
      time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
      NI.margin = as.numeric(margins_Weir[margins_Weir$id == i, 'DS']), 
      sig.level = alpha, summary.measure = 'DS',
      unfavourable = unfav,
      test.type = 'flexsurv.PH.delta',
      tau = tau,
      print.out = FALSE)
    Weir_outcomes[Weir_outcomes$id == i, 'margin_DS_clin'] <- outcome$p
  }
  
  # margin = DRMST (clin)
  ## flexsurv method
  outcome <- test.NI.survival(
    time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
    NI.margin = as.numeric(margins_Weir[margins_Weir$id == i, 'DRMST']), 
    sig.level = alpha, summary.measure = 'DRMST',
    unfavourable = unfav,
    test.type = 'flexsurv.PH.delta',
    tau = tau,
    print.out = FALSE)
  Weir_outcomes[Weir_outcomes$id == i, 'margin_DRMST_clin'] <- outcome$p
}

# look at the outcomes
for(i in intersect(id_disagree, 35:70)){
  print(paste0('Trial ', i, ' had the following properties'))
  print(paste0('  original summary measure: ', ifelse(i %in% c(33, 53, 57), 'HR', 'DS')))
  print(paste0('  significance level: ', sign_levels$alpha[sign_levels$id == i]))
  print(paste0('    HR p-value: ', round(flex_NI_outcomes$margin_HR[flex_NI_outcomes$id == i], 3)))
  print(paste0('    DS p-value: ', round(flex_NI_outcomes$margin_DS_clin[flex_NI_outcomes$id == i], 3)))
  print(paste0('    DRMST p-value: ', round(flex_NI_outcomes$margin_DRMST_clin[flex_NI_outcomes$id == i], 3)))
  print(paste0('  -Using Isabelle\'s margins-'))
  print(paste0('    HR p-value: ', round(Weir_outcomes$margin_HR[Weir_outcomes$id == i], 3)))
  print(paste0('    DS p-value: ', round(Weir_outcomes$margin_DS_clin[Weir_outcomes$id == i], 3)))
  print(paste0('    DRMST p-value: ', round(Weir_outcomes$margin_DRMST_clin[Weir_outcomes$id == i], 3)))
  print(paste0(' '))
}

# ------------------------------------------------------------------------------
# for Isabelle's trials: look at the margins she used - using the KM estimation
Weir_outcomes_KM <- data.frame(id = intersect(id_disagree, 35:70), 
                            margin_HR = NA, margin_DS_clin = NA, margin_DRMST_clin = NA)

for(i in intersect(id_disagree, 35:70)){
  # temporary dataframe containing only that study
  df.temp <- reconstructed[reconstructed$study_nr == i,] %>%
    mutate(arm = revalue(as.character(arm), c('1' = 0, '2' = 1)))
  unfav <- ifelse(i == 16, FALSE, TRUE) # is this true for all studies?
  alpha <- sign_levels$alpha[sign_levels$id == i]
  tau <- time_hors$time_hor[time_hors$id == i]
  tau <- ifelse(tau > min(max(df.temp$time[df.temp$arm == 0]), max(df.temp$time[df.temp$arm == 1])), 
                min(max(df.temp$time[df.temp$arm == 0]), max(df.temp$time[df.temp$arm == 1])), tau)
  
  # margin = HR
  outcome <- test.NI.survival(
    time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
    NI.margin = as.numeric(margins_Weir[margins_Weir$id == i, 'HR']), 
    sig.level = alpha, summary.measure = 'HR',
    unfavourable = unfav,
    test.type = 'Cox.PH',
    print.out = FALSE)
  Weir_outcomes_KM[Weir_outcomes_KM$id == i, 'margin_HR'] <- outcome$p
  
  # margin = DS (clin)
  if(!is.na(margins_Weir[margins_Weir$id == i, 'DS'])){
    outcome <- test.NI.survival(
      time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
      NI.margin = as.numeric(margins_Weir[margins_Weir$id == i, 'DS']), 
      sig.level = alpha, summary.measure = 'DS',
      unfavourable = unfav,
      test.type = 'KM',
      tau = tau,
      print.out = FALSE)
    Weir_outcomes_KM[Weir_outcomes_KM$id == i, 'margin_DS_clin'] <- outcome$p
  }
  
  # margin = DRMST (clin)
  ## flexsurv method
  outcome <- test.NI.survival(
    time = df.temp$time, event = df.temp$surv, treat = df.temp$arm,
    NI.margin = as.numeric(margins_Weir[margins_Weir$id == i, 'DRMST']), 
    sig.level = alpha, summary.measure = 'DRMST',
    unfavourable = unfav,
    test.type = 'KM',
    tau = tau,
    print.out = FALSE)
  Weir_outcomes_KM[Weir_outcomes_KM$id == i, 'margin_DRMST_clin'] <- outcome$p
}

# look at the outcomes
for(i in intersect(id_disagree, 35:70)){
  print(paste0('Trial ', i, ' had the following properties'))
  print(paste0('  original summary measure: ', ifelse(i %in% c(33, 53, 57), 'HR', 'DS')))
  print(paste0('  significance level: ', sign_levels$alpha[sign_levels$id == i]))
  print(paste0('    HR p-value: ', round(flex_NI_outcomes$margin_HR[flex_NI_outcomes$id == i], 3)))
  print(paste0('    DS p-value: ', round(flex_NI_outcomes$margin_DS_clin[flex_NI_outcomes$id == i], 3)))
  print(paste0('    DRMST p-value: ', round(flex_NI_outcomes$margin_DRMST_clin[flex_NI_outcomes$id == i], 3)))
  print(paste0('  -Using Isabelle\'s margins-'))
  print(paste0('    HR p-value: ', round(Weir_outcomes_KM$margin_HR[Weir_outcomes_KM$id == i], 3)))
  print(paste0('    DS p-value: ', round(Weir_outcomes_KM$margin_DS_clin[Weir_outcomes_KM$id == i], 3)))
  print(paste0('    DRMST p-value: ', round(Weir_outcomes_KM$margin_DRMST_clin[Weir_outcomes_KM$id == i], 3)))
  print(paste0(' '))
}
