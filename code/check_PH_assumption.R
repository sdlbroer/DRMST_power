# clear environment
rm(list = ls())

# load necessary libraries
library(dplyr) # data wrangling
library(survival) # PH test

# load data
source('C:/Users/rmjwlsd/OneDrive - University College London/analyses/preprocessing_data.R')
rm(list = setdiff(ls(), 'reconstructed'))

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
  PH[PH$id == i, 'PH_adhered'] <- PH[PH$id == i, 'pvalue'] >= 0.1
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
