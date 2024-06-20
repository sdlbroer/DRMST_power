 # clear environment
rm(list = ls())

# load necessary libraries
library(flexsurv) # flexible parametric models

# load data
source('margin_conversion_exponential.R')

# define function to predict survival at a given time point
S.control <- function(t){
  as.numeric(predict(fit.flexsurvs, type="survival", times=t)$.pred_survival[1])
}

# dataframe to save all margins
NI_margins_flexsurv <- data.frame('id' = sort(c(unique(HR_margins$id), unique(DS_margins$id), unique(DRMST_margins$id))),
                                  'margin_HR' = NA, 
                                  'margin_DRMST_clin' = NA, 'margin_DRMST_max' = NA,
                                  'margin_DS_clin' = NA, 'margin_DS_max' = NA)

# convert margins: original margin = HR 
for(i in HR_margins$id){
  # temporary dataframe containing only that study
  df.temp <- reconstructed[reconstructed$study_nr == i,]
  
  # define parameters
  rate.exp <- p0$lambda[p0$id == i]
  tau <- as.numeric(HR_margins[HR_margins$id == i, 'time_hor'])
  margin.original <- as.numeric(HR_margins[HR_margins$id == i, 'NI_margin'])
  
  # fit flexible parametric model on control arm
  fit.flexsurvs <- try(flexsurvspline(Surv(time,surv) ~ 1, data = df.temp[df.temp$arm == 1,], k = 2))
  
  # HR margin
  NI_margins_flexsurv[NI_margins_flexsurv$id == i, 'margin_HR'] <- margin.original
  # DRMST margin (clinically relevant tau)
  NI_margins_flexsurv[NI_margins_flexsurv$id == i, 'margin_DRMST_clin'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                                  summary.measure.original = "HR",
                                                                                                  summary.measure.target = "DRMST", 
                                                                                                  NI.margin.original = margin.original,
                                                                                                  tau.RMST = tau,
                                                                                                  BH.est = "surv.func", 
                                                                                                  S.control = S.control)
  # DS margin (clinically relevant t)
  NI_margins_flexsurv[NI_margins_flexsurv$id == i, 'margin_DS_clin'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                               summary.measure.original = 'HR', 
                                                                                               summary.measure.target = 'DS',
                                                                                               NI.margin.original = margin.original, 
                                                                                               t.DS = tau,
                                                                                               BH.est = "surv.func", 
                                                                                               S.control = S.control)
  
  if(!is.na(HR_margins$max_time[HR_margins$id == i])){
    tau <- max(df.temp$time)
    # DRMST margin (maximal tau)
    NI_margins_flexsurv[NI_margins_flexsurv$id == i, 'margin_DRMST_max'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                                   summary.measure.original = "HR",
                                                                                                   summary.measure.target = "DRMST", 
                                                                                                   NI.margin.original = margin.original,
                                                                                                   tau.RMST = tau,
                                                                                                   BH.est = "surv.func", 
                                                                                                   S.control = S.control)
    # DS margin (maximal t)
    NI_margins_flexsurv[NI_margins_flexsurv$id == i, 'margin_DS_max'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                                summary.measure.original = 'HR', 
                                                                                                summary.measure.target = 'DS',
                                                                                                NI.margin.original = margin.original, 
                                                                                                t.DS = tau,
                                                                                                BH.est = "surv.func", 
                                                                                                S.control = S.control)
  }
}
  
# convert margins: original margin = DS
for(i in DS_margins$id){
  # temporary dataframe containing only that study
  df.temp <- reconstructed[reconstructed$study_nr == i,]
  
  # define parameters
  rate.exp <- p0$lambda[p0$id == i]
  tau <- as.numeric(DS_margins[DS_margins$id == i, 'time_hor'])
  margin.original <- as.numeric(DS_margins[DS_margins$id == i, 'NI_margin'])
  
  # fit flexible parametric model on control arm
  fit.flexsurvs <- try(flexsurvspline(Surv(time,surv) ~ 1, data = df.temp[df.temp$arm == 1,], k = 2), silent = T)
  if(i == 31) fit.flexsurvs <- try(flexsurvspline(Surv(time,surv) ~ 1, data = df.temp[df.temp$arm == 1,], k = 2, knots = c(2.7, 3.2)))
  
  # DS margin
  NI_margins_flexsurv[NI_margins_flexsurv$id == i, 'margin_DS_clin'] <- margin.original
  # DRMST margin
  NI_margins_flexsurv[NI_margins_flexsurv$id == i, 'margin_DRMST_clin'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                                  summary.measure.original = 'DS', 
                                                                                                  summary.measure.target = 'DRMST',
                                                                                                  NI.margin.original = margin.original, 
                                                                                                  tau.RMST = tau, 
                                                                                                  t.DS = tau,
                                                                                                  BH.est = "surv.func", 
                                                                                                  S.control = S.control)
  # HR margin
  NI_margins_flexsurv[NI_margins_flexsurv$id == i, 'margin_HR'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                          summary.measure.original = 'DS', 
                                                                                          summary.measure.target = 'HR',
                                                                                          NI.margin.original = margin.original,
                                                                                          t.DS = tau,
                                                                                          BH.est = "surv.func", 
                                                                                          S.control = S.control)
}

# convert margins: original margin = DRMST
for(i in DRMST_margins$id){
  # temporary dataframe containing only that study
  df.temp <- reconstructed[reconstructed$study_nr == i,]
  
  # define parameters
  rate.exp <- p0$lambda[p0$id == i]
  tau <- as.numeric(DRMST_margins[DRMST_margins$id == i, 'time_hor'])
  margin.original <- as.numeric(DRMST_margins[DRMST_margins$id == i, 'NI_margin'])
  
  # fit flexible parametric model on control arm
  fit.flexsurvs <- try(flexsurvspline(Surv(time,surv) ~ 1, data = df.temp[df.temp$arm == 1,], k = 2))
  
  # DRMST margin
  NI_margins_flexsurv[NI_margins_flexsurv$id == i, 'margin_DRMST_clin'] <- margin.original
  # DS margin
  NI_margins_flexsurv[NI_margins_flexsurv$id == i, 'margin_DS_clin'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                               summary.measure.original = 'DRMST', 
                                                                                               summary.measure.target = 'DS',
                                                                                               NI.margin.original = margin.original,  
                                                                                               tau.RMST = tau, 
                                                                                               t.DS = tau,
                                                                                               BH.est = "surv.func", 
                                                                                               S.control = S.control)
  # HR margin
  NI_margins_flexsurv[NI_margins_flexsurv$id == i, 'margin_HR'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                          summary.measure.original = 'DRMST', 
                                                                                          summary.measure.target = 'HR',
                                                                                          NI.margin.original = margin.original,  
                                                                                          tau.RMST = tau, 
                                                                                          BH.est = "surv.func", 
                                                                                          S.control = S.control)
}

# clean up environment
rm(i, df.temp, fit.flexsurvs, p0, tau, S.control, margin.original, rate.exp)
