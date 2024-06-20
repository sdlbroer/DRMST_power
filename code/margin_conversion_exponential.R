# clear environment
rm(list = ls())

# load necessary libraries
library(dani) # margin conversion

# load data
source('preprocessing_data.R')

# dataframe to save all margins
NI_margins_exp <- data.frame('id' = sort(c(unique(HR_margins$id), unique(DS_margins$id), unique(DRMST_margins$id))),
                             'margin_HR' = NA, 
                             'margin_DRMST_clin' = NA, 'margin_DRMST_max' = NA,
                             'margin_DS_clin' = NA, 'margin_DS_max' = NA)

# convert margin: original margin = HR 
for(i in HR_margins$id){
  # temporary dataframe containing only that study
  df.temp <- reconstructed[reconstructed$study_nr == i,]
  
  # define parameters
  rate.exp <- p0$lambda[p0$id == i]
  tau <- as.numeric(HR_margins[HR_margins$id == i, 'time_hor'])
  margin.original <- as.numeric(HR_margins[HR_margins$id == i, 'NI_margin'])
  
  # HR margin
  NI_margins_exp[NI_margins_exp$id == i, 'margin_HR'] <- margin.original
  # DRMST margin (clinically relevant tau)
  NI_margins_exp[NI_margins_exp$id == i, 'margin_DRMST_clin'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                        summary.measure.original = 'HR',
                                                                                        summary.measure.target = 'DRMST',
                                                                                        NI.margin.original = margin.original, 
                                                                                        tau.RMST = tau)
  # DS margin (clinically relevant t)
  NI_margins_exp[NI_margins_exp$id == i, 'margin_DS_clin'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                     summary.measure.original = 'HR', 
                                                                                     summary.measure.target = 'DS',
                                                                                     NI.margin.original = margin.original, 
                                                                                     t.DS = tau)
  
  if(!is.na(HR_margins$max_time[HR_margins$id == i])){
    tau <- max(df.temp$time)
    # DRMST margin (maximal tau)
    NI_margins_exp[NI_margins_exp$id == i, 'margin_DRMST_max'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                         summary.measure.original = 'HR',
                                                                                         summary.measure.target = 'DRMST',
                                                                                         NI.margin.original = margin.original, 
                                                                                         tau.RMST = tau)
    # DS margin (maximal t)
    NI_margins_exp[NI_margins_exp$id == i, 'margin_DS_max'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                      summary.measure.original = 'HR', 
                                                                                      summary.measure.target = 'DS',
                                                                                      NI.margin.original = margin.original, 
                                                                                      t.DS = tau)
  }
}

# convert margin: original margin = DS
for(i in DS_margins$id){
  if(i != 37){
    # define parameters
    rate.exp <- p0$lambda[p0$id == i]
    tau <- as.numeric(DS_margins[DS_margins$id == i, 'time_hor'])
    margin.original <- as.numeric(DS_margins[DS_margins$id == i, 'NI_margin'])
      
      # DS margin
      NI_margins_exp[NI_margins_exp$id == i, 'margin_DS_clin'] <- margin.original
    # DRMST margin
    NI_margins_exp[NI_margins_exp$id == i, 'margin_DRMST_clin'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                          summary.measure.original = 'DS', 
                                                                                          summary.measure.target = 'DRMST',
                                                                                          NI.margin.original = margin.original, 
                                                                                          tau.RMST = tau,
                                                                                          t.DS = tau)
    # HR margin
    NI_margins_exp[NI_margins_exp$id == i, 'margin_HR'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                  summary.measure.original = 'DS', 
                                                                                  summary.measure.target = 'HR',
                                                                                  NI.margin.original = margin.original,
                                                                                  t.DS = tau)
  }
  
  if(i == 37){
    # define parameters
    rate.exp <- p0$lambda[p0$id == i]
    tau <- as.numeric(DS_margins[DS_margins$id == i, 'time_hor'])/52
    margin.original <- as.numeric(DS_margins[DS_margins$id == i, 'NI_margin'])
    
    # DS margin
    NI_margins_exp[NI_margins_exp$id == i, 'margin_DS_clin'] <- margin.original
    # DRMST margin
    NI_margins_exp[NI_margins_exp$id == i, 'margin_DRMST_clin'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                          summary.measure.original = 'DS', 
                                                                                          summary.measure.target = 'DRMST',
                                                                                          NI.margin.original = margin.original, 
                                                                                          tau.RMST = tau,
                                                                                          t.DS = tau)
    NI_margins_exp[NI_margins_exp$id == i, 'margin_DRMST_clin'] <- NI_margins_exp[NI_margins_exp$id == i, 'margin_DRMST_clin']*52
    # HR margin
    NI_margins_exp[NI_margins_exp$id == i, 'margin_HR'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                  summary.measure.original = 'DS', 
                                                                                  summary.measure.target = 'HR',
                                                                                  NI.margin.original = margin.original,
                                                                                  t.DS = tau)
  }
}

# convert margin: original margin = DRMST
for(i in DRMST_margins$id){
  # define parameters
  rate.exp <- p0$lambda[p0$id == i]
  tau <- as.numeric(DRMST_margins[DRMST_margins$id == i, 'time_hor'])
  margin.original <- as.numeric(DRMST_margins[DRMST_margins$id == i, 'NI_margin'])
  
  # DRMST margin
  NI_margins_exp[NI_margins_exp$id == i, 'margin_DRMST_clin'] <- margin.original
  # DS margin
  NI_margins_exp[NI_margins_exp$id == i, 'margin_DS_clin'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                summary.measure.original = 'DRMST', 
                                                                                summary.measure.target = 'DS',
                                                                                NI.margin.original = margin.original,
                                                                                tau.RMST = tau,
                                                                                t.DS = tau)
  # HR margin
  NI_margins_exp[NI_margins_exp$id == i, 'margin_HR'] <- convertmargin.survival(rate.control.expected = rate.exp,
                                                                                summary.measure.original = 'DRMST',
                                                                                summary.measure.target = 'HR',
                                                                                NI.margin.original = margin.original,
                                                                                tau.RMST = tau)
}

# clean up environment
rm(i, tau, margin.original, rate.exp, df.temp)
