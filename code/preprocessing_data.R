# clear environment
rm(list = ls())

# load necessary libraries
library(readxl) # read in data
library(plyr) # plyr
library(dplyr) # data wrangling
library(data.table) # data wrangling

# read file of all HR NI margins
HR_margins_new <- read_excel('manuscript_information.xlsx', 
                sheet = 'HR', col_types = c('numeric', 
                                            'numeric', 'text', 'numeric', 'text', 
                                            'numeric', 'skip', 'skip'))
names(HR_margins_new) <- c('id', 'time_hor', 'unit', 'NI_margin', 'onetwosided', 'CI_perc')

HR_margins_Weir <- read_excel('manuscript_information.xlsx', 
                              sheet = 'HR_Weir', col_types = c('skip', 
                                                               'numeric', 'numeric', 'text', 'numeric', 
                                                               'text', 'numeric', 'skip', 'skip', 
                                                               'skip')) %>%
  filter(!is.na(id))
names(HR_margins_Weir) <- c('id', 'time_hor', 'unit', 'NI_margin', 'onetwosided', 'CI_perc')

HR_margins <- rbind(select(HR_margins_new, id, time_hor, unit, NI_margin, onetwosided, CI_perc),
                    select(HR_margins_Weir, id, time_hor, unit, NI_margin, onetwosided, CI_perc)) %>%
  mutate(original_margin = 'HR',
         id = as.numeric(id))

# read file of all DS NI margins
DS_margins_new <- read_excel('manuscript_information.xlsx', 
                             sheet = 'DS', col_types = c('numeric', 
                                                         'numeric', 'text', 'numeric', 'text', 
                                                         'numeric', 'skip', 'skip'))
names(DS_margins_new) <- c('id', 'time_hor', 'unit', 'NI_margin', 'onetwosided', 'CI_perc')

DS_margins_Weir <- read_excel('manuscript_information.xlsx', 
                              sheet = 'DS_Weir', col_types = c('skip', 
                                                               'numeric', 'numeric', 'text', 'numeric', 
                                                               'text', 'numeric', 'skip', 'skip', 
                                                               'skip')) %>%
  filter(!is.na(id))
names(DS_margins_Weir) <- c('id', 'time_hor', 'unit', 'NI_margin', 'onetwosided', 'CI_perc')

DS_margins <- rbind(select(DS_margins_new, id, time_hor, unit, NI_margin, onetwosided, CI_perc),
                    select(DS_margins_Weir, id, time_hor, unit, NI_margin, onetwosided, CI_perc))  %>%
  mutate(original_margin = 'DS',
         NI_margin = NI_margin/100,
         NI_margin = - abs(NI_margin), # make all margins negative
         id = as.numeric(id))

# read file of all DRMST NI margins
DRMST_margins <- read_excel('manuscript_information.xlsx', 
                            sheet = 'DRMST', col_types = c('numeric', 
                                                           'numeric', 'text', 'numeric', 'text', 
                                                           'numeric', 'skip', 'skip'))
names(DRMST_margins) <- c('id', 'time_hor', 'unit', 'NI_margin', 'onetwosided', 'CI_perc')

DRMST_margins <- DRMST_margins %>%
  mutate(original_margin = 'DRMST',
         NI_margin = - abs(NI_margin), # make all margins negative
         id = as.numeric(id))

# read in the reconstructed study data
reconstructed <- read_excel('reconstructed.xlsx')

# read in 
p0_new <- read_excel('manuscript_information.xlsx', 
                     sheet = 'nr_control', col_types = c('numeric', 
                                                         'numeric', 'numeric', 'text', 'numeric', 
                                                         'numeric', 'numeric', 'numeric', 
                                                         'numeric', 'skip', 'skip'))
p0_weir <- read_excel('manuscript_information.xlsx', 
                      sheet = 'nr_control_Weir', col_types = c('skip', 
                                                               'numeric', 'numeric', 'numeric', 
                                                               'text', 'numeric', 'numeric', 'numeric', 
                                                               'numeric', 'numeric', 'skip', 'skip', 
                                                               'skip'))
p0 <- rbind(p0_new, p0_weir) %>%
  mutate(lambda = ifelse(!is.na(lambda) & unit == 'd', lambda/365, lambda)) %>%
  mutate(lambda = ifelse(!is.na(lambda) & unit == 'w', lambda/52, lambda)) %>%
  mutate(lambda = ifelse(!is.na(lambda) & unit == 'm', lambda/12, lambda)) %>%
  mutate(lambda = ifelse(is.na(lambda), -log(1-expected_rate)/expected_time, lambda)) %>%
  mutate(expected_rate = NULL,
         expected_time = NULL,
         unit = NULL)

# add max time if tau =/= max time 
HR_margins <- left_join(HR_margins, select(p0, id, max_time))

# clean up environment
rm(p0_new, p0_weir)
