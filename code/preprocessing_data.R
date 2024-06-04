# clear environment
rm(list = ls())

# load necessary libraries
library(readxl) # read in data
library(plyr) # plyr
library(dplyr) # data wrangling
library(data.table) # data wrangling

# read file of all HR NI margins
HR_margins_new <- read_excel('C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/NI_margins.xlsx', 
                             sheet = 'HR', 
                             col_types = c('numeric', 'numeric', 'text', 
                                           'text', 'numeric', 'text', 'numeric', 
                                           'text', 'text', 'text', 'skip', 'skip', 
                                           'skip', 'skip'))
names(HR_margins_new) <- c('id', 'time_hor', 'unit', 'adjusted', 'NI_margin', 'onetwosided', 'CI_perc', 'upperlower', 'outcome_type', 'posneg')

HR_margins_Weir <- read_excel('C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/NI_margins.xlsx', 
                              sheet = 'HR_Weir', 
                              col_types = c('numeric', 'numeric', 'skip', 'numeric', 'text', 'text',
                                            'numeric', 'text', 'numeric', 'text', 
                                            'skip', 'numeric', 'skip', 'skip',
                                            'skip', 'skip')) %>%
  filter(!is.na(id))
names(HR_margins_Weir) <- c('id_weir', 'id', 'time_hor', 'unit', 'adjusted', 'NI_margin', 'onetwosided', 'CI_perc', 'upperlower', 'converted_DRMST')

HR_margins <- rbind(select(HR_margins_new, id, time_hor, unit, adjusted, NI_margin, onetwosided, CI_perc, upperlower),
                    select(HR_margins_Weir, id, time_hor, unit, adjusted, NI_margin, onetwosided, CI_perc, upperlower)) %>%
  mutate(original_margin = 'HR',
         id = as.numeric(id))

# read file of all DS NI margins
DS_margins_new <- read_excel('C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/NI_margins.xlsx',
                             sheet = 'DS', 
                             col_types = c('numeric', 'numeric', 'text', 
                                           'text', 'numeric', 'text', 'numeric', 
                                           'text', 'text', 'text', 'skip', 'skip', 
                                           'skip'))
names(DS_margins_new) <- c('id', 'time_hor', 'unit', 'adjusted', 'NI_margin', 'onetwosided', 'CI_perc', 'upperlower', 'outcome_type', 'posneg')
DS_margins_Weir <- read_excel('C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/NI_margins.xlsx', 
                              sheet = 'DS_Weir', 
                              col_types = c('numeric', 'numeric', 'skip', 'numeric', 'text', 
                                            'text', 'numeric', 'text', 'numeric', 
                                            'text', 'numeric', 'numeric', 'skip', 
                                            'skip', 'skip', 'skip')) %>%
  filter(!is.na(id))
names(DS_margins_Weir) <- c('id_weir', 'id', 'time_hor', 'unit', 'adjusted', 'NI_margin', 'onetwosided', 'CI_perc', 'upperlower', 'converted_HR', 'converted_DRMST')
DS_margins_Weir <- DS_margins_Weir %>%
  mutate(converted_DRMST = ifelse(unit == 'd', converted_DRMST*365, converted_DRMST),
         converted_DRMST = ifelse(unit == 'w', converted_DRMST*52, converted_DRMST),
         converted_DRMST = ifelse(unit == 'm', converted_DRMST*12, converted_DRMST))

DS_margins <- rbind(select(DS_margins_new, id, time_hor, unit, adjusted, NI_margin, onetwosided, CI_perc, upperlower),
                    select(DS_margins_Weir, id, time_hor, unit, adjusted, NI_margin, onetwosided, CI_perc, upperlower))  %>%
  mutate(original_margin = 'DS',
         NI_margin = NI_margin/100,
         NI_margin = - abs(NI_margin), # make all margins negative
         id = as.numeric(id))

# read file of all DRMST NI margins
DRMST_margins <- read_excel('C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/NI_margins.xlsx',
                            sheet = 'DRMST', 
                            col_types = c('numeric', 'numeric', 'text', 
                                          'text', 'numeric', 'text', 'numeric', 
                                          'text', 'text', 'text', 'skip', 'skip', 
                                          'skip'))
names(DRMST_margins) <- c('id', 'time_hor', 'unit', 'adjusted', 'NI_margin', 'onetwosided', 'CI_perc', 'upperlower', 'outcome_type', 'posneg')

DRMST_margins <- DRMST_margins %>%
  mutate(original_margin = 'DRMST',
         NI_margin = - abs(NI_margin), # make all margins negative
         id = as.numeric(id))

# read in the reconstructed study data 
paths <- c('C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/01_2024_Min/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/02_2024_Plante/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/03_2023_Mehra/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/04_2024_Varcoe/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/05_2021_Turkova',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/06_2023_Lincoff/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/07_2023_Coles/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/08_2022_Ruff/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/09_2023_Schroder/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/10_2023_Limaye/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/11_2023_Schrag/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/12_2023_Pan/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/13_2023_Tsuyoshi/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/14_2023_Diletti/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/15_2023_Yuan/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/16_2023_Brown/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/17_2023_Nasser/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/20_2023_Mao/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/23_2022_Connolly/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/24_2022_Tang/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/26_2022_Tie/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/27_2022_UKTAVItrialinvestigators/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/28_2022_Saji/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/29_2022_Devereaux/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/30_2022_Fearon/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/31_2022_Tang/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/32_2022_Yu/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/33_2022_Ytterberg (MACE)/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/34_2022_Ytterberg (cancer)/',
           'C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/35_2022_Huang')
files <- file.path(paths, 'reconstructed_data.txt')
reconstructed_new <- rbindlist(lapply(files, fread), fill = TRUE)

reconstructed_weir <- fread("C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/36to70_Weir/all_reconstructed_data_corrected02MAY2024.txt")
key_weir <- read_excel("C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/NI_margins.xlsx", 
                       sheet = "key_reconstructed_Weir", col_types = c("skip", "numeric", "numeric"))
reconstructed_weir <- left_join(reconstructed_weir, key_weir, by = c('ID' = 'ID_Weir')) %>%
  mutate(ID = NULL,
         trialname = NULL,
         Arm = Arm + 1)# make our and their coding of treatment arm identical
names(reconstructed_weir) <- c('time', 'surv', 'arm', 'study_nr')
reconstructed <- rbind(reconstructed_new, reconstructed_weir)

# read in 
p0_new <- read_excel("C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/NI_margins.xlsx", 
                     sheet = "nr_control", col_types = c("numeric", 
                                                         "skip", "skip", "skip", "skip", "skip", 
                                                         "numeric", "numeric", "skip", "numeric", 
                                                         "skip", "numeric", "numeric", "skip", 
                                                         "skip", "skip", 'numeric', 'numeric'))
p0_weir <- read_excel("C:/Users/rmjwlsd/OneDrive - University College London/literature_search_outcomes/NI_margins.xlsx", 
                      sheet = "nr_control_Weir", col_types = c("skip", 
                                                               "numeric", "skip", "skip", "skip", 
                                                               "skip", "skip", "skip", "numeric", 
                                                               "numeric", "skip", "numeric", "skip", 
                                                               "numeric", "numeric", "skip", "skip", 
                                                               "skip", 'numeric', 'numeric'))
p0 <- rbind(p0_new, p0_weir) %>%
  mutate(lambda = ifelse(is.na(lambda), -log(1-expected_rate)/expected_time, lambda), # calculate lambdas 
         expected_rate = NULL,
         expected_time = NULL)

# add max time if tau =/= max time 
HR_margins <- left_join(HR_margins, select(p0, id, max_time))
#DS_margins <- left_join(DS_margins, select(p0, id, max_time)) # only relevant if orig NI margin was HR
#DRMST_margins <- left_join(DRMST_margins, select(p0, id, max_time)) ## is only on in this df and not applic.

# clean up environment
rm(files, paths, reconstructed_new, reconstructed_weir, p0_new, p0_weir, key_weir)

# export data 
#library(writexl)
#write_xlsx(reconstructed[reconstructed$study_nr %in% 1:35,], 'C:/Users/rmjwlsd/OneDrive - University College London/analyses/reconstructed.xlsx')
#write.table(reconstructed, 
#            'C:/Users/rmjwlsd/OneDrive - University College London/analyses/reconstructed.txt', 
#            append = FALSE, sep = " ", dec = ".",
#            row.names = FALSE, col.names = TRUE)
