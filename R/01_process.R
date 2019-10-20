# 01_process: Process data
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ####################################################

# load packages
library(magrittr) # for using pipes
library(dplyr)    # for manipulating data
library(tibble)   # for more informative data frames
library(tidyr)    # for reshaping datasets
library(purrr)    # for working with lists

# create functions
'%!in%' <- function(x, y) !(x %in% y) # evaluate if x is not in vector y

# set experimental parameters
# participant-level exclusion criteria
min.time      <- 2000       # min. looking time
# trial-level inclusion criteria
min.age       <- 7.5*30-10  # min. age (in days)
max.age       <- 9*30+10    # max  age (in days)
min.trials    <- 8          # min. number of trials
outlier.sd    <- 2.5        # SD to be considered outlier
min.weight    <- 2600       # min. birth weight
min.gestation <- 36         # min. gestation weeks

#### import data ###############################################
data <- read.delim('Data/00_raw.txt') %>% as_tibble()

#### apply trial-level exclusion criteria ######################
# replace unsuccessful trials with successful trials within each participant
# for each failed trial, each participant performed an indentical extra trial to replace the previous one
# this was repeated a maximun number of 4 times
# in this block of code we replace each failed trial with its correspondent extra trial (if successful and available)

trials.remove <- # trials to be replaced
  data %>%
  filter(time < min.time & block < 4) %>% 
  group_by(participant) %>%
  mutate(replacement = row_number()) %>% # index each failed trial with a numeric label
  ungroup()

trials.extra <- # trials to insert
  data %>%
  filter(time >= min.time & block == 4) %>% # extra trials are indexed as part of a 4th block
  rename(time_new = time) %>%
  group_by(participant) %>%
  mutate(replacement_new = row_number()) %>% # index each extra trial with a numeric label (identical to that of its correspondent failed trial)
  select(participant, replacement_new, time_new) %>%
  ungroup()

trials.replaced <- # join failed trials to their correspondent extra trial (if available) and drop the looking time of the former
  right_join(trials.remove, trials.extra, by = c('participant', 'replacement' = 'replacement_new')) %>%
  select(-c(time, replacement)) %>%
  rename(time = time_new)

data <- # add these new trials to the main dataset
  data %>%
  filter(time >= min.time,
         block < 4) %>%
  rbind(trials.replaced)

#### apply participant-level exclusion criteria ###################

# get participants with <= min.trials
id.fail <-
  data %>%
  group_by(participant) %>%
  summarise(n = n()) %>% # count nuber of trials per participant
  filter(n < min.trials) %$%
  participant %>%
  as.vector() # get a vector of labels corresponding to particpants to exclude 

# get participants with outlier scores in total looking time
id.outlier <-
  data %>%
  group_by(participant) %>%
  summarise(time = mean(time, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(time > (mean(data$time) + outlier.sd*mean(data$time)),
         time < (mean(data$time) - outlier.sd*mean(data$time))) %$%
  participant %>%
  as.character()

# is the participant to be excluded for other reasons?
id.other <-
  data %>%
  filter(inclusion_status == 0) %>%
  select(participant, rejection_verbose) %>%
  group_by(participant) %>%
  summarise(rejection_verbose = sample(rejection_verbose, 1))
  
# drop unsuccessful and outlier participants
data <-
  data %>%
  mutate(success_participant = participant %!in% id.fail,
         success_trial        = time >= min.time,
         not_outlier          = participant %!in% id.outlier)

# final filter (by min.trials and outliers)
data <-
  data %>%
  filter(
    gestation_weeks      >= min.gestation,
    birth_weight         >= min.weight,
    age                  >= min.age,
    age                  <= max.age,
    success_participant  == 1,
    success_trial        == 1,
    not_outlier          == 1,
    inclusion_status     == 1
  ) %>%
  select(participant, sex, age, profile, dominance, language, condition, tester,
         block, trial, trial_type, item, location,
         time, looksaway, prelook, postlook,
         birth_date, test_date,
         gestation = gestation_weeks, weight = birth_weight,
         mother_education, father_education,
         comments)
  
# summary of exluded participants
excluded <-
  tibble(participant = c(id.fail, id.outlier)) %>%
  mutate(rejection_verbose = case_when(participant %in% id.fail    ~ 'few_trials',
                                       participant %in% id.outlier ~ 'outlier',
                                       TRUE                        ~ 'other')) %>%
  rbind(id.other) %>%
  mutate(
    few_trials           = case_when(grepl('trials', rejection_verbose)       ~ 1, TRUE ~ 0),
    linguistic_profile   = case_when(grepl('profile', rejection_verbose)      ~ 1, TRUE ~ 0),
    incomplete           = case_when(grepl('incomplete', rejection_verbose)   ~ 1, TRUE ~ 0),
    experimental_failure = case_when(grepl('experiment', rejection_verbose)   ~ 1, TRUE ~ 0),
    interference         = case_when(grepl('interference', rejection_verbose) ~ 1, TRUE ~ 0),
    health_issues        = case_when(grepl('health', rejection_verbose)       ~ 1, TRUE ~ 0)
    )
#### export data #################################################
write.table(data, 'Data/01_processed.txt', sep = '\t', dec = '.')
write.table(excluded, 'Data/01_processed-excluded.txt', sep = '\t', dec = '.')




