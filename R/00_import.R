# 00_import: Import participant data from processed WISP files
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up #####################################################################

# load packages
library(magrittr)  # for pipes
library(tibble)    # for tidy datasets
library(tidyr)     # for reshaping datasets
library(readxl)    # for importing Excel data
library(lubridate) # for working with dates
library(dplyr)     # for data manipulation
library(purrr)     # for working with lists
library(stringr)   # to transform dominance scores

# set paths
file.paths <- list.files('Data/Test', pattern = '_Headturn', full.names = TRUE)  # get file names

# set parameters
min.mon <- 80 # minimum degree of exposure to L1 for monolinguals
max.bil <- 80 # maximun degree of exposure to L2 for bilinguals

#### import demographic data ####################################################
demo.data  <- # demographic data
  read_excel("Data/demo_data.xlsx", sheet = "participants", na = c('NA')) %>%
  select(
    participant,
    dominance,
    birth_date,
    test_date,
    gestation_weeks,
    birth_weight,
    mother_education,
    father_education,
    inclusion_status,
    rejection_verbose
  ) %>%
  mutate(
    birth_date = as_date(birth_date),
    test_date  = as_date(test_date)
  )

#### import test data ############################################################

# names for columns
col.names <- c("trial", "phase", "item", "location", "block", "time", "looksaway", "prelook",
               "postlook", "protocol", "participant", "tester", "sex", "age", "comments")
# classes for columns
col.classes <- c('numeric', 'numeric', 'factor', "factor", "character", "numeric", "numeric", "numeric",
                 'numeric', 'character', 'character', "character", "numeric", "character")

test.data <-
  map(
    file.paths,
    read.table,                                     
    sep              = '\t',
    header           = FALSE,
    col.names        = col.names,
    row.names        = NULL,
    na.strings       = c(' ', '<NA>', 'NA', '\t'),
    stringsAsFactors = FALSE,
    skip             = 2, # some rows within the first two are missing in some files. Since we are not interested in familiarisation phase, we just don't read them.
  ) %>%
  bind_rows() %>%  # create a long version of the data and export it as .txt
  drop_na(block) %>% # clean blank rows
  separate(protocol, c('language', 'condition'), sep = '_') %>%
  filter(phase == 3) %>% # get only test trials (we are not interested in familiarization data)
  select( # select and reorder variables  variables of interest
    participant,
    age,
    sex,
    language,
    condition,
    tester,
    block,
    trial,
    item,
    location,
    time,
    looksaway,
    prelook,
    postlook,
    comments
  ) %>%
  mutate( # make sure sex is coded consistently
    sex = case_when(
      sex == 'm' | sex == 'male'   ~ 'male',
      sex == 'f' | sex == 'female' ~ 'female',
      TRUE                         ~ sex)
  )

#### merge demographic and test data ############################################
data <-
  left_join(test.data, demo.data, by = 'participant') %>% # join demographic data and loking times
  mutate( # rename items
    item = case_when(
      item == 1 ~ 'gon',
      item == 2 ~ 'mus',
      item == 3 ~ 'for',
      item == 4 ~ 'pul',
      TRUE      ~ 'NA'
    ),
    profile = case_when( # recode linguistic profile as categorical
      dominance > min.mon*100 | dominance == 1000 ~ 'monolingual',
      dominance <= max.bil*100                    ~ 'bilingual',
      TRUE                                        ~ 'other'
    ),
    dominance = as.numeric(substr(as.character(dominance), start = 1, stop = 2)),
    dominance = case_when(dominance < 50 ~ 100,
                          TRUE ~ dominance),
    trial_type = case_when( # code familiarity with the item at test
      condition == 'gonmus' & item %in% c('gon', 'mus') | condition == 'forpul' & item %in% c('for', 'pul') ~ 'familiar',
      TRUE                                                                                                  ~ 'novel')
  ) %>% select( # select and reorder varibales of interest
    participant,
    block,
    trial,
    trial_type,
    item,
    location,
    time,
    looksaway,
    prelook,
    postlook,
    language,
    condition,
    tester,
    profile,
    dominance,
    sex,
    age,
    birth_date,
    test_date,
    gestation_weeks,
    birth_weight,
    mother_education,
    father_education,
    inclusion_status,
    rejection_verbose,
    comments
  ) %>%
  as_tibble()

#### export data ################################################################
write.table(data, 'Data/00_raw.txt', sep = '\t', dec = '.', row.names = FALSE) # export data








