# 03_aggregate: Aggregate data by participant
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up #############################################

# load packages
library(magrittr) # for using pipes
library(dplyr)    # for manipulating data
library(tibble)   # for more informative data frames
library(purrr)    # for working with lists
library(readxl)   # for importing Excel files
library(tidyr)    # for reshaping datasets
library(forcats)  # for manipulating categorical variables
library(here)     # for locating files

#### import data #######################################
data <- read.table(here("Data", "01_processed.txt")) %>% as_tibble() # import list of datasets

#### aggregate data ####################################
data <-
  data %>%
  group_by(participant, trial_type) %>%
  summarise(
    time             = mean(time),
    condition        = sample(condition, 1),
    birth_date       = as.Date(sample(birth_date, 1)),
    test_date        = as.Date(sample(test_date, 1)),
    age              = test_date-birth_date,
    sex              = sample(sex, 1),
    profile          = sample(profile, 1),
    dominance        = sample(dominance, 1),
    language         = sample(language, 1),
    weight           = sample(weight, 1),
    mother_education = sample(mother_education, 1),
    father_education = sample(father_education, 1),
    comments         = sample(comments, 1)
  ) %>%
  spread(trial_type, time) %>%
  mutate(
    time       = familiar+novel,                          # overall looking time
    difference = novel-familiar,                          # uncorrected preference score
    preference = ((novel-familiar)/(novel+familiar))*100  # preference for familiar items (%)
  )

#### export data #######################################
write.table(data, "Data/02_aggregated.txt", sep = "\t", dec = ".")
