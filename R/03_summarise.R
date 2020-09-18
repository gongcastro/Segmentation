# 03_summarise: Summarise data by condition
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up ###############################################

# load packages
library(tibble)   # for tidy data presentation
library(dplyr)    # for data manipulation
library(magrittr) # for using pipes
library(tidyr)    # for reshaping datasets
library(here)     # for locating files

# specify parameters
alpha <- 0.05 # significance criterion

#### import data ###########################################
data.long <- read.table(here("Data", "01_processed.txt"), sep = "\t") %>% as_tibble()
data.wide <- read.table(here("Data", "02_aggregated.txt"), sep = "\t") %>% as_tibble()

#### aggregate data by trial ###############################
summary <-
  data.long %>%
  group_by(participant, language, condition, block, trial_type) %>%
  summarise(time = mean(time, na.rm = TRUE)) %>%
  group_by(language, condition, block, trial_type) %>%
  summarise(n         = n(),
            mean      = mean(time, na.rm = TRUE),
            median    = median(time, na.rm = TRUE),
            sd        = sd(time, na.rm = TRUE),
            sem       = sd/sqrt(n),
            ci_lower  = mean-abs(qnorm(alpha/2))*sem,
            ci_high   = mean+abs(qnorm(alpha/2))*sem) %>%
  ungroup()

#### export data ###########################################
write.table(summary, here("Data", "03_summary.txt"), sep = "\t", dec = ".")

