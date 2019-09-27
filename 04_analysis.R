# 04_analysis: significance testing
# Gonzalo García-Castro, gonzalo.garciadecastro@upf.edu
# Center for Brain and Cognition, Pompeu Fabra University

#### set up #############################################

# load packages
library(magrittr) # for working with pipes
library(dplyr)    # for manipulating data
library(purrr)    # for working with lists
library(tidyr)    # for reshaping datasets
library(tibble)   # for tidy data presentation
library(lme4)     # for LMEM
library(car) # fir p-values
library(TOSTER)   # for equivalence testing

# set statistical parameters
alpha <- 0.05 # significance criterion
beta  <- 0.70 # desired statistical power
sesoi <- 0.50 # smallest effect size of interest (SESOI)

#### import data ##############################################

# participant-level long data ready for main analysis
data <- read.delim('Data/02_aggregated.txt') %>% # wide data
  as_tibble() %>%
  select(participant, familiar, novel) %>%
  gather(key = 'trial_type', value = 'time', -participant) %>%
  mutate(trial_type = as.factor(trial_type))

#### significance testing #####################################

# equivalence testing
tost <- data %>%
  spread(trial_type, time) %>%
  dataTOSTpaired(
    data         = .,
    pairs        = list(c(i1 = 'familiar', i2 = 'novel')),
    low_eqbound  = -sesoi,
    high_eqbound = sesoi,
    alpha        = alpha,
    desc         = TRUE,
    plots        = TRUE
  )

#### power analysis ###################################################
n <- nrow(data)

power <- powerTOSTpaired(
  alpha           = alpha, 
  N               = n/2,
  low_eqbound_dz  = -sesoi,
  high_eqbound_dz = sesoi
)

#### check assumptions ################################################
residuals <- residuals(lme4::lmer(time ~ trial_type + (1|participant), data = data))

# normality of residuals
hist(residuals)         # check distribution of residuals
qqnorm(residuals)       # check residuals' fit to the normal distribution
shapiro.test(residuals) # test normality of residuals 

# homocedasticity
leveneTest(time ~ trial_type, center = mean, data = data)

#### export data #######################################################
write.table(residuals, 'Data/04_analysis-residuals.txt', sep = '\t', dec = '.')
write.table(tost$tost, 'Data/04_analysis-tost.txt', sep = '\t', dec = '.')
