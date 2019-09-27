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
library(car)      # fir checking assumptions
library(TOSTER)   # for equivalence testing

# set statistical parameters
alpha <- 0.05 # significance criterion
beta  <- 0.80 # desired statistical power
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

tost.table <- data.frame(
  test = c(as.character(tost$tost$asDF$`b[0]`), as.character(tost$tost$asDF$`b[1]`), as.character(tost$tost$asDF$`b[2]`)),
  t    = c(tost$tost$asDF$`t[0]`, tost$tost$asDF$`t[1]`, tost$tost$asDF$`t[2]`),
  df   = c(tost$tost$asDF$`df[0]`, tost$tost$asDF$`df[1]`, tost$tost$asDF$`df[2]`),
  p    = c(tost$tost$asDF$`p[0]`, tost$tost$asDF$`p[1]`, tost$tost$asDF$`p[2]`),
  d    = c(tost$tost$asDF$`t[0]`/sqrt(n), tost$tost$asDF$`t[1]`/sqrt(n), tost$tost$asDF$`t[2]`/sqrt(n))
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
write.table(tost.table, 'Data/04_analysis.txt', sep = '\t', dec = '.')
