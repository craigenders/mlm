# MULTILEVEL MODEL WITH LAGGED (DYNAMIC) EFFECTS

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

library(rblimp)
set_blimp('/applications/blimp/blimp-nightly')

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
data_url <- 'https://raw.githubusercontent.com/blimp-stats/blimp-book/main/data/diary.csv'

# create data frame from github data
diary <- read.csv(data_url)

#------------------------------------------------------------------------------#
# AR1 MODEL ----
#------------------------------------------------------------------------------#

# dsem with lagged predictors
model1 <- rblimp(
  data = diary,
  clusterid = 'person', 
  timeid = 'day',
  latent = 'person = b0i b1i;',
  model = '
        posaff_lag = posaff.lag - b0i;
        posaff ~ intercept@b0i posaff_lag@b1i;
        intercept -> b0i b1i;
        b0i ~~ b1i;',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model1)
