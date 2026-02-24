#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

# load packages
library(ggplot2)
library(rblimp)

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# github url for raw data
filepath <- 'https://raw.githubusercontent.com/craigenders/mlm/main/data/Employee.csv'

# create data frame from github data
Employee <- read.csv(filepath, stringsAsFactors = T)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# RANDOM SLOPE MODEL ----
#------------------------------------------------------------------------------#

# random slope for lmx
model1 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team',
  center = 'groupmean = LMX; grandmean = LMX.mean Climate',
  model = 'Empower ~ intercept LMX Male LMX.mean Climate | intercept LMX;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

# print output
output(model1)

#------------------------------------------------------------------------------#
# MODERATED MODEL WITH CROSS-LEVEL AND BETWEEN-LEVEL INTERACTIONS ----
#------------------------------------------------------------------------------#

# interactive model
model2 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team',    
  center = 'groupmean = LMX; grandmean = Male LMX.mean Climate',   
  model = 'Empower ~ LMX Male LMX.mean Climate LMX*Climate LMX.mean*Climate | LMX', 
  simple = 'LMX | Climate; LMX.mean | Climate;',
  seed = 90291,
  burn = 10000,
  iter = 20000)

# print output
output(model2)

# plot simple slopes
simple_plot(Empower ~ LMX | Climate, model = model2)



