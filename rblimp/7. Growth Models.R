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
filepath <- 'https://raw.githubusercontent.com/craigenders/mlm/main/data/TrialData.csv'

# create data frame from github data
Trial <- read.csv(filepath, stringsAsFactors = T)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# REPEATED MEASURES ANALYSIS ----
#------------------------------------------------------------------------------#

# repeated measures model
model1 <- rblimp(
  data = Trial,
  nominal = 'Week',
  clusterid = 'Person',    
  model = 'Severity ~ intercept Week | intercept',  
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model1)

# plot means by time
bivariate_plot(Severity.predicted ~ Week, 
               model = model1, 
               discrete_x = 'Week',
               points = F)

#------------------------------------------------------------------------------#
# LINEAR GROWTH MODEL ----
#------------------------------------------------------------------------------#

# linear growth model
model2 <- rblimp(
  data = Trial,
  clusterid = 'Person',  
  transform = 'Time = Week - 2',
  model = 'Severity ~ intercept Time | intercept Time',  
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# plot growth curve and individual trajectories
bivariate_plot(Severity.predicted ~ Week, 
               model = model2, 
               lines = T, 
               points = F)

# plot standardized residuals by time
bivariate_plot(Severity.residual ~ Week, 
               model = model2, 
               discrete_x = 'Week')

#------------------------------------------------------------------------------#
# REPEATED MEASURES ANALYSIS WITH PREDICTOR ----
#------------------------------------------------------------------------------#

# repeated measures model
model3 <- rblimp(
  data = Trial,
  nominal = 'Week Drug',
  clusterid = 'Person',    
  model = 'Severity ~ intercept Week Drug Week*Drug | intercept',  
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model3)

# plot means by time
bivariate_plot(Severity.predicted ~ Week | Drug, 
               model = model3, 
               discrete_x = 'Week', 
               points = F)

#------------------------------------------------------------------------------#
# LINEAR GROWTH MODEL WITH PREDICTOR ----
#------------------------------------------------------------------------------#

# linear model w predictor
model4 <- rblimp(
  data = Trial,
  clusterid = 'Person', 
  ordinal = 'Drug',
  transform = 'Time = Week - 2',  
  model = 'Severity ~ intercept Time Drug Time*Drug | intercept Time', 
  simple = 'Time | Drug',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model4)

# plot simple slopes
simple_plot(Severity ~ Time | Drug, model4)

# plot standardized residuals by time
bivariate_plot(Severity.residual ~ Week, model = model4, discrete_x = 'Week')

