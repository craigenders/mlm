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
filepath <- 'https://raw.githubusercontent.com/craigenders/mlm/main/data/ClinicalTrialData.csv'

# create data frame from github data
ClinicalTrial <- read.csv(filepath, stringsAsFactors = T)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# REPEATED MEASURES ANALYSIS ----
#------------------------------------------------------------------------------#

# repeated measures model
model1 <- rblimp(
  data = ClinicalTrial,
  clusterid = 'Person', 
  nominal = 'Week',
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
  data = ClinicalTrial,
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
# GROUP-BY-TIME REPEATED MEASURES ANALYSIS ----
#------------------------------------------------------------------------------#

# repeated measures model
model3 <- rblimp(
  data = ClinicalTrial,
  clusterid = 'Person',  
  nominal = 'Week',
  ordinal = 'Drug',
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
# LINEAR GROWTH MODEL W TIME-INVARIANT PREDICTOR ----
#------------------------------------------------------------------------------#

# linear growth model
model4 <- rblimp(
  data = ClinicalTrial,
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

# plot group-specific trajectories
simple_plot(Severity ~ Time | Drug, model = model4)

# plot growth curve and individual trajectories
bivariate_plot(Severity.predicted ~ Week | Drug, 
               model = model4, 
               lines = T, 
               points = F)

# plot standardized residuals by time
bivariate_plot(Severity.residual ~ Week, 
               model = model2, 
               discrete_x = 'Week')

#------------------------------------------------------------------------------#
# QUADRATIC GROWTH MODELS ----
#------------------------------------------------------------------------------#

# quadratic growth model
model5 <- rblimp(
  data = ClinicalTrial,
  clusterid = 'Person',  
  transform = 'Time = Week - 2',
  model = 'Severity ~ intercept Time Time^2 | intercept Time',  
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model5)

# plot growth curve and individual trajectories
bivariate_plot(Severity.predicted ~ Week, 
               model = model5, 
               lines = T, 
               points = F)

# quadratic growth model with predictor
model6 <- rblimp(
  data = ClinicalTrial,
  clusterid = 'Person',  
  ordinal = 'Drug',
  transform = 'Time = Week - 2',
  model = 'Severity ~ intercept Time Time^2 Drug Drug*Time Drug*Time^2 | intercept Time',  
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model6)

# plot growth curve and individual trajectories
bivariate_plot(Severity.predicted ~ Week | Drug, 
               model = model6, 
               lines = T, 
               points = F)

#------------------------------------------------------------------------------#
# PIECEWISE GROWTH MODELS ----
#------------------------------------------------------------------------------#

# piecewise growth model
model7 <- rblimp(
  data = ClinicalTrial,
  clusterid = 'Person',  
  transform = '
    Time = Week - 2;
    Phase1 = ifelse(Time < 0, Time, 0);
    Phase2 = ifelse(Time < 0, 0, Time)',
  model = 'Severity ~ intercept Phase1 Phase2 | intercept Phase2',  
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model7)

# plot growth curve and individual trajectories
bivariate_plot(Severity.predicted ~ Week, 
               model = model7, 
               discrete_x = 'Week',
               lines = T, 
               points = F)

# piecewise growth model with predictor
model8 <- rblimp(
  data = ClinicalTrial,
  clusterid = 'Person', 
  ordinal = 'Drug',
  transform = '
    Time = Week - 2;
    Phase1 = ifelse(Time < 0, Time, 0);
    Phase2 = ifelse(Time < 0, 0, Time)',
  model = 'Severity ~ intercept Phase1 Phase2 Drug Phase1*Drug Phase2*Drug | intercept Phase2',  
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model8)

# plot growth curve and individual trajectories
bivariate_plot(Severity.predicted ~ Week | Drug, 
               model = model8, 
               discrete_x = 'Week',
               lines = T, 
               points = F)



