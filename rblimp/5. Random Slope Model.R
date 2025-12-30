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
# ESTIMATE ICCS ----
#------------------------------------------------------------------------------#

# estimate icc for each level-1 variable
model1 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team',
  model = '{ Empower LMX Male } ~ intercept | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

# print output
output(model1)

#------------------------------------------------------------------------------#
# RANDOM INTERCEPT MODEL ----
#------------------------------------------------------------------------------#

# random intercept model
model2 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team',
  center = 'groupmean = LMX; grandmean = LMX.mean Climate',
  model = 'Empower ~ intercept LMX Male LMX.mean Climate | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

# print output
output(model2)

#------------------------------------------------------------------------------#
# RANDOM SLOPE MODELS ----
#------------------------------------------------------------------------------#

# random slope for lmx
model3 <- rblimp(
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
output(model3)

# chi-bar significance test of random slope
chibar_test(model = model3, DV = 'Empower', IV = 'LMX')

# random slope for sex
model4 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team',
  center = 'groupmean = LMX; grandmean = LMX.mean Climate',
  model = 'Empower ~ intercept LMX Male LMX.mean Climate | intercept Male;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

# print output
output(model4)

# chi-bar significance test of random slope
chibar_test(model = model4, DV = 'Empower', IV = 'Male')

#------------------------------------------------------------------------------#
# FINAL MODEL ----
#------------------------------------------------------------------------------#

model5 <- rblimp(
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
output(model5)

#------------------------------------------------------------------------------#
# GRAPHING RESIDUALS FROM MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# final model with 20 data sets
model6 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team',
  center = 'groupmean = LMX; grandmean = LMX.mean Climate',
  model = 'Empower ~ intercept LMX Male LMX.mean Climate | intercept LMX;',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model6)

# print output
output(model6)

# get variable names from the imputed data sets
names(model6)

# plot multiply imputed residuals
univariate_plot(vars = c('Empower[Team]','Empower$LMX[Team]','Empower.residual'), 
                model = model6,
                stats = T)

#------------------------------------------------------------------------------#
# LATENT VARIABLE SPECIFICATION (MLSEM) ----
#------------------------------------------------------------------------------#

model7 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team', 
  latent = 'Team = beta0j beta1j',   
  center = 'groupmean = LMX; grandmean = LMX.mean Climate;',  
  model = '
    beta0j ~ intercept LMX.mean Climate;
    beta1j ~ intercept;
    beta0j ~~ beta1j;    # correlate random intercepts and slopes
    Empower ~ intercept@beta0j LMX@beta1j Male;',  
  seed = 90291,
  burn = 10000,
  iter = 10000)
output(model7)
