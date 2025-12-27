#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

# load packages
library(rblimp)
library(ggplot2)

#------------------------------------------------------------------------------#
# READ DATA ----
#------------------------------------------------------------------------------#

# load data
connect <- url("https://raw.githubusercontent.com/craigenders/mlm/main/PainDiaryData.RData", "rb")
load(connect); close(connect)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# COMBINED MODEL SPECIFICATION ----
#------------------------------------------------------------------------------#

# empty model to estimate icc (combined-model specification)
model1 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',
  model = 'PosAffect ~ intercept | intercept',
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model1)

#------------------------------------------------------------------------------#
# GRAPHING RESIDUALS FROM MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# empty model to estimate icc (combined-model specification)
model2 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',
  model = 'PosAffect ~ intercept | intercept',
  seed = 90291,
  burn = 10000,
  iter = 10000,
  nimps = 20)

# print output
output(model2)

# get variable names from the imputed data sets
names(model2)

# plot multiply imputed residuals
univariate_plot(vars = c('PosAffect[Person]','PosAffect.residual'), 
                model = model2,
                stats = T)

#------------------------------------------------------------------------------#
# LATENT VARIABLE SPECIFICATION (MLSEM) ----
#------------------------------------------------------------------------------#

# empty model to estimate icc (alternate level-1 and level-2 latent variable specification)
model3 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',
  latent = 'Person = beta0j',
  model = '
  beta0j ~ intercept;
  PosAffect ~ intercept@beta0j',
  parameters = 'icc = beta0j.totalvar / (beta0j.totalvar + PosAffect.totalvar)',
  seed = 90291,
  burn = 10000,
  iter = 20000
)

# print output
output(model3)


