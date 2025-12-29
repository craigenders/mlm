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
filepath <- 'https://raw.githubusercontent.com/craigenders/mlm/main/data/PainDiary.csv'

# create data frame from github data
PainDiary <- read.csv(filepath, stringsAsFactors = T)

# plotting functions
source('https://raw.githubusercontent.com/blimp-stats/blimp-book/main/misc/functions.R')

#------------------------------------------------------------------------------#
# ESTIMATE ICCS ----
#------------------------------------------------------------------------------#

# estimate icc for each level-1 variable
model1 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',
  model = '{ PosAffect SleepQual Pain } ~ intercept | intercept;',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

# print output
output(model1)

#------------------------------------------------------------------------------#
# WITHIN-CLUSTER PREDICTORS ----
#------------------------------------------------------------------------------#

# within-person predictors and latent group mean centering
model2 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'groupmean = SleepQual Pain',  
  model = 'PosAffect ~ intercept SleepQual Pain | intercept',   
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model2)

#------------------------------------------------------------------------------#
# BETWEEN-CLUSTER PREDICTORS ----
#------------------------------------------------------------------------------#

# add level-2 predictors
model3 <- rblimp(
  data = PainDiary,
  nominal = 'Female',
  clusterid = 'Person', 
  center = 'grandmean = SleepQual.mean Pain.mean Stress; groupmean = SleepQual Pain',  
  model = 'PosAffect ~ intercept SleepQual Pain SleepQual.mean Pain.mean Female Stress | intercept',   
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model3)

#------------------------------------------------------------------------------#
# LATENT VARIABLE SPECIFICATION (MLSEM) ----
#------------------------------------------------------------------------------#

# within-person predictors and latent group mean centering
model4 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',  
  latent = 'Person = beta0j', 
  center = 'groupmean = SleepQual Pain',    
  model = '
   beta0j ~ intercept;
   PosAffect ~ intercept@beta0j SleepQual Pain',   
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model4)

# add level-2 predictors
model5 <- rblimp(
  data = PainDiary,
  nominal = 'Female',  
  clusterid = 'Person',  
  latent = 'Person = beta0j', 
  center = 'groupmean = SleepQual Pain; grandmean = SleepQual.mean Pain.mean Stress',    
  model = '
   beta0j ~ intercept SleepQual.mean Pain.mean Female Stress;
   PosAffect ~ intercept@beta0j SleepQual Pain',   
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model5)