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
# ESTIMATE ICCS ----
#------------------------------------------------------------------------------#

# estimate icc for each variable
model1 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',
  model = '
    PosAffect ~ intercept | intercept;
    SleepQual ~ intercept | intercept',
  seed = 90291,
  burn = 10000,
  iter = 10000
)

# print output
output(model1)

#------------------------------------------------------------------------------#
# ESTIMATE LEVEL-1 SLOPE ----
#------------------------------------------------------------------------------#

# within-person sleep as a predictor
model2 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'groupmean = SleepQual',  
  model = 'PosAffect ~ intercept SleepQual | intercept',   
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model2)

#------------------------------------------------------------------------------#
# ESTIMATE LEVEL-2 SLOPE ----
#------------------------------------------------------------------------------#

# between-person sleep as a predictor
model3 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'grandmean = SleepQual.mean',  
  model = 'PosAffect ~ intercept SleepQual.mean | intercept',   
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model3)

#------------------------------------------------------------------------------#
# ESTIMATE LEVEL-1 AND LEVEL-2 SLOPES ----
#------------------------------------------------------------------------------#

# disaggregated model with within-person and between-person sleep as a predictor
model4 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'grandmean = SleepQual.mean; groupmean = SleepQual',  
  model = 'PosAffect ~ intercept SleepQual SleepQual.mean | intercept',   
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model4)

#------------------------------------------------------------------------------#
# LATENT VARIABLE SPECIFICATION (MLSEM) ----
#------------------------------------------------------------------------------#

# within-person sleep as a predictor
model5 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',  
  latent = 'Person = beta0j', 
  center = 'groupmean = SleepQual',    
  model = '
   beta0j ~ intercept;
   PosAffect ~ intercept@beta0j SleepQual',   
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model5)

# disaggregated model with within-person and between-person sleep as a predictor
model6 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',  
  latent = 'Person = beta0j', 
  center = 'groupmean = SleepQual; grandmean = SleepQual.mean',    
  model = '
   beta0j ~ intercept SleepQual.mean;
   PosAffect ~ intercept@beta0j SleepQual',   
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model6)

# between-person sleep as a predictor
model7 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',  
  latent = 'Person = beta0j', 
  center = 'grandmean = SleepQual.mean',    
  model = '
   beta0j ~ intercept SleepQual.mean;
   PosAffect ~ intercept@beta0j',   
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model7)


