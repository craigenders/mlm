# install.packages('remotes')
# install.packages('ggplot2')
# remotes::install_github('blimp-stats/rblimp')

# load packages
library(rblimp)
library(ggplot2)

# load data
connect <- url("https://raw.githubusercontent.com/craigenders/mlm/main/rBlimp/PainDiaryData.RData", "rb")
load(connect); close(connect)

# load misc functions
source("https://raw.githubusercontent.com/craigenders/mlm/main/mlm-functions.R")

# boxplots of raw data by cluster
boxplots_by_cluster(data = PainDiary, var2plot = "PosAffect", lev2id = "Person", numboxes = 20)
boxplots_by_cluster(data = PainDiary, var2plot = "SleepQual", lev2id = "Person", numboxes = 20)

################################################################
# combined-model specification
################################################################

# estimate icc for each variable
model1 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',
  model = '
    PosAffect ~ intercept | intercept;
    SleepQual ~ intercept | intercept',
  seed = 90291,
  burn = 10000,
  iter = 20000
)
output(model1)

# within-person sleep as a predictor
model2 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'groupmean = SleepQual',  
  model = 'PosAffect ~ intercept SleepQual | intercept',   
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model2)

# disaggregated model with within-person and between-person sleep as a predictor
model3 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'grandmean = SleepQual.mean; groupmean = SleepQual',  
  model = 'PosAffect ~ intercept SleepQual SleepQual.mean | intercept',   
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model3)

# between-person sleep as a predictor
model4 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'grandmean = SleepQual.mean',  
  model = 'PosAffect ~ intercept SleepQual.mean | intercept',   
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model4)

################################################################
# alternate level-1 and level-2 latent variable specification
################################################################

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
  iter = 20000)
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
  iter = 20000)
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
  iter = 20000)
output(model7)


