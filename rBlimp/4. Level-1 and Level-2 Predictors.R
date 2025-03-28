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
boxplots_by_cluster(data = PainDiary, var2plot = "Pain", lev2id = "Person", numboxes = 20)

################################################################
# combined-model specification
################################################################

# estimate icc for each variable
model1 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',
  model = '
    PosAffect ~ intercept | intercept;
    SleepQual ~ intercept | intercept;
    Pain ~ intercept | intercept',
  seed = 90291,
  burn = 10000,
  iter = 20000
)
output(model1)

# within-person sleep as a predictor
model2 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'groupmean = SleepQual Pain',  
  model = 'PosAffect ~ intercept SleepQual Pain | intercept',   
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model2)

# disaggregated model with within-person and between-person sleep as a predictor
model3a <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'grandmean = SleepQual.mean Pain.mean; groupmean = SleepQual Pain',  
  model = 'PosAffect ~ intercept SleepQual Pain SleepQual.mean Pain.mean | intercept',   
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model3a)

# test whether level-2 slopes differ from zero (model2 vs. model1)
model3b <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'grandmean = SleepQual.mean Pain.mean; groupmean = SleepQual Pain',  
  model = 'PosAffect ~ intercept SleepQual Pain SleepQual.mean@b3 Pain.mean@b4 | intercept',   
  waldtest = 'b3:b4 = 0',
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model3b)

# test whether level-2 slopes differ from level-1 slopes (model2 vs. smushed model)
model3c <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'grandmean = SleepQual.mean Pain.mean; groupmean = SleepQual Pain',  
  model = 'PosAffect ~ intercept SleepQual@b1 Pain@b2 SleepQual.mean@b3 Pain.mean@b4 | intercept',   
  waldtest = 'b1 = b3; b2 = b4',
seed = 90291,
burn = 10000,
iter = 20000)
output(model3c)

# add level-2 predictors
model4a <- rblimp(
  data = PainDiary,
  nominal = 'Female',
  clusterid = 'Person', 
  center = 'grandmean = SleepQual.mean Pain.mean Stress; groupmean = SleepQual Pain',  
  model = 'PosAffect ~ intercept SleepQual Pain SleepQual.mean Pain.mean Female Stress | intercept',   
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model4a)

# add level-2 predictors
model4b <- rblimp(
  data = PainDiary,
  nominal = 'Female',
  clusterid = 'Person', 
  center = 'grandmean = SleepQual.mean Pain.mean Stress; groupmean = SleepQual Pain',  
  model = 'PosAffect ~ intercept SleepQual Pain SleepQual.mean Pain.mean Female@b5 Stress@b6 | intercept',   
  waldtest = 'b5:b6 = 0', 
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model4b)

################################################################
# alternate level-1 and level-2 latent variable specification
################################################################

# within-person sleep as a predictor
model5 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',  
  latent = 'Person = beta0j', 
  center = 'groupmean = SleepQual Pain',    
  model = '
   beta0j ~ intercept;
   PosAffect ~ intercept@beta0j SleepQual Pain',   
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model5)

# disaggregated model with within-person and between-person sleep as a predictor
model6 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',  
  latent = 'Person = beta0j', 
  center = 'groupmean = SleepQual Pain; grandmean = SleepQual.mean Pain.mean',    
  model = '
   beta0j ~ intercept SleepQual.mean Pain.mean;
   PosAffect ~ intercept@beta0j SleepQual Pain',   
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model6)

# add level-2 predictors
model7 <- rblimp(
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
  iter = 20000)
output(model7)