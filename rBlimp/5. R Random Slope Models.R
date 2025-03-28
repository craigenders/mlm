# install.packages('remotes')
# install.packages('ggplot2')
# remotes::install_github('blimp-stats/rblimp')

# load packages
library(rblimp)
library(ggplot2)

# load data
connect <- url("https://raw.githubusercontent.com/craigenders/mlm/main/EmployeeSatisfactionData.RData", "rb")
load(connect); close(connect)

# load misc functions
source("https://raw.githubusercontent.com/craigenders/mlm/main/mlm-functions.R")
source("https://raw.githubusercontent.com/craigenders/rblimp-adds/main/ChiBarWaldTest.R")

# boxplots of raw data by cluster
boxplots_by_cluster(data = Employee, var2plot = "JobSat", lev2id = "Team", numboxes = 20)
boxplots_by_cluster(data = Employee, var2plot = "Empower", lev2id = "Team", numboxes = 20)
boxplots_by_cluster(data = Employee, var2plot = "LMX", lev2id = "Team", numboxes = 20)
boxplots_by_cluster(data = Employee, var2plot = "Male", lev2id = "Team", numboxes = 20)

################################################################
# combined-model specification
################################################################

# icc
model1 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team',    
  model = '{ JobSat LMX Empower Male } ~ intercept | intercept',  
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model1)

# add level-1 predictors
model2 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team', 
  center = 'groupmean = LMX Empower; grandmean = Male',   
  model = 'JobSat ~ intercept LMX Empower Male | intercept',  
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model2)

# add random slope for lmx
model3 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team', 
  center = 'groupmean = LMX Empower; grandmean = Male',   
  model = 'JobSat ~ intercept LMX Empower Male | intercept LMX',  
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model3)

# test random slope variance
chibar_test_slopes(model3, testvars = c("LMX"))

# add random slope for empowerment
model4 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team',    
  center = 'groupmean = LMX Empower; grandmean = Male',   
  model = 'JobSat ~ intercept LMX Empower Male | intercept Empower',
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model4)

# test random slope variance
chibar_test_slopes(model4, testvars = c("Empower"))

# add both random slopes
model5 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team',    
  center = 'groupmean = LMX Empower; grandmean = Male',   
  model = 'JobSat ~ intercept LMX Empower Male | intercept LMX Empower',  
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model5)

# test random slope variances
chibar_test_slopes(model5, testvars = c("LMX","Empower"))

# final model
model6 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team',    
  center = 'groupmean = LMX Empower; grandmean = Male LMX.mean Empower.mean',   
  model = 'JobSat ~ intercept LMX Empower Male LMX.mean Empower.mean | intercept Empower',  
  seed = 90291,
  burn = 10000,
  iter = 30000)
output(model6)

# latent variable specification for final model
model7 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team',
  latent = 'Team = beta0j beta2j',
  center = 'groupmean = LMX Empower; grandmean = Male LMX.mean Empower.mean',   
  model = '
    beta0j ~ intercept LMX.mean Empower.mean;
    beta2j ~ intercept;
    beta0j ~~ beta2j; # correlate random intercepts and slopes
    JobSat ~ intercept@beta0j LMX Empower@beta2j Male',  
  seed = 90291,
  burn = 20000,
  iter = 30000)
output(model7)

