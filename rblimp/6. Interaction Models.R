# install.packages('remotes')
# install.packages('ggplot2')
# remotes::install_github('blimp-stats/rblimp')

# load packages
library(rblimp)
library(ggplot2)

# load data
connect <- url("https://raw.githubusercontent.com/craigenders/mlm/main/data/EmployeeSatisfactionData.RData", "rb")
load(connect); close(connect)

write.csv(Employee, file = '~/desktop/Employee.csv', row.names = F)

# load misc functions
source("https://raw.githubusercontent.com/craigenders/mlm/main/mlm-functions.R")

# random slope model
model1 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team',    
  center = 'groupmean = LMX; grandmean = Male LMX.mean Climate',   
  model = 'Empower ~ intercept LMX Male LMX.mean Climate | intercept LMX',  
  seed = 90291,
  burn = 10000,
  iter = 20000)

# summarize results and plot parameter distributions
output(model1)
posterior_plot(model1,'Empower')

# plot ols lmx slopes
slopes_by_cluster(data = Employee, y = 'Empower', x = 'LMX', lev2id = 'Team', numlines = 50)

# disaggregated interaction effects
model2 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team',    
  center = 'groupmean = LMX; grandmean = Male LMX.mean Climate',   
  model = 'Empower ~ LMX Male LMX.mean Climate LMX*Climate LMX.mean*Climate | LMX', 
  seed = 90291,
  burn = 10000,
  iter = 20000)

# summarize results and plot parameter distributions
output(model2)
posterior_plot(model2,'Empower')

# disaggregated interaction effects with simple intercepts and slopes
model3 <- rblimp(
  data = Employee,
  nominal = 'Male',
  clusterid = 'Team',    
  center = 'groupmean = LMX; grandmean = Male LMX.mean Climate',   
  model = 'Empower ~ LMX Male LMX.mean Climate LMX*Climate LMX.mean*Climate | LMX', 
  simple = 'LMX | Climate',
  seed = 90291,
  burn = 10000,
  iter = 20000)

# summarize results and plot parameter distributions
output(model3)
posterior_plot(model3,'Empower')
simple_plot(Empower ~ LMX | Climate, model3)



