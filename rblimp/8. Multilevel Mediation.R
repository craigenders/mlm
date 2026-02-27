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
# 1-1-1 MODEL: WITHIN-CLUSTER MEDIATION ----
#------------------------------------------------------------------------------#

# random slope model with mediation at level-1
model1 <- rblimp(
  data = Employee,
  clusterid = 'Team', 
  latent = 'Team = a0j a1j b0j b1j b2j',
  center = 'groupmean = LMX',
  model = '
    level2:   # arbitrary label that groups models together on the output
    intercept –> a0j a1j@a1mean b0j b1j@b1mean b2j;  # add intercept into multiple equations with –>
    a0j ~~ b0j;   
    a1j ~~ b1j@a1b1corr;    
    level1:   # arbitrary label that groups models together on the output
    Empower ~ intercept@a0j LMX@a1j;  
    JobSat ~ intercept@b0j (Empower – a0j)@b1j LMX@b2j;', 
  parameters = '
    a1b1cov = a1b1corr * sqrt(a1j.totalvar * b1j.totalvar); # covariance between A and B random slopes
    indirect_w = a1mean * b1mean + a1b1cov; # within-cluster indirect effect',
  seed = 90291,
  burn = 20000,
  iter = 20000)

output(model1)
posterior_plot(model1,'indirect_w')

#------------------------------------------------------------------------------#
# 1-1-1 MODEL: WITHIN- AND BETWEEN-CLUSTER MEDIATION ----
#------------------------------------------------------------------------------#

# random slope model with mediation at both levels
model2 <- rblimp(
  data = Employee,
  clusterid = 'Team', 
  latent = 'Team = a0j a1j b0j b1j b2j',
  center = 'groupmean = LMX',
  model = '
    level2:   # arbitrary label that groups models together on the output
    intercept –> a0j a1j@a1mean b0j b1j@b1mean b2j;  # add intercept into multiple equations with –>
    a0j ~ LMX.mean@l2apath;
    b0j ~ a0j@l2bpath LMX.mean;
    a1j ~~ b1j@a1b1corr;    
    level1:   # arbitrary label that groups models together on the output
    Empower ~ intercept@a0j LMX@a1j;  
    JobSat ~ intercept@b0j (Empower – a0j)@b1j LMX@b2j;', 
  parameters = '
    a1b1_cov = a1b1corr * sqrt(a1j.totalvar * b1j.totalvar); # covariance between A and B random slopes
    indirect_w = a1mean * b1mean + a1b1_cov; # within-cluster indirect effect
    indirect_b = l2apath * l2bpath;',
  seed = 90291,
  burn = 25000,
  iter = 25000)

output(model2) 
posterior_plot(model2,'indirect_w')
posterior_plot(model2,'indirect_b')