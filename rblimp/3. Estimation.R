#------------------------------------------------------------------------------#
# LOAD R PACKAGES ----
#------------------------------------------------------------------------------#

# load packages
library(ggplot2)
library(lme4)
library(rblimp)
library(rockchalk)

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
# GROUP MEAN CENTER FOR FIML AND REML  ----
#------------------------------------------------------------------------------#

# within-cluster (group mean) center sleep quality and add cluster-specific arithmetic group means to the data
PainDiary <- gmc(PainDiary, c("SleepQual"), by = c("Person"), FUN = mean, suffix = c(".meanj", ".w"), fulldataframe = T)

# center the level-2 group means
PainDiary$SleepQual.meanj.cent <- PainDiary$SleepQual.meanj - mean(PainDiary$SleepQual)

#------------------------------------------------------------------------------#
# FIML AND REML  ----
#------------------------------------------------------------------------------#

# fiml with arithmetic (manifest) group means
fiml <- lmer(PosAffect ~ SleepQual.w + SleepQual.meanj.cent + (1 | Person), data = PainDiary,  REML = F)
summary(fiml)

# reml with arithmetic (manifest) group means
reml <- lmer(PosAffect ~ SleepQual.w + SleepQual.meanj.cent + (1 | Person), data = PainDiary,  REML = T)
summary(reml, ddf = "Kenward-Roger")

#------------------------------------------------------------------------------#
# MCMC ESTIMATION ----
#------------------------------------------------------------------------------#

# disaggregated model with within-person and between-person sleep as a predictor
model1 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'grandmean = SleepQual.mean; groupmean = SleepQual',  
  model = 'PosAffect ~ intercept SleepQual SleepQual.mean | intercept',   
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model1)

# trace plot of first 1000 iterations
trace_plot(model1) + ggplot2::xlim(0, 1000)

#------------------------------------------------------------------------------#
# GRAPHING RESIDUALS FROM MULTIPLE IMPUTATIONS ----
#------------------------------------------------------------------------------#

# disaggregated model with within-person and between-person sleep as a predictor
model2 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'grandmean = SleepQual.mean; groupmean = SleepQual',  
  model = 'PosAffect ~ intercept SleepQual SleepQual.mean | intercept',   
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

# plot standardized residuals vs. predictors (check for linearity)
bivariate_plot(PosAffect.residual ~ SleepQual, standardize = 'y', model = model2)
bivariate_plot(PosAffect[Person] ~ SleepQual.mean[Person], standardize = 'y', model = model2)

#------------------------------------------------------------------------------#
# HETEROGENEOUS VARIATION MODEL ----
#------------------------------------------------------------------------------#
set_blimp('/applications/blimp/blimp-nightly')
# hev option gives cluster-specific level-1 variances
model3 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'grandmean = SleepQual.mean; groupmean = SleepQual',  
  model = 'PosAffect ~ intercept SleepQual SleepQual.mean | intercept',   
  seed = 90291,
  burn = 10000,
  iter = 10000,
  options = 'hev')

# print output
output(model3)



