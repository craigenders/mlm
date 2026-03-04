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
# AR(1) MODEL ----
#------------------------------------------------------------------------------#

# ar(1) with autoregressive effect
model1 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  timeid = 'Day',   
  latent = 'Person = PosAff_PM PosAff_AR',  
  model = '
    intercept –> PosAff_PM PosAff_AR;                      # empty model for random intercepts and ar1 slopes
    PosAff_PM ~~ PosAff_AR;                                # correlate intercepts and slopes
    PosAffLag = PosAffect.lag – PosAff_PM;                 # definition variable that centers lagged predictor
    PosAffect ~ intercept@PosAff_PM PosAffLag@PosAff_AR;   # set level-1 coefficients equal to random intercepts and slopes
  ', 
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model1)

#------------------------------------------------------------------------------#
# VAR(1) MODEL ----
#------------------------------------------------------------------------------#

# var(1) with autoregressive and cross-lagged effects
model2 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  timeid = 'Day',   
  latent = 'Person = Pain_PM Pain_AR Pain_CL PosAff_PM PosAff_AR PosAffect_CL',  
  model = '
    intercept -> Pain_PM Pain_AR Pain_CL PosAff_PM PosAff_AR PosAffect_CL;  # empty model for random intercepts and slopes
    Pain_PM Pain_AR Pain_CL PosAff_PM PosAff_AR PosAffect_CL ~~    
       Pain_PM Pain_AR Pain_CL PosAff_PM PosAff_AR PosAffect_CL;   # correlations
    Pain_lag = Pain.lag - Pain_PM;               # definition variable that centers lagged predictor
    PosAff_lag = PosAffect.lag - PosAff_PM;      # definition variable that centers lagged predictor
    Pain ~ intercept@Pain_PM Pain_lag@Pain_AR PosAff_lag@PosAffect_CL;      # set level-1 coefficients equal to random intercepts and slopes
    PosAffect ~ intercept@PosAff_PM PosAff_lag@PosAff_AR Pain_lag@Pain_CL;  # set level-1 coefficients equal to random intercepts and slopes
    Pain ~~ PosAffect;   # level-1 residual correlation',  
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model2)

#------------------------------------------------------------------------------#
# VAR(1) MODEL WITH HETEROGENEOUS WITHIN-PERSON VARIATION ----
#------------------------------------------------------------------------------#

# VAR(1) with person-specific intraindividual variation
model3 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  timeid = 'Day',   
  latent = 'Person = Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAffect_CL PosAff_LV',  
  model = '
    intercept -> Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAffect_CL PosAff_LV;  # empty model for random intercepts and slopes
    Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAffect_CL PosAff_LV ~~    
       Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAffect_CL PosAff_LV;   # correlations
    Pain_lag = Pain.lag - Pain_PM;               # definition variable that centers lagged predictor
    PosAff_lag = PosAffect.lag - PosAff_PM;      # definition variable that centers lagged predictor
    Pain ~ intercept@Pain_PM Pain_lag@Pain_AR PosAff_lag@PosAffect_CL;      # set level-1 coefficients equal to random intercepts and slopes
    var(Pain) ~ intercept@Pain_LV;           # heterogeneous within-person variance
    PosAffect ~ intercept@PosAff_PM PosAff_lag@PosAff_AR Pain_lag@Pain_CL;  # set level-1 coefficients equal to random intercepts and slopes
    var(PosAffect) ~ intercept@PosAff_LV;    # heterogeneous within-person variance 
    Pain ~~ PosAffect;   # level-1 residual correlation',  
  seed = 90291,
  burn = 10000,
  iter = 10000)

# print output
output(model3)
