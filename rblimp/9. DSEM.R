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
  latent = 'Person = Pain_PM Pain_AR Pain_CL PosAff_PM PosAff_AR PosAff_CL',  
  model = '
    intercept -> Pain_PM Pain_AR Pain_CL PosAff_PM PosAff_AR PosAff_CL;  # empty model for random intercepts and slopes
    Pain_PM Pain_AR Pain_CL PosAff_PM PosAff_AR PosAff_CL ~~    
       Pain_PM Pain_AR Pain_CL PosAff_PM PosAff_AR PosAff_CL;   # correlations
    Pain_lag = Pain.lag - Pain_PM;               # definition variable that centers lagged predictor
    PosAff_lag = PosAffect.lag - PosAff_PM;      # definition variable that centers lagged predictor
    Pain ~ intercept@Pain_PM Pain_lag@Pain_AR PosAff_lag@PosAff_CL;      # set level-1 coefficients equal to random intercepts and slopes
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
  latent = 'Person = Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAff_CL PosAff_LV',  
  model = '
    intercept -> Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAff_CL PosAff_LV;  # empty model for random intercepts and slopes
    Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAff_CL PosAff_LV ~~    
       Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAff_CL PosAff_LV;   # correlations
    Pain_lag = Pain.lag - Pain_PM;               # definition variable that centers lagged predictor
    PosAff_lag = PosAffect.lag - PosAff_PM;      # definition variable that centers lagged predictor
    Pain ~ intercept@Pain_PM Pain_lag@Pain_AR PosAff_lag@PosAff_CL;      # set level-1 coefficients equal to random intercepts and slopes
    var(Pain) ~ intercept@Pain_LV ;           # heterogeneous within-person variance
    PosAffect ~ intercept@PosAff_PM PosAff_lag@PosAff_AR Pain_lag@Pain_CL;  # set level-1 coefficients equal to random intercepts and slopes
    var(PosAffect) ~ intercept@PosAff_LV;    # heterogeneous within-person variance 
    Pain ~~ PosAffect;   # level-1 residual correlation',  
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(model3)

#------------------------------------------------------------------------------#
# MODERATED VAR(1) MODEL WITH HETEROGENEOUS WITHIN-PERSON VARIATION ----
#------------------------------------------------------------------------------#

# VAR(1) with person-specific intraindividual variation
model4 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  timeid = 'Day',   
  latent = 'Person = Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAff_CL PosAff_LV', 
  center = 'grandmean = stress',
  model = '
    intercept -> Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAff_CL PosAff_LV;  # empty model for random intercepts and slopes
    Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAff_CL PosAff_LV ~~    
       Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAff_CL PosAff_LV;   # correlations
    Pain_AR ~ stress;     # autoregressive effect moderated by stress
    PosAff_AR ~ stress;   # autoregressive effect moderated by stress
    Pain_lag = Pain.lag - Pain_PM;               # definition variable that centers lagged predictor
    PosAff_lag = PosAffect.lag - PosAff_PM;      # definition variable that centers lagged predictor
    Pain ~ intercept@Pain_PM Pain_lag@Pain_AR PosAff_lag@PosAff_CL;      # set level-1 coefficients equal to random intercepts and slopes
    var(Pain) ~ intercept@Pain_LV ;           # heterogeneous within-person variance
    PosAffect ~ intercept@PosAff_PM PosAff_lag@PosAff_AR Pain_lag@Pain_CL;  # set level-1 coefficients equal to random intercepts and slopes
    var(PosAffect) ~ intercept@PosAff_LV;    # heterogeneous within-person variance 
    Pain ~~ PosAffect;   # level-1 residual correlation',  
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(model4)

#------------------------------------------------------------------------------#
# VAR(1) MODEL WITH PREDICTOR OF HETEROGENEOUS WITHIN-PERSON VARIATION ----
#------------------------------------------------------------------------------#

# VAR(1) with predictor of person-specific intraindividual variation
model5 <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  timeid = 'Day',   
  latent = 'Person = Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAff_CL PosAff_LV', 
  center = 'grandmean = stress',
  model = '
    intercept -> Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAff_CL PosAff_LV;  # empty model for random intercepts and slopes
    Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAff_CL PosAff_LV ~~    
       Pain_PM Pain_AR Pain_CL Pain_LV PosAff_PM PosAff_AR PosAff_CL PosAff_LV;   # correlations
    Pain_LV ~ stress;     # heterogeneous within-person variance w predictor
    PosAff_LV ~ stress;   # heterogeneous within-person variance w predictor
    Pain_lag = Pain.lag - Pain_PM;               # definition variable that centers lagged predictor
    PosAff_lag = PosAffect.lag - PosAff_PM;      # definition variable that centers lagged predictor
    Pain ~ intercept@Pain_PM Pain_lag@Pain_AR PosAff_lag@PosAff_CL;      # set level-1 coefficients equal to random intercepts and slopes
    var(Pain) ~ intercept@Pain_LV;           # heterogeneous within-person variance
    PosAffect ~ intercept@PosAff_PM PosAff_lag@PosAff_AR Pain_lag@Pain_CL;  # set level-1 coefficients equal to random intercepts and slopes
    var(PosAffect) ~ intercept@PosAff_LV;    # heterogeneous within-person variance
    Pain ~~ PosAffect;   # level-1 residual correlation',  
  seed = 90291,
  burn = 20000,
  iter = 20000)

# print output
output(model5)


