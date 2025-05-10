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

# random intercept model with mediation at both levels
model1 <- rblimp(
  data = Employee,
  clusterid = 'Team', 
  latent = 'Team = LMX_b Empower_b JobSat_b',  
  model = '
    LMX_w = LMX – LMX_b;   # text substitution alias to simplify the equations
    Empower_w = Empower – Empower_b; 
    level2:   # label that groups summary tables for a set of models
    LMX_b ~ intercept; 
    Empower_b ~ intercept LMX_b@apath_b;   # @ labels the between-cluster slopes
    JobSat_b ~ intercept LMX_b@tpath_b Empower_b@bpath_b; 
    level1:   # label that groups summary tables for a set of models
    LMX ~ intercept@LMX_b; 
    Empower ~ intercept@Empower_b LMX_w@apath_w;   # @ labels the within-cluster slopes
    JobSat ~ intercept@JobSat_b LMX_w@tpath_w Empower_w@bpath_w;',  
  parameters =  '
    ab_w = apath_w * bpath_w;  
    ab_b = apath_b * bpath_b;',
  seed = 90291,
  burn = 10000,
  iter = 20000)

output(model1)
posterior_plot(model1, 'ab_w')
posterior_plot(model1, 'ab_b')

# random slope model with mediation at both levels
model2 <- rblimp(
  data = Employee,
  clusterid = 'Team', 
  latent = 'team = LMX_b Empower_b JobSat_b apath_w bpath_w tpath_w',
  model = '
    LMX_w = LMX – LMX_b;   # text substitution alias to simplify the equations
    Empower_w = Empower – Empower_b; 
    level2:   # label that groups summary tables for a set of models
    LMX_b ~ intercept; 
    Empower_b ~ intercept LMX_b@apath_b;   # @ labels the between-cluster slopes
    JobSat_b ~ intercept LMX_b@tpath_b Empower_b@bpath_b;
    apath_w ~ intercept@apathw_mean; # random slope latent variable with its mean labeled
    bpath_w ~ intercept@bpathw_mean; # random slope latent variable with its mean labeled
    tpath_w ~ intercept; # random slope latent variable
    apath_w ~~ bpath_w@ab_corr; # correlate random slopes and attach a label;
    apath_w bpath_w ~~ tpath_w;
    level1:   # label that groups summary tables for a set of models
    LMX ~ intercept@LMX_b; 
    Empower ~ intercept@Empower_b LMX_w@apath_w;   # @ fixes coefficients to their random slopes
    JobSat ~ intercept@JobSat_b LMX_w@tpath_w Empower_w@bpath_w;', 
  parameters = '
    ab_cov = ab_corr * sqrt(apath_w.totalvar * bpath_w.totalvar); # covariance between random slopes uses .totalvar to get the variance
    ab_w = apathw_mean * bpathw_mean + ab_cov;
    ab_b = apath_b * bpath_b',
  seed = 90291,
  burn = 10000,
  iter = 20000)

output(model2)
posterior_plot(model2,'ab_w')
posterior_plot(model2, 'ab_b')

# random slope model with a level-2 moderating the within-cluster a and p paths
model3 <- rblimp(
  data = Employee,
  clusterid = 'Team', 
  latent = 'team = LMX_b Empower_b JobSat_b apath_w bpath_w tpath_w',
  center = 'grandmean = Climate',
  model = '
    LMX_w = LMX – LMX_b;   # text substitution alias to simplify the equations
    Empower_w = Empower – Empower_b; 
    level2:   # label that groups summary tables for a set of models
    Climate ~ intercept; 
    LMX_b ~ intercept; 
    Empower_b ~ intercept LMX_b@apath_b;   # @ labels the between-cluster slopes
    JobSat_b ~ intercept LMX_b@tpath_b Empower_b@bpath_b;
    apath_w ~ intercept@apathw_mean climate@apathw_mod; # random slope predicted by level-2 moderator
    bpath_w ~ intercept@bpathw_mean climate@bpathw_mod;# random slope predicted by level-2 moderator
    tpath_w ~ intercept; # random slope latent variable
    apath_w ~~ bpath_w@ab_corr; # correlate random slopes and attach a label;
    apath_w bpath_w ~~ tpath_w;
    level1:   # label that groups summary tables for a set of models
    LMX ~ intercept@LMX_b; 
    Empower ~ intercept@Empower_b LMX_w@apath_w;   # @ fixes coefficients to their random slopes
    JobSat ~ intercept@JobSat_b LMX_w@tpath_w Empower_w@bpath_w;', 
  parameters = '
    ab_cov = ab_corr * sqrt(apath_w.totalvar * bpath_w.totalvar); # covariance between random slopes uses .totalvar to get the variance
    a_hi = apathw_mean + apathw_mod*sqrt(climate.totalvar); # a path at different values of the moderator
    a_mean = apathw_mean;
    a_lo = apathw_mean - apathw_mod*sqrt(climate.totalvar);
    b_hi = bpathw_mean + bpathw_mod*sqrt(climate.totalvar); # b path at different values of the moderator
    b_mean = bpathw_mean;
    b_lo = bpathw_mean - bpathw_mod*sqrt(climate.totalvar);
    ab_w_hi = a_hi*b_hi + ab_cov;   # within-cluster mediated effect at different values of the moderator
    ab_w_mean = a_mean*b_mean + ab_cov;
    ab_w_lo = a_lo*b_lo + ab_cov;
    ab_b = apath_b*bpath_b',
  seed = 90291,
  burn = 10000,
  iter = 20000)

output(model3)
posterior_plot(model3,'ab_w_hi')
posterior_plot(model3,'ab_w_mean')
posterior_plot(model3,'ab_w_lo')
posterior_plot(model3, 'ab_b')


