# install.packages('rockchalk')
# install.packages('r2mlm')
# install.packages('lmerTest')
# install.packages('varTestnlme')
# install.packages('ggplot2')

# load packages
library(rockchalk)
library(r2mlm)
library(lmerTest)
library(varTestnlme)
library(ggplot2)

# load data
connect <- url("https://raw.githubusercontent.com/craigenders/mlm/main/EmployeeSatisfactionData.RData", "rb")
load(connect); close(connect)

# load misc functions
source("https://raw.githubusercontent.com/craigenders/mlm/main/mlm-functions.R")

# boxplots of raw data by cluster
boxplots_by_cluster(data = Employee, var2plot = "JobSat", lev2id = "Team", numboxes = 20)
boxplots_by_cluster(data = Employee, var2plot = "Empower", lev2id = "Team", numboxes = 20)
boxplots_by_cluster(data = Employee, var2plot = "LMX", lev2id = "Team", numboxes = 20)
boxplots_by_cluster(data = Employee, var2plot = "Male", lev2id = "Team", numboxes = 20)

# within-cluster (group mean) center LMX and empowerment add cluster-specific group means to the data
Employee <- rockchalk::gmc(Employee, c("LMX","Empower"), by = c("Team"), FUN = mean, suffix = c("_mean", "_cwc"), fulldataframe = TRUE)

# grand mean center variables
Employee$LMX_mean_cgm <- Employee$LMX_mean - mean(Employee$LMX_mean)
Employee$Empower_mean_cgm <- Employee$Empower_mean - mean(Employee$Empower_mean)
Employee$Male_cgm <- Employee$Male - mean(Employee$Male)

# unconditional (empty) multilevel model with reml estimation and K-W degrees of freedom
model1a <- lmer(JobSat ~ 1 + (1 | Team), data = Employee, REML = T)
model1b <- lmer(LMX ~ 1 + (1 | Team), data = Employee, REML = T)
model1c <- lmer(Empower ~ 1 + (1 | Team), data = Employee, REML = T)
model1d <- lmer(Male ~ 1 + (1 | Team), data = Employee, REML = T)
summary(model1a, ddf = "Kenward-Roger")
summary(model1b, ddf = "Kenward-Roger")
summary(model1c, ddf = "Kenward-Roger")
summary(model1d, ddf = "Kenward-Roger")

# random intercept model with cluster-specific intercepts (means)
model2_fiml <- lmer(JobSat ~ LMX_cwc + Empower_cwc + Male_cgm + (1 | Team), data = Employee, REML = F)
summary(model2_fiml)

# r-square effect sizes
r2mlm(model2_fiml, bargraph = F)

# random coefficient model with cluster-specific LMX slopes
model3_fiml <- lmer(JobSat ~ LMX_cwc + Empower_cwc + Male_cgm + (1 + LMX_cwc | Team), data = Employee, REML = F)
summary(model3_fiml)

# r-square effect sizes
r2mlm(model3_fiml, bargraph = F)

# likelihood ratio test with mixture chi-square (chibar) distribution for testing if random slope variances and covariances = 0
varCompTest(model3_fiml, model2_fiml, pval.comp = "bounds")

# random coefficient model with cluster-specific LMX slopes
model4_fiml <- lmer(JobSat ~ LMX_cwc + Empower_cwc + Male_cgm + (1 + Empower_cwc | Team), data = Employee, REML = F)
summary(model4_fiml)

# r-square effect sizes
r2mlm(model4_fiml, bargraph = F)

# likelihood ratio test with mixture chi-square (chibar) distribution for testing if random slope variances and covariances = 0
varCompTest(model4_fiml, model2_fiml, pval.comp = "bounds")

# final model
model5 <- lmer(JobSat ~ LMX_cwc + Empower_cwc + Male_cgm + LMX_mean_cgm + Empower_mean_cgm + (1 + Empower_cwc | Team), data = Employee, REML = T)
summary(model5, ddf = "Kenward-Roger")

# r-square effect sizes
r2mlm(model5, bargraph = F)