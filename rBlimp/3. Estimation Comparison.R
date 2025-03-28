# install.packages('remotes')
# install.packages('rockchalk')
# install.packages('lmerTest')
# install.packages('lavaan')
# install.packages('ggplot2')
# remotes::install_github('blimp-stats/rblimp')

library(rockchalk)
library(lmerTest)
library(lavaan)
library(rblimp)

# load data
connect <- url("https://raw.githubusercontent.com/craigenders/mlm/main/PainDiaryData.RData", "rb")
load(connect); close(connect)

# within-cluster (group mean) center sleep quality and add cluster-specific arithmetic group means to the data
PainDiary <- gmc(PainDiary, c("SleepQual"), by = c("Person"), FUN = mean, suffix = c(".meanj", ".w"), fulldataframe = T)

# center the level-2 group means
PainDiary$SleepQual.mean <- PainDiary$SleepQual.meanj - mean(PainDiary$SleepQual.mean)

# fiml with arithmetic (manifest) group means
fiml <- lmer(PosAffect ~ SleepQual.w + SleepQual.meanj + (1 | Person), data = PainDiary,  REML = F)
summary(fiml)

# reml with arithmetic (manifest) group means
reml <- lmer(PosAffect ~ SleepQual.w + SleepQual.meanj + (1 | Person), data = PainDiary,  REML = T)
summary(reml, ddf = "Kenward-Roger")

# fiml with latent group means
model <- '
    level: 1
        PosAffect_w =~ PosAffect
        SleepQual_w =~ SleepQual
        PosAffect_w ~ SleepQual_w
    level: 2
        PosAffect_b =~ PosAffect
        SleepQual_b =~ SleepQual
        PosAffect_b ~ SleepQual_b'

fiml_latent <- sem(model,
                data = PainDiary,
                cluster = "Person",
                estimator = "MLR",
                missing = "fiml")
summary(fiml_latent, standardized = T)

# mcmc with latent group means
mcmc <- rblimp(
  data = PainDiary,
  clusterid = 'Person', 
  center = 'grandmean = SleepQual.mean; groupmean = SleepQual',  
  model = 'PosAffect ~ intercept SleepQual SleepQual.mean | intercept',   
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(mcmc)
