# install.packages('rockchalk')
# install.packages('r2mlm')
# install.packages('lmerTest')
# install.packages('ggplot2')

# load packages
library(rockchalk)
library(r2mlm)
library(lmerTest)
library(ggplot2)

# load data
connect <- url("https://raw.githubusercontent.com/craigenders/mlm/main/PainDiaryData.RData", "rb")
load(connect); close(connect)

# load misc functions
source("https://raw.githubusercontent.com/craigenders/mlm/main/mlm-functions.R")

# boxplots of raw data by cluster
boxplots_by_cluster(data = PainDiary, var2plot = "PosAffect", lev2id = "Person", numboxes = 20)
boxplots_by_cluster(data = PainDiary, var2plot = "SleepQual", lev2id = "Person", numboxes = 20)
boxplots_by_cluster(data = PainDiary, var2plot = "Pain", lev2id = "Person", numboxes = 20)

# within-cluster (group mean) center sleep quality and add cluster-specific group means to the data
PainDiary <- gmc(PainDiary, c("SleepQual","Pain"), by = c("Person"), FUN = mean, suffix = c("_mean", "_cwc"), fulldataframe = TRUE)

# grand mean center variables
PainDiary$Pain_mean_cgm <- PainDiary$Pain_mean - mean(PainDiary$Pain_mean)
PainDiary$SleepQual_mean_cgm <- PainDiary$SleepQual_mean - mean(PainDiary$SleepQual_mean)
PainDiary$Stress_cgm <- PainDiary$Stress - mean(PainDiary$Stress)

# unconditional (empty) multilevel model with reml estimation and K-W degrees of freedom
model1a <- lmer(PosAffect ~ 1 + (1 | Person), data = PainDiary, REML = T)
model1b <- lmer(SleepQual ~ 1 + (1 | Person), data = PainDiary, REML = T)
model1c <- lmer(Pain ~ 1 + (1 | Person), data = PainDiary, REML = T)
summary(model1a, ddf = "Kenward-Roger")
summary(model1b, ddf = "Kenward-Roger")
summary(model1c, ddf = "Kenward-Roger")

# random intercept model with level-1 predictors
model2 <- lmer(PosAffect ~ SleepQual_cwc + Pain_cwc + (1 | Person), data = PainDiary, REML = T)
summary(model2, ddf = "Kenward-Roger")

# r-square effect sizes
r2mlm(model2, bargraph = F)

# random intercept model with level-1 and their means
model3 <- lmer(PosAffect ~ SleepQual_cwc + Pain_cwc + SleepQual_mean_cgm + Pain_mean_cgm + (1 | Person), data = PainDiary, REML = T)
summary(model3, ddf = "Kenward-Roger")

# r-square effect sizes
r2mlm(model3, bargraph = F)

# random intercept model with level-1 and level-2 predictors (lecture topic #5)
model4 <- lmer(PosAffect ~ SleepQual_cwc + Pain_cwc + SleepQual_mean_cgm + Pain_mean_cgm + Stress_cgm + Female + (1 | Person), data = PainDiary, REML = T)
summary(model4, ddf = "Kenward-Roger")

# r-square effect sizes
r2mlm(model4, bargraph = F)
