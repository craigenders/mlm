# install.packages('rockchalk')
# install.packages('lmerTest')
# install.packages('ggplot2')

# load packages
library(rockchalk)
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

# within-cluster (group mean) center sleep quality and add cluster-specific group means to the data
PainDiary <- gmc(PainDiary, c("SleepQual"), by = c("Person"), FUN = mean, suffix = c("_mean", "_cwc"), fulldataframe = TRUE)

# grand mean center variables
PainDiary$SleepQual_mean_cent <- PainDiary$SleepQual_mean - mean(PainDiary$SleepQual_mean)

# unconditional (empty) multilevel model with reml estimation and K-W degrees of freedom
model1 <- lmer(PosAffect ~ 1 + (1 | Person), data = PainDiary, REML = T)
summary(model1, ddf = "Kenward-Roger")

# within-cluster part of sleep predicting affect
model2 <- lmer(PosAffect ~ SleepQual_cwc + (1 | Person), data = PainDiary, REML = T)
summary(model2, ddf = "Kenward-Roger")

# between-cluster part of sleep (cluster means) predicting affect
model3 <- lmer(PosAffect ~ SleepQual_cwc + SleepQual_mean_cent + (1 | Person), data = PainDiary, REML = T)
summary(model3, ddf = "Kenward-Roger")
