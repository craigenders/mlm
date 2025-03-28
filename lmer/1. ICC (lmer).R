# install.packages('lmerTest')
# install.packages('r2mlm')
# install.packages('ggplot2')

# load packages
library(lmerTest)
library(r2mlm)
library(ggplot2)

# load data
connect <- url("https://raw.githubusercontent.com/craigenders/mlm/main/PainDiaryData.RData", "rb")
load(connect); close(connect)

# load misc functions
source("https://raw.githubusercontent.com/craigenders/mlm/main/mlm-functions.R")

# boxplots of raw data by cluster
boxplots_by_cluster(data = PainDiary, var2plot = "PosAffect", lev2id = "Person", numboxes = 20)

# unconditional (empty) multilevel model with reml estimation and K-W degrees of freedom
model1 <- lmer(PosAffect ~ 1 + (1 | Person), data = PainDiary, REML = T)
summary(model1, ddf = "Kenward-Roger")

# r-square effect sizes
r2mlm(model1, bargraph = F)