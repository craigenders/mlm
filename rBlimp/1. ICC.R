# install.packages('remotes')
# install.packages('ggplot2')
# remotes::install_github('blimp-stats/rblimp')

# load packages
library(rblimp)
library(ggplot2)

# load data
connect <- url("https://raw.githubusercontent.com/craigenders/mlm/main/rBlimp/PainDiaryData.RData", "rb")
load(connect); close(connect)

# load misc functions
source("https://raw.githubusercontent.com/craigenders/mlm/main/mlm-functions.R")

# boxplots of raw data by cluster
boxplots_by_cluster(data = PainDiary, var2plot = "PosAffect", lev2id = "Person", numboxes = 20)

# empty model to estimate icc (combined-model specification)
model1 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',
  model = 'PosAffect ~ intercept | intercept',
  seed = 90291,
  burn = 10000,
  iter = 20000
)
output(model1)

# empty model to estimate icc (alternate level-1 and level-2 latent variable specification)
model2 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',
  latent = 'Person = beta0j',
  model = '
  beta0j ~ intercept;
  PosAffect ~ intercept@beta0j',
  parameters = 'icc = beta0j.totalvar / (beta0j.totalvar + PosAffect.totalvar)',
  seed = 90291,
  burn = 10000,
  iter = 20000
)
output(model2)


