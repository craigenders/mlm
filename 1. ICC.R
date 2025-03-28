library(fdir)
library(rblimp)
library(ggplot2)

# set the working directory to the folder that contains this script and load the data
set()

# load data
load("Pain Diary Data.RData")

################################################################
# box plots to display raw data
################################################################

# subset the data to select first 20 level-2 identifiers for graphing
ids2plot <- unique(PainDiary$Person)[1:20]
data2plot <- subset(PainDiary, Person %in% ids2plot)
mean(data2plot$PosAffect)

# plot the scores with jitter and add the mean for each person as a orange dot
ggplot(data2plot, aes(x = factor(Person), y = PosAffect)) +
  geom_boxplot(width = 0.3, outlier.shape = NA, fill = NA, color = "grey") +
  geom_jitter(width = 0.1, height = 0, size = 2, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 4) +
  scale_y_continuous(limits = c(2, 8), breaks = c(2,4,6,8)) +
  labs(x = "Person", y = "Positive Affect",
  title = "Positive Affect Scores by Cluster") +
  theme_minimal()

################################################################
# estimate icc
################################################################

# empty model to estimate icc
# combined-model specification
model1 <- rblimp(
  data = PainDiary,
  clusterid = 'Person',
  model = 'PosAffect ~ intercept | intercept',
  seed = 90291,
  burn = 10000,
  iter = 20000
)
output(model1)

# empty model to estimate icc
# alternate level-1 and level-2 latent variable specification
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


