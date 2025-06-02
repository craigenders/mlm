# install.packages('remotes')
# install.packages('ggplot2')
# remotes::install_github('blimp-stats/rblimp')

# load packages
library(rblimp)
library(ggplot2)

# load data
connect <- url("https://raw.githubusercontent.com/craigenders/mlm/main/ClinicalTrial.RData", "rb")
load(connect); close(connect)

################################################################
# combined-model specification
################################################################

# repeated measures anova
model1 <- rblimp(
  data = ClinicalTrial,
  clusterid = 'Person',    
  model = 'Severity ~ intercept@0 (Week==1) (Week==2) (Week==4) (Week==7) | intercept',  
  seed = 90291,
  burn = 10000,
  iter = 20000)

# summarize results
output(model1)
posterior_plot(model1,'Severity')

# extract means from estimates object (always starting in 3rd row and first column)
means <- model1@estimates[3:6,1] # extract means
waves <- c(1,2,4,7) # specify values of the measurement occasions
means <- as.data.frame(cbind(waves,means))

# plot means
ggplot(means, aes(x = waves, y = means)) +
  geom_line() +     
  geom_point() +
  labs(x = "Week",y = "Severity",title = "Line Plot of Means")

# linear growth model
model2 <- rblimp(
  data = ClinicalTrial,
  clusterid = 'Person',  
  transform = 'Time = Week - 2',
  model = 'Severity ~ intercept Time | intercept Time',  
  seed = 90291,
  burn = 10000,
  iter = 20000)

# summarize results
output(model2)
posterior_plot(model2,'Severity')

# quadratic growth model
model3 <- rblimp(
  data = ClinicalTrial,
  clusterid = 'Person',  
  transform = 'Time = Week - 2',
  model = 'Severity ~ intercept Time Time^2 | intercept Time',  
  seed = 90291,
  burn = 10000,
  iter = 20000)

# summarize results
output(model3)
posterior_plot(model2,'Severity')

model4 <- rblimp(
  data = ClinicalTrial,
  clusterid = 'Person', 
  ordinal = 'Drug',
  transform = 'Time = Week - 2',  
  model = 'Severity ~ intercept Time Drug Time*Drug | intercept Time', 
  simple = 'Time | Drug',
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(model4)
posterior_plot(model4, 'Severity')
simple_plot(Severity ~ Time | Drug, model4)
