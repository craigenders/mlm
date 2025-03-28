* Encoding: UTF-8.

*/ add grand means to the data.
aggregate
  /outfile = * mode = addvariables overwritevars = yes
  /SleepGrandMean = mean(SleepQual).

*/ add group means to the data.
aggregate
  /outfile = * mode = addvariables overwritevars = yes
  /break = Participant
  /SleepQual.meanj = mean(SleepQual).

*/ center predictors.
compute SleepQual.meanj.cgm = SleepQual.meanj - SleepGrandMean.
compute SleepQual.cwc = SleepQual - SleepQual.meanj.
exe. 

*/ random intercept model with level-1 predictors.
mixed PosAffect with SleepQual.cwc SleepQual.meanj.cgm
  /fixed = SleepQual.cwc SleepQual.meanj.cgm | 
  /random = intercept | subject(Participant) covtype(id)
  /method = reml
  /print = descriptives solution testcov.
