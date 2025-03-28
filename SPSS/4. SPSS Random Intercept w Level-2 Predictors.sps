* Encoding: UTF-8.

*/ add grand means to the data.
aggregate
  /outfile = * mode = addvariables overwritevars = yes
  /PainGrandMean = mean(Pain)
  /SleepGrandMean = mean(SleepQual)
  /StressGrandMean = mean(Stress).

*/ add group means to the data.
aggregate
  /outfile = * mode = addvariables overwritevars = yes
  /break = Participant
  /SleepQual.meanj = mean(SleepQual)
  /Pain.meanj = mean(Pain).

*/ center predictors.
compute Pain.cwc = Pain - meanj.
compute SleepQual.cwc = SleepQual - SleepQual.meanj.
exe. 

*/ random intercept model with level-1 and level-2 predictors.
mixed PosAffect with SleepQual.cwc Pain.cgm SleepQual.meanj.cgm Stress.cgm Female
  /fixed = SleepQual.cwc Pain.cwc SleepQual.meanj Pain.meanj Stress Female | 
  /random = intercept | subject(Participant) covtype(id)
  /method = reml
  /print = descriptives solution testcov.
