// compute means
egen SleepGrandMean = mean(SleepQual)
egen PainGrandMean = mean(Pain)
egen StressGrandMean = mean(Stress)
egen SleepQual_meanj = mean(SleepQual), by(Participant)
egen Pain_meanj = mean(Pain), by(Participant)

// center predictors
gen SleepQual_cwc = SleepQual - SleepQual_meanj
gen Pain_cwc = Pain - Pain_meanj
gen SleepQual_meanj_cgm = SleepQual_meanj - SleepGrandMean
gen Pain_meanj_cgm = Pain_meanj - PainGrandMean
gen Stress_cgm = Stress - StressGrandMean

// random intercept model with level-1 and level-2 predictors
mixed PosAffect SleepQual_cwc Pain_cwc SleepQual_meanj_cgm Pain_meanj_cgm Stress_cgm Female, || Participant:, covariance(unstructured) reml dfmethod(kroger) stddeviations
