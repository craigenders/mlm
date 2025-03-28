// compute means
egen SleepGrandMean = mean(SleepQual)
egen SleepQual_meanj = mean(SleepQual), by(Participant)

// center predictors
gen SleepQual_cwc = SleepQual - SleepQual_meanj
gen SleepQual_meanj_cgm = SleepQual_meanj - SleepGrandMean

// random intercept model with level-1 predictors
mixed PosAffect SleepQual_cwc SleepQual_meanj_cgm, || Participant:, covariance(unstructured) reml dfmethod(kroger) stddeviations
