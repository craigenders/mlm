// compute means
egen LMXGrandMean = mean(LMX)
egen ClimateGrandMean = mean(Climate)
egen LMX_meanj = mean(LMX), by(Team)

// center predictors
gen LMX_cwc = LMX - LMX_meanj
gen LMX_meanj_cgm = LMX_meanj - LMXGrandMean
gen Climate_cgm = Climate - ClimateGrandMean

// cross-level interaction model
mixed JobSat LMX_cwc Empower_cwc Male LMX_meanj Empower_meanj Climate_cgm c.LMX_cwc##c.Climate_cgm || Team: Empower_cwc, covariance(unstructured) reml dfmethod(kroger) stddeviations

