* Encoding: ISO-8859-1.

*/ add grand means to the data.
aggregate
  /outfile = * mode = addvariables overwritevars = yes
  /LMXGrandMean = mean(LMX).

*/ add group means to the data.
aggregate
  /outfile = * mode = addvariables overwritevars = yes
  /break = Team
  /LMX.meanj = mean(LMX)
  /Empower.meanj = mean(Empower).

*/ center predictors.
compute LMX.cwc = LMX - LMX.meanj.
compute Empower.cwc = Empower - Empower.meanj.
exe. 

*/ random slope model with cluster-specific empower coefficients.
mixed Empower with LMX.cwc Empower.cwc Male LMX.meanj Empower.meanj 
  /fixed = LMX.cwc Empower.cwc Male LMX.meanj Empower.meanj  | 
  /random = intercept Empower.cwc | subject(Team) covtype(un)
  /method = reml
  /print = descriptives solution testcov.
