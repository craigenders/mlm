* Encoding: UTF-8.

*/ add grand means to the data.
aggregate
  /outfile = * mode = addvariables overwritevars = yes
  /LMXGrandMean = mean(LMX)
  /ClimateGrandMean = mean(Climate).

*/ add group means to the data.
aggregate
  /outfile = * mode = addvariables overwritevars = yes
  /break = Team
  /LMX.meanj = mean(LMX).

*/ center predictors.
compute LMX.meanj.cgm = LMX.meanj - LMXGrandMean.
compute LMX.cwc = LMX - LMX.meanj.
compute Climate.cgm = Climate - ClimateGrandMean.
exe. 

*/ random slope model.
mixed Empower with LMX.cwc Male LMX.meanj.cgm Climate.cgm
  /fixed = LMX.cwc Male LMX.meanj.cgm Climate.cgm | 
  /random = intercept LMX.cwc | subject(Team) covtype(un)
  /method = reml
  /print = descriptives solution testcov.

*/ cross-level interaction.
*/ note that the climate standard deviation is 4.02.
*/ each test command is plugging in a value into the regression equation listed on the fixed line starting with the intercept.
mixed Empower with LMX.cwc Male LMX.meanj.cgm Climate.cgm
  /fixed = LMX.cwc Male LMX.meanj.cgm Climate.cgm LMX.cwc*Climate.cgm | 
  /random = intercept LMX.cwc | subject(Team) covtype(un)
  /method = reml
  /print = descriptives solution testcov
  /test 'simple intercept @ +1 SD' all 1 0 0 4.02 0
  /test 'simple intercept @ mean' all 1 0 0 0 0
  /test 'simple intercept @ -1 SD' all 1 0 0 -4.02 0
  /test 'simple slope @ +1 SD' all 0 1 0 0 4.02
  /test 'simple slope @ mean' all 0 1 0 0 0
  /test 'simple slope @ +1 SD' all 0 1 0 0 -4.02.

*/ cross-level and between-cluster interaction.
mixed Empower with LMX.cwc Male LMX.meanj.cgm Climate.cgm
  /fixed = LMX.cwc Male LMX.meanj.cgm Climate.cgm LMX.cwc*Climate.cgm LMX.meanj.cgm*Climate.cgm | 
  /random = intercept LMX.cwc | subject(Team) covtype(un)
  /method = reml
  /print = descriptives solution testcov.
