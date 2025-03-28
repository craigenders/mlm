* Encoding: UTF-8.

*/ add grand means to the data.
aggregate
  /outfile = * mode = addvariables overwritevars = yes
  /MaleGrandMean = mean(Male).

*/ center predictors.
compute Male.cgm = Male - MaleGrandMean.
compute Week4 = Week - 4.
exe. 

* ******************************************.
*   linear growth models
* ******************************************.

*/ random intercept model.
mixed Severity with Week4
  /fixed = Week4 | 
  /random = intercept | subject(Participant) covtype(un)
  /method = reml
  /print = descriptives solution testcov.

*/ linear growth model.
mixed Severity with Week4
  /fixed = Week4 | 
  /random = intercept Week4 | subject(Participant) covtype(un)
  /method = reml
  /print = descriptives solution testcov.

*/ linear growth model with predictors.
mixed Severity with Week4 Drug Male.cgm
  /fixed = Drug Male.cgm Week4 Drug*Week4 | 
  /random = intercept Week4 | subject(Participant) covtype(un)
  /method = reml
  /print = descriptives solution testcov
  /test 'placebo group intercept' all 1 0 0 0 0
  /test 'placebo group slope' all 0 0 0 1 0
  /test 'medication group intercept' all 1 1 0 0 0
  /test 'medication group slope' all 0 0 0 1 1.

* ******************************************.
*   piecewise growth models
* ******************************************.

*/ compute temporal predictors.
compute Week2 = Week - 2.
compute WeekEp1 = Week2.
compute WeekEp2 = Week2.
recode WeekEp1 (0 thru hi = 0).
recode WeekEp2 (lo thru 0 = 0).
exe.

*/ piecewise growth model.
mixed Severity with WeekEp1 WeekEp2
  /fixed = WeekEp1 WeekEp2 | 
  /random = intercept WeekEp1 WeekEp2 | subject(Participant) covtype(un)
  /method = reml
  /print = descriptives solution testcov.

*/ piecewise growth model with predictors.
mixed Severity with WeekEp1 WeekEp2 Drug
  /fixed = WeekEp1 WeekEp2 Drug  WeekEp1*Drug WeekEp2*Drug  | 
  /random = intercept WeekEp1 WeekEp2 | subject(Participant) covtype(un)
  /method = reml
  /print = descriptives solution testcov.


* ******************************************.
*   quadratic growth models
* ******************************************.

*/ center predictors.
compute Week2 = Week - 2.
compute Week2.sq = Week2**2.
exe. 

*/ quadratic growth model.
mixed Severity with Week2 Week2.sq
  /fixed = Week2 Week2.sq | 
  /random = intercept Week2 | subject(Participant) covtype(un)
  /method = reml
  /print = descriptives solution testcov.

*/ quadratic growth model with predictors.
mixed Severity with Week2 Week2.sq Drug
  /fixed = Week2 Week2.sq Drug Week2*Drug Week2.sq*Drug | 
  /random = intercept Week2 | subject(Participant) covtype(un)
  /method = reml
  /print = descriptives solution testcov.
