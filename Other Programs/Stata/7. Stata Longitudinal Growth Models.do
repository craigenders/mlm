/////////////////////////////
// linear growth models    //
/////////////////////////////

// compute means
egen MaleGrandMean = mean(Male)

// center predictors
gen Week4 = Week - 4
gen Male_cgm = Male - MaleGrandMean

// random intercept model
mixed Severity Week4 || Participant:, reml

// linear growth model
mixed Severity Week4, || Participant: Week4, covariance(unstructured) reml dfmethod(kroger) stddeviations

// linear growth model with predictors
mixed Severity Drug Male_cgm Week4 c.Drug##c.Week4, || Participant: Week4, covariance(unstructured) reml dfmethod(kroger) stddeviations

/////////////////////////////
// piecewise growth models //
/////////////////////////////

// compute temporal predictors
gen Week2 = Week - 2
gen WeekEp1 = Week2
gen WeekEp2 = Week2
recode WeekEp1 (2 5 = 0)
recode WeekEp2 (-1 = 0)

// piecewise growth model
mixed Severity WeekEp1 WeekEp2, || Participant: WeekEp1 WeekEp2, covariance(unstructured) reml dfmethod(kroger) stddeviations

// piecewise growth model with predictors
mixed Severity WeekEp1 WeekEp2 Drug c.WeekEp1##c.Drug c.WeekEp2##c.Drug, || Participant: WeekEp1 WeekEp2, covariance(unstructured) reml dfmethod(kroger) stddeviations

/////////////////////////////
// quadratic growth models //
/////////////////////////////

// center predictors
gen Week2 = Week - 2
gen Week2_sq = Week2*Week2

// quadratic growth model
mixed Severity Week2 Week2_sq, || Participant: Week2, covariance(unstructured) reml dfmethod(kroger) stddeviations

// quadratic growth model with predictors
mixed Severity Week2 Week2_sq Drug c.Week2##c.Drug c.Week2_sq##c.Drug, || Participant: Week2, covariance(unstructured) reml dfmethod(kroger) stddeviations
