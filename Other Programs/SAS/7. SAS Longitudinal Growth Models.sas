data drugtrial;
set "/folders/myfolders/schizophreniatrialdata.sas7bdat";
run;

*******************************************;
*   linear growth models
*******************************************;

*/ compute grand means;
proc means data = drugtrial noprint;
var Male;
output out = grandmeans (drop = _TYPE_ _FREQ_) mean = MaleGrandMean; 
run;

*/ add grand means means to data and center;
data drugtrial;
if _N_ = 1 then set grandmeans;
set drugtrial;
Male_cgm = Male - MaleGrandMean;
Week4 = Week - 4;
run;

*/ random intercept model;
proc mixed method = reml covtest noclprint;
class Participant;
model Severity = Week4 / solution ddfm = kr;
random intercept / subject = Participant type = un;
run;

*/ linear growth model;
proc mixed method = reml covtest noclprint;
class Participant;
model Severity = Week4 / solution ddfm = kr;
random intercept Week4 / subject = Participant type = un;
run;

*/ linear growth model with predictors;
proc mixed method = reml covtest noclprint;
class Participant;
model Severity = Drug Male_cgm Week4 Drug*Week4 / solution ddfm = kr;
random intercept Week4 / subject = Participant type = un;
run;

*******************************************;
*   piecewise growth models
*******************************************;

*/ compute temporal predictors;
data drugtrial;
set drugtrial;
Week2 = Week - 2;
WeekEp1 = Week2;
WeekEp2 = Week2;
if (WeekEp1 ge 0) then WeekEp1 = 0;
if (WeekEp2 le 0) then WeekEp2 = 0;
run;

*/ piecewise growth model;
proc mixed method = reml covtest noclprint;
class Participant;
model Severity = WeekEp1 WeekEp2 / solution ddfm = kr;
random intercept WeekEp1 WeekEp2 / subject = Participant type = un;
run;

*/ piecewise growth model with predictors;
proc mixed method = reml covtest noclprint;
class Participant;
model Severity = WeekEp1 WeekEp2 Drug WeekEp1*Drug WeekEp2*Drug / solution ddfm = kr;
random intercept WeekEp1 WeekEp2 / subject = Participant type = un;
run;

*******************************************;
*   quadratic growth models
*******************************************;

*/ compute temporal predictors;
data drugtrial;
set drugtrial;
Week2 = Week - 2;
Week2_sq = Week2**2;
run;

*/ quadratic growth model;
proc mixed method = reml covtest noclprint;
class Participant;
model Severity = Week2 Week2_sq / solution ddfm = kr;
random intercept Week2 / subject = Participant type = un;
run;

*/ quadratic growth model with predictors;
proc mixed method = reml covtest noclprint;
class Participant;
model Severity = Week2 Week2_sq Drug Week2*Drug Week2_sq*Drug / solution ddfm = kr;
random intercept Week2 / subject = Participant type = un;
run;
