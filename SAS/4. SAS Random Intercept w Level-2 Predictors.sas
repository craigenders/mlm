data pain;
set "/folders/myfolders/paindiarydata.sas7bdat";
run;

proc sort data = pain;
by Participant;
run;

*/ compute cluster means;
proc means data = pain noprint;
var SleepQual Pain;
by Participant;
output out = groupmeans (drop = _TYPE_ _FREQ_) mean = SleepQual_meanj Pain_meanj;
run;

*/ add cluster means means to data and center;
data pain;
merge groupmeans pain;
by Participant;
SleepQual_cwc = SleepQual - SleepQual_meanj;
Pain_cwc = Pain - Pain_meanj;
run;

*/ compute grand means;
proc means data = pain noprint;
var SleepQual Pain Stress;
output out = grandmeans (drop = _TYPE_ _FREQ_) mean = SleepQualGrandMean PainGrandMean StressGrandMean; 
run;

*/ add grand means means to data and center;
data pain;
if _N_ = 1 then set grandmeans;
set pain;
SleepQual_meanj_cgm = SleepQual_meanj - SleepQualGrandMean;
Pain_meanj_cgm = Pain_meanj - PainGrandMean;
Stress_cgm = Stress - StressGrandMean;
run;

*/ random intercept model with level-1 and level-2 predictors;
proc mixed method = reml covtest noclprint;
class Participant;
model PosAffect = SleepQual_cwc Pain_cwc SleepQual_meanj_cgm Pain_meanj_cgm Stress_cgm Female / solution ddfm = kr;
random intercept / subject = Participant type = un;
run;