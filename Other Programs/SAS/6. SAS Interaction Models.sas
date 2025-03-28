data employee;
set "/folders/myfolders/employeesatisfactiondata.sas7bdat";
run;

proc sort data = employee;
by Team;
run;

*/ compute cluster means;
proc means data = employee noprint;
var LMX Empower;
by Team;
output out = groupmeans (drop = _TYPE_ _FREQ_) mean = LMX_meanj Empower_meanj;
run;

*/ add cluster means means to data and center;
data employee;
merge groupmeans employee;
by Team;
LMX_cwc = LMX - LMX_meanj;
Empower_cwc = Empower - Empower_meanj;
run;

*/ compute grand means;
proc means data = employee noprint;
var Climate;
output out = grandmeans (drop = _TYPE_ _FREQ_) mean = ClimateGrandMean; 
run;

*/ add grand means means to data and center;
data employee;
if _N_ = 1 then set grandmeans;
set employee;
Climate_cgm = Climate - ClimateGrandMean;
run;

*/ random slope model and cross-level interaction
proc mixed method = reml covtest noclprint;
class Team;
model JobSat = LMX_cwc Empower_cwc Male LMX_meanj Empower_meanj Climate_cgm*LMX_cwc / solution ddfm = kr;
random intercept Empower_cwc / subject = Team type = un;
run;
