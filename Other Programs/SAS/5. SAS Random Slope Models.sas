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

*/ random slope model with cluster-specific empower coefficients;
proc mixed method = reml covtest noclprint;
class Team;
model JobSat = LMX_cwc Empower_cwc Male LMX_meanj Empower_meanj / solution ddfm = kr;
random intercept Empower_cwc / subject = Team type = un;
run;