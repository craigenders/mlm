data pain;
set "/folders/myfolders/paindiarydata.sas7bdat";
run;

*/ unconditional (empty) multilevel model with reml estimation
proc mixed method = reml covtest noclprint;
class Participant;
model PosAffect = / solution ddfm = kr;
random intercept / subject = Participant type = un;
run;

*/ unconditional (empty) multilevel model with fiml estimation
proc mixed method = ml covtest noclprint;
class Participant;
model PosAffect = / solution ddfm = kr;
random intercept / subject = Participant type = un;
run;