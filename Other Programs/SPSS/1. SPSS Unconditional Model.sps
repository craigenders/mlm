* Encoding: UTF-8.

*/ unconditional (empty) multilevel model with fiml estimation.
mixed PosAffect   
  /fixed = | 
  /random = intercept | subject(Participant) covtype(id)
  /method = ml
  /print = descriptives solution testcov.

*/ unconditional (empty) multilevel model with reml estimation.
mixed PosAffect   
  /fixed = |
  /random = intercept | subject(Participant) covtype(id)
  /method = reml
  /print = descriptives solution testcov.