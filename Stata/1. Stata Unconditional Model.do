// unconditional (empty) multilevel model with reml estimation
mixed PosAffect || Participant:, reml

// unconditional (empty) multilevel model with fiml estimation
mixed PosAffect || Participant:, mle
