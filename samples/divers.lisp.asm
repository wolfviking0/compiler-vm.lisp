(JMP (@ 61))
(LABEL 62)
(0)
(LABEL 61)
(LABEL COMPTEUR)
(LOAD (@ 62) R0)
(RTN)
(LABEL COMPTEUR++)
(LOAD (@ 62) R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(POP R1)
(POP R0)
(ADD R1 R0)
(PUSH R0)
(POP R0)
(STORE R0 (@ 62))
(RTN)
(LABEL RESET)
(MOVE ($ 0) R0)
(PUSH R0)
(POP R0)
(STORE R0 (@ 62))
(RTN)
(DECR SP)
(HALT)
(LABEL ADJOINT)
(MOVE (-2 FP) R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ ATOM))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(CMP R0)
(JEQ (@ 57))
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(CAR R1 R0)
(PUSH R0)
(MOVE (-3 FP) R0)
(PUSH R0)
(POP R1)
(POP R0)
(CMP R0 R1)
(JEQ (@ 59))
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(CAR R1 R0)
(PUSH R0)
(MOVE (-3 FP) R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(CDR R1 R0)
(PUSH R0)
(MOVE ($ 2) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ ADJOINT))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(PUSH R0)
(MOVE ($ 2) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ CONS))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(JMP (@ 60))
(LABEL 59)
(MOVE (-2 FP) R0)
(LABEL 60)
(JMP (@ 58))
(LABEL 57)
(MOVE (-3 FP) R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(MOVE ($ 2) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ CONS))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(LABEL 58)
(RTN)
(HALT)
(LABEL CONCAT)
(MOVE (-3 FP) R0)
(PUSH R0)
(POP R1)
(CDR R1 R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ ATOM))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(CMP R0)
(JEQ (@ 55))
(MOVE (-3 FP) R0)
(PUSH R0)
(POP R1)
(CAR R1 R0)
(PUSH R0)
(MOVE (-3 FP) R0)
(PUSH R0)
(POP R1)
(CDR R1 R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(MOVE ($ 2) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ CONCAT))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(PUSH R0)
(MOVE ($ 2) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ CONS))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(JMP (@ 56))
(LABEL 55)
(MOVE (-3 FP) R0)
(PUSH R0)
(POP R1)
(CAR R1 R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(MOVE ($ 2) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ CONS))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(LABEL 56)
(RTN)
(HALT)
(LABEL SUPPRIME)
(MOVE (-2 FP) R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ ATOM))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(CMP R0)
(JEQ (@ 51))
(MOVE (-3 FP) R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(CAR R1 R0)
(PUSH R0)
(POP R1)
(POP R0)
(CMP R0 R1)
(JEQ (@ 53))
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(CAR R1 R0)
(PUSH R0)
(MOVE (-3 FP) R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(CDR R1 R0)
(PUSH R0)
(MOVE ($ 2) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ SUPPRIME))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(PUSH R0)
(MOVE ($ 2) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ CONS))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(JMP (@ 54))
(LABEL 53)
(MOVE (-3 FP) R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(CDR R1 R0)
(PUSH R0)
(MOVE ($ 2) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ SUPPRIME))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(LABEL 54)
(JMP (@ 52))
(LABEL 51)
(MOVE (-2 FP) R0)
(LABEL 52)
(RTN)
(HALT)
(LABEL CREERLISTED)
(MOVE (-2 FP) R0)
(PUSH R0)
(MOVE ($ 0) R0)
(PUSH R0)
(POP R1)
(POP R0)
(CMP R0 R1)
(JEQ (@ 49))
(MOVE (-2 FP) R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(POP R1)
(POP R0)
(SUB R1 R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ CREERLISTED))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(PUSH R0)
(MOVE ($ 2) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ CONS))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(JMP (@ 50))
(LABEL 49)
(MOVE ($ NIL) R0)
(LABEL 50)
(RTN)
(HALT)
(LABEL CREERLISTECBIS)
(MOVE ($ 0) R0)
(PUSH R0)
(MOVE (-3 FP) R0)
(PUSH R0)
(POP R1)
(POP R0)
(CMP R0 R1)
(JEQ (@ 47))
(MOVE (-2 FP) R0)
(PUSH R0)
(MOVE (-3 FP) R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(POP R1)
(POP R0)
(SUB R1 R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(POP R1)
(POP R0)
(ADD R1 R0)
(PUSH R0)
(MOVE ($ 2) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ CREERLISTECBIS))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(PUSH R0)
(MOVE ($ 2) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ CONS))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(JMP (@ 48))
(LABEL 47)
(MOVE ($ NIL) R0)
(LABEL 48)
(RTN)
(HALT)
(LABEL DERNIER)
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(CDR R1 R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ CONSP))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(CMP R0)
(JEQ (@ 45))
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(CAR R1 R0)
(JMP (@ 46))
(LABEL 45)
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(CDR R1 R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ DERNIER))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(LABEL 46)
(RTN)
(HALT)
(LABEL MEMBRE)
(MOVE (-2 FP) R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ ATOM))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(CMP R0)
(JEQ (@ 41))
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(CAR R1 R0)
(PUSH R0)
(MOVE (-3 FP) R0)
(PUSH R0)
(POP R1)
(POP R0)
(CMP R0 R1)
(JEQ (@ 43))
(MOVE (-3 FP) R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(CDR R1 R0)
(PUSH R0)
(MOVE ($ 2) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ MEMBRE))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(JMP (@ 44))
(LABEL 43)
(MOVE (-2 FP) R0)
(LABEL 44)
(JMP (@ 42))
(LABEL 41)
(MOVE (-2 FP) R0)
(LABEL 42)
(RTN)
(HALT)
(LABEL LONGUEUR-T)
(MOVE ($ NIL) R0)
(PUSH R0)
(MOVE (-3 FP) R0)
(PUSH R0)
(POP R1)
(POP R0)
(CMP R0 R1)
(JEQ (@ 39))
(MOVE (-3 FP) R0)
(PUSH R0)
(POP R1)
(CDR R1 R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(POP R1)
(POP R0)
(ADD R1 R0)
(PUSH R0)
(MOVE ($ 2) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ LONGUEUR-T))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(JMP (@ 40))
(LABEL 39)
(MOVE (-2 FP) R0)
(LABEL 40)
(RTN)
(HALT)
(LABEL LONGUEUR)
(MOVE ($ NIL) R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(POP R0)
(CMP R0 R1)
(JEQ (@ 37))
(MOVE ($ 1) R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(CDR R1 R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(INCR R0)
(MOVE FP R1)
(MOVE SP FP)
(MOVE SP R2)
(SUB R0 R2)
(PUSH R2)
(PUSH R1)
(JSR (@ LONGUEUR))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(PUSH R0)
(POP R1)
(POP R0)
(ADD R1 R0)
(JMP (@ 38))
(LABEL 37)
(MOVE ($ 0) R0)
(LABEL 38)
(RTN)
(HALT)
nil