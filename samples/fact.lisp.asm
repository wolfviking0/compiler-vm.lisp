(LABEL FACT3)
(MOVE FP R0)
(PUSH R0)
(MOVE FP R2)
(MOVE (3 FP) FP)
(MOVE (-2 FP) R0)
(MOVE R2 FP)
(PUSH R0)
(MOVE ($ 1) R0)
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
(JSR (@ 32))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(DECR SP)
(JMP (@ 34))
(LABEL 32)
(MOVE FP R0)
(PUSH R0)
(MOVE (-3 FP) R0)
(PUSH R0)
(MOVE ($ 0) R0)
(PUSH R0)
(POP R1)
(POP R0)
(CMP R0 R1)
(JEQ (@ 35))
(MOVE (-3 FP) R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(POP R1)
(POP R0)
(SUB R1 R0)
(PUSH R0)
(MOVE (-3 FP) R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(POP R0)
(MUL R1 R0)
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
(JSR (@ 32))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(JMP (@ 36))
(LABEL 35)
(MOVE (-2 FP) R0)
(LABEL 36)
(DECR SP)
(RTN)
(LABEL 34)
(RTN)
(HALT)
(LABEL FACT2)
(MOVE (-3 FP) R0)
(PUSH R0)
(MOVE ($ 0) R0)
(PUSH R0)
(POP R1)
(POP R0)
(CMP R0 R1)
(JEQ (@ 30))
(MOVE (-3 FP) R0)
(PUSH R0)
(MOVE ($ 1) R0)
(PUSH R0)
(POP R1)
(POP R0)
(SUB R1 R0)
(PUSH R0)
(MOVE (-3 FP) R0)
(PUSH R0)
(MOVE (-2 FP) R0)
(PUSH R0)
(POP R1)
(POP R0)
(MUL R1 R0)
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
(JSR (@ FACT2))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(JMP (@ 31))
(LABEL 30)
(MOVE (-2 FP) R0)
(LABEL 31)
(RTN)
(HALT)
(LABEL FACT1)
(MOVE (-2 FP) R0)
(PUSH R0)
(MOVE ($ 0) R0)
(PUSH R0)
(POP R1)
(POP R0)
(CMP R0 R1)
(JEQ (@ 28))
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
(JSR (@ FACT1))
(POP R1)
(POP R2)
(MOVE R1 FP)
(MOVE R2 SP)
(PUSH R0)
(POP R1)
(POP R0)
(MUL R1 R0)
(JMP (@ 29))
(LABEL 28)
(MOVE ($ 1) R0)
(LABEL 29)
(RTN)
(HALT)
nil