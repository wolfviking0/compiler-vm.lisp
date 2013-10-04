compiler & vm lisp
==================

Launch compiler lisp :
----------------------

:$clisp

[1]> (load "gc.lisp")

[2]> (load_file_gc "samples/fibo.lisp" "sample/fibo.lisp.asm")

[3]> (load_file_gc "samples/fact.lisp" "sample/fact.lisp.asm")

[4]> (load_file_gc "samples/divers.lisp" "sample/divers.lisp.asm")

[5]> (run_gc '(fibo5 5))
((MOVE ($ 5) R0) (PUSH R0) (MOVE ($ 1) R0) (PUSH R0) (INCR R0) (MOVE FP R1) (MOVE SP FP)
 (MOVE SP R2) (SUB R0 R2) (PUSH R2) (PUSH R1) (JSR (@ FIBO5)) (POP R1) (POP R2) (MOVE R1 FP)
 (MOVE R2 SP) (HALT))
 


Launch VM lisp :
----------------

[1]> (load "vm.lisp")

[2]> (make_vm 'test 1048576)

[3]> (load_file_vm 'test "samples/fibo.lisp.asm") // Load the fibo asm code inside the vm

[4]> (apply_vm 'test '((MOVE ($ 5) R0) (PUSH R0) (MOVE ($ 1) R0) (PUSH R0) (INCR R0) (MOVE FP R1) (MOVE SP FP)
 (MOVE SP R2) (SUB R0 R2) (PUSH R2) (PUSH R1) (JSR (@ FIBO5)) (POP R1) (POP R2) (MOVE R1 FP)
 (MOVE R2 SP) (HALT)) 1) // Call (fibo5 5) in debug steb by step (press enter)
 
[5]> (apply_vm 'test '((MOVE ($ 5) R0) (PUSH R0) (MOVE ($ 1) R0) (PUSH R0) (INCR R0) (MOVE FP R1) (MOVE SP FP)
 (MOVE SP R2) (SUB R0 R2) (PUSH R2) (PUSH R1) (JSR (@ FIBO5)) (POP R1) (POP R2) (MOVE R1 FP)
 (MOVE R2 SP) (HALT)) nil) // Call (fibo5 5) in release

License :
--------

compiler-vm.lisp is MIT licensed, see LICENSE.