#!/usr/local/bin/gosh
(define *debug-print-label* #f)

(add-load-path ".")

(use gauche.parseopt)

(use unlambda.compiler)
(use unlambda.prelude)
(use string)
(use util)
(use enum)
(use parser)
(use variable)
(use rand)
(use object)
(use room)
(use dwarf)
(use proc)

;; initial environment
(unl-module 'initial-world
	    (make-initial-map memory-map))

(add-unl-syntax! 'debug-print-label
  (lambda (form rename compare)
    (let ((label (cadr form))
	  (e (caddr form)))
      (if *debug-print-label*
	  (quasirename rename
	    `(begin
              ((nth ,label label-names) #\newline I)
              ,e))
	  e))))

;; trampoline driver
(defrecmacro (trampoline label world)
  (debug-print-label label
    (((nth label program-table) world) trampoline)))

(defmacro main
  ((call/cc
    (lambda (ret)
      (cons initial-label
	    (set-return initial-world (K ret)))))
   trampoline))


(define (main args)
  (let-args (cdr args)
	    ((outfile "o|outfile=s" "advent.unl")
             (program-size "s|size")
             )
    (compile-modules)
    (if program-size
	(begin (print-program-table-sizes)
	       (exit 0)))
    (compile-program-table)
    (with-output-to-file outfile
      (lambda ()
	(display "#!/usr/bin/env unlambda\n")
        (print-as-unl 'main))))
  0)
