#!/usr/local/bin/gosh
(define *debug-print-label* #f)

(add-load-path ".")

(use gauche.parseopt)

(use unlambda.compiler)
(use unlambda.prelude)
(set! *compress-string* #t)
(use enum)
(use parser)
(use variable)
(use rand)
(use object)
(use room)
(use dwarf)
(use proc)

;; initial environment
(add-unl-macro!
 'initial-world '()
 (compile-to-file
  "world.unlo"
  (make-initial-map memory-map)))

(defsyntax (debug-print-label label e)
  (if *debug-print-label*
      `(begin
         ((nth ,label label-names) #\newline I)
         ,e)
      e))

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
    (if program-size
	(begin (print-program-table-sizes)
	       (exit 0)))
    (call-with-output-file outfile
      (lambda (port)
	(display "#!/usr/bin/env unlambda\n" port)
        (print-as-unl 'main port))))
  0)
