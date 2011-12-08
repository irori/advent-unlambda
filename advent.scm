#!/usr/local/bin/gosh
(define *debug-print-label* #f)
(define *debug-print-dwarf* #f)

(add-load-path ".")

(use gauche.parseopt)

(require "unlc.scm")
(require "lib.scm")
(require "enum.scm")
(require "parser.scm")
(require "variable.scm")
(require "rand.scm")
(require "object.scm")
(require "room.scm")
(require "dwarf.scm")
(require "proc.scm")

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
