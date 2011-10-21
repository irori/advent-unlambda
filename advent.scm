#!/usr/local/bin/gosh
(add-load-path ".")

(use gauche.parseopt)

(require "unlc.scm")
(require "lib.scm")
(require "churchnum.scm")

(require "parser.scm")
(require "rand.scm")
(require "variable.scm")

(defsyntax (pct n world x y)
  (let ((nn (round (/ (* n 64) 100))))
    `(if< (random (c5 cons1 V) (rand ,world)) ,(churchnum nn) ,x ,y)))

(require "object.scm")
(require "room.scm")
(require "proc.scm")


;; initial environment
(defmacro initial-location road)
(defmacro initial-newloc road)
(defmacro initial-oldlocs (icons road road))
(defmacro initial-word12 V)
(defmacro initial-mot V)
(defmacro initial-verb V)
(defmacro initial-obj V)
(defmacro initial-dummy V)

(define (make-initial-map tree)
  (if (pair? tree)
      `(icons ,(make-initial-map (car tree))
              ,(make-initial-map (cdr tree)))
      (string->symbol (string-append "initial-" (symbol->string tree)))))

(add-unl-macro! 'initial-world '()
                (make-initial-map memory-map))


;; main loop
(defrecmacro (advent-mainloop pc world)
  (((nth pc program-table) world) advent-mainloop))

(defmacro main
  (advent-mainloop initial-pc initial-world))


(define (main args)
  (let-args (cdr args)
	    ((outfile "o|outfile=s" "advent.unl"))
    (call-with-output-file outfile
      (lambda (port)
	(display "#!/usr/bin/env unlambda\n" port)
  ; (compile-profile 'main)
	(print-as-unl 'main port))))
  0)
