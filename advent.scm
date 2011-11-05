#!/usr/local/bin/gosh
(add-load-path ".")

(use gauche.parseopt)

(require "unlc.scm")
(require "lib.scm")
(require "churchnum.scm")

(require "parser.scm")
(require "rand.scm")
(require "variable.scm")

(defsyntax (pct n world)
  (let ((nn (round (/ (* n 64) 100))))
    `(if< (random (c5 cons1 V) (rand ,world)) ,(churchnum nn) I V)))
(defsyntax (let-rand var n body)
  `(let ((,var (pct ,n world))
         (world (set-rand world (c5 cdr))))
     ,body))

(require "object.scm")
(require "room.scm")
(require "proc.scm")


;; initial environment
(defmacro initial-location road)
(defmacro initial-newloc road)
(defmacro initial-oldlocs (icons road road))
(defmacro initial-verbose (to-cons1 c4))
(defmacro initial-limit (to-cons1 c330))
(defmacro initial-turns V)
(defmacro initial-was-dark V)
(defmacro initial-word12 V)
(defmacro initial-mot V)
(defmacro initial-verb V)
(defmacro initial-oldverb V)
(defmacro initial-not-warned I)
(defmacro initial-hinted (list V V V V V V V V))
(defmacro initial-hint-count V)
(defmacro initial-death-count c0)
(defmacro initial-obj V)
(defmacro initial-oldobj V)
(defmacro initial-dummy V)

(define (make-initial-map tree)
  (if (pair? tree)
      `(icons ,(make-initial-map (car tree))
              ,(make-initial-map (cdr tree)))
      (string->symbol (string-append "initial-" (symbol->string tree)))))

(add-unl-macro!
 'initial-world '()
 (compile-to-file
  "world.unlo"
  (make-initial-map memory-map)))


;; main loop
(defrecmacro (advent-mainloop pc world)
  (((nth pc program-table) world) advent-mainloop))

(defmacro main
  (advent-mainloop initial-pc initial-world))


(define (main args)
  (let-args (cdr args)
	    ((outfile "o|outfile=s" "advent.unl")
             (profile "p|profile")
             (mexpand "m|macroexpand")
             )
    (if mexpand
        (begin (write (macroexpand unl-macros 'main))
               (exit 0)))
    (call-with-output-file outfile
      (lambda (port)
	(display "#!/usr/bin/env unlambda\n" port)
        (if profile
            (compile-profile 'main port)
            (print-as-unl 'main port)))))
  0)
