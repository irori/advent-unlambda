#!/usr/local/bin/gosh
(define *debug-print-label* #f)
(define *debug-print-dwarf* #f)

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
         (world (set-rand world (c6 cdr))))
     ,body))

(require "object.scm")
(require "room.scm")
(require "dwarf.scm")
(require "proc.scm")


;; initial environment
(defmacro initial-return V)
(defmacro initial-location road)
(defmacro initial-newloc road)
(defmacro initial-oldlocs (icons road road))
(defmacro initial-verbose (to-cons1 c4))
(defmacro initial-limit (to-cons1 c330))
(defmacro initial-turns V)
(defmacro initial-clock1 (to-cons1 c15))
(defmacro initial-clock2 (to-cons1 c30))
(defmacro initial-tally c15)
(defmacro initial-lost-treasures c0)
(defmacro initial-panic V)
(defmacro initial-was-dark V)
(defmacro initial-west-count (to-cons1 c10))
(defmacro initial-foobar c0)
(defmacro initial-word12 V)
(defmacro initial-mot V)
(defmacro initial-verb V)
(defmacro initial-oldverb V)
(defmacro initial-not-warned I)
(defmacro initial-hinted (list c2 c2 c2 c4 c5 c3  ; hint2-7
                               c5 c10  ; hint0, hint1
                               c4  ; gave_up
                               c0))  ; bonus
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
             (profile "p|profile")
             (mexpand "m|macroexpand")
             (program-size "s|size")
             )
    (if program-size
	(begin (print-program-table-sizes)
	       (exit 0)))
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
