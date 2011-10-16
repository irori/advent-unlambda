#!/usr/local/bin/gosh -I.
(require "unlc.scm")
(require "lib.scm")
(require "churchnum.scm")

(require "parser.scm")
(require "rand.scm")
(require "variable.scm")
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
(defmacro initial-dummy V)

(define (make-initial-map tree)
  (if (pair? tree)
      `(icons ,(make-initial-map (car tree))
              ,(make-initial-map (cdr tree)))
      (string->symbol (string-append "initial-" (symbol->string tree)))))

(add-unl-macro! 'initial-world '()
                (make-initial-map memory-map))


;; main loop
(defrecmacro (mainloop pc world)
  (((nth pc program-table) world) mainloop))

(defmacro main
  (mainloop initial-pc initial-world))


(define (main args)
  ; (compile-profile 'main)
  (print-as-unl 'main)
  0)
