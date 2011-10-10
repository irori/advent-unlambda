#!/usr/local/bin/gosh -I.
(require "unlc.scm")
(require "lib.scm")
(require "churchnum.scm")

(require "room.scm")
(require "parser.scm")

;; mapping of the "world" data structure
(define memory-map
  (cons (cons 'location
              'word12)
        (cons (cons 'default-msg
                    'message)
              (cons 'long-desc
                    'short-desc))))

(define (getter-name sym) sym)
(define (setter-name sym)
  (string->symbol (string-append "set-" (symbol->string sym))))

(define (modifier-body plist)
  (list 'world
        (fold
         (lambda (p e)
           `(lambda (_hd _tl)
              (cons
               ,(if (eq? p 'car) (if e `(_hd ,e) '(modifier _hd)) '_hd)
               ,(if (eq? p 'cdr) (if e `(_tl ,e) '(modifier _tl)) '_tl))))
         #f plist)))

;; generates accessor macros for the memory map
(define (generate-accessors plist tree)
  (if (pair? tree)
      (begin
        (generate-accessors (cons 'car plist) (car tree))
        (generate-accessors (cons 'cdr plist) (cdr tree)))
      (begin
        (add-unl-macro! (getter-name tree) '(world)
                        (fold-right list 'world plist))
        (add-unl-macro! (setter-name tree) '(world modifier)
                        (modifier-body plist)))))

(generate-accessors '() memory-map)



(require "proc.scm")


;; initial environment
(defmacro initial-location road)
(defmacro initial-word12 V)
(defmacro initial-dummy V)

(define (make-initial-map tree)
  (if (pair? tree)
      `(cons ,(make-initial-map (car tree))
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
  (print-as-unl 'main))
