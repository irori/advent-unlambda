#!/usr/local/bin/gosh -I.
(require "unlc.scm")
(require "lib.scm")
(require "churchnum.scm")

(require "room.scm")
(require "parser.scm")

;; mapping of the "world" data structure
(define memory-map
  (cons (cons 'location
              'score)
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


;; initial environment
(defmacro initial-location road)
(defmacro initial-score c32)

(define (make-initial-map tree)
  (if (pair? tree)
      `(cons ,(make-initial-map (car tree))
             ,(make-initial-map (cdr tree)))
      (string->symbol (string-append "initial-" (symbol->string tree)))))

(add-unl-macro! 'initial-world '()
                (make-initial-map memory-map))


(defmacro (unknown-word world)
  (print$ "I DON'T KNOW THAT WORD.\n" world))

(defmacro (command world)
  (call/cc
   (lambda (return)
     (listen
      (lambda (w1 w2)
        (if (word? w1)
            (if (special-word? w1)
                ((word-data (word-table-of world) w1 I) (return world))
              (if (special-word? w2)
                  ((word-data (word-table-of world) w2 I) (return world))
                (return world)))
          (return (unknown-word world))))))))

(defrecmacro (turn world)
  (begin
   ; describe his situation
   ((room-ldesc (lookup-room (room-table-of world) (location world))) I)
   (let ((world2 (command world)))
     (turn world2))))

(defmacro test-room
  ((nth road (short-desc initial-world)) I))

(defmacro test-score
  (let ((world initial-world))
    (let ((world2 (set-score world (add c100))))
      (print-digit (score world2) I))))

(defmacro main
  (turn initial-world))

(define (main args)
  (print-as-unl 'test-room))
