#!/usr/local/bin/gosh -I.
(load "unlc.scm")
(load "lib.scm")
(load "churchnum.scm")

(load "room.scm")
(load "parser.scm")

(defmacro variable-table-of car)
(defmacro room-table-of cadr)
(defmacro (func-table-of world) (car (cddr world)))
(defmacro (word-table-of world) (cadr (cddr world)))

;; functions
(defmacro def-func-table
  (cons print-digit
        I))
(defmacro (Print-digit world) (car (func-table-of world)))


;; variables
(defmacro initial-location c1)
(defmacro initial-score c32)

(defmacro initial-variable-table
  (cons initial-location
	initial-score))

(defsyntax (access-vtbl pos vtbl)
  (fold
   (lambda (p e)
     (list (if p 'cdr 'car)
	   e))
   vtbl
   (map (lambda (x) (eq? x #\1)) (string->list pos))))

(defsyntax (modify-vtbl pos f vtbl)
  (list
   vtbl
   (fold-right
    (lambda (p e)
      `(lambda (_hd _tl)
	 (cons
	  ,(if p '_hd (if e `(_hd ,e) `(,f _hd)))
	  ,(if p (if e `(_tl ,e) `(,f _tl)) '_tl))))
    #f
    (map (lambda (x) (eq? x #\1)) (string->list pos)))))

(defmacro (location world) (access-vtbl "00" world))
(defmacro (set-location world modifier) (modify-vtbl "00" modifier world))

(defmacro (score world) (access-vtbl "01" world))
(defmacro (set-score world modifier) (modify-vtbl "01" modifier world))

;; global environment
(defmacro initial-world
  (list initial-variable-table
        def-room-table
        def-func-table
        def-word-table))


(defmacro (unknown-word world)
  (print$ "I DON'T KNOW THAT WORD.\n" world))

(defmacro (command world)
  (call/cc
   (lambda (return)
     (getin-loop
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

(defmacro main-old
  (lambda (world)
    (let ((world2 (set-score world (add c100))))
      (Print-digit world2 (score world2) I))))

(defmacro main
  (turn initial-world))

(print-as-unl 'main)
