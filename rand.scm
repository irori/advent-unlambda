#!/usr/local/bin/gosh -I.
(require "unlc.scm")
(require "lib.scm")
(require "churchnum.scm")

(defmacro (xor a b)
  (a (b (K I) K) b))

(defmacro (xor4 a b c d)
  (xor (xor a b) (xor c d)))

(defmacro rand-8
  (((lambda (x) (x x))
    (lambda (rec b0 b1 b2 b3 b4 b5 b6 b7)
      (icons b0
             (rec rec b1 b2 b3 b4 b5 b6 b7 (xor4 b0 b2 b3 b4)))))
   K (K I) (K I) K K (K I) K (K I)))

(defmacro rand-7
  (((lambda (x) (x x))
    (lambda (rec b0 b1 b2 b3 b4 b5 b6)
      (icons b0
             (rec rec b1 b2 b3 b4 b5 b6 (xor b0 b1)))))
   K (K I) K K K (K I) K))

(defmacro initial-rand rand-7)

(defmacro rand-main
  (c100 (lambda (r)
          (r (lambda (b next)
               ((b #\1 #\0) next))))
        rand-8))

;(print-as-unl 'rand-main)
