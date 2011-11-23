#!/usr/local/bin/gosh -I.
(require "unlc.scm")
(require "lib.scm")

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

(defrecmacro (random n bits)
  (if (cons1? n)
      (bits
       (lambda (b bs)
	 ((b succ I)
	  (dbl (random (1-of-1 n) bs)))))
      ((car bits) c1 c0)))

; Randomely select an element from lst
(defmacro (random-select lst bits)
  (let ((n (to-cons1 (random (to-cons1 c5) bits))))
    (let loop ((n n)
               (l lst))
      (cond ((null? l) (loop n lst))
            ((cons1? n) (loop (1-of-1 n) (cdr l)))
            (else (car l))))))

(defmacro rand-main
  (c100 (lambda (r)
	  (begin
	    (print-digit (rand (c5 cons1 V) r) I (#\newline I))
	    (c6 cdr r)))
        rand-8))

(defmacro random-select-test
  (c100 (lambda (bits)
          (begin
            ((random-select (list #\1 #\2 #\3 #\4 #\5) bits) I)
            (c6 cdr bits)))
        rand-7))

;(print-as-unl 'random-select-test)
