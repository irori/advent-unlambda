#!/usr/local/bin/gosh -I.
(require "unlc.scm")
(require "lib.scm")

(defmacro (xor a b)
  (a (b (K I) K) b))

(defmacro rand-7
  ((lambda-rec rec (b0 b1 b2 b3 b4 b5 b6)
       (icons b0
              (rec b1 b2 b3 b4 b5 b6 (xor b0 b1))))
   K (K I) K K K (K I) K))

(defmacro initial-rand rand-7)

(defrecmacro (random n bits)
  (if (cons1? n)
      (bits
       (lambda (b bs)
	 ((b succ I)
	  (dbl (random (1-of-1 n) bs)))))
      ((car bits) c1 c0)))

; Randomly select an element from lst
(defmacro (random-select lst bits)
  (let ((n (to-cons1 (random (to-cons1 c5) bits))))
    (let loop ((n n)
               (l lst))
      (cond ((null? l) (loop n lst))
            ((cons1? n) (loop (1-of-1 n) (cdr l)))
            (else (car l))))))

(defsyntax (pct n world)
  (let ((nn (round (/ (* n 64) 100))))
    `(if< (random (c5 cons1 V) (rand ,world)) ,(churchnum nn) I V)))
(defsyntax (let-rand var n body)
  `(let ((,var (pct ,n world))
         (world (set-rand world (c6 cdr))))
     ,body))
