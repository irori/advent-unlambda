#!/usr/local/bin/gosh -I.
(require "unlc.scm")
(require "lib.scm")

; K is bit 1 and (K I) is bit 0
(defmacro (xor a b)
  (a (b (K I) K) b))

; Infinite list of psuedo-random bits generated by 7-bit LFSR
; see http://en.wikipedia.org/wiki/Linear_feedback_shift_register
(defmacro rand-7
  ((lambda-rec rec (b0 b1 b2 b3 b4 b5 b6)
       (icons b0
              (rec b1 b2 b3 b4 b5 b6 (xor b0 b1))))
   K (K I) K K K (K I) K))

(defmacro initial-rand rand-7)

; (random n lst) generates a church number from head (n+1) bits of lst
(defmacro (random n)
  (n
   (lambda (f bits)
     (bits
      (lambda (b bs)
	((b succ I)
	 (dbl (f bs))))))
   (lambda (bits)
     ((car bits) c1 c0))))

; Randomly select an element from lst
(defmacro (random-select lst bits)
  (let ((n (to-cons1 (random c5 bits))))
    (let loop ((n n)
               (l lst))
      (cond ((null? l) (loop n lst))
            ((cons1? n) (loop (1-of-1 n) (cdr l)))
            (else (car l))))))

; Returns true n% of the time.
(defsyntax (pct n world)
  (let ((nn (round (/ (* n 64) 100))))
    `(< (random c5 (rand ,world)) ,(churchnum nn))))

(defsyntax (let-rand var n body)
  `(let ((,var (pct ,n world))
         (world (set-rand world (c6 cdr))))
     ,body))