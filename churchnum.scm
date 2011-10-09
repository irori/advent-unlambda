(load "lib.scm")

(defmacro (T x y) (y x))

(defsyntax (churchnum n)
  `(lambda (s z)
     ,(let rec ((n n))
	(if (zero? n) 'z `(s ,(rec (- n 1)))))))

(defsyntax (add-n n num)
  `(lambda (s z)
     ,(let rec ((n n))
	(if (zero? n)
	    `(,num s z)
	    `(s ,(rec (- n 1)))))))

(defmacro zero (lambda (s z) z))
(defmacro (succ n) (lambda (s z) (s (n s z))))
(defmacro (zero? n) (n V I))
(defmacro (nonzero? n) (n (K I) V))
(defmacro (ifnonzero n x y) ((n (K x)) y))

(defmacro (add x y)
  (lambda (s z)
    (x s (y s z))))

(defmacro (mul x y)
  (lambda (s z)
    (x (y s) z)))

(defmacro (pow x n)
  (n x))

(defmacro (pred n)
  (lambda (s z)
    (((n (lambda (g h) (h (g s)))) (lambda (x) z)) I)))

; (pred 2)
; (lambda (s z) ((2 (lambda (g h) (h (g s))) (lambda (x) z)) I))
; (lambda (s z) (((lambda (g h) (h (g s))) ((lambda (g h) (h (g s))) (lambda (x) z))) I)
; (lambda (s z) (I (((lambda (g h) (h (g s))) (lambda (x) z)) s)))
; (lambda (s z) (((lambda (g h) (h (g s))) (lambda (x) z)) s))
; (lambda (s z) (s ((lambda (x) z) s)))
; (lambda (s z) (s z))

(defmacro (nth n lst)
  (car (n cdr lst)))

(defrecmacro (length xs)
  (if (pair? xs)
      (succ (length (cdr xs)))
      zero))

(defrecmacro (cons1-length xs)
  (if (cons1? xs)
      (succ (cons1-length (1-of-1 xs)))
      zero))

(defmacro (sub x y)
  (cons1-length ((y 1-of-1) (x cons1 V))))

(defmacro (if<= m n x y) ((m T (K x)) (n T (K y))))
(defmacro (if>  m n x y) (if<= m n y x))
(defmacro (if>= m n x y) (if<= n m x y))
(defmacro (if<  m n x y) (if<= n m y x))

(defmacro (<  m n) (if<  m n #t #f))
(defmacro (>  m n) (if>  m n #t #f))
(defmacro (<= m n) (if<= m n #t #f))
(defmacro (>= m n) (if>= m n #t #f))
(defmacro (= m n) (nth n (m (cons #f) (cons #t nil))))

(defmacro (div x y)
  (let rec ((xs (cons1 (x cons1 V))))
    (let ((xs2 ((y 1-of-1) xs)))
      (if (cons1? xs2)
	  (succ (rec xs2))
	  zero))))

;(defrecmacro (div x y)
;  (if (< x y)
;      zero
;      (succ (div (sub x y) y))))

(defmacro (mod x y)
  (let rec ((xs (cons1 (x cons1 V))))
    (let ((xs2 ((y 1-of-1) xs)))
      (if (cons1? xs2)
	  (rec xs2)
	  (cons1-length (1-of-1 xs))))))

(defmacro (divmod x y)
  (let rec ((xs (cons1 (x cons1 V))) (q zero))
    (let ((xs2 ((y 1-of-1) xs)))
      (if (cons1? xs2)
	  (rec xs2 (succ q))
	  (cons q (cons1-length (1-of-1 xs)))))))

;(defrecmacro (mod x y)
;  (if (< x y)
;      x
;      (mod (sub x y) y)))

(load "./churchnum.tbl")

(defmacro (print-digit1 n)
  (nth n (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(defrecmacro (print-digit n)
  (if (<= n c9)
      (print-digit1 n)
      ((divmod n c10)
       (lambda (m d)
	 (S (print-digit m) (print-digit1 d))))))

(defmacro parse-digit1-fn
 (lambda (q)
   (((((((((((((? #\0) I) q) c0)
	    ((((? #\1) I) q) c1))
	   ((((? #\2) I) q) c2))
	  ((((? #\3) I) q) c3))
	 ((((? #\4) I) q) c4))
	((((? #\5) I) q) c5))
       ((((? #\6) I) q) c6))
      ((((? #\7) I) q) c7))
     ((((? #\8) I) q) c8))
    ((((? #\9) I) q) c9))))

(defmacro parse-digit1
  (call/cc parse-digit1-fn))

(defmacro (read-digit1)
  ((@ I) parse-digit1))
