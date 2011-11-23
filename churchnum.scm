(require "lib.scm")

(define (churchnum n)
  (if (< n 0)
      (error "negative argument" n)
      (string->symbol (string-append "c" (number->string n)))))

(defmacro (churchnum? x) (x I I))
(defmacro zero (lambda (s z) z))
(defmacro (succ n) (lambda (s z) (s (n s z))))
(defmacro (zero? n) (n V I))
(defmacro (nonzero? n) (n (K I) V))
(defmacro (ifnonzero n x y) ((n (K x)) y))

(defmacro (add x y)
  (lambda (s z)
    (x s (y s z))))

(defsyntax (+ e . es)
  (fold (lambda (x y) `(add ,y ,x)) e es))

(defmacro (mul x y)
  (lambda (s z)
    (x (y s) z)))

(defsyntax (* e . es)
  (fold (lambda (x y) `(mul ,y ,x)) e es))

(defmacro dbl
  (lambda (x s z)
    (x s (x s z))))

(defmacro (pow x n)
  (n x))

(defmacro (pred n)
  (lambda (s z)
    (((n (lambda (g h) (h (g s)))) (lambda (x) z)) I)))

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

(defmacro (<  m n) (if<  m n I V))
(defmacro (>  m n) (if>  m n I V))
(defmacro (<= m n) (if<= m n I V))
(defmacro (>= m n) (if>= m n I V))
(defmacro (= m n) (nth n (m (icons V) (icons I nil))))

(defmacro (div x y)
  (let rec ((xs (cons1 (x cons1 V))))
    (let ((xs2 ((y 1-of-1) xs)))
      (if (cons1? xs2)
	  (succ (rec xs2))
	  zero))))

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
	  (icons q (cons1-length (1-of-1 xs)))))))

(require "./churchnum.tbl")

(defmacro (print-digit1 n)
  (nth n (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(defrecmacro (print-digit n)
  (if (<= n c9)
      (print-digit1 n)
      ((divmod n c10)
       (lambda (m d)
	 (S (print-digit m) (print-digit1 d))))))
