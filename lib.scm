; -*- scheme-*-
;; character predicates
(defmacro ?space (? #\space))
(defmacro ?newline (? #\newline))

;; control structures
(defmacro *if*
  ; (lambda (b) (call/cc (lambda (q) ((K (K I)) ((b q) K)))))
  ((S (K call/cc)) ((S (K (S (K (K (K I)))))) ((S S) (K (K K))))))

(defsyntax (if condition consequent alternative)
  `((**if**
     ,condition
     (lambda (**if-dummy**) ,consequent)
     (lambda (**if-dummy**) ,alternative)) I))

(defsyntax (begin . es)
  (reduce-right (lambda (e rest)
		  `((lambda (*dummy*) ,rest) ,e))
		'I
		es))

(defsyntax (when condition consequent)
  `((,condition
     (lambda (**when-dummy**) ,consequent)) I))

(defmacro (unless condition alternative)
  (if condition #f alternative))

; (cond (c1 b1) (c2 b2) ... (cn bn) [(else b)])
; c1..cn-1 �� false, cn �� true �̂Ƃ��A b1..bn-1 ���]�������̂Œ���
(defsyntax (cond . clauses)
  `(call/cc
    (lambda (*q*)
      ,(map (lambda (pair)
	      (if (eq? (car pair) 'else)
		  `(*q* ,(cadr pair))
		  `(,(car pair) *q* ,(cadr pair))))
	    clauses))))

(defsyntax (let* binds body)
  (fold-right
   (lambda (bind rest)
     `(let (,bind) ,rest))
   body
   binds))

;; I/O functions
(defsyntax (read-char=? c . cs)
  (if (null? cs)
      `((? ,c) I)
      `(call/cc
        (lambda (_c)
          ,(map (lambda (ch) `((? ,ch) I _c I))
                (cons c cs))))))
(defmacro (read-char) (@ I))
(defmacro (reprint-char) ((! I) I))

(defsyntax (string s)
  ; (string "abc") => (d (#\b (#\a #\c)))
  (let* ((len (string-length s))
	 (lastc (string-ref s (- len 1)))
	 (cs (string->list (substring s 0 (- len 1)))))
    `(D ,(fold list lastc cs))))

(defsyntax (print$ s e)
  ; (print$ "abc" e) => (#\c (#\b (#\a e)))
  (fold list e (string->list s)))

;; boolean functions
(defmacro (not b) (if b #f #t))
;(defmacro (and a b) (a b))
;(defmacro (or a b) (if a #t b))
(defsyntax (or . es)
  (let rec ((es es))
    (if (null? (cdr es))
	(car es)
	`(if ,(car es) #t ,(rec (cdr es))))))
(defsyntax (and . es)
  es)

;; list functions
(defmacro nil V)
(defmacro (cons a b) (lambda (_f) (_f a b)))
(defmacro (snoc a b) (lambda (f) (f b a)))
(defmacro ncons (cons)) ; non-inlined cons
(defmacro nsnoc (snoc)) ; non-inlined snoc
(defmacro (pair? x) (x (lambda (a b) #t)))
(defmacro (null? x) (not (pair? x)))
(defmacro (car x) (x (lambda (a b) a)))
(defmacro (cdr x) (x (lambda (a b) b)))
(defmacro ->car (lambda (a b) a))
(defmacro ->cdr (lambda (a b) b))
(defmacro (caar x) (car (car x)))
(defmacro (cadr x) (car (cdr x)))
(defmacro (cdar x) (cdr (car x)))
(defmacro (cddr x) (cdr (cdr x)))

(defsyntax (list . es)
  (fold-right (lambda (hd tl) `(cons ,hd ,tl))
	      'nil
	      es))

; (cons* a b c) => (cons a (cons b c))
(defsyntax (cons* . args)
  (let rec ((args args))
    (if (null? (cddr args))
	`(cons ,(car args) ,(cdr args))
	`(cons ,(car args) ,(rec (cdr args))))))

(defmacro (cons1 x)
  (lambda (f) (f x)))
(defmacro (cons1? x) (x (lambda (a) #t)))
(defmacro (1-of-1 o) (o I))

;; utilities
(defmacro (compose f g)
  (lambda (x) (f (g x))))
(defmacro (M x) (x x))
