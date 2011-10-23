; -*- scheme-*-
;; character predicates
(defmacro ?space (? #\space))
(defmacro ?newline (? #\newline))

(defsyntax (let . forms)
  (if (symbol? (car forms))  ; named-let?
      (let* ((name (car forms))
	     (bindings (cadr forms))
	     (vars (map car bindings))
	     (vals (map cadr bindings))
	     (body (caddr forms)))
	`(letrec ((,name (lambda ,vars ,body)))
	   (,name ,@vals)))
      (let* ((bindings (car forms))
	     (vars (map car bindings))
	     (vals (map cadr bindings))
	     (body (cadr forms)))
	`((lambda ,vars ,body)
	  ,@vals))))

;; control structures
(defmacro *if*
  ; (lambda (b) (call/cc (lambda (q) ((K (K I)) ((b q) K)))))
  ((S (K call/cc)) ((S (K (S (K (K (K I)))))) ((S S) (K (K K))))))

(defsyntax (if condition consequent alternative)
  `((*if*
     ,condition
     (lambda (**if-dummy**) ,consequent)
     (lambda (**if-dummy**) ,alternative)) I))

(defsyntax (begin . es)
  (reduce-right (lambda (e rest)
                  (if rest `(K I ,e ,rest) e))
                #f
                es))

(defsyntax (when condition consequent)
  `((,condition
     (lambda (**when-dummy**) ,consequent)) I))

(defmacro (unless condition alternative)
  (if condition V alternative))

; (cond (c1 b1) (c2 b2) ... (cn bn) [(else b)])
; c1..cn-1 ‚ª false, cn ‚ª true ‚Ì‚Æ‚«A b1..bn-1 ‚à•]‰¿‚³‚ê‚é‚Ì‚Å’ˆÓ
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
    `(delay ,(fold list lastc cs))))

(defsyntax (print$ s e)
  ; (print$ "abc" e) => (#\c (#\b (#\a e)))
  (fold list e (string->list s)))

;; boolean functions
(defmacro (not b) (if b V I))
;(defmacro (and a b) (a b))
;(defmacro (or a b) (if a I b))
(defsyntax (or . es)
  (let rec ((es es))
    (if (null? (cdr es))
	(car es)
	`(if ,(car es) I ,(rec (cdr es))))))
(defsyntax (and . es)
  es)

;; list functions
(defmacro nil V)
(defmacro cons (lambda (a b f) (f a b)))
(defmacro (icons a b) (lambda (_f) (_f a b)))  ; inlined cons
(defmacro (pair? x) (x (lambda (a b) I)))
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
  (fold-right (lambda (hd tl) `(icons ,hd ,tl))
	      'nil
	      es))

; (cons* a b c) => (cons a (cons b c))
(defsyntax (cons* . args)
  (let rec ((args args))
    (if (null? (cddr args))
	`(icons ,(car args) ,(cdr args))
	`(icons ,(car args) ,(rec (cdr args))))))

(defmacro (cons1 x)
  (lambda (f) (f x)))
(defmacro (cons1? x) (x (lambda (a) I)))
(defmacro (1-of-1 o) (o I))
(defmacro (to-cons1 churchnum) (churchnum cons1 V))

(defrecmacro (for-each f lst)
  (lst
   (lambda (hd tl)
     (begin (f hd) (for-each f tl)))))

(defrecmacro (modify-nth n f lst)
  (if (cons1? n)
      (lst (lambda (hd tl)
             (cons hd (modify-nth (1-of-1 n) f tl))))
      (lst (lambda (hd tl)
             (cons (f hd) tl)))))

;; utilities
(defmacro (compose f g)
  (lambda (x) (f (g x))))
(defmacro (M x) (x x))
