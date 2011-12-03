; -*- scheme-*-
(define *compress-string* #f)

;; utilities
(defmacro KI (K I))
(defmacro (compose f g)
  (lambda (x) (f (g x))))
(defmacro (T x y) (y x))

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
        `((lambda-rec ,name ,vars ,body)
          ,@vals))
      (let* ((bindings (car forms))
	     (vars (map car bindings))
	     (vals (map cadr bindings))
	     (body (cadr forms)))
	`((lambda ,vars ,body)
	  ,@vals))))

(defsyntax (let* binds body)
  (fold-right
   (lambda (bind rest)
     `(let (,bind) ,rest))
   body
   binds))

(defmacro *if*
  ; (lambda (b) (call/cc (lambda (q) ((K (K I)) ((b q) K)))))
  ((S (K call/cc)) ((S (K (S (K (K (K I)))))) ((S S) (K (K K))))))

(defmacro (if condition consequent alternative)
  ((*if*
    condition
    (lambda (**if-dummy**) consequent)
    (lambda (**if-dummy**) alternative))
   I))

; (cond (c1 b1) (c2 b2) ... (cn bn) [(else b)])
(defsyntax (cond . clauses)
  `((call/cc
     (lambda (*q*)
       ,(map (lambda (pair)
	       (if (eq? (car pair) 'else)
		   `(*q* (lambda (**cond-dummy**) ,(cadr pair)))
		   `(,(car pair) *q* (lambda (**cond-dummy**) ,(cadr pair)))))
	     clauses)))
    V))

(defsyntax (begin . es)
  (if (null? es)
      'I
      (reduce-right (lambda (e rest)
                      (if rest `(K I ,e ,rest) e))
                    #f
                    es)))

;; I/O functions
(defsyntax (read-char=? c . cs)
  (if (null? cs)
      `((? ,c) I)
      `(call/cc
        (lambda (_c)
          ,(map (lambda (ch) `((? ,ch) I _c I))
                (cons c cs))))))

(define (compress-string str)
  (define result #f)
  (define (return e size)
    (if (or (not result)
            (< size (cdr result)))
        (set! result (cons e size))))

  (define memo (make-hash-table))
  (define (check esize val chars)
    (let* ((ht (hash-table-get memo chars))
           (size (hash-table-get ht val 99999))
           (fsize (hash-table-get ht #f 99999)))
      (if (or (<= size esize)
              (and val (<= (+ fsize (chsize val)) esize)))
          #f
          (begin (hash-table-put! ht val esize)
                 #t))))

  (define (step e esize val chars)
    (if (check esize val chars)
        (match chars
          (()
           (if (not val)
               (return (list 'fill 'I e) (+ esize 2))
               (return `(K I ,e) (+ 4 esize))))
          ((c)
           (cond ((not val)
                  (return (list 'fill c e)
                          (+ esize (chsize c))))
                 ((eq? c val)
                  (return e esize))
                 (else
                  (return `(K ,c ,e)
                          (+ 2 (chsize c) esize)))))
          ((c1 c2 c2_ c3 . rest) (=> next)
           (if (not (eq? c2 c2_))
               (next))
           (cond ((eq? c2 val)
                  (step `((,e (call/cc ,c1)) ,c3)
                        (+ esize 2 (chsize c1) (chsize c3))
                        c3 rest))
                 ((eq? c1 val)
                  (step `((,c2 (call/cc ,e)) ,c3)
                        (+ (chsize c2) 2 esize (chsize c3))
                        c3 rest))
                 ((not val)
                  (step `((,c2 (call/cc (fill ,c1 ,e))) ,c3)
                        (+ (chsize c2) 2 esize (chsize c1) (chsize c3))
                        c3 rest)))
           (next))
          ((c1 c2 c3 . rest) (=> next)
           (cond ((eq? c1 val)
                  (step `(S ,e ,c2 ,c3)
                        (+ 2 esize (chsize c2) (chsize c3))
                        c3 rest))
                 ((eq? c2 val)
                  (step `(S ,c1 ,e ,c3)
                        (+ 2 (chsize c1) esize (chsize c3))
                        c3 rest))
                 ((eq? c3 val)
                  (step `(S ,c1 ,c2 ,e)
                        (+ 2 (chsize c1) (chsize c2) esize)
                        c3 rest))
                 ((not val)
                  (step `(S (fill ,c1 ,e) ,c2 ,c3)
                        (+ 2 esize (chsize c1) (chsize c2) (chsize c3))
                        c3 rest)))
           (next))
          ((c . rest)
           (if (eq? c val)
               (step `(,e hole) esize #f rest)
               (step `(,c ,e) (+ (chsize c) esize) val rest)))
          )))

  (define (chsize ch)
    (if (eq? ch #\newline) 2 3))

  (define (do-fill x e)
    (match e
      (('fill xx ee)
       (do-fill xx ee))
      ('hole
       x)
      (else
       (if (pair? e)
           (map (pa$ do-fill x) e)
           e))))

  (let ((chars (string->list str)))
    (pair-for-each (lambda (lst)
                     (hash-table-put! memo lst (make-hash-table)))
                   chars)
    (hash-table-put! memo '() (make-hash-table))
    (step 'hole -1 #f chars)
    (do-fill #f result)))

;; string constant. ((string "foo") x) prints "foo" and returns x
(defsyntax (string s)
  (if *compress-string*
      `(delay ,(compress-string s))
      (let* ((len (string-length s))
             (lastc (string-ref s (- len 1)))
             (cs (string->list (substring s 0 (- len 1)))))
        `(delay ,(fold list lastc cs)))))

(defsyntax (print$ s e)
  ; (print$ "abc" e) => (#\c (#\b (#\a e)))
  (fold list e (string->list s)))

(defmacro (print s)
  (print$ s I))

;; boolean functions
(defmacro not
  (lambda (b) (call/cc (lambda (q) (K I (b q V))))))
(defsyntax (and . es) es)
(defsyntax (or . es)
  (let rec ((es es))
    (if (null? (cdr es))
	(car es)
	`(if ,(car es) I ,(rec (cdr es))))))

;; list functions
(defmacro nil V)
(defmacro cons (lambda (a b f) (f a b)))
(defmacro (icons a b) (lambda (_f) (_f a b)))  ; inlined cons
(defmacro (snoc b a) (lambda (_f) (_f a b)))
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

(defmacro (repeat x)
  (lambda-rec rec () (icons x rec)))

(defmacro (cons1 x)
  (lambda (f) (f x)))
(defmacro (cons1? x) (x (lambda (a) I)))
(defmacro (1-of-1 o) (o I))
(defmacro (to-cons1 churchnum) (churchnum cons1 V))

(defrecmacro (for-each f lst)
  (lst
   (lambda (hd tl)
     (begin (f hd) (for-each f tl)))))

(defmacro (remove f lst)
  (let loop ((l lst))
    (l (lambda (hd tl)
         ((if (f hd) I (icons hd))
          (loop tl))))))

; (update-nth f n lst) replaces n-th value of lst with (f (nth n lst))
(defmacro (update-nth f n)
  (n
   (lambda (g lst)
     (lst (lambda (hd tl)
	    (S (S I (K hd)) (K (g tl))))))  ; (cons hd (g tl))
   (lambda (_lst)
     (_lst (lambda (_hd _tl)
	    ((snoc _tl) (f _hd)))))))

;; Church number
(require "./churchnum.tbl")

(define (churchnum n)
  (if (< n 0)
      (error "negative argument" n)
      (string->symbol (string-append "c" (number->string n)))))

(defmacro (churchnum? x) (x I I))
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
      c0))

(defrecmacro (cons1-length xs)
  (if (cons1? xs)
      (succ (cons1-length (1-of-1 xs)))
      c0))

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
(defmacro (= m n) ((m (T I) (n T KI)) V))

(defmacro (div x y)
  (let rec ((xs (cons1 (x cons1 V))))
    (let ((xs2 ((y 1-of-1) xs)))
      (if (cons1? xs2)
	  (succ (rec xs2))
	  c0))))

(defmacro (mod x y)
  (let rec ((xs (cons1 (x cons1 V))))
    (let ((xs2 ((y 1-of-1) xs)))
      (if (cons1? xs2)
	  (rec xs2)
	  (cons1-length (1-of-1 xs))))))

(defmacro (divmod x y)
  (let rec ((xs (cons1 (x cons1 V))) (q c0))
    (let ((xs2 ((y 1-of-1) xs)))
      (if (cons1? xs2)
	  (rec xs2 (succ q))
	  (icons q (cons1-length (1-of-1 xs)))))))

(defmacro (print-digit1 n)
  (nth n (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(defrecmacro (print-digit n)
  (if (<= n c9)
      (print-digit1 n)
      ((divmod n c10)
       (lambda (m d)
	 (S (print-digit m) (print-digit1 d))))))
