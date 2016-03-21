;; Unlambda code generator

(define-module unlc
  (use srfi-1)
  (use file.util)
  (use util.match)
  (use text.tree)
  (export defmacro defrecmacro add-unl-macro!
	  defsyntax add-unl-syntax!
	  print-as-unl compile-to-string compile-to-file)
  )
(select-module unlc)

(define-macro (atom? x)
  `(not (pair? ,x)))

(define (eliminate-lambda-rec x)
  (match x
    (('lambda-rec name args body)
     `((lambda (x) (x x))
       (lambda (,name ,@args)
	 ,(eliminate-lambda-rec
           (macroexpand (list (cons name (lambda (args)
                                           (append (list name name) args))))
                        body)))))
    (?-
     (if (or (atom? x) (null? x))
	 x
	 (map eliminate-lambda-rec x)))))

(define (make-s arg1 arg2)
  (match (list arg1 arg2)
    ((('K gg) ('K hh)) (=> fail)
     (if (normalform? (list gg hh))
	 `(K (,gg ,hh))
	 (fail)))
    ((('delay ('K gg)) ('K hh)) (=> fail)
     (if (and (normalform? hh) (not (contains-free-variables? hh)))
	 `(delay (K (,gg ,hh)))
	 (fail)))
    ((('K gg) 'V) (=> fail)
     (if (normalform? (list gg 'V))
	 `(K (,gg V))
	 (fail)))
    ((('K gg) 'I) gg)
    ((('delay ('K gg)) 'I) `(delay ,gg))
    ((gg hh) `((S ,gg) ,hh))))

(define (eliminate-abstruction var body)
  (cond ((eq? var body)
	 'I)
	((or (atom? body)
	     (pass-through? body))
	 (list 'K body))
	((eq? (car body) 'lambda)
	 (eliminate-abstruction var (eliminate-lambda body)))
	((and (not (bound-in? var body))
	      (not (contains-free-variables? body)))
	 (let ((body (eliminate-lambda body)))
	   (if (normalform? body)
	       (list 'K body)
	       `(delay (K ,body)))))
	(else
	 (let ((g (car body))
	       (h (cadr body)))
	   (if (eq? g 'delay)
	       (list 'K body)
	       (make-s (eliminate-abstruction var g)
		       (eliminate-abstruction var h)))))))

(define (normalform? e)
  (match e
    (('K x) (normalform? x))
    (('S f) (normalform? f))
    ((('S f) g) (and (normalform? f) (normalform? g)))
    (('delay x) #t)
    (('unsafe x) #t)
    (('? x) #t)
    (- (atom? e))))

(define (eliminate-lambda x)
  (cond ((or (atom? x) (pass-through? x))
	 x)
	((eq? (car x) 'lambda)
	 (eliminate-abstruction (caadr x) (caddr x)))
	(else
	 (list (eliminate-lambda (car x)) (eliminate-lambda (cadr x))))))


(define (bound-in? a x)
  (cond ((atom? x) (eq? x a))
	((pass-through? x) #f)
	((eq? (car x) 'lambda)
	 (if (eq? (caadr x) a)
	     #f
	     (bound-in? a (caddr x))))
	(else (or (bound-in? a (car x)) (bound-in? a (cadr x))))))

(define (closed? e env)
  (cond ((pass-through? e) #t)
	((atom? e) (memq e env))
	((eq? (car e) 'lambda)
	 (let ((vars (cadr e))
	       (body (caddr e)))
	   (closed? body (append vars env))))
	(else (and (closed? (car e) env)
		   (closed? (cadr e) env)))))

(define (contains-free-variables? x)
  (not (closed? x '())))

(define (pass-through? x)
  (if (pair? x)
      (eq? (car x) '?)
      (or (memq x '(I K V delay S call/cc unsafe @ !))
	  (char? x))))

(define (optimize-ski e)
  (if (pair? e)
      (let ((f (optimize-ski (car e)))
	    (x (optimize-ski (cadr e))))
	(cond ((and (eq? f 'K)
                    (eq? x 'V))
               'V)  ; (K V) => V
              ((and (not (eq? f 'delay))
                    (pair? x)
                    (or (eq? (car x) '@)
                        (eq? (car x) '!))
                    (eq? (cadr x) 'I))
               (list (car x) f))  ; (x ([@!] I)) => ([@!] x)
              (else
               (list f x))))
      e))

(define (unlambdify x)
  (match x
    ('I #\i)
    ('K #\k)
    ('S #\s)
    ('delay #\d)
    ('V #\v)
    ('@ #\@)
    ('! #\|)
    (('? c) (list #\? c))
    (('unsafe x) (unlambdify x))
    ('call/cc #\c)
    ('exit #\e)
    ((f a) (list #\` (unlambdify f) (unlambdify a)))
    (#\newline #\r)
    (-
     (cond ((char? x) (list #\. x))
           ((string? x) x)
           (else (error "unlambdify" "illegal unlambda" x))))))

(define (optimize-curried x)
  (cond ((or (atom? x) (pass-through? x))
	 x)
	((eq? (car x) 'lambda)	;(lambda (a) (lambda (b) a)) -> K
	 (let ((a (caadr x))
	       (b (caddr x)))
	   (if (and (pair? b)
		    (eq? (car b) 'lambda)
		    (not (eq? (caadr b) a))
		    (eq? (caddr b) a))
	       'K
	       `(lambda (,a) ,(optimize-curried b)))))
	(else
	 (list (optimize-curried (car x)) (optimize-curried (cadr x))))))

(define (curry x)
  (cond ((atom? x)
	 x)
	((eq? (car x) 'lambda)
	 (let ((a (cadr x))
	       (b (caddr x)))
	   (cond ((null? a)
		  `(lambda (**dummy**) ,(curry b)))
		 ((null? (cdr a))
		  `(lambda ,a ,(curry b)))
		 (else
		  `(lambda (,(car a)) ,(curry `(lambda ,(cdr a) ,b)))))))
	(else
         (fold (lambda (ei e) (if e (list e ei) ei))
               #f (map curry x)))))

(define curried
  (compose optimize-curried
	   curry
	   eliminate-lambda-rec))

(define compile
  (compose optimize-ski eliminate-lambda curried))

(define (lambda->unlambda x)
  (unlambdify (compile x)))

(define (eliminate-names macros names)
  (filter (lambda (macro) (not (memq (car macro) names))) macros))

(define (macroexpand-arg arg)
    (lambda (arg-args)
      (if (null? arg-args)
	  arg
	  (cons arg arg-args))))

(define (make-macros args arg-names)
    (if (or (null? args) (null? arg-names))
	(values '() args arg-names)
        (receive (macros rest-args rest-arg-names)
            (make-macros (cdr args) (cdr arg-names))
	  (values (acons (car arg-names) (macroexpand-arg (car args)) macros)
		  rest-args rest-arg-names))))

(define (macroexpand-application arg-names body macros)
  (lambda (args)
    (receive (arg-macros args arg-names) (make-macros args arg-names)
      (let ((macros (append arg-macros macros)))
        (cond ((and (null? args) (null? arg-names))
               (macroexpand macros body))
              ((null? args)
               (macroexpand macros `(lambda ,arg-names ,body)))
              ((null? arg-names)
               (cons (macroexpand macros body) args))
              (else
               (error "macroexpand-application" "make-macros does not work" (cons args arg-names))))))))

;; a macro is a (name . (lambda (macro-args) *body*))
(define (macroexpand macros expr)
    (match expr
      (('lambda args body)
       `(lambda ,args ,(macroexpand (eliminate-names macros args) body)))
      (('lambda-rec name args body)
       `(lambda-rec ,name ,args ,(macroexpand (eliminate-names macros (cons name args)) body)))
      ((func . args)
       (let ((macro (and (atom? func) (assq func macros))))
         (if macro
	     ((cdr macro) (map (lambda (arg) (macroexpand macros arg)) args))
             (cons (macroexpand macros func) (map (lambda (arg) (macroexpand macros arg)) args)))))
      (-
       (let ((macro (assq expr macros)))
	 (if macro
	     ((cdr macro) '())
	     expr)))))

(define (make-syntax name arg-names body macros)
  (let ((cl (eval `(letrec ((,name (lambda ,arg-names ,body))) ,name)
		  (interaction-environment))))
    (lambda (args)
      (macroexpand macros (apply cl args)))))


(define unl-macros '())

(define (add-unl-macro! name args body)
  ; (print name args body)
  (if (assq name unl-macros)
      (error "redefined " name))
  (set! unl-macros
        (acons name
               (macroexpand-application args body unl-macros)
               unl-macros)))

(define (add-unl-syntax! name f)
  (if (assq name unl-macros)
      (error "redefined " name))
  (set!
   unl-macros
   (acons name
          (lambda (args) (macroexpand unl-macros (apply f args)))
          unl-macros)))

(define-macro (defmacro name-args body)
  (let ((name (if (pair? name-args) (car name-args) name-args))
        (args (if (pair? name-args) (cdr name-args) '())))
    (add-unl-macro! name args body))
  #t)

(define-macro (defrecmacro name-args body)
  (let ((name (if (pair? name-args) (car name-args) name-args))
        (args (if (pair? name-args) (cdr name-args) '())))
    (add-unl-macro! name '() `(lambda-rec ,name ,args ,body)))
  #t)

(define-macro (defsyntax name-args body)
  (if (assq (car name-args) unl-macros)
      (error "redefined " name))
  (set!
   unl-macros
   (acons (car name-args)
          (make-syntax (car name-args) (cdr name-args) body unl-macros)
          unl-macros))
  #t)

(define (print-as-unl expr :optional (port (current-output-port)))
  (write-tree (lambda->unlambda (macroexpand unl-macros expr)) port))

(define (compile-to-string expr)
  (tree->string (lambda->unlambda (macroexpand unl-macros expr))))

(define (compile-to-file file expr)
  (let ((expanded (macroexpand unl-macros expr))
	(obj (file->sexp-list file :if-does-not-exist #f)))
    (if (and obj (equal? (car obj) expanded))
	(cadr obj)
	(let ((compiled (tree->string (lambda->unlambda expanded))))
	  (call-with-output-file file
	    (lambda (port)
	      (write expanded port)
	      (write compiled port)))
	  compiled))))
