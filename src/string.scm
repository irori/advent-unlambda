(define-library (string)
  (export save-string-cache load-string-cache)
  (import (scheme base))
  (import (scheme comparator))
  (import (scheme cxr))
  (import (scheme hash-table))
  (import (unlambda compiler))
  (import (chibi match))
  (import (srfi 1))
  (import (gauche base))
  (import (file util))
  (import (only (unlambda er-macro) quasirename))

  (begin

(define cache (make-hash-table (make-default-comparator)))

(define (save-string-cache fname)
  (sexp-list->file fname (hash-table->alist cache)))

(define (load-string-cache fname)
  (let ((alist (file->sexp-list fname :if-does-not-exist #f)))
    (if alist
	(set! cache (alist->hash-table alist (make-default-comparator))))))

;; TODO: rename result
(define (compress-string str)
  (define result #f)
  (define (return e size)
    (if (or (not result)
	    (< size (cdr result)))
        (set! result (cons e size))))

  (define memo (make-hash-table (make-default-comparator)))
  (define (check esize val chars)
    (let* ((ht (hash-table-ref memo chars))
           (size (hash-table-ref/default ht val #f))
           (fsize (hash-table-ref/default ht #f #f)))
      (if (or (and size (<= size esize))
	      (and val fsize (<= (+ fsize (chsize val)) esize)))
          #f
          (begin (hash-table-set! ht val esize)
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
	   (if (eq? c2 c2_)
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
                            c3 rest))))
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
	   (map (lambda (ee) (do-fill x ee)) e)
	   e))))

  (define (compress str)
    (let ((chars (string->list str)))
      (pair-for-each (lambda (lst)
		       (hash-table-set! memo lst (make-hash-table (make-default-comparator))))
                     chars)
      (hash-table-set! memo '() (make-hash-table (make-default-comparator)))
      (step 'hole -1 #f chars)
      (do-fill #f (car result))))

  (let ((cached (hash-table-ref/default cache str #f)))
    (or cached
	(let ((compressed (compress str)))
	  (hash-table-set! cache str compressed)
	  compressed))))

;; Replaces the string macro in prelude.
(add-unl-syntax! 'string
  (lambda (form rename compare)
    (let ((s (cadr form)))
      `(delay ,(compress-string s)))))

;; Replaces the print-and-return macro in prelude.
(add-unl-syntax! 'print-and-return
  (lambda (form rename compare)
    (let ((s (cadr form))
	  (e (caddr form)))
      (list (compress-string s) e))))

))
