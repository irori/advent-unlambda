(define-module util
  (use unlambda.compiler)
  (export unl-module compile-modules)
  )
(select-module util)

(define *pending-modules* '())

(define (unl-module name expr)
  (set! *pending-modules*
	(acons name expr *pending-modules*)))

(define (compile-modules)
  (for-each (lambda (mod)
	      (let* ((name (car mod))
		     (expr (cdr mod)))
		(add-unl-macro! name '() (compile-to-string expr))))
	    (reverse! *pending-modules*))
  (set! *pending-modules* '()))
