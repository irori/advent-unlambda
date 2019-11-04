(define-module util
  (use file.util)
  (use text.tree)
  (use unlambda.compiler)
  (export unl-module compile-modules)
  )
(select-module util)

(define (compile-to-file file expr)
  (let ((expanded (expand-macros expr))
	(obj (file->sexp-list file :if-does-not-exist #f)))
    (if (and obj (equal? (car obj) expanded))
	(cadr obj)
	(let ((compiled (tree->string (compile expanded))))
	  (call-with-output-file file
	    (lambda (port)
	      (write expanded port)
	      (write compiled port)))
	  compiled))))

(define *pending-modules* '())

(define (unl-module name expr)
  (set! *pending-modules*
	(acons name expr *pending-modules*)))

(define (compile-modules)
  (for-each (lambda (mod)
	      (let* ((name (car mod))
		     (expr (cdr mod))
		     (file (string-append (symbol->string name) ".unlo")))
		(add-unl-macro! name '() (compile-to-file file expr))))
	    (reverse! *pending-modules*))
  (set! *pending-modules* '()))
