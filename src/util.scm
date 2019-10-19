(define-module util
  (use file.util)
  (use text.tree)
  (use unlambda.compiler)
  (export compile-to-file)
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
