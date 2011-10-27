(use gauche.process)
(require "./advent.scm")

(define (read-process-output process input)
  (display input (process-input process))
  (close-output-port (process-input process))
  (let ((out (read-block 4096 (process-output process))))
    (process-wait process)
    (string-incomplete->complete out)))

(define (test-proc proc-id testcode expect)
  (let* ((unl (compile-to-string
	      (list testcode 'initial-world
		    (car (drop (reverse procedures) (lookup-enum proc-id))))))
	 (process (run-process '(unlambda) :input :pipe :output :pipe))
	 (out (read-process-output process unl)))
    (if (string=? expect out)
	#t
	(begin
	  (print "FAIL " proc-id)
	  (print "EXPECT: " expect)
	  (print "ACTUAL: " out)
	  #f))))

(define (expect-enum sym)
  (list "{" (make-string (lookup-enum sym) #\*) "}"))

(defmacro (print-stars n)
  (#\{ n #\* #\} I))

(test-proc 'mainloop
  '(lambda (world proc)
     (let-world ((set-newloc world (K like1)))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
	    (print-stars (newloc world)))))))
  (tree->string (list
		 (expect-enum 'commence)
		 (expect-enum 'like1))))

(test-proc 'get-user-input
  '(lambda (world proc)
     (let-world ((set-verb world (K TAKE))
		 (set-obj world (K LAMP)))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
	    (print-stars (verb world))
	    (print-stars (obj world)))))))
  (tree->string (list
		 (expect-enum 'cycle)
		 (expect-enum 'ABSTAIN)
		 (expect-enum 'NOTHING))))

(define (main args)
  0)
