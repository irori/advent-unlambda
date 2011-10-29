(use gauche.process)
(require "./advent.scm")

(define (read-process-output process input)
  (display input (process-input process))
  (close-output-port (process-input process))
  (let ((out (read-block 4096 (process-output process))))
    (process-wait process)
    (string-incomplete->complete out)))

(define tests '())
(define (define-test proc-id testname testcode expect)
  (push! tests (list proc-id testname testcode expect)))

(define (test-proc proc-id testname testcode expect)
  (let* ((unl (compile-to-string
	      (list testcode 'initial-world
		    (car (drop (reverse procedures) (lookup-enum proc-id))))))
	 (process (run-process '(unlambda) :input :pipe :output :pipe))
         (expect-str (tree->string expect))
	 (out (read-process-output process unl)))
    (if (string=? expect-str out)
        (begin
          (display #\.)
          (flush))
	(begin
          (newline)
	  (print "FAIL " proc-id ", " testname)
	  (print "EXPECT: " expect-str)
	  (print "ACTUAL: " out)
	  #f))))

(define (expect-enum sym)
  (list "{" (make-string (lookup-enum sym) #\*) "}"))

(define (expect-bool b)
  (list "{" (if b "t" "f") "}"))

(defmacro (print-stars n)
  (#\{ n #\* #\} I))

(defmacro (print-bool b)
  (#\{ (if b #\t #\f) #\} I))


(define-test 'mainloop ""
  '(lambda (world proc)
     (let-world (($set-newloc (K like1)))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
	    (print-stars $newloc))))))
  (list (expect-enum 'commence)
        (expect-enum 'like1)))

(define-test 'commence "goto-death"
  '(lambda (world proc)
     (let-world (($set-location (K limbo)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (expect-enum 'death))

(define-test 'commence "pitch-dark-death"
  '(lambda (world proc)
     (let-world (($set-location (K bird))
                 ($set-was-dark (K I))
                 ($set-rand (K (list KI KI KI KI KI KI))))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (expect-enum 'pitch-dark))

(define-test 'commence "pitch-dark-msg"
  '(lambda (world proc)
     (let-world (($set-location (K bird)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "\nIt is now pitch dark.  If you proceed you will most likely fall into a pit.\n"
        (expect-enum 'get-user-input)))

(define-test 'commence "longdesc"
  '(lambda (world proc)
     (let-world (($set-location (K house)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "\nYou are inside a building, a well house for a large spring.\n"
        (expect-enum 'describe-objects)))

(define-test 'commence "shortdesc"
  '(lambda (world proc)
     (let-world (($set-location (K house))
                 ($set-nth set-visits house (K (cons1 V))))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "\nYou're inside building.\n"
        (expect-enum 'describe-objects)))

(define-test 'commence "bear"
  '(lambda (world proc)
     (let-world (($set-location (K house))
                 ($carry BEAR))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "You are being followed by a very large, tame bear.\n"
        "\nYou are inside a building, a well house for a large spring.\n"
        (expect-enum 'describe-objects)))

(define-test 'commence "forced-move"
  '(lambda (world proc)
     (let-world (($set-location (K crack)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "\nThe crack is far too small for you to follow.\n"
        (expect-enum 'try-move)))

(define-test 'describe-objects "count-visits"
  '(lambda (world proc)
     ((proc world)
      (lambda (cont world)
        (begin
          (print-stars (cons1-length (nth initial-location $visits)))
          (print-stars cont)))))
  (list "{****}"
        (expect-enum 'get-user-input)))

(define-test 'describe-objects "count-visits2"
  '(lambda (world proc)
     (let-world (($set-nth set-visits initial-location (K (cons1 (cons1 V)))))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars (cons1-length (nth initial-location $visits)))
            (print-stars cont))))))
  (list "{*}"
        (expect-enum 'get-user-input)))

(define-test 'describe-objects "describe"
  '(lambda (world proc)
     (let-world (($set-location (K house))
                 ($set-prop-of LAMP (K c1)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "There are some keys on the ground here.\n"
        "There is a lamp shining nearby.\n"
        "There is food here.\n"
        "There is a bottle of water here.\n"
        (expect-enum 'get-user-input)))

(define-test 'describe-objects "based"
  '(lambda (world proc)
     (let-world (($set-location (K inside)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "The grate is locked.\n"
        (expect-enum 'get-user-input)))

(define-test 'describe-objects "treads-gold"
  '(lambda (world proc)
     (let-world (($set-location (K spit))
                 ($carry GOLD))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list (expect-enum 'get-user-input)))

(define-test 'describe-objects "treads-spit"
  '(lambda (world proc)
     (let-world (($set-location (K spit)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "Rough stone steps lead down the pit.\n"
        (expect-enum 'get-user-input)))

(define-test 'describe-objects "treads-emist"
  '(lambda (world proc)
     (let-world (($set-location (K emist)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "Rough stone steps lead up the dome.\n"
        (expect-enum 'get-user-input)))

(define-test 'describe-objects "treasure"
  '(lambda (world proc)
     (let-world (($set-location (K west)))
       ((proc world)
        (lambda (cont world)
          (print-stars ($prop-of COINS))))))
  (list "There are many coins here!\n"
        "{}"))

(define-test 'describe-objects "rug"
  '(lambda (world proc)
     (let-world (($set-location (K scan3)))
       ((proc world)
        (lambda (cont world)
          (print-stars ($prop-of RUG))))))
  (list "A huge green fierce dragon bars the way!\n"
        "The dragon is sprawled out on a Persian rug!!\n"
        "{*}"))

(define-test 'describe-objects "chain"
  '(lambda (world proc)
     (let-world (($set-location (K barr)))
       ((proc world)
        (lambda (cont world)
          (print-stars ($prop-of CHAIN))))))
  (list "There is a ferocious cave bear eying you from the far end of the room!\n"
        "The bear is locked to the wall with a golden chain!\n"
        "{*}"))

(define-test 'get-user-input ""
  '(lambda (world proc)
     (let-world (($set-verb (K TAKE))
		 ($set-obj (K LAMP)))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
	    (print-stars $verb)
	    (print-stars $oldverb)
	    (print-stars $obj)
            (print-stars $oldobj))))))
  (list (expect-enum 'cycle)
        (expect-enum 'ABSTAIN)
        (expect-enum 'ABSTAIN)
        (expect-enum 'NOTHING)
        (expect-enum 'LAMP)))

(define-test 'pre-parse "increment-turns"
  '(lambda (world proc)
     (let-world (($set-turns (K (cons1 (cons1 V)))))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
	    (print-stars (cons1-length $turns)))))))
  (list (expect-enum 'clocks-and-lamp)
        "{***}"))

(defmacro dummy-word (lambda (f) (f I I I)))

(define-test 'pre-parse "say-something"
  '(lambda (world proc)
     (let-world (($set-verb (K SAY))
                 ($set-word12 (K (cons dummy-word dummy-word))))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
	    (print-stars $verb))))))
  (list (expect-enum 'clocks-and-lamp)
        (expect-enum 'ABSTAIN)))

(define-test 'pre-parse "say-nothing"
  '(lambda (world proc)
     (let-world (($set-verb (K SAY))
                 ($set-word12 (K (cons dummy-word V))))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
	    (print-stars $verb))))))
  (list (expect-enum 'transitive)
        (expect-enum 'SAY)))

(define-test 'check-the-lamp "lamp-off"
  '(lambda (world proc)
     (let-world (($set-limit (K (cons1 (cons1 V)))))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
            (print-stars (cons1-length $limit)))))))
  (list (expect-enum 'handle-special-inputs)
        "{**}"))

(define-test 'check-the-lamp "lamp-on"
  '(lambda (world proc)
     (let-world (($set-prop-of LAMP (K c1))
                 ($set-limit (K (cons1 (cons1 V)))))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
            (print-stars (cons1-length $limit)))))))
  (list (expect-enum 'handle-special-inputs)
        "{*}"))

(define-test 'check-the-lamp "lamp-extinguish"
  '(lambda (world proc)
     (let-world (($carry LAMP)
                 ($set-prop-of LAMP (K c1))
                 ($set-limit (K (cons1 V))))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
            (print-stars ($prop-of LAMP))
            (print-stars (cons1-length $limit))
            )))))
  (list "Your lamp has run out of power.\n"
        (expect-enum 'handle-special-inputs)
        "{}"
        "{}"
        ))

(define-test 'check-the-lamp "replace-batteries"
  '(lambda (world proc)
     (let-world (($carry LAMP)
                 ($carry BATTERIES)
                 ($set-prop-of LAMP (K c1))
                 ($set-limit (K (to-cons1 c31))))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
            (print-stars ($prop-of BATTERIES))
            (print-stars (nth BATTERIES $place))
            (print-stars (cons1-length $limit))
            )))))
  (list "Your lamp is getting dim.  I'm taking the liberty of replacing\nthe batteries.\n"
        (expect-enum 'handle-special-inputs)
        "{*}"
        (expect-enum 'road)
        "{" (make-string 2500 #\*) "}"
        ))

(define-test 'check-the-lamp "giveup"
  '(lambda (world proc)
     (let-world (($set-limit (K V)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "There's not much point in wandering around out here, and you can't\nexplore the cave without a lamp.  So let's just call it a day.\n"
        (expect-enum 'give-up)))

(define-test 'check-the-lamp "warn-lamp"
  '(lambda (world proc)
     (let-world (($carry LAMP)
                 ($set-limit (K (to-cons1 c30))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool $not-warned))))))
  (list "Your lamp is getting dim.  You'd best start wrapping this up, unless\nyou can find some fresh batteries.  I seem to recall that there's\na vending machine in the maze.  Bring some coins with you.\n"
        (expect-enum 'handle-special-inputs)
        (expect-bool #f)))

(define-test 'check-the-lamp "warn-lamp-out-of-batteries"
  '(lambda (world proc)
     (let-world (($carry LAMP)
                 ($set-limit (K (to-cons1 c30)))
                 ($set-prop-of BATTERIES (K c1)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool $not-warned))))))
  (list "Your lamp is getting dim, and you're out of spare batteries.  You'd\nbest start wrapping this up.\n"
        (expect-enum 'handle-special-inputs)
        (expect-bool #f)))

(define-test 'check-the-lamp "warn-lamp-batteries-left"
  '(lambda (world proc)
     (let-world (($carry LAMP)
                 ($set-limit (K (to-cons1 c30)))
                 ($drop BATTERIES house))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool $not-warned))))))
  (list "Your lamp is getting dim.  You'd best go back for those batteries.\n"
        (expect-enum 'handle-special-inputs)
        (expect-bool #f)))

(define-test 'handle-special-inputs "wet"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (motion-word ENTER)
                                       (motion-word STREAM)))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "Your feet are now wet.\n"
        (expect-enum 'get-user-input)))

(define-test 'handle-special-inputs "not-wet"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (motion-word ENTER) (object-word WATER))))
                 ($set-location (K hill)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "Where?\n"
        (expect-enum 'get-user-input)))

(define-test 'handle-special-inputs "enter-house"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (motion-word ENTER) (motion-word HOUSE)))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (expect-enum 'shift))

(define-test 'handle-special-inputs "enter"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (motion-word ENTER) V))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (expect-enum 'parse-label))

(define-test 'handle-special-inputs "water-plant"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (object-word WATER) (object-word PLANT))))
                 ($set-location (K wpit)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool (verb? (cdr $word12)))
            (print-stars (word-meaning (cdr $word12))))))))
  (list (expect-enum 'parse-label)
        (expect-bool #t)
        (expect-enum 'POUR)))

(define-test 'handle-special-inputs "normal"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (object-word WATER) (object-word PLANT)))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool (noun? (cdr $word12)))
            (print-stars (word-meaning (cdr $word12))))))))
  (list (expect-enum 'parse-label)
        (expect-bool #t)
        (expect-enum 'PLANT)))

(define-test 'shift ""
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (action-word EAT) (object-word FOOD)))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool (noun? (car $word12)))
            (print-stars (word-meaning (car $word12)))
            (print-bool (word? (cdr $word12))))))))
  (list (expect-enum 'parse-label)
        (expect-bool #t)
        (expect-enum 'FOOD)
        (expect-bool #f)))

(define-test 'look-at-word1 "unknown-word"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons V V))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "I don't know that word.\n"
        (expect-enum 'cycle)))

(define-test 'look-at-word1 "motion"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (motion-word NE) V))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $mot))))))
  (list (expect-enum 'try-move)
        (expect-enum 'NE)))

(define-test 'look-at-word1 "object-here"
  '(lambda (world proc)
     (let-world (($set-location (K house))
                 ($set-word12 (K (cons (object-word LAMP) V))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $obj))))))
  (list (expect-enum 'handle-object-word)
        (expect-enum 'LAMP)))

(define-test 'look-at-word1 "object-not-here"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (object-word LAMP) V))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $obj))))))
  (list (expect-enum 'check-object-location)
        (expect-enum 'LAMP)))

(define-test 'look-at-word1 "verb-with-word2"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (action-word DROP) (object-word LAMP)))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $verb))))))
  (list (expect-enum 'shift)
        (expect-enum 'DROP)))

(define-test 'look-at-word1 "verb-transitive"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (action-word DROP) V)))
                 ($set-obj (K LAMP)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $verb))))))
  (list (expect-enum 'transitive)
        (expect-enum 'DROP)))

(define-test 'look-at-word1 "verb-intransitive"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (action-word DROP) V)))
                 ($set-obj (K NOTHING)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $verb))))))
  (list (expect-enum 'intransitive)
        (expect-enum 'DROP)))

(define-test 'look-at-word1 "message-word"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (message-word c4) V))))  ; LOST
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "I'm as confused as you are.\n"
        (expect-enum 'get-user-input)))

(define-test 'handle-object-word "shift"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons V (action-word TAKE)))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (expect-enum 'shift))

(define-test 'handle-object-word "with-verb"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons V V)))
                 ($set-verb (K TAKE)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (expect-enum 'transitive))

(defmacro (object-word-with-str meaning str)
  (lambda (f) (f (lambda (_ x _ _) x) meaning str)))

(define-test 'handle-object-word "without-verb"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (object-word-with-str LAMP (string "lamp")) V)))
                 ($set-verb (K ABSTAIN)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "What do you want to do with the lamp?\n"
        (expect-enum 'cycle)))

(define-test 'cant-see-it ""
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (object-word-with-str LAMP (string "lamp")) V))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "I see no lamp here.\n"
        (expect-enum 'get-user-input)))

(define-test 'cant-see-it "find"
  '(lambda (world proc)
     (let-world (($set-verb (K FIND))
                 ($set-word12 (K (icons V V))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (expect-enum 'transitive))


(define (main args)
  (let ((testname (if (null? (cdr args)) #f (string->symbol (cadr args)))))
    (for-each
     (lambda (testcase)
       (if (or (not testname) (eq? testname (car testcase)))
           (apply test-proc testcase)))
     (reverse tests)))
  0)
