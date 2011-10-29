(use gauche.process)
(require "./advent.scm")

(define (read-process-output process input)
  (display input (process-input process))
  (close-output-port (process-input process))
  (let ((out (read-block 4096 (process-output process))))
    (process-wait process)
    (string-incomplete->complete out)))

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


(test-proc 'mainloop ""
  '(lambda (world proc)
     (let-world (($set-newloc (K like1)))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
	    (print-stars $newloc))))))
  (list (expect-enum 'commence)
        (expect-enum 'like1)))

(test-proc 'commence "goto-death"
  '(lambda (world proc)
     (let-world (($set-location (K limbo)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (expect-enum 'death))

(test-proc 'commence "pitch-dark-death"
  '(lambda (world proc)
     (let-world (($set-location (K bird))
                 ($set-was-dark (K I))
                 ($set-rand (K (list KI KI KI KI KI KI))))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (expect-enum 'pitch-dark))

(test-proc 'commence "pitch-dark-msg"
  '(lambda (world proc)
     (let-world (($set-location (K bird)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "\nIt is now pitch dark.  If you proceed you will most likely fall into a pit.\n"
        (expect-enum 'get-user-input)))

(test-proc 'commence "longdesc"
  '(lambda (world proc)
     (let-world (($set-location (K house)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "\nYou are inside a building, a well house for a large spring.\n"
        (expect-enum 'describe-objects)))

(test-proc 'commence "shortdesc"
  '(lambda (world proc)
     (let-world (($set-location (K house))
                 ($set-nth set-visits house (K (cons1 V))))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "\nYou're inside building.\n"
        (expect-enum 'describe-objects)))

(test-proc 'commence "bear"
  '(lambda (world proc)
     (let-world (($set-location (K house))
                 ($carry BEAR))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "You are being followed by a very large, tame bear.\n"
        "\nYou are inside a building, a well house for a large spring.\n"
        (expect-enum 'describe-objects)))

(test-proc 'commence "forced-move"
  '(lambda (world proc)
     (let-world (($set-location (K crack)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "\nThe crack is far too small for you to follow.\n"
        (expect-enum 'try-move)))

(test-proc 'describe-objects "count-visits"
  '(lambda (world proc)
     ((proc world)
      (lambda (cont world)
        (begin
          (print-stars (cons1-length (nth initial-location $visits)))
          (print-stars cont)))))
  (list "{****}"
        (expect-enum 'get-user-input)))

(test-proc 'describe-objects "count-visits2"
  '(lambda (world proc)
     (let-world (($set-nth set-visits initial-location (K (cons1 (cons1 V)))))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars (cons1-length (nth initial-location $visits)))
            (print-stars cont))))))
  (list "{*}"
        (expect-enum 'get-user-input)))

(test-proc 'describe-objects "describe"
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

(test-proc 'describe-objects "based"
  '(lambda (world proc)
     (let-world (($set-location (K inside)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "The grate is locked.\n"
        (expect-enum 'get-user-input)))

(test-proc 'describe-objects "treads-gold"
  '(lambda (world proc)
     (let-world (($set-location (K spit))
                 ($carry GOLD))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list (expect-enum 'get-user-input)))

(test-proc 'describe-objects "treads-spit"
  '(lambda (world proc)
     (let-world (($set-location (K spit)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "Rough stone steps lead down the pit.\n"
        (expect-enum 'get-user-input)))

(test-proc 'describe-objects "treads-emist"
  '(lambda (world proc)
     (let-world (($set-location (K emist)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "Rough stone steps lead up the dome.\n"
        (expect-enum 'get-user-input)))

(test-proc 'describe-objects "treasure"
  '(lambda (world proc)
     (let-world (($set-location (K west)))
       ((proc world)
        (lambda (cont world)
          (print-stars ($prop-of COINS))))))
  (list "There are many coins here!\n"
        "{}"))

(test-proc 'describe-objects "rug"
  '(lambda (world proc)
     (let-world (($set-location (K scan3)))
       ((proc world)
        (lambda (cont world)
          (print-stars ($prop-of RUG))))))
  (list "A huge green fierce dragon bars the way!\n"
        "The dragon is sprawled out on a Persian rug!!\n"
        "{*}"))

(test-proc 'describe-objects "chain"
  '(lambda (world proc)
     (let-world (($set-location (K barr)))
       ((proc world)
        (lambda (cont world)
          (print-stars ($prop-of CHAIN))))))
  (list "There is a ferocious cave bear eying you from the far end of the room!\n"
        "The bear is locked to the wall with a golden chain!\n"
        "{*}"))

(test-proc 'get-user-input ""
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

(test-proc 'pre-parse "increment-turns"
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

(test-proc 'pre-parse "say-something"
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

(test-proc 'pre-parse "say-nothing"
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

(test-proc 'check-the-lamp "lamp-off"
  '(lambda (world proc)
     (let-world (($set-limit (K (cons1 (cons1 V)))))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
            (print-stars (cons1-length $limit)))))))
  (list (expect-enum 'handle-special-inputs)
        "{**}"))

(test-proc 'check-the-lamp "lamp-on"
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

(test-proc 'check-the-lamp "lamp-extinguish"
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

(test-proc 'check-the-lamp "replace-batteries"
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

(test-proc 'check-the-lamp "giveup"
  '(lambda (world proc)
     (let-world (($set-limit (K V)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "There's not much point in wandering around out here, and you can't\nexplore the cave without a lamp.  So let's just call it a day.\n"
        (expect-enum 'give-up)))

(test-proc 'check-the-lamp "warn-lamp"
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

(test-proc 'check-the-lamp "warn-lamp-out-of-batteries"
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

(test-proc 'check-the-lamp "warn-lamp-batteries-left"
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

(test-proc 'handle-special-inputs "wet"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (motion-word ENTER)
                                       (motion-word STREAM)))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "Your feet are now wet.\n"
        (expect-enum 'get-user-input)))

(test-proc 'handle-special-inputs "not-wet"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (motion-word ENTER) (object-word WATER))))
                 ($set-location (K hill)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "Where?\n"
        (expect-enum 'get-user-input)))

(test-proc 'handle-special-inputs "enter-house"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (motion-word ENTER) (motion-word HOUSE)))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (expect-enum 'shift))

(test-proc 'handle-special-inputs "enter"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (motion-word ENTER) V))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (expect-enum 'parse-label))

(test-proc 'handle-special-inputs "water-plant"
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

(test-proc 'handle-special-inputs "normal"
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

(test-proc 'shift ""
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

(define (main args)
  0)
