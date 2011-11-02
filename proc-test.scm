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
		    (car (drop program-table (lookup-enum proc-id))))))
	 (process (run-process '(unlambda) :input :pipe :output :pipe))
         (expect-str (make-expected-string expect))
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

(define (make-expected-string lst)
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (x)
	 (cond ((symbol? x)
		(display #\{) (display (make-string (lookup-enum x) #\*)) (display #\}))
	       ((number? x)
		(display #\{) (display (make-string x #\*)) (display #\}))
	       ((eq? #t x)
		(display "{t}"))
	       ((eq? #f x)
		(display "{f}"))
	       (else
		(display x))))
       lst))))

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
  (list 'commence
        'like1))

(define-test 'commence "goto-death"
  '(lambda (world proc)
     (let-world (($set-location (K limbo)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list 'death))

(define-test 'commence "pitch-dark-death"
  '(lambda (world proc)
     (let-world (($set-location (K bird))
                 ($set-was-dark (K I))
                 ($set-rand (K (list KI KI KI KI KI KI))))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list 'pitch-dark))

(define-test 'commence "pitch-dark-msg"
  '(lambda (world proc)
     (let-world (($set-location (K bird)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "\nIt is now pitch dark.  If you proceed you will most likely fall into a pit.\n"
        'get-user-input))

(define-test 'commence "longdesc"
  '(lambda (world proc)
     (let-world (($set-location (K house)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "\nYou are inside a building, a well house for a large spring.\n"
        'describe-objects))

(define-test 'commence "shortdesc"
  '(lambda (world proc)
     (let-world (($set-location (K house))
                 ($set-nth set-visits house (K (cons1 V))))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "\nYou're inside building.\n"
        'describe-objects))

(define-test 'commence "bear"
  '(lambda (world proc)
     (let-world (($set-location (K house))
                 ($carry BEAR))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "You are being followed by a very large, tame bear.\n"
        "\nYou are inside a building, a well house for a large spring.\n"
        'describe-objects))

(define-test 'commence "forced-move"
  '(lambda (world proc)
     (let-world (($set-location (K crack)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "\nThe crack is far too small for you to follow.\n"
        'try-move))

(define-test 'describe-objects "count-visits"
  '(lambda (world proc)
     ((proc world)
      (lambda (cont world)
        (begin
          (print-stars (cons1-length (nth initial-location $visits)))
          (print-stars cont)))))
  (list 4
        'get-user-input))

(define-test 'describe-objects "count-visits2"
  '(lambda (world proc)
     (let-world (($set-nth set-visits initial-location (K (cons1 (cons1 V)))))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars (cons1-length (nth initial-location $visits)))
            (print-stars cont))))))
  (list 1
        'get-user-input))

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
        'get-user-input))

(define-test 'describe-objects "based"
  '(lambda (world proc)
     (let-world (($set-location (K inside)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "The grate is locked.\n"
        'get-user-input))

(define-test 'describe-objects "treads-gold"
  '(lambda (world proc)
     (let-world (($set-location (K spit))
                 ($carry GOLD))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list 'get-user-input))

(define-test 'describe-objects "treads-spit"
  '(lambda (world proc)
     (let-world (($set-location (K spit)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "Rough stone steps lead down the pit.\n"
        'get-user-input))

(define-test 'describe-objects "treads-emist"
  '(lambda (world proc)
     (let-world (($set-location (K emist)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "Rough stone steps lead up the dome.\n"
        'get-user-input))

(define-test 'describe-objects "treasure"
  '(lambda (world proc)
     (let-world (($set-location (K west)))
       ((proc world)
        (lambda (cont world)
          (print-stars ($prop-of COINS))))))
  (list "There are many coins here!\n"
        0))

(define-test 'describe-objects "rug"
  '(lambda (world proc)
     (let-world (($set-location (K scan3)))
       ((proc world)
        (lambda (cont world)
          (print-stars ($prop-of RUG))))))
  (list "A huge green fierce dragon bars the way!\n"
        "The dragon is sprawled out on a Persian rug!!\n"
        1))

(define-test 'describe-objects "chain"
  '(lambda (world proc)
     (let-world (($set-location (K barr)))
       ((proc world)
        (lambda (cont world)
          (print-stars ($prop-of CHAIN))))))
  (list "There is a ferocious cave bear eying you from the far end of the room!\n"
        "The bear is locked to the wall with a golden chain!\n"
        1))

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
  (list 'cycle
        'ABSTAIN
        'ABSTAIN
        'NOTHING
        'LAMP))

(define-test 'pre-parse "increment-turns"
  '(lambda (world proc)
     (let-world (($set-turns (K (cons1 (cons1 V)))))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
	    (print-stars (cons1-length $turns)))))))
  (list 'clocks-and-lamp
        3))

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
  (list 'clocks-and-lamp
        'ABSTAIN))

(define-test 'pre-parse "say-nothing"
  '(lambda (world proc)
     (let-world (($set-verb (K SAY))
                 ($set-word12 (K (cons dummy-word V))))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
	    (print-stars $verb))))))
  (list 'transitive
        'SAY))

(define-test 'check-the-lamp "lamp-off"
  '(lambda (world proc)
     (let-world (($set-limit (K (cons1 (cons1 V)))))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
            (print-stars (cons1-length $limit)))))))
  (list 'handle-special-inputs
        2))

(define-test 'check-the-lamp "lamp-on"
  '(lambda (world proc)
     (let-world (($set-prop-of LAMP (K c1))
                 ($set-limit (K (cons1 (cons1 V)))))
       ((proc world)
	(lambda (cont world)
	  (begin
	    (print-stars cont)
            (print-stars (cons1-length $limit)))))))
  (list 'handle-special-inputs
        1))

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
        'handle-special-inputs
        0
        0
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
        'handle-special-inputs
        1
        'road
        2500
        ))

(define-test 'check-the-lamp "giveup"
  '(lambda (world proc)
     (let-world (($set-limit (K V)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "There's not much point in wandering around out here, and you can't\nexplore the cave without a lamp.  So let's just call it a day.\n"
        'give-up))

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
        'handle-special-inputs
        #f))

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
        'handle-special-inputs
        #f))

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
        'handle-special-inputs
        #f))

(define-test 'handle-special-inputs "wet"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (motion-word ENTER)
                                       (motion-word STREAM)))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "Your feet are now wet.\n"
        'get-user-input))

(define-test 'handle-special-inputs "not-wet"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (motion-word ENTER) (object-word WATER))))
                 ($set-location (K hill)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "Where?\n"
        'get-user-input))

(define-test 'handle-special-inputs "enter-house"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (motion-word ENTER) (motion-word HOUSE)))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list 'shift))

(define-test 'handle-special-inputs "enter"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (motion-word ENTER) V))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list 'parse-label))

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
  (list 'parse-label
        #t
        'POUR))

(define-test 'handle-special-inputs "normal"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (object-word WATER) (object-word PLANT)))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool (noun? (cdr $word12)))
            (print-stars (word-meaning (cdr $word12))))))))
  (list 'parse-label
        #t
        'PLANT))

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
  (list 'parse-label
        #t
        'FOOD
        #f))

(define-test 'look-at-word1 "unknown-word"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons V V))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "I don't know that word.\n"
        'cycle))

(define-test 'look-at-word1 "motion"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (motion-word NE) V))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $mot))))))
  (list 'try-move
        'NE))

(define-test 'look-at-word1 "object-here"
  '(lambda (world proc)
     (let-world (($set-location (K house))
                 ($set-word12 (K (cons (object-word LAMP) V))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $obj))))))
  (list 'handle-object-word
        'LAMP))

(define-test 'look-at-word1 "second-object-here"
  '(lambda (world proc)
     (let-world (($set-location (K inside))
                 ($set-word12 (K (cons (object-word GRATE) V))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $obj))))))
  (list 'handle-object-word
        'GRATE))

(define-test 'look-at-word1 "object-not-here"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (object-word LAMP) V))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $obj))))))
  (list 'check-object-location
        'LAMP))

(define-test 'look-at-word1 "verb-with-word2"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (action-word DROP) (object-word LAMP)))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $verb))))))
  (list 'shift
        'DROP))

(define-test 'look-at-word1 "verb-transitive"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (action-word DROP) V)))
                 ($set-obj (K LAMP)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $verb))))))
  (list 'transitive
        'DROP))

(define-test 'look-at-word1 "verb-intransitive"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (action-word DROP) V)))
                 ($set-obj (K NOTHING)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $verb))))))
  (list 'intransitive
        'DROP))

(define-test 'look-at-word1 "message-word"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (message-word c4) V))))  ; LOST
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "I'm as confused as you are.\n"
        'get-user-input))

(define-test 'handle-object-word "shift"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons V (action-word TAKE)))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list 'shift))

(define-test 'handle-object-word "with-verb"
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons V V)))
                 ($set-verb (K TAKE)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list 'transitive))

(defmacro (action-word-with-str meaning str)
  (lambda (f) (f (lambda (_ _ x _) x) meaning str)))
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
        'cycle))

(define-test 'cant-see-it ""
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (object-word-with-str LAMP (string "lamp")) V))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "I see no lamp here.\n"
        'get-user-input))

(define-test 'cant-see-it "find"
  '(lambda (world proc)
     (let-world (($set-verb (K FIND))
                 ($set-word12 (K (icons V V))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list 'transitive))

(define-test 'try-move "go"
  '(lambda (world proc)
     (let-world (($set-location (K forest))
                 ($set-oldlocs (K (cons hill house)))
                 ($set-mot (K SOUTH)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $newloc)
            (print-stars (car $oldlocs))
            (print-stars (cdr $oldlocs)))))))
  (list 'go-for-it
        'forest
        'forest
        'hill))

(define-test 'try-move "nowhere"
  '(lambda (world proc)
     (let-world (($set-location (K forest))
                 ($set-mot (K NOWHERE)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $newloc))))))
  (list 'mainloop
        'forest))

(define-test 'try-move "look"
  '(lambda (world proc)
     (let-world (($set-location (K forest))
                 ($set-mot (K LOOK))
                 ($set-was-dark (K I))
                 ($set-nth set-visits forest (K (cons1 V))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool $was-dark)
            (print-stars (cons1-length (nth forest $visits))))))))
  (list "Sorry, but I am not allowed to give more detail.  I will repeat the\nlong description of your location.\n"
        'mainloop
        #f
        0))

(define-test 'try-move "look-quiet"
  '(lambda (world proc)
     (let-world (($set-verbose (K (cons1 V)))
                 ($set-location (K forest))
                 ($set-mot (K LOOK))
                 ($set-was-dark (K I))
                 ($set-nth set-visits forest (K (cons1 V))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool $was-dark)
            (print-stars (cons1-length (nth forest $visits))))))))
  (list 'mainloop
        #f
        0))

(define-test 'try-move "cave"
  '(lambda (world proc)
     (let-world (($set-location (K forest))
                 ($set-mot (K CAVE)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "I can't see where the cave is, but hereabouts no stream can run on\nthe surface for long.  I would try the stream.\n"
        'mainloop))

(define-test 'try-move "cave2"
  '(lambda (world proc)
     (let-world (($set-location (K inside))
                 ($set-mot (K CAVE)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "I need more detailed instructions to do that.\n"
        'mainloop))

(define-test 'go-for-it "go"
  '(lambda (world proc)
     (let-world (($set-location (K road))
                 ($set-mot (K ENTER)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $newloc))))))
  (list 'mainloop
        'house))

(define-test 'go-for-it "crawl"
  '(lambda (world proc)
     (let-world (($set-location (K road))
                 ($set-mot (K CRAWL)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "Which way?\n"
        'mainloop))

(define-test 'go-for-it "xyzzy"
  '(lambda (world proc)
     (let-world (($set-location (K road))
                 ($set-mot (K XYZZY)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "Nothing happens.\n"
        'mainloop))

(define-test 'go-for-it "find"
  '(lambda (world proc)
     (let-world (($set-location (K road))
                 ($set-mot (K SLIT))
                 ($set-verb (K FIND)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "I can only tell you what you see as you move about and manipulate\nthings.  I cannot tell you where remote things are.\n"
        'mainloop))

(define-test 'go-for-it "out"
  '(lambda (world proc)
     (let-world (($set-location (K road))
                 ($set-mot (K OUT)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "I don't know in from out here.  Use compass points or name something\nin the general direction you want to go.\n"
        'mainloop))

(define-test 'go-for-it "right"
  '(lambda (world proc)
     (let-world (($set-location (K road))
                 ($set-mot (K R)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "I am unsure how you are facing.  Use compass points or nearby objects.\n"
        'mainloop))

(define-test 'go-for-it "ne"
  '(lambda (world proc)
     (let-world (($set-location (K road))
                 ($set-mot (K NE)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "There is no way to go in that direction.\n"
        'mainloop))

(define-test 'go-for-it "location-word"
  '(lambda (world proc)
     (let-world (($set-location (K road))
                 ($set-mot (K SLIT)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "I don't know how to apply that word here.\n"
        'mainloop))

(define-test 'go-for-it "remark"
  '(lambda (world proc)
     (let-world (($set-location (K slit))
                 ($set-mot (K D)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $newloc))))))
  (list "You don't fit through a two-inch slit!\n"
        'mainloop
        'slit))

(define-test 'go-for-it "random-pass"
  '(lambda (world proc)
     (let-world (($set-location (K witt))
                 ($set-mot (K SOUTH))
                 ($set-rand (K (list K K K K K K))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $newloc))))))
  (list 'mainloop
        'ante))

(define-test 'go-for-it "random-not-pass"
  '(lambda (world proc)
     (let-world (($set-location (K witt))
                 ($set-mot (K SOUTH))
                 ($set-rand (K (list KI KI KI KI KI KI))))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $newloc))))))
  (list "You have crawled around in some little holes and wound up back in the\nmain passage.\n"
        'mainloop
        'witt))

(define-test 'go-for-it "property-ok"
  '(lambda (world proc)
     (let-world (($set-location (K outside))
                 ($set-prop-of GRATE (K c1))
                 ($set-mot (K ENTER)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $newloc))))))
  (list 'mainloop
        'inside))

(define-test 'go-for-it "property-ng"
  '(lambda (world proc)
     (let-world (($set-location (K outside))
                 ($set-mot (K ENTER)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $newloc))))))
  (list "You can't go through a locked steel grate!\n"
        'mainloop
        'outside))

(define-test 'go-for-it "holds-true"
  '(lambda (world proc)
     (let-world (($set-location (K shell))
                 ($carry CLAM)
                 ($set-mot (K SOUTH)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $newloc))))))
  (list "You can't fit this five-foot clam through that little passage!\n"
        'mainloop
        'shell))

(define-test 'go-for-it "holds-false"
  '(lambda (world proc)
     (let-world (($set-location (K shell))
                 ($set-mot (K SOUTH)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $newloc))))))
  (list 'mainloop
        'complex))

(define-test 'go-for-it "ppass"
  '(lambda (world proc)
     (let-world (($set-location (K alcove))
                 ($set-mot (K PASSAGE)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $newloc))))))
  (list 'mainloop
        'proom))

(define-test 'go-for-it "ppass-proom"
  '(lambda (world proc)
     (let-world (($set-location (K proom))
                 ($set-mot (K PASSAGE))
                 ($carry EMERALD))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $newloc))))))
  (list 'mainloop
        'alcove))

(define-test 'go-for-it "ppass-fail"
  '(lambda (world proc)
     (let-world (($set-location (K alcove))
                 ($set-mot (K PASSAGE))
                 ($carry LAMP))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $newloc))))))
  (list "Something you're carrying won't fit through the tunnel with you.\nYou'd best take inventory and drop something.\n"
        'mainloop
        'alcove))

(define-test 'go-for-it "pdrop"
  '(lambda (world proc)
     (let-world (($set-location (K y2))
                 ($set-mot (K PLOVER))
                 ($carry EMERALD))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $newloc)
            (print-stars (nth EMERALD $place)))))))
  (list 'mainloop
        'proom
        'y2))

(define-test 'report-default ""
  '(lambda (world proc)
     (let-world (($set-verb (K DROP)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "You aren't carrying it!\n"
        'get-user-input))

(define-test 'get-object ""
  '(lambda (world proc)
     (let-world (($set-word12 (K (cons (action-word-with-str TOSS (string "throw")) V))))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "throw what?\n"
        'cycle))

(define-test 'intransitive-take "no-object"
  '(lambda (world proc)
     (let-world (($set-location (K road)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list 'get-object))

(define-test 'intransitive-take "one-object"
  '(lambda (world proc)
     (let-world (($set-location (K west)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $obj))))))
  (list 'transitive
        'COINS))

(define-test 'intransitive-take "many-objects"
  '(lambda (world proc)
     (let-world (($set-location (K house)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list 'get-object))

(define-test 'intransitive-eat "no-food"
  '(lambda (world proc)
     (let-world (($set-location (K road)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list 'get-object))

(define-test 'intransitive-eat "food-here"
  '(lambda (world proc)
     (let-world (($set-location (K house)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $obj))))))
  (list 'transitive
        'FOOD))

(define-test 'intransitive-open "grate"
  '(lambda (world proc)
     (let-world (($set-location (K outside)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $obj))))))
  (list 'transitive
        'GRATE))

(define-test 'intransitive-open "chain"
  '(lambda (world proc)
     (let-world (($carry CHAIN))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $obj))))))
  (list 'transitive
        'CHAIN))

(define-test 'intransitive-open "grate-chain"
  '(lambda (world proc)
     (let-world (($set-location (K outside))
                 ($carry CHAIN))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont))))))
  (list 'get-object))

(define-test 'intransitive-open "nothing"
  '(lambda (world proc)
     (let-world (($set-location (K road)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "There is nothing here with a lock!\n"
        'get-user-input))

(define-test 'intransitive-inventory "nothing"
  '(lambda (world proc)
     ((proc world)
      (lambda (cont world)
        (print-stars cont))))
  (list "You're not carrying anything.\n"
        'get-user-input))

(define-test 'intransitive-inventory "objects"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($carry LAMP))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "You are currently holding the following:\n"
        " Set of keys\n"
        " Brass lantern\n"
        'get-user-input))

(define-test 'intransitive-inventory "bear"
  '(lambda (world proc)
     (let-world (($carry BEAR))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "You are being followed by a very large, tame bear.\n"
        'get-user-input))

(define-test 'intransitive-inventory "object-and-bear"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($carry BEAR))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "You are currently holding the following:\n"
        " Set of keys\n"
        "You are being followed by a very large, tame bear.\n"
        'get-user-input))

(define-test 'intransitive-brief ""
  '(lambda (world proc)
     ((proc world)
      (lambda (cont world)
        (begin
          (print-stars cont)
          (print-stars (cons1-length $verbose))))))
  (list "Okay, from now on I'll only describe a place in full the first time\nyou come to it.  To get the full description, say \"LOOK\".\n"
        'get-user-input
        0))

(define-test 'transitive-eat "food"
  '(lambda (world proc)
     (let-world (($set-obj (K FOOD)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "Thank you, it was delicious!\n"
        'get-user-input))

(define-test 'transitive-eat "snake"
  '(lambda (world proc)
     (let-world (($set-obj (K SNAKE)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list "I think I just lost my appetite.\n"
        'get-user-input))

(define-test 'transitive-eat "other"
  '(lambda (world proc)
     (let-world (($set-obj (K KEYS)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list 'report-default))

(define-test 'transitive-on "no-lamp"
  '(lambda (world proc)
     ((proc world)
      (lambda (cont world)
        (print-stars cont))))
  (list 'report-default))

(define-test 'transitive-on "out-of-power"
  '(lambda (world proc)
     (let-world (($carry LAMP)
                 ($set-limit (K V)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($prop-of LAMP)))))))
  (list "Your lamp has run out of power.\n"
        'get-user-input
        0))

(define-test 'transitive-on "was-dark"
  '(lambda (world proc)
     (let-world (($carry LAMP)
                 ($set-was-dark (K I)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($prop-of LAMP)))))))
  (list "Your lamp is now on.\n"
        'commence
        1))

(define-test 'transitive-on "not-was-dark"
  '(lambda (world proc)
     (let-world (($carry LAMP)
                 ($set-was-dark (K V)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($prop-of LAMP)))))))
  (list "Your lamp is now on.\n"
        'get-user-input
        1))

(define-test 'transitive-off "no-lamp"
  '(lambda (world proc)
     ((proc world)
      (lambda (cont world)
        (print-stars cont))))
  (list 'report-default))

(define-test 'transitive-off "not-dark"
  '(lambda (world proc)
     (let-world (($carry LAMP))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($prop-of LAMP)))))))
  (list "Your lamp is now off.\n"
        'get-user-input
        0))

(define-test 'transitive-off "dark"
  '(lambda (world proc)
     (let-world (($carry LAMP)
                 ($set-location (K debris)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($prop-of LAMP)))))))
  (list "Your lamp is now off.\n"
        "It is now pitch dark.  If you proceed you will most likely fall into a pit.\n"
        'get-user-input
        0))

(define-test 'transitive-take "carrying"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($set-obj (K KEYS)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list 'report-default))

(define-test 'transitive-take "immovable"
  '(lambda (world proc)
     (let-world (($set-obj (K GRATE)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "You can't be serious!\n"
        'get-user-input))

(define-test 'transitive-take "immovable-chain"
  '(lambda (world proc)
     (let-world (($set-obj (K CHAIN))
                 ($set-prop-of BEAR (K c1)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "The chain is still locked.\n"
        'get-user-input))

(define-test 'transitive-take "immovable-bear"
  '(lambda (world proc)
     (let-world (($set-obj (K BEAR))
                 ($set-prop-of BEAR (K c1)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "The bear is still chained to the wall.\n"
        'get-user-input))

(define-test 'transitive-take "immovable-plant"
  '(lambda (world proc)
     (let-world (($set-obj (K PLANT)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "The plant has exceptionally deep roots and cannot be pulled free.\n"
        'get-user-input))

(define-test 'transitive-take "ok"
  '(lambda (world proc)
     (let-world (($set-obj (K LAMP)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool ($toting? LAMP)))))))
  (list "OK.\n"
        'get-user-input
        #t))

(define-test 'transitive-take "toomany"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($carry LAMP)
                 ($carry CAGE)
                 ($carry ROD)
                 ($carry BIRD)
                 ($carry FOOD)
                 ($carry AXE)
                 ($set-obj (K MAG)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "You can't carry anything more.  You'll have to drop something first.\n"
        'get-user-input))

(define-test 'transitive-take "do-not-count-water-in-bottle"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($carry LAMP)
                 ($carry CAGE)
                 ($carry ROD)
                 ($carry BIRD)
                 ($carry BOTTLE)
                 ($carry WATER)
                 ($set-obj (K MAG)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool ($toting? MAG)))))))
  (list "OK.\n"
        'get-user-input
        #t))

(define-test 'transitive-take "water-in-bottle"
  '(lambda (world proc)
     (let-world (($set-location (K house))
                 ($set-obj (K WATER)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $obj)
            (print-bool ($toting? BOTTLE)))))))
  (list "OK.\n"
        'get-user-input
        'BOTTLE
        #t))

(define-test 'transitive-take "change-to-fill"
  '(lambda (world proc)
     (let-world (($carry BOTTLE)
                 ($set-prop-of BOTTLE (K c1))
                 ($set-obj (K WATER))
                 ($set-verb (K TAKE)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $obj)
            (print-stars $verb)
            (print-stars $oldverb))))))
  (list 'transitive
        'BOTTLE
        'FILL
        'TAKE))

(define-test 'transitive-take "no-bottle"
  '(lambda (world proc)
     (let-world (($set-obj (K WATER)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "You have nothing in which to carry it.\n"
        'get-user-input))

(define-test 'transitive-take "liquid-in-bottle"
  '(lambda (world proc)
     (let-world (($set-location (K house))
                 ($set-obj (K BOTTLE)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool ($toting? BOTTLE))
            (print-bool ($toting? WATER)))))))
  (list "OK.\n"
        'get-user-input
        #t
        #t))

(define-test 'transitive-take "bird"
  '(lambda (world proc)
     (let-world (($carry CAGE)
                 ($set-obj (K BIRD)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool ($toting? BIRD))
            (print-stars ($prop-of BIRD)))))))
  (list "OK.\n"
        'get-user-input
        #t
        1))

(define-test 'transitive-take "bird-without-cage"
  '(lambda (world proc)
     (let-world (($set-obj (K BIRD)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool ($toting? BIRD))
            (print-stars ($prop-of BIRD)))))))
  (list "You can catch the bird, but you cannot carry it.\n"
        'get-user-input
        #f
        0))

(define-test 'transitive-take "bird-with-rod"
  '(lambda (world proc)
     (let-world (($carry ROD)
                 ($set-obj (K BIRD)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool ($toting? BIRD))
            (print-stars ($prop-of BIRD)))))))
  (list "The bird was unafraid when you entered, but as you approach it becomes\ndisturbed and you cannot catch it.\n"
        'get-user-input
        #f
        0))

(define-test 'transitive-take "bird-in-cage"
  '(lambda (world proc)
     (let-world (($set-prop-of BIRD (K c1))
                 ($set-obj (K BIRD)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool ($toting? BIRD))
            (print-bool ($toting? CAGE)))))))
  (list "OK.\n"
        'get-user-input
        #t
        #t))

(define-test 'transitive-take "cage-with-bird"
  '(lambda (world proc)
     (let-world (($set-prop-of BIRD (K c1))
                 ($set-obj (K CAGE)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-bool ($toting? BIRD))
            (print-bool ($toting? CAGE)))))))
  (list "OK.\n"
        'get-user-input
        #t
        #t))


(define (main args)
  (let ((testname (if (null? (cdr args)) #f (string->symbol (cadr args)))))
    (for-each
     (lambda (testcase)
       (if (or (not testname) (eq? testname (car testcase)))
           (apply test-proc testcase)))
     (reverse tests)))
  0)
