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
            (print-stars ($place-of BATTERIES))
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
            (print-stars ($place-of EMERALD)))))))
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

(define-test 'check-object-location "default"
  '(lambda (world proc)
     (let-world (($set-obj (K KEYS)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list 'cant-see-it))

(define-test 'check-object-location "grate-at-road"
  '(lambda (world proc)
     (let-world (($set-obj (K GRATE)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $mot))))))
  (list 'try-move
        'DEPRESSION))

(define-test 'check-object-location "grate-at-spit"
  '(lambda (world proc)
     (let-world (($set-obj (K GRATE))
                 ($set-location (K spit)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $mot))))))
  (list 'try-move
        'ENTRANCE))

(define-test 'check-object-location "plant2"
  '(lambda (world proc)
     (let-world (($set-obj (K PLANT))
                 ($set-location (K e2pit))
                 ($set-prop-of PLANT2 (K c1)))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $obj))))))
  (list 'handle-object-word
        'PLANT2))

(define-test 'check-object-location "rod2"
  '(lambda (world proc)
     (let-world (($set-obj (K ROD))
                 ($carry ROD2))
       ((proc world)
	(lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars $obj))))))
  (list 'handle-object-word
        'ROD2))

(define-test 'check-object-location "water-in-bottle"
  '(lambda (world proc)
     (let-world (($set-obj (K WATER))
                 ($carry BOTTLE))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list 'handle-object-word))

(define-test 'check-object-location "oil-here"
  '(lambda (world proc)
     (let-world (($set-obj (K OIL))
                 ($set-location (K epit)))
       ((proc world)
	(lambda (cont world)
          (print-stars cont)))))
  (list 'handle-object-word))

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

(define-test 'transitive-drop "ok"
  '(lambda (world proc)
     (let-world (($carry LAMP)
                 ($set-obj (K LAMP)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($place-of LAMP)))))))
  (list "OK.\n"
        'get-user-input
        'road))

(define-test 'transitive-drop "not-toting"
  '(lambda (world proc)
     (let-world (($set-obj (K LAMP)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list 'report-default))

(define-test 'transitive-drop "rod2"
  '(lambda (world proc)
     (let-world (($carry ROD2)
                 ($set-obj (K ROD)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($place-of ROD2)))))))
  (list "OK.\n"
        'get-user-input
        'road))

(define-test 'transitive-drop "drop-coins"
  '(lambda (world proc)
     (let-world (($set-location (K pony))
                 ($carry COINS)
                 ($set-obj (K COINS)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($place-of COINS))
            (print-stars ($place-of BATTERIES))
            (print-stars ($prop-of BATTERIES)))))))
  (list "There are fresh batteries here.\n"
        'get-user-input
        'limbo
        'pony
        0))

(define-test 'transitive-drop "drop-bird"
  '(lambda (world proc)
     (let-world (($carry BIRD)
                 ($set-obj (K BIRD))
                 ($set-prop-of BIRD (K c1)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($place-of BIRD))
            (print-stars ($prop-of BIRD)))))))
  (list "OK.\n"
        'get-user-input
        'road
        0))

(define-test 'transitive-drop "drop-bird-at-snake"
  '(lambda (world proc)
     (let-world (($carry BIRD)
                 ($set-obj (K BIRD))
                 ($set-prop-of BIRD (K c1))
                 ($set-location (K hmk)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($place-of BIRD))
            (print-stars ($prop-of BIRD))
            (print-stars ($place-of SNAKE))
            (print-stars ($prop-of SNAKE)))))))
  (list "The little bird attacks the green snake, and in an astounding flurry\ndrives the snake away.\n"
        'get-user-input
        'hmk
        0
        'limbo
        1))

(define-test 'transitive-drop "drop-vase"
  '(lambda (world proc)
     (let-world (($carry VASE)
                 ($set-obj (K VASE)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($place-of VASE))
            (print-stars ($prop-of VASE))
            (print-stars ($base-of VASE)))))))
  (list "The Ming vase drops with a delicate crash.\n"
        'get-user-input
        'road
        2
        'VASE))

(define-test 'transitive-drop "drop-vase-on-pillow"
  '(lambda (world proc)
     (let-world (($carry VASE)
                 ($set-obj (K VASE))
                 ($drop PILLOW road))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($place-of VASE))
            (print-stars ($prop-of VASE))
            (print-stars ($base-of VASE)))))))
  (list "The vase is now resting, delicately, on a velvet pillow.\n"
        'get-user-input
        'road
        0
        'NOTHING))

(define-test 'transitive-drop "drop-bear"
  '(lambda (world proc)
     (let-world (($carry BEAR)
                 ($set-obj (K BEAR))
                 ($set-location (K neside)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($place-of BEAR))
            (print-stars ($place-of TROLL))
            (print-stars ($place-of TROLL_))
            (print-stars ($place-of TROLL2))
            (print-stars ($place-of TROLL2_))
            (print-stars ($prop-of TROLL)))))))
  (list "The bear lumbers toward the troll, who lets out a startled shriek and\nscurries away.  The bear soon gives up the pursuit and wanders back.\n"
        'get-user-input
        'neside
        'limbo
        'limbo
        'swside
        'neside
        2))

(define-test 'transitive-drop "drop-water-in-bottle"
  '(lambda (world proc)
     (let-world (($carry BOTTLE)
                 ($carry WATER)
                 ($set-obj (K WATER)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($place-of BOTTLE))
            (print-stars ($place-of WATER)))))))
  (list "OK.\n"
        'get-user-input
        'road
        'limbo))

(define-test 'transitive-drop "drop-oil-in-bottle"
  '(lambda (world proc)
     (let-world (($carry BOTTLE)
                 ($set-prop-of BOTTLE (K c2))
                 ($carry OIL)
                 ($set-obj (K BOTTLE)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($place-of BOTTLE))
            (print-stars ($place-of OIL)))))))
  (list "OK.\n"
        'get-user-input
        'road
        'limbo))

(define-test 'transitive-drop "cage-with-bird"
  '(lambda (world proc)
     (let-world (($carry CAGE)
                 ($carry BIRD)
                 ($set-prop-of BIRD (K c1))
                 ($set-obj (K CAGE)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($place-of CAGE))
            (print-stars ($place-of BIRD)))))))
  (list "OK.\n"
        'get-user-input
        'road
        'road))

(define-test 'transitive-open "default"
  '(lambda (world proc)
     (let-world (($set-verb (K OPEN))
                 ($set-obj (K LAMP)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list 'report-default))

(define-test 'transitive-open "keys"
  '(lambda (world proc)
     (let-world (($set-verb (K OPEN))
                 ($set-obj (K KEYS)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "You can't lock or unlock the keys.\n"
        'get-user-input))

(define-test 'transitive-open "cage"
  '(lambda (world proc)
     (let-world (($set-verb (K OPEN))
                 ($set-obj (K CAGE)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "It has no lock.\n"
        'get-user-input))

(define-test 'transitive-open "door-ok"
  '(lambda (world proc)
     (let-world (($set-verb (K OPEN))
                 ($set-obj (K DOOR))
                 ($set-prop-of DOOR (K c1)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "OK.\n"
        'get-user-input))

(define-test 'transitive-open "door-fail"
  '(lambda (world proc)
     (let-world (($set-verb (K OPEN))
                 ($set-obj (K DOOR))
                 ($set-prop-of DOOR (K c0)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "The door is extremely rusty and refuses to open.\n"
        'get-user-input))

(define-test 'transitive-open "open-grate-without-keys"
  '(lambda (world proc)
     (let-world (($set-verb (K OPEN))
                 ($set-obj (K GRATE)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "You have no keys!\n"
        'get-user-input))

(define-test 'transitive-open "close-locked-grate"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($set-prop-of GRATE (K c0))
                 ($set-verb (K CLOSE))
                 ($set-obj (K GRATE)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($prop-of GRATE)))))))
  (list "It was already locked.\n"
        'get-user-input
        0))

(define-test 'transitive-open "close-unlocked-grate"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($set-prop-of GRATE (K c1))
                 ($set-verb (K CLOSE))
                 ($set-obj (K GRATE)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($prop-of GRATE)))))))
  (list "The grate is now locked.\n"
        'get-user-input
        0))

(define-test 'transitive-open "open-locked-grate"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($set-prop-of GRATE (K c0))
                 ($set-verb (K OPEN))
                 ($set-obj (K GRATE)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($prop-of GRATE)))))))
  (list "The grate is now unlocked.\n"
        'get-user-input
        1))

(define-test 'transitive-open "open-unlocked-grate"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($set-prop-of GRATE (K c1))
                 ($set-verb (K OPEN))
                 ($set-obj (K GRATE)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($prop-of GRATE)))))))
  (list "It was already unlocked.\n"
        'get-user-input
        1))

(define-test 'transitive-open "open-chain-without-keys"
  '(lambda (world proc)
     (let-world (($set-verb (K OPEN))
                 ($set-obj (K CHAIN)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "You have no keys!\n"
        'get-user-input))

(define-test 'transitive-open "close-chain-outside-barr"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($set-verb (K CLOSE))
                 ($set-obj (K CHAIN)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "There is nothing here to which the chain can be locked.\n"
        'get-user-input))

(define-test 'transitive-open "close-locked-chain"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($set-location (K barr))
                 ($set-prop-of CHAIN (K c2))
                 ($set-verb (K CLOSE))
                 ($set-obj (K CHAIN)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "It was already locked.\n"
        'get-user-input))

(define-test 'transitive-open "close-chain"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($set-location (K barr))
                 ($set-prop-of CHAIN (K c0))
                 ($set-base-of CHAIN (K NOTHING))
                 ($carry CHAIN)
                 ($set-verb (K CLOSE))
                 ($set-obj (K CHAIN)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($prop-of CHAIN))
            (print-stars ($base-of CHAIN))
            (print-stars ($place-of CHAIN)))))))
  (list "The chain is now locked.\n"
        'get-user-input
        2
        'CHAIN
        'barr))

(define-test 'transitive-open "open-unlocked-chain"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($set-prop-of CHAIN (K c0))
                 ($set-verb (K OPEN))
                 ($set-obj (K CHAIN)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "It was already unlocked.\n"
        'get-user-input))

(define-test 'transitive-open "open-chain-bear"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($set-prop-of CHAIN (K c1))
                 ($set-prop-of BEAR (K c0))
                 ($set-verb (K OPEN))
                 ($set-obj (K CHAIN)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "There is no way to get past the bear to unlock the chain, which is\nprobably just as well.\n"
        'get-user-input))

(define-test 'transitive-open "open-chain"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($set-prop-of CHAIN (K c1))
                 ($set-prop-of BEAR (K c1))
                 ($set-verb (K OPEN))
                 ($set-obj (K CHAIN)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($prop-of CHAIN))
            (print-stars ($base-of CHAIN))
            (print-stars ($prop-of BEAR))
            (print-stars ($base-of BEAR)))))))
  (list "The chain is now unlocked.\n"
        'get-user-input
        0
        'NOTHING
        2
        'NOTHING))

(define-test 'transitive-open "open-chain-bear-dead"
  '(lambda (world proc)
     (let-world (($carry KEYS)
                 ($set-prop-of CHAIN (K c2))
                 ($set-prop-of BEAR (K c3))
                 ($set-verb (K OPEN))
                 ($set-obj (K CHAIN)))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($prop-of CHAIN))
            (print-stars ($base-of CHAIN))
            (print-stars ($prop-of BEAR))
            (print-stars ($base-of BEAR)))))))
  (list "The chain is now unlocked.\n"
        'get-user-input
        0
        'NOTHING
        3
        'BEAR))

(define-test 'transitive-open "close-clam"
  '(lambda (world proc)
     (let-world (($set-verb (K CLOSE))
                 ($set-obj (K CLAM)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "What?\n"
        'get-user-input))

(define-test 'transitive-open "open-clam-without-trident"
  '(lambda (world proc)
     (let-world (($set-verb (K OPEN))
                 ($set-obj (K CLAM)))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "You don't have anything strong enough to open the clam.\n"
        'get-user-input))

(define-test 'transitive-open "open-clam-in-hand"
  '(lambda (world proc)
     (let-world (($set-verb (K OPEN))
                 ($set-obj (K CLAM))
                 ($carry TRIDENT)
                 ($carry CLAM))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "I advise you to put down the clam before opening it.  >STRAIN!<\n"
        'get-user-input))

(define-test 'transitive-open "open-oyster-in-hand"
  '(lambda (world proc)
     (let-world (($set-verb (K OPEN))
                 ($set-obj (K OYSTER))
                 ($carry TRIDENT)
                 ($carry OYSTER))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "I advise you to put down the oyster before opening it.  >WRENCH!<\n"
        'get-user-input))

(define-test 'transitive-open "open-clam"
  '(lambda (world proc)
     (let-world (($set-verb (K OPEN))
                 ($set-obj (K CLAM))
                 ($carry TRIDENT))
       ((proc world)
        (lambda (cont world)
          (begin
            (print-stars cont)
            (print-stars ($place-of CLAM))
            (print-stars ($place-of OYSTER))
            (print-stars ($place-of PEARL)))))))
  (list "A glistening pearl falls out of the clam and rolls away.  Goodness,\nthis must really be an oyster.  (I never was very good at identifying\nbivalves.)  Whatever it is, it has now snapped shut again.\n"
        'get-user-input
        'limbo
        'road
        'sac))

(define-test 'transitive-open "open-oyster"
  '(lambda (world proc)
     (let-world (($set-verb (K OPEN))
                 ($set-obj (K OYSTER))
                 ($carry TRIDENT))
       ((proc world)
        (lambda (cont world)
          (print-stars cont)))))
  (list "The oyster creaks open, revealing nothing but oyster inside.\nIt promptly snaps shut again.\n"
        'get-user-input))


(define (main args)
  (let ((testname (if (null? (cdr args)) #f (string->symbol (cadr args)))))
    (for-each
     (lambda (testcase)
       (if (or (not testname) (eq? testname (car testcase)))
           (apply test-proc testcase)))
     (reverse tests)))
  0)
