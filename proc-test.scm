(use gauche.process)
(require "./advent.scm")

(define (read-process-output process input)
  (display input (process-input process))
  (close-output-port (process-input process))
  (let ((out (read-block 4096 (process-output process))))
    (process-wait process)
    (string-incomplete->complete out)))

(define tests '())
(define (define-test proc-id testname setups outputs expect)
  (push! tests (list proc-id testname setups "" outputs expect)))

(define (define-input-test proc-id testname setups input outputs expect)
  (push! tests (list proc-id testname setups input outputs expect)))

(define (test-proc proc-id testname setups stdin outputs expect)
  (let* ((testcode `((lambda (world proc)
		       (let-world ,setups
			 ((proc world)
			  (lambda (cont world)
			    (begin ,@outputs)))))
		     initial-world
		     ,(car (drop program-table (lookup-enum proc-id)))))
	 (unl (compile-to-string testcode))
	 (process (run-process '(unlambda) :input :pipe :output :pipe))
         (expect-str (make-expected-string expect))
	 (out (read-process-output process (string-append unl "\n" stdin))))
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

(define yesno-prompt "\n>> ")

(defmacro (print-stars n)
  (#\{ n #\* #\} I))

(defmacro (print-bool b)
  (#\{ (if b #\t #\f) #\} I))


(define-input-test 'offer0 "no"
  '()
  "no\n"
  '((print-stars cont)
    (print-stars (nth c6 $hinted))
    (print-stars (cons1-length $limit)))
  (list "Welcome to Adventure!!  Would you like instructions?"
        yesno-prompt
        'mainloop
        5
        330))

(define-input-test 'offer0 "hoge-no"
  '()
  "hoge\nno\n"
  '((print-stars cont)
    (print-stars (nth c6 $hinted))
    (print-stars (cons1-length $limit)))
  (list "Welcome to Adventure!!  Would you like instructions?"
        yesno-prompt
        " Please answer Yes or No.\n"
        "Welcome to Adventure!!  Would you like instructions?"
        yesno-prompt
        'mainloop
        5
        330))

(define-input-test 'offer0 "empty-no"
  '()
  "\nno\n"
  '((print-stars cont)
    (print-stars (nth c6 $hinted))
    (print-stars (cons1-length $limit)))
  (list "Welcome to Adventure!!  Would you like instructions?"
        yesno-prompt
        " Please answer Yes or No.\n"
        "Welcome to Adventure!!  Would you like instructions?"
        yesno-prompt
        'mainloop
        5
        330))

(define-input-test 'offer0 "yes"
  '()
  "yes\n"
  '((print-stars cont)
    (print-stars (nth c6 $hinted))
    (print-stars (cons1-length $limit)))
  (list "Welcome to Adventure!!  Would you like instructions?"
        yesno-prompt
        "Somewhere nearby is Colossal Cave, where others have found fortunes in\n\
treasure and gold, though it is rumored that some who enter are never\n\
seen again.  Magic is said to work in the cave.  I will be your eyes\n\
and hands.  Direct me with commands of one or two words.  I should\n\
warn you that I look at only the first five letters of each word, so\n\
you'll have to enter \"NORTHEAST\" as \"NE\" to distinguish it from\n\
\"NORTH\".  Should you get stuck, type \"HELP\" for some general hints.\n\
For information on how to end your adventure, etc., type \"INFO\".\n\
                        -  -  -\n\
The first adventure program was developed by Willie Crowther.\n\
Most of the features of the current program were added by Don Woods;\n\
all of its bugs were added by Don Knuth.\n"
        'mainloop
        0
        1000))

(define-test 'mainloop ""
  '(($set-newloc (K like1)))
  '((print-stars cont)
    (print-stars $newloc))
  (list 'commence
        'like1))

(define-test 'commence "goto-death"
  '(($set-location (K limbo)))
  '((print-stars cont))
  (list 'death))

(define-test 'commence "pitch-dark-death"
  '(($set-location (K bird))
    ($set-was-dark (K I))
    ($set-rand (K (list KI KI KI KI KI KI))))
  '((print-stars cont))
  (list 'pitch-dark))

(define-test 'commence "pitch-dark-msg"
  '(($set-location (K bird)))
  '((print-stars cont))
  (list "\nIt is now pitch dark.  If you proceed you will most likely fall into a pit.\n"
        'get-user-input))

(define-test 'commence "longdesc"
  '(($set-location (K house)))
  '((print-stars cont))
  (list "\nYou are inside a building, a well house for a large spring.\n"
        'describe-objects))

(define-test 'commence "shortdesc"
  '(($set-location (K house))
    ($set-nth set-visits house (K (cons1 V))))
  '((print-stars cont))
  (list "\nYou're inside building.\n"
        'describe-objects))

(define-test 'commence "bear"
  '(($set-location (K house))
    ($carry BEAR))
  '((print-stars cont))
  (list "You are being followed by a very large, tame bear.\n"
        "\nYou are inside a building, a well house for a large spring.\n"
        'describe-objects))

(define-test 'commence "forced-move"
  '(($set-location (K crack)))
  '((print-stars cont))
  (list "\nThe crack is far too small for you to follow.\n"
        'try-move))

(define-test 'describe-objects "count-visits"
  '()
  '((print-stars (cons1-length (nth initial-location $visits)))
    (print-stars cont))
  (list 4
        'get-user-input))

(define-test 'describe-objects "count-visits2"
  '(($set-nth set-visits initial-location (K (cons1 (cons1 V)))))
  '((print-stars (cons1-length (nth initial-location $visits)))
    (print-stars cont))
  (list 1
        'get-user-input))

(define-test 'describe-objects "describe"
  '(($set-location (K house))
    ($set-prop-of LAMP (K c1)))
  '((print-stars cont))
  (list "There are some keys on the ground here.\n"
        "There is a lamp shining nearby.\n"
        "There is food here.\n"
        "There is a bottle of water here.\n"
        'get-user-input))

(define-test 'describe-objects "based"
  '(($set-location (K inside)))
  '((print-stars cont))
  (list "The grate is locked.\n"
        'get-user-input))

(define-test 'describe-objects "treads-gold"
  '(($set-location (K spit))
    ($carry GOLD))
  '((print-stars cont))
  (list 'get-user-input))

(define-test 'describe-objects "treads-spit"
  '(($set-location (K spit)))
  '((print-stars cont))
  (list "Rough stone steps lead down the pit.\n"
        'get-user-input))

(define-test 'describe-objects "treads-emist"
  '(($set-location (K emist)))
  '((print-stars cont))
  (list "Rough stone steps lead up the dome.\n"
        'get-user-input))

(define-test 'describe-objects "treasure"
  '(($set-location (K west)))
  '((print-stars ($prop-of COINS))
    (print-stars $tally))
  (list "There are many coins here!\n"
        0
        14))

(define-test 'describe-objects "zap-the-lamp"
  '(($set-location (K west))
    ($set-tally (K c2))
    ($set-lost-treasures (K c1)))
  '((print-stars ($prop-of COINS))
    (print-stars $tally)
    (print-stars (cons1-length $limit)))
  (list "There are many coins here!\n"
        0
        1
        35))

(define-test 'describe-objects "rug"
  '(($set-location (K scan3)))
  '((print-stars ($prop-of RUG)))
  (list "A huge green fierce dragon bars the way!\n"
        "The dragon is sprawled out on a Persian rug!!\n"
        1))

(define-test 'describe-objects "chain"
  '(($set-location (K barr)))
  '((print-stars ($prop-of CHAIN)))
  (list "There is a ferocious cave bear eying you from the far end of the room!\n"
        "The bear is locked to the wall with a golden chain!\n"
        1))

(define-test 'describe-objects "after-closed"
  '(($set-closed (K I))
    ($set-location (K neend))
    ($drop ROD neend)
    ($set-prop-of ROD (K V)))
  '((print-stars cont)
    (print-bool (churchnum? ($prop-of ROD))))
  (list 'get-user-input
        #f))

(define-test 'get-user-input ""
  '(($set-verb (K TAKE))
    ($set-obj (K LAMP)))
  '((print-stars cont)
    (print-stars $verb)
    (print-stars $oldverb)
    (print-stars $obj)
    (print-stars $oldobj))
  (list 'cycle
        'ABSTAIN
        'ABSTAIN
        'NOTHING
        'LAMP))

(define-test 'pre-parse "increment-turns"
  '(($set-turns (K (cons1 (cons1 V)))))
  '((print-stars cont)
    (print-stars (cons1-length $turns)))
  (list 'clocks-and-lamp
        3))

(defmacro dummy-word (lambda (f) (f I I I)))

(define-test 'pre-parse "say-something"
  '(($set-verb (K SAY))
    ($set-word12 (K (cons dummy-word dummy-word))))
  '((print-stars cont)
    (print-stars $verb))
  (list 'clocks-and-lamp
        'ABSTAIN))

(define-test 'pre-parse "say-nothing"
  '(($set-verb (K SAY))
    ($set-word12 (K (cons dummy-word V))))
  '((print-stars cont)
    (print-stars $verb))
  (list 'transitive
        'SAY))

(define-test 'check-the-lamp "lamp-off"
  '(($set-limit (K (cons1 (cons1 V)))))
  '((print-stars cont)
    (print-stars (cons1-length $limit)))
  (list 'handle-special-inputs
        2))

(define-test 'check-the-lamp "lamp-on"
  '(($set-prop-of LAMP (K c1))
    ($set-limit (K (cons1 (cons1 V)))))
  '((print-stars cont)
    (print-stars (cons1-length $limit)))
  (list 'handle-special-inputs
        1))

(define-test 'check-the-lamp "lamp-extinguish"
  '(($carry LAMP)
    ($set-prop-of LAMP (K c1))
    ($set-limit (K (cons1 V))))
  '((print-stars cont)
    (print-stars ($prop-of LAMP))
    (print-stars (cons1-length $limit))
    )
  (list "Your lamp has run out of power.\n"
        'handle-special-inputs
        0
        0
        ))

(define-test 'check-the-lamp "replace-batteries"
  '(($carry LAMP)
    ($carry BATTERIES)
    ($set-prop-of LAMP (K c1))
    ($set-limit (K (to-cons1 c31))))
  '((print-stars cont)
    (print-stars ($prop-of BATTERIES))
    (print-stars ($place-of BATTERIES))
    (print-stars (cons1-length $limit))
    )
  (list "Your lamp is getting dim.  I'm taking the liberty of replacing\nthe batteries.\n"
        'handle-special-inputs
        1
        'road
        2500
        ))

(define-test 'check-the-lamp "giveup"
  '(($set-limit (K V)))
  '((print-stars cont))
  (list "There's not much point in wandering around out here, and you can't\nexplore the cave without a lamp.  So let's just call it a day.\n"
        'give-up))

(define-test 'check-the-lamp "warn-lamp"
  '(($carry LAMP)
    ($set-limit (K (to-cons1 c30))))
  '((print-stars cont)
    (print-bool $not-warned))
  (list "Your lamp is getting dim.  You'd best start wrapping this up, unless\nyou can find some fresh batteries.  I seem to recall that there's\na vending machine in the maze.  Bring some coins with you.\n"
        'handle-special-inputs
        #f))

(define-test 'check-the-lamp "warn-lamp-out-of-batteries"
  '(($carry LAMP)
    ($set-limit (K (to-cons1 c30)))
    ($set-prop-of BATTERIES (K c1)))
  '((print-stars cont)
    (print-bool $not-warned))
  (list "Your lamp is getting dim, and you're out of spare batteries.  You'd\nbest start wrapping this up.\n"
        'handle-special-inputs
        #f))

(define-test 'check-the-lamp "warn-lamp-batteries-left"
  '(($carry LAMP)
    ($set-limit (K (to-cons1 c30)))
    ($drop BATTERIES house))
  '((print-stars cont)
    (print-bool $not-warned))
  (list "Your lamp is getting dim.  You'd best go back for those batteries.\n"
        'handle-special-inputs
        #f))

(define-test 'handle-special-inputs "wet"
  '(($set-word12 (K (cons (motion-word ENTER)
			  (motion-word STREAM)))))
  '((print-stars cont))
  (list "Your feet are now wet.\n"
        'get-user-input))

(define-test 'handle-special-inputs "not-wet"
  '(($set-word12 (K (cons (motion-word ENTER) (object-word WATER))))
    ($set-location (K hill)))
  '((print-stars cont))
  (list "Where?\n"
        'get-user-input))

(define-test 'handle-special-inputs "enter-house"
  '(($set-word12 (K (cons (motion-word ENTER) (motion-word HOUSE)))))
  '((print-stars cont))
  (list 'shift))

(define-test 'handle-special-inputs "enter"
  '(($set-word12 (K (cons (motion-word ENTER) V))))
  '((print-stars cont))
  (list 'parse-label))

(define-test 'handle-special-inputs "water-plant"
  '(($set-word12 (K (cons (object-word WATER) (object-word PLANT))))
    ($set-location (K wpit)))
  '((print-stars cont)
    (print-bool (verb? (cdr $word12)))
    (print-stars (word-meaning (cdr $word12))))
  (list 'parse-label
        #t
        'POUR))

(define-test 'handle-special-inputs "normal"
  '(($set-word12 (K (cons (object-word WATER) (object-word PLANT)))))
  '((print-stars cont)
    (print-bool (noun? (cdr $word12)))
    (print-stars (word-meaning (cdr $word12))))
  (list 'parse-label
        #t
        'PLANT))

(define-test 'shift ""
  '(($set-word12 (K (cons (action-word EAT) (object-word FOOD)))))
  '((print-stars cont)
    (print-bool (noun? (car $word12)))
    (print-stars (word-meaning (car $word12)))
    (print-bool (word? (cdr $word12))))
  (list 'parse-label
        #t
        'FOOD
        #f))

(define-test 'look-at-word1 "unknown-word"
  '(($set-word12 (K (cons V V))))
  '((print-stars cont))
  (list "I don't know that word.\n"
        'cycle))

(define-test 'look-at-word1 "motion"
  '(($set-word12 (K (cons (motion-word NE) V))))
  '((print-stars cont)
    (print-stars $mot))
  (list 'try-move
        'NE))

(define-test 'look-at-word1 "object-here"
  '(($set-location (K house))
    ($set-word12 (K (cons (object-word LAMP) V))))
  '((print-stars cont)
    (print-stars $obj))
  (list 'handle-object-word
        'LAMP))

(define-test 'look-at-word1 "second-object-here"
  '(($set-location (K inside))
    ($set-word12 (K (cons (object-word GRATE) V))))
  '((print-stars cont)
    (print-stars $obj))
  (list 'handle-object-word
        'GRATE))

(define-test 'look-at-word1 "object-not-here"
  '(($set-word12 (K (cons (object-word LAMP) V))))
  '((print-stars cont)
    (print-stars $obj))
  (list 'check-object-location
        'LAMP))

(define-test 'look-at-word1 "verb-with-word2"
  '(($set-word12 (K (cons (action-word DROP) (object-word LAMP)))))
  '((print-stars cont)
    (print-stars $verb))
  (list 'shift
        'DROP))

(define-test 'look-at-word1 "verb-transitive"
  '(($set-word12 (K (cons (action-word DROP) V)))
    ($set-obj (K LAMP)))
  '((print-stars cont)
    (print-stars $verb))
  (list 'transitive
        'DROP))

(define-test 'look-at-word1 "verb-intransitive"
  '(($set-word12 (K (cons (action-word DROP) V)))
    ($set-obj (K NOTHING)))
  '((print-stars cont)
    (print-stars $verb))
  (list 'intransitive
        'DROP))

(define-test 'look-at-word1 "message-word"
  '(($set-word12 (K (cons (message-word c4) V))))  ; LOST
  '((print-stars cont))
  (list "I'm as confused as you are.\n"
        'get-user-input))

(define-test 'handle-object-word "shift"
  '(($set-word12 (K (cons V (action-word TAKE)))))
  '((print-stars cont))
  (list 'shift))

(define-test 'handle-object-word "with-verb"
  '(($set-word12 (K (cons V V)))
    ($set-verb (K TAKE)))
  '((print-stars cont))
  (list 'transitive))

(defmacro (action-word-with-str meaning str)
  (lambda (f) (f (lambda (_ _ x _) x) meaning str)))
(defmacro (object-word-with-str meaning str)
  (lambda (f) (f (lambda (_ x _ _) x) meaning str)))

(define-test 'handle-object-word "without-verb"
  '(($set-word12 (K (cons (object-word-with-str LAMP (string "lamp")) V)))
    ($set-verb (K ABSTAIN)))
  '((print-stars cont))
  (list "What do you want to do with the lamp?\n"
        'cycle))

(define-test 'cant-see-it ""
  '(($set-word12 (K (cons (object-word-with-str LAMP (string "lamp")) V))))
  '((print-stars cont))
  (list "I see no lamp here.\n"
        'get-user-input))

(define-test 'cant-see-it "find"
  '(($set-verb (K FIND))
    ($set-word12 (K (icons V V))))
  '((print-stars cont))
  (list 'transitive))

(define-test 'try-move "go"
  '(($set-location (K forest))
    ($set-oldlocs (K (cons hill house)))
    ($set-mot (K SOUTH)))
  '((print-stars cont)
    (print-stars $newloc)
    (print-stars (car $oldlocs))
    (print-stars (cdr $oldlocs)))
  (list 'go-for-it
        'forest
        'forest
        'hill))

(define-test 'try-move "nowhere"
  '(($set-location (K forest))
    ($set-mot (K NOWHERE)))
  '((print-stars cont)
    (print-stars $newloc))
  (list 'mainloop
        'forest))

(define-test 'try-move "look"
  '(($set-location (K forest))
    ($set-mot (K LOOK))
    ($set-was-dark (K I))
    ($set-nth set-visits forest (K (cons1 V))))
  '((print-stars cont)
    (print-bool $was-dark)
    (print-stars (cons1-length (nth forest $visits))))
  (list "Sorry, but I am not allowed to give more detail.  I will repeat the\nlong description of your location.\n"
        'mainloop
        #f
        0))

(define-test 'try-move "look-quiet"
  '(($set-verbose (K (cons1 V)))
    ($set-location (K forest))
    ($set-mot (K LOOK))
    ($set-was-dark (K I))
    ($set-nth set-visits forest (K (cons1 V))))
  '((print-stars cont)
    (print-bool $was-dark)
    (print-stars (cons1-length (nth forest $visits))))
  (list 'mainloop
        #f
        0))

(define-test 'try-move "cave"
  '(($set-location (K forest))
    ($set-mot (K CAVE)))
  '((print-stars cont))
  (list "I can't see where the cave is, but hereabouts no stream can run on\nthe surface for long.  I would try the stream.\n"
        'mainloop))

(define-test 'try-move "cave2"
  '(($set-location (K inside))
    ($set-mot (K CAVE)))
  '((print-stars cont))
  (list "I need more detailed instructions to do that.\n"
        'mainloop))

(define-test 'go-back "cannot-back"
  '(($set-location (K road))
    ($set-oldlocs (K (cons road road))))
  '((print-stars cont))
  (list "Sorry, but I no longer seem to remember how you got here.\n"
        'mainloop))

(define-test 'go-back "cannot-back-forced"
  '(($set-location (K spit))
    ($set-oldlocs (K (cons crack spit))))
  '((print-stars cont)
    (print-stars (car $oldlocs))
    (print-stars (cdr $oldlocs)))
  (list "Sorry, but I no longer seem to remember how you got here.\n"
        'mainloop
        'spit
        'crack))

(define-test 'go-back "back"
  '(($set-location (K hill))
    ($set-oldlocs (K (cons road house))))
  '((print-stars cont)
    (print-stars $mot)
    (print-stars (car $oldlocs))
    (print-stars (cdr $oldlocs)))
  (list 'go-for-it
        'ROAD
        'hill
        'road))

(define-test 'go-back "back-forced"
  '(($set-location (K hill))
    ($set-oldlocs (K (cons crack road))))
  '((print-stars cont)
    (print-stars $mot)
    (print-stars (car $oldlocs))
    (print-stars (cdr $oldlocs)))
  (list 'go-for-it
        'ROAD
        'hill
        'crack))

(define-test 'go-back "no-way"
  '(($set-location (K cobbles))
    ($set-oldlocs (K (cons house road))))
  '((print-stars cont)
    (print-stars (car $oldlocs))
    (print-stars (cdr $oldlocs)))
  (list "You can't get there from here.\n"
        'mainloop
        'cobbles
        'house))

(define-test 'go-for-it "go"
  '(($set-location (K road))
    ($set-mot (K ENTER)))
  '((print-stars cont)
    (print-stars $newloc))
  (list 'mainloop
        'house))

(define-test 'go-for-it "crawl"
  '(($set-location (K road))
    ($set-mot (K CRAWL)))
  '((print-stars cont))
  (list "Which way?\n"
        'mainloop))

(define-test 'go-for-it "xyzzy"
  '(($set-location (K road))
    ($set-mot (K XYZZY)))
  '((print-stars cont))
  (list "Nothing happens.\n"
        'mainloop))

(define-test 'go-for-it "find"
  '(($set-location (K road))
    ($set-mot (K SLIT))
    ($set-verb (K FIND)))
  '((print-stars cont))
  (list "I can only tell you what you see as you move about and manipulate\nthings.  I cannot tell you where remote things are.\n"
        'mainloop))

(define-test 'go-for-it "out"
  '(($set-location (K road))
    ($set-mot (K OUT)))
  '((print-stars cont))
  (list "I don't know in from out here.  Use compass points or name something\nin the general direction you want to go.\n"
        'mainloop))

(define-test 'go-for-it "right"
  '(($set-location (K road))
    ($set-mot (K R)))
  '((print-stars cont))
  (list "I am unsure how you are facing.  Use compass points or nearby objects.\n"
        'mainloop))

(define-test 'go-for-it "ne"
  '(($set-location (K road))
    ($set-mot (K NE)))
  '((print-stars cont))
  (list "There is no way to go in that direction.\n"
        'mainloop))

(define-test 'go-for-it "location-word"
  '(($set-location (K road))
    ($set-mot (K SLIT)))
  '((print-stars cont))
  (list "I don't know how to apply that word here.\n"
        'mainloop))

(define-test 'go-for-it "remark"
  '(($set-location (K slit))
    ($set-mot (K D)))
  '((print-stars cont)
    (print-stars $newloc))
  (list "You don't fit through a two-inch slit!\n"
        'mainloop
        'slit))

(define-test 'go-for-it "random-pass"
  '(($set-location (K witt))
    ($set-mot (K SOUTH))
    ($set-rand (K (list K K K K K K))))
  '((print-stars cont)
    (print-stars $newloc))
  (list 'mainloop
        'ante))

(define-test 'go-for-it "random-not-pass"
  '(($set-location (K witt))
    ($set-mot (K SOUTH))
    ($set-rand (K (list KI KI KI KI KI KI))))
  '((print-stars cont)
    (print-stars $newloc))
  (list "You have crawled around in some little holes and wound up back in the\nmain passage.\n"
        'mainloop
        'witt))

(define-test 'go-for-it "property-ok"
  '(($set-location (K outside))
    ($set-prop-of GRATE (K c1))
    ($set-mot (K ENTER)))
  '((print-stars cont)
    (print-stars $newloc))
  (list 'mainloop
        'inside))

(define-test 'go-for-it "property-ng"
  '(($set-location (K outside))
    ($set-mot (K ENTER)))
  '((print-stars cont)
    (print-stars $newloc))
  (list "You can't go through a locked steel grate!\n"
        'mainloop
        'outside))

(define-test 'go-for-it "holds-true"
  '(($set-location (K shell))
    ($carry CLAM)
    ($set-mot (K SOUTH)))
  '((print-stars cont)
    (print-stars $newloc))
  (list "You can't fit this five-foot clam through that little passage!\n"
        'mainloop
        'shell))

(define-test 'go-for-it "holds-false"
  '(($set-location (K shell))
    ($set-mot (K SOUTH)))
  '((print-stars cont)
    (print-stars $newloc))
  (list 'mainloop
        'complex))

(define-test 'go-for-it "ppass"
  '(($set-location (K alcove))
    ($set-mot (K PASSAGE)))
  '((print-stars cont)
    (print-stars $newloc))
  (list 'mainloop
        'proom))

(define-test 'go-for-it "ppass-proom"
  '(($set-location (K proom))
    ($set-mot (K PASSAGE))
    ($carry EMERALD))
  '((print-stars cont)
    (print-stars $newloc))
  (list 'mainloop
        'alcove))

(define-test 'go-for-it "ppass-fail"
  '(($set-location (K alcove))
    ($set-mot (K PASSAGE))
    ($carry LAMP))
  '((print-stars cont)
    (print-stars $newloc))
  (list "Something you're carrying won't fit through the tunnel with you.\nYou'd best take inventory and drop something.\n"
        'mainloop
        'alcove))

(define-test 'go-for-it "pdrop"
  '(($set-location (K y2))
    ($set-mot (K PLOVER))
    ($carry EMERALD))
  '((print-stars cont)
    (print-stars $newloc)
    (print-stars ($place-of EMERALD)))
  (list 'mainloop
        'proom
        'y2))

(define-test 'report-default ""
  '(($set-verb (K DROP)))
  '((print-stars cont))
  (list "You aren't carrying it!\n"
        'get-user-input))

(define-test 'get-object ""
  '(($set-word12 (K (cons (action-word-with-str TOSS (string "throw")) V))))
  '((print-stars cont))
  (list "throw what?\n"
        'cycle))

(define-test 'check-object-location "default"
  '(($set-obj (K KEYS)))
  '((print-stars cont))
  (list 'cant-see-it))

(define-test 'check-object-location "grate-at-road"
  '(($set-obj (K GRATE)))
  '((print-stars cont)
    (print-stars $mot))
  (list 'try-move
        'DEPRESSION))

(define-test 'check-object-location "grate-at-spit"
  '(($set-obj (K GRATE))
    ($set-location (K spit)))
  '((print-stars cont)
    (print-stars $mot))
  (list 'try-move
        'ENTRANCE))

(define-test 'check-object-location "plant2"
  '(($set-obj (K PLANT))
    ($set-location (K e2pit))
    ($set-prop-of PLANT2 (K c1)))
  '((print-stars cont)
    (print-stars $obj))
  (list 'handle-object-word
        'PLANT2))

(define-test 'check-object-location "rod2"
  '(($set-obj (K ROD))
    ($carry ROD2))
  '((print-stars cont)
    (print-stars $obj))
  (list 'handle-object-word
        'ROD2))

(define-test 'check-object-location "water-in-bottle"
  '(($set-obj (K WATER))
    ($carry BOTTLE))
  '((print-stars cont))
  (list 'handle-object-word))

(define-test 'check-object-location "oil-here"
  '(($set-obj (K OIL))
    ($set-location (K epit)))
  '((print-stars cont))
  (list 'handle-object-word))

(define-test 'intransitive-take "no-object"
  '(($set-location (K road)))
  '((print-stars cont))
  (list 'get-object))

(define-test 'intransitive-take "one-object"
  '(($set-location (K west)))
  '((print-stars cont)
    (print-stars $obj))
  (list 'transitive
        'COINS))

(define-test 'intransitive-take "many-objects"
  '(($set-location (K house)))
  '((print-stars cont))
  (list 'get-object))

(define-test 'intransitive-eat "no-food"
  '(($set-location (K road)))
  '((print-stars cont))
  (list 'get-object))

(define-test 'intransitive-eat "food-here"
  '(($set-location (K house)))
  '((print-stars cont)
    (print-stars $obj))
  (list 'transitive
        'FOOD))

(define-test 'intransitive-open "grate"
  '(($set-location (K outside)))
  '((print-stars cont)
    (print-stars $obj))
  (list 'transitive
        'GRATE))

(define-test 'intransitive-open "chain"
  '(($carry CHAIN))
  '((print-stars cont)
    (print-stars $obj))
  (list 'transitive
        'CHAIN))

(define-test 'intransitive-open "grate-chain"
  '(($set-location (K outside))
    ($carry CHAIN))
  '((print-stars cont))
  (list 'get-object))

(define-test 'intransitive-open "nothing"
  '(($set-location (K road)))
  '((print-stars cont))
  (list "There is nothing here with a lock!\n"
        'get-user-input))

(define-test 'intransitive-read "nothing"
  '()
  '((print-stars cont))
  (list 'get-object))

(define-test 'intransitive-read "mag"
  '(($carry MAG))
  '((print-stars cont)
    (print-stars $obj))
  (list 'transitive
        'MAG))

(define-test 'intransitive-read "mag-and-tablet"
  '(($carry MAG)
    ($carry TABLET))
  '((print-stars cont)
    (print-stars $obj))
  (list 'get-object
        'MAG))

(define-test 'intransitive-read "tablet-and-message"
  '(($carry MESSAGE)
    ($carry TABLET)
    ($carry OYSTER))
  '((print-stars cont)
    (print-stars $obj))
  (list 'get-object
        'TABLET))

(define-test 'intransitive-read "oyster-closed"
  '(($set-closed (K I))
    ($carry TABLET)
    ($carry OYSTER))
  '((print-stars cont)
    (print-stars $obj))
  (list 'transitive
        'OYSTER))

(define-test 'intransitive-read "dark"
  '(($set-location (K ante)))
  '((print-stars cont))
  (list 'get-object))

(define-test 'intransitive-inventory "nothing"
  '()
  '((print-stars cont))
  (list "You're not carrying anything.\n"
        'get-user-input))

(define-test 'intransitive-inventory "objects"
  '(($carry KEYS)
    ($carry LAMP))
  '((print-stars cont))
  (list "You are currently holding the following:\n"
        " Set of keys\n"
        " Brass lantern\n"
        'get-user-input))

(define-test 'intransitive-inventory "bear"
  '(($carry BEAR))
  '((print-stars cont))
  (list "You are being followed by a very large, tame bear.\n"
        'get-user-input))

(define-test 'intransitive-inventory "object-and-bear"
  '(($carry KEYS)
    ($carry BEAR))
  '((print-stars cont))
  (list "You are currently holding the following:\n"
        " Set of keys\n"
        "You are being followed by a very large, tame bear.\n"
        'get-user-input))

(define-test 'intransitive-brief ""
  '()
  '((print-stars cont)
    (print-stars (cons1-length $verbose)))
  (list "Okay, from now on I'll only describe a place in full the first time\nyou come to it.  To get the full description, say \"LOOK\".\n"
        'get-user-input
        0))

(define-input-test 'intransitive-score "yes"
  '()
  "yes\n"
  '((print-stars cont))
  (list "If you were to quit now, you would score 32\nout of a possible 350.\n"
        "Do you indeed wish to quit now?"
        yesno-prompt
        "OK.\n"
        'give-up))

(define-input-test 'intransitive-score "no"
  '()
  "no\n"
  '((print-stars cont))
  (list "If you were to quit now, you would score 32\nout of a possible 350.\n"
        "Do you indeed wish to quit now?"
        yesno-prompt
        "OK.\n"
        'get-user-input))

(define-input-test 'intransitive-score "bonus"
  '((set-nth world set-hinted c9 (K c10)))
  "yes\n"
  '((print-stars cont))
  (list "If you were to quit now, you would score 42\nout of a possible 350.\n"
        "Do you indeed wish to quit now?"
        yesno-prompt
        "OK.\n"
        'give-up))

(define-input-test 'intransitive-score "death"
  '(($set-death-count (K c1)))
  "yes\n"
  '((print-stars cont))
  (list "If you were to quit now, you would score 22\nout of a possible 350.\n"
        "Do you indeed wish to quit now?"
        yesno-prompt
        "OK.\n"
        'give-up))

(define-input-test 'intransitive-score "witt"
  '(($drop MAG witt))
  "yes\n"
  '((print-stars cont))
  (list "If you were to quit now, you would score 33\nout of a possible 350.\n"
        "Do you indeed wish to quit now?"
        yesno-prompt
        "OK.\n"
        'give-up))

(define-input-test 'intransitive-score "found-gold"
  '(($set-prop-of GOLD (K c0)))
  "yes\n"
  '((print-stars cont))
  (list "If you were to quit now, you would score 34\nout of a possible 350.\n"
        "Do you indeed wish to quit now?"
        yesno-prompt
        "OK.\n"
        'give-up))

(define-input-test 'intransitive-score "got-gold"
  '(($set-prop-of GOLD (K c0))
    ($drop GOLD house))
  "yes\n"
  '((print-stars cont))
  (list "If you were to quit now, you would score 44\nout of a possible 350.\n"
        "Do you indeed wish to quit now?"
        yesno-prompt
        "OK.\n"
        'give-up))

(define-input-test 'intransitive-score "got-chest"
  '(($set-prop-of CHEST (K c0))
    ($drop CHEST house))
  "yes\n"
  '((print-stars cont))
  (list "If you were to quit now, you would score 46\nout of a possible 350.\n"
        "Do you indeed wish to quit now?"
        yesno-prompt
        "OK.\n"
        'give-up))

(define-input-test 'intransitive-score "got-chain"
  '(($set-prop-of CHAIN (K c0))
    ($drop CHAIN house))
  "yes\n"
  '((print-stars cont))
  (list "If you were to quit now, you would score 48\nout of a possible 350.\n"
        "Do you indeed wish to quit now?"
        yesno-prompt
        "OK.\n"
        'give-up))

(define-input-test 'intransitive-quit "yes"
  '()
  "yes\n"
  '((print-stars cont))
  (list "Do you really wish to quit now?"
        yesno-prompt
        "OK.\n"
        'give-up))

(define-input-test 'intransitive-quit "no"
  '()
  "no\n"
  '((print-stars cont))
  (list "Do you really wish to quit now?"
        yesno-prompt
        "OK.\n"
        'get-user-input))

(define-test 'give-up ""
  '()
  '((print-stars cont)
    (print-stars (nth c8 $hinted)))
  (list 'quit
        0))

(define-test 'transitive-eat "food"
  '(($set-obj (K FOOD)))
  '((print-stars cont))
  (list "Thank you, it was delicious!\n"
        'get-user-input))

(define-test 'transitive-eat "snake"
  '(($set-obj (K SNAKE)))
  '((print-stars cont))
  (list "I think I just lost my appetite.\n"
        'get-user-input))

(define-test 'transitive-eat "other"
  '(($set-obj (K KEYS)))
  '((print-stars cont))
  (list 'report-default))

(define-test 'transitive-on "no-lamp"
  '()
  '((print-stars cont))
  (list 'report-default))

(define-test 'transitive-on "out-of-power"
  '(($carry LAMP)
    ($set-limit (K V)))
  '((print-stars cont)
    (print-stars ($prop-of LAMP)))
  (list "Your lamp has run out of power.\n"
        'get-user-input
        0))

(define-test 'transitive-on "was-dark"
  '(($carry LAMP)
    ($set-was-dark (K I)))
  '((print-stars cont)
    (print-stars ($prop-of LAMP)))
  (list "Your lamp is now on.\n"
        'commence
        1))

(define-test 'transitive-on "not-was-dark"
  '(($carry LAMP)
    ($set-was-dark (K V)))
  '((print-stars cont)
    (print-stars ($prop-of LAMP)))
  (list "Your lamp is now on.\n"
        'get-user-input
        1))

(define-test 'transitive-off "no-lamp"
  '()
  '((print-stars cont))
  (list 'report-default))

(define-test 'transitive-off "not-dark"
  '(($carry LAMP))
  '((print-stars cont)
    (print-stars ($prop-of LAMP)))
  (list "Your lamp is now off.\n"
        'get-user-input
        0))

(define-test 'transitive-off "dark"
  '(($carry LAMP)
    ($set-location (K debris)))
  '((print-stars cont)
    (print-stars ($prop-of LAMP)))
  (list "Your lamp is now off.\n"
        "It is now pitch dark.  If you proceed you will most likely fall into a pit.\n"
        'get-user-input
        0))

(define-test 'transitive-take "carrying"
  '(($carry KEYS)
    ($set-obj (K KEYS)))
  '((print-stars cont))
  (list 'report-default))

(define-test 'transitive-take "immovable"
  '(($set-obj (K GRATE)))
  '((print-stars cont))
  (list "You can't be serious!\n"
        'get-user-input))

(define-test 'transitive-take "immovable-chain"
  '(($set-obj (K CHAIN))
    ($set-prop-of BEAR (K c1)))
  '((print-stars cont))
  (list "The chain is still locked.\n"
        'get-user-input))

(define-test 'transitive-take "immovable-bear"
  '(($set-obj (K BEAR))
    ($set-prop-of BEAR (K c1)))
  '((print-stars cont))
  (list "The bear is still chained to the wall.\n"
        'get-user-input))

(define-test 'transitive-take "immovable-plant"
  '(($set-obj (K PLANT)))
  '((print-stars cont))
  (list "The plant has exceptionally deep roots and cannot be pulled free.\n"
        'get-user-input))

(define-test 'transitive-take "ok"
  '(($set-obj (K LAMP)))
  '((print-stars cont)
    (print-bool ($toting? LAMP)))
  (list "OK.\n"
        'get-user-input
        #t))

(define-test 'transitive-take "toomany"
  '(($carry KEYS)
    ($carry LAMP)
    ($carry CAGE)
    ($carry ROD)
    ($carry BIRD)
    ($carry FOOD)
    ($carry AXE)
    ($set-obj (K MAG)))
  '((print-stars cont))
  (list "You can't carry anything more.  You'll have to drop something first.\n"
        'get-user-input))

(define-test 'transitive-take "do-not-count-water-in-bottle"
  '(($carry KEYS)
    ($carry LAMP)
    ($carry CAGE)
    ($carry ROD)
    ($carry BIRD)
    ($carry BOTTLE)
    ($carry WATER)
    ($set-obj (K MAG)))
  '((print-stars cont)
    (print-bool ($toting? MAG)))
  (list "OK.\n"
        'get-user-input
        #t))

(define-test 'transitive-take "water-in-bottle"
  '(($set-location (K house))
    ($set-obj (K WATER)))
  '((print-stars cont)
    (print-stars $obj)
    (print-bool ($toting? BOTTLE)))
  (list "OK.\n"
        'get-user-input
        'BOTTLE
        #t))

(define-test 'transitive-take "change-to-fill"
  '(($carry BOTTLE)
    ($set-prop-of BOTTLE (K c1))
    ($set-obj (K WATER))
    ($set-verb (K TAKE)))
  '((print-stars cont)
    (print-stars $obj)
    (print-stars $verb)
    (print-stars $oldverb))
  (list 'transitive
        'BOTTLE
        'FILL
        'TAKE))

(define-test 'transitive-take "no-bottle"
  '(($set-obj (K WATER)))
  '((print-stars cont))
  (list "You have nothing in which to carry it.\n"
        'get-user-input))

(define-test 'transitive-take "liquid-in-bottle"
  '(($set-location (K house))
    ($set-obj (K BOTTLE)))
  '((print-stars cont)
    (print-bool ($toting? BOTTLE))
    (print-bool ($toting? WATER)))
  (list "OK.\n"
        'get-user-input
        #t
        #t))

(define-test 'transitive-take "bird"
  '(($carry CAGE)
    ($set-obj (K BIRD)))
  '((print-stars cont)
    (print-bool ($toting? BIRD))
    (print-stars ($prop-of BIRD)))
  (list "OK.\n"
        'get-user-input
        #t
        1))

(define-test 'transitive-take "bird-without-cage"
  '(($set-obj (K BIRD)))
  '((print-stars cont)
    (print-bool ($toting? BIRD))
    (print-stars ($prop-of BIRD)))
  (list "You can catch the bird, but you cannot carry it.\n"
        'get-user-input
        #f
        0))

(define-test 'transitive-take "bird-with-rod"
  '(($carry ROD)
    ($set-obj (K BIRD)))
  '((print-stars cont)
    (print-bool ($toting? BIRD))
    (print-stars ($prop-of BIRD)))
  (list "The bird was unafraid when you entered, but as you approach it becomes\ndisturbed and you cannot catch it.\n"
        'get-user-input
        #f
        0))

(define-test 'transitive-take "bird-in-cage"
  '(($set-prop-of BIRD (K c1))
    ($set-obj (K BIRD)))
  '((print-stars cont)
    (print-bool ($toting? BIRD))
    (print-bool ($toting? CAGE)))
  (list "OK.\n"
        'get-user-input
        #t
        #t))

(define-test 'transitive-take "cage-with-bird"
  '(($set-prop-of BIRD (K c1))
    ($set-obj (K CAGE)))
  '((print-stars cont)
    (print-bool ($toting? BIRD))
    (print-bool ($toting? CAGE)))
  (list "OK.\n"
        'get-user-input
        #t
        #t))

(define-test 'transitive-drop "ok"
  '(($carry LAMP)
    ($set-obj (K LAMP)))
  '((print-stars cont)
    (print-stars ($place-of LAMP)))
  (list "OK.\n"
        'get-user-input
        'road))

(define-test 'transitive-drop "not-toting"
  '(($set-obj (K LAMP)))
  '((print-stars cont))
  (list 'report-default))

(define-test 'transitive-drop "rod2"
  '(($carry ROD2)
    ($set-obj (K ROD)))
  '((print-stars cont)
    (print-stars ($place-of ROD2)))
  (list "OK.\n"
        'get-user-input
        'road))

(define-test 'transitive-drop "drop-coins"
  '(($set-location (K pony))
    ($carry COINS)
    ($set-obj (K COINS)))
  '((print-stars cont)
    (print-stars ($place-of COINS))
    (print-stars ($place-of BATTERIES))
    (print-stars ($prop-of BATTERIES)))
  (list "There are fresh batteries here.\n"
        'get-user-input
        'limbo
        'pony
        0))

(define-test 'transitive-drop "drop-bird"
  '(($carry BIRD)
    ($set-obj (K BIRD))
    ($set-prop-of BIRD (K c1)))
  '((print-stars cont)
    (print-stars ($place-of BIRD))
    (print-stars ($prop-of BIRD)))
  (list "OK.\n"
        'get-user-input
        'road
        0))

(define-test 'transitive-drop "drop-bird-at-snake"
  '(($carry BIRD)
    ($set-obj (K BIRD))
    ($set-prop-of BIRD (K c1))
    ($set-location (K hmk)))
  '((print-stars cont)
    (print-stars ($place-of BIRD))
    (print-stars ($prop-of BIRD))
    (print-stars ($place-of SNAKE))
    (print-stars ($prop-of SNAKE)))
  (list "The little bird attacks the green snake, and in an astounding flurry\ndrives the snake away.\n"
        'get-user-input
        'hmk
        0
        'limbo
        1))

(define-test 'transitive-drop "drop-bird-at-snake-after-closed"
  '(($carry BIRD)
    ($set-obj (K BIRD))
    ($set-prop-of BIRD (K c1))
    ($set-location (K hmk))
    ($set-closed (K I)))
  '((print-stars cont))
  (list "The little bird attacks the green snake, and in an astounding flurry\ndrives the snake away.\n"
        'dwarves-upset))

(define-test 'transitive-drop "drop-vase"
  '(($carry VASE)
    ($set-obj (K VASE)))
  '((print-stars cont)
    (print-stars ($place-of VASE))
    (print-stars ($prop-of VASE))
    (print-stars ($base-of VASE)))
  (list "The Ming vase drops with a delicate crash.\n"
        'get-user-input
        'road
        2
        'VASE))

(define-test 'transitive-drop "drop-vase-on-pillow"
  '(($carry VASE)
    ($set-obj (K VASE))
    ($drop PILLOW road))
  '((print-stars cont)
    (print-stars ($place-of VASE))
    (print-stars ($prop-of VASE))
    (print-stars ($base-of VASE)))
  (list "The vase is now resting, delicately, on a velvet pillow.\n"
        'get-user-input
        'road
        0
        'NOTHING))

(define-test 'transitive-drop "drop-bear"
  '(($carry BEAR)
    ($set-obj (K BEAR))
    ($set-location (K neside)))
  '((print-stars cont)
    (print-stars ($place-of BEAR))
    (print-stars ($place-of TROLL))
    (print-stars ($place-of TROLL_))
    (print-stars ($place-of TROLL2))
    (print-stars ($place-of TROLL2_))
    (print-stars ($prop-of TROLL)))
  (list "The bear lumbers toward the troll, who lets out a startled shriek and\nscurries away.  The bear soon gives up the pursuit and wanders back.\n"
        'get-user-input
        'neside
        'limbo
        'limbo
        'swside
        'neside
        2))

(define-test 'transitive-drop "drop-water-in-bottle"
  '(($carry BOTTLE)
    ($carry WATER)
    ($set-obj (K WATER)))
  '((print-stars cont)
    (print-stars ($place-of BOTTLE))
    (print-stars ($place-of WATER)))
  (list "OK.\n"
        'get-user-input
        'road
        'limbo))

(define-test 'transitive-drop "drop-oil-in-bottle"
  '(($carry BOTTLE)
    ($set-prop-of BOTTLE (K c2))
    ($carry OIL)
    ($set-obj (K BOTTLE)))
  '((print-stars cont)
    (print-stars ($place-of BOTTLE))
    (print-stars ($place-of OIL)))
  (list "OK.\n"
        'get-user-input
        'road
        'limbo))

(define-test 'transitive-drop "cage-with-bird"
  '(($carry CAGE)
    ($carry BIRD)
    ($set-prop-of BIRD (K c1))
    ($set-obj (K CAGE)))
  '((print-stars cont)
    (print-stars ($place-of CAGE))
    (print-stars ($place-of BIRD)))
  (list "OK.\n"
        'get-user-input
        'road
        'road))

(define-test 'transitive-open "default"
  '(($set-verb (K OPEN))
    ($set-obj (K LAMP)))
  '((print-stars cont))
  (list 'report-default))

(define-test 'transitive-open "keys"
  '(($set-verb (K OPEN))
    ($set-obj (K KEYS)))
  '((print-stars cont))
  (list "You can't lock or unlock the keys.\n"
        'get-user-input))

(define-test 'transitive-open "cage"
  '(($set-verb (K OPEN))
    ($set-obj (K CAGE)))
  '((print-stars cont))
  (list "It has no lock.\n"
        'get-user-input))

(define-test 'transitive-open "door-ok"
  '(($set-verb (K OPEN))
    ($set-obj (K DOOR))
    ($set-prop-of DOOR (K c1)))
  '((print-stars cont))
  (list "OK.\n"
        'get-user-input))

(define-test 'transitive-open "door-fail"
  '(($set-verb (K OPEN))
    ($set-obj (K DOOR))
    ($set-prop-of DOOR (K c0)))
  '((print-stars cont))
  (list "The door is extremely rusty and refuses to open.\n"
        'get-user-input))

(define-test 'transitive-open "open-grate-without-keys"
  '(($set-verb (K OPEN))
    ($set-obj (K GRATE)))
  '((print-stars cont))
  (list "You have no keys!\n"
        'get-user-input))

(define-test 'transitive-open "close-locked-grate"
  '(($carry KEYS)
    ($set-prop-of GRATE (K c0))
    ($set-verb (K CLOSE))
    ($set-obj (K GRATE)))
  '((print-stars cont)
    (print-stars ($prop-of GRATE)))
  (list "It was already locked.\n"
        'get-user-input
        0))

(define-test 'transitive-open "close-unlocked-grate"
  '(($carry KEYS)
    ($set-prop-of GRATE (K c1))
    ($set-verb (K CLOSE))
    ($set-obj (K GRATE)))
  '((print-stars cont)
    (print-stars ($prop-of GRATE)))
  (list "The grate is now locked.\n"
        'get-user-input
        0))

(define-test 'transitive-open "open-locked-grate"
  '(($carry KEYS)
    ($set-prop-of GRATE (K c0))
    ($set-verb (K OPEN))
    ($set-obj (K GRATE)))
  '((print-stars cont)
    (print-stars ($prop-of GRATE)))
  (list "The grate is now unlocked.\n"
        'get-user-input
        1))

(define-test 'transitive-open "open-unlocked-grate"
  '(($carry KEYS)
    ($set-prop-of GRATE (K c1))
    ($set-verb (K OPEN))
    ($set-obj (K GRATE)))
  '((print-stars cont)
    (print-stars ($prop-of GRATE)))
  (list "It was already unlocked.\n"
        'get-user-input
        1))

(define-test 'transitive-open "open-chain-without-keys"
  '(($set-verb (K OPEN))
    ($set-obj (K CHAIN)))
  '((print-stars cont))
  (list "You have no keys!\n"
        'get-user-input))

(define-test 'transitive-open "close-chain-outside-barr"
  '(($carry KEYS)
    ($set-verb (K CLOSE))
    ($set-obj (K CHAIN)))
  '((print-stars cont))
  (list "There is nothing here to which the chain can be locked.\n"
        'get-user-input))

(define-test 'transitive-open "close-locked-chain"
  '(($carry KEYS)
    ($set-location (K barr))
    ($set-prop-of CHAIN (K c2))
    ($set-verb (K CLOSE))
    ($set-obj (K CHAIN)))
  '((print-stars cont))
  (list "It was already locked.\n"
        'get-user-input))

(define-test 'transitive-open "close-chain"
  '(($carry KEYS)
    ($set-location (K barr))
    ($set-prop-of CHAIN (K c0))
    ($set-base-of CHAIN (K NOTHING))
    ($carry CHAIN)
    ($set-verb (K CLOSE))
    ($set-obj (K CHAIN)))
  '((print-stars cont)
    (print-stars ($prop-of CHAIN))
    (print-stars ($base-of CHAIN))
    (print-stars ($place-of CHAIN)))
  (list "The chain is now locked.\n"
        'get-user-input
        2
        'CHAIN
        'barr))

(define-test 'transitive-open "open-unlocked-chain"
  '(($carry KEYS)
    ($set-prop-of CHAIN (K c0))
    ($set-verb (K OPEN))
    ($set-obj (K CHAIN)))
  '((print-stars cont))
  (list "It was already unlocked.\n"
        'get-user-input))

(define-test 'transitive-open "open-chain-bear"
  '(($carry KEYS)
    ($set-prop-of CHAIN (K c1))
    ($set-prop-of BEAR (K c0))
    ($set-verb (K OPEN))
    ($set-obj (K CHAIN)))
  '((print-stars cont))
  (list "There is no way to get past the bear to unlock the chain, which is\nprobably just as well.\n"
        'get-user-input))

(define-test 'transitive-open "open-chain"
  '(($carry KEYS)
    ($set-prop-of CHAIN (K c1))
    ($set-prop-of BEAR (K c1))
    ($set-verb (K OPEN))
    ($set-obj (K CHAIN)))
  '((print-stars cont)
    (print-stars ($prop-of CHAIN))
    (print-stars ($base-of CHAIN))
    (print-stars ($prop-of BEAR))
    (print-stars ($base-of BEAR)))
  (list "The chain is now unlocked.\n"
        'get-user-input
        0
        'NOTHING
        2
        'NOTHING))

(define-test 'transitive-open "open-chain-bear-dead"
  '(($carry KEYS)
    ($set-prop-of CHAIN (K c2))
    ($set-prop-of BEAR (K c3))
    ($set-verb (K OPEN))
    ($set-obj (K CHAIN)))
  '((print-stars cont)
    (print-stars ($prop-of CHAIN))
    (print-stars ($base-of CHAIN))
    (print-stars ($prop-of BEAR))
    (print-stars ($base-of BEAR)))
  (list "The chain is now unlocked.\n"
        'get-user-input
        0
        'NOTHING
        3
        'BEAR))

(define-test 'transitive-open "close-clam"
  '(($set-verb (K CLOSE))
    ($set-obj (K CLAM)))
  '((print-stars cont))
  (list "What?\n"
        'get-user-input))

(define-test 'transitive-open "open-clam-without-trident"
  '(($set-verb (K OPEN))
    ($set-obj (K CLAM)))
  '((print-stars cont))
  (list "You don't have anything strong enough to open the clam.\n"
        'get-user-input))

(define-test 'transitive-open "open-clam-in-hand"
  '(($set-verb (K OPEN))
    ($set-obj (K CLAM))
    ($carry TRIDENT)
    ($carry CLAM))
  '((print-stars cont))
  (list "I advise you to put down the clam before opening it.  >STRAIN!<\n"
        'get-user-input))

(define-test 'transitive-open "open-oyster-in-hand"
  '(($set-verb (K OPEN))
    ($set-obj (K OYSTER))
    ($carry TRIDENT)
    ($carry OYSTER))
  '((print-stars cont))
  (list "I advise you to put down the oyster before opening it.  >WRENCH!<\n"
        'get-user-input))

(define-test 'transitive-open "open-clam"
  '(($set-verb (K OPEN))
    ($set-obj (K CLAM))
    ($carry TRIDENT))
  '((print-stars cont)
    (print-stars ($place-of CLAM))
    (print-stars ($place-of OYSTER))
    (print-stars ($place-of PEARL)))
  (list "A glistening pearl falls out of the clam and rolls away.  Goodness,\nthis must really be an oyster.  (I never was very good at identifying\nbivalves.)  Whatever it is, it has now snapped shut again.\n"
        'get-user-input
        'limbo
        'road
        'sac))

(define-test 'transitive-open "open-oyster"
  '(($set-verb (K OPEN))
    ($set-obj (K OYSTER))
    ($carry TRIDENT))
  '((print-stars cont))
  (list "The oyster creaks open, revealing nothing but oyster inside.\nIt promptly snaps shut again.\n"
        'get-user-input))

(define-test 'transitive-read "dark"
  '(($set-location (K debris))
    ($set-obj (K MAG)))
  '((print-stars cont))
  (list 'cant-see-it))

(define-test 'transitive-read "magazine"
  '(($set-obj (K MAG)))
  '((print-stars cont))
  (list "I'm afraid the magazine is written in dwarvish.\n"
        'get-user-input))

(define-test 'transitive-read "tablet"
  '(($set-obj (K TABLET)))
  '((print-stars cont))
  (list "\"CONGRATULATIONS ON BRINGING LIGHT INTO THE DARK-ROOM!\"\n"
        'get-user-input))

(define-test 'transitive-read "message"
  '(($set-obj (K MESSAGE)))
  '((print-stars cont))
  (list "\"This is not the maze where the pirate hides his treasure chest.\"\n"
        'get-user-input))

(define-test 'transitive-read "default"
  '(($set-obj (K LAMP)))
  '((print-stars cont))
  (list 'report-default))

(define-input-test 'transitive-read "oyster-hinted"
  '(($set-obj (K OYSTER))
    ($set-closed (K I))
    ($carry OYSTER)
    (set-nth world set-hinted c7 (K c0)))
  "n\n"
  '((print-stars cont))
  (list "It says the same thing it did before.\n"
        'get-user-input))

(define-input-test 'transitive-read "oyster-reject-hint"
  '(($set-obj (K OYSTER))
    ($set-closed (K I))
    ($carry OYSTER))
  "n\n"
  '((print-stars cont)
    (print-stars (nth c7 $hinted)))
  (list "Hmmm, this looks like a clue, which means it'll cost you 10 points to\nread it.  Should I go ahead and read it anyway?"
        yesno-prompt
        "OK.\n"
        'get-user-input
        10))

(define-input-test 'transitive-read "oyster-accept-hint"
  '(($set-obj (K OYSTER))
    ($set-closed (K I))
    ($carry OYSTER)
    ($set-limit (K (to-cons1 c50))))
  "y\n"
  '((print-stars cont)
    (print-stars (nth c7 $hinted))
    (print-stars (cons1-length $limit)))
  (list "Hmmm, this looks like a clue, which means it'll cost you 10 points to\nread it.  Should I go ahead and read it anyway?"
        yesno-prompt
        "It says, \"There is something strange about this place, such that one\nof the words I've always known now has a new effect.\"\n"
        'get-user-input
        0
        350))

(define-input-test 'transitive-read "do-not-enpower-lamp"
  '(($set-obj (K OYSTER))
    ($set-closed (K I))
    ($carry OYSTER)
    ($set-limit (K (to-cons1 c30))))
  "y\n"
  '((print-stars cont)
    (print-stars (nth c7 $hinted))
    (print-stars (cons1-length $limit)))
  (list "Hmmm, this looks like a clue, which means it'll cost you 10 points to\nread it.  Should I go ahead and read it anyway?"
        yesno-prompt
        "It says, \"There is something strange about this place, such that one\nof the words I've always known now has a new effect.\"\n"
        'get-user-input
        0
        30))

(define-test 'pitch-dark ""
  '(($set-oldlocs (K (icons house road)))
    ($set-location (K debris)))
  '((print-stars cont)
    (print-stars (car $oldlocs))
    (print-stars (cdr $oldlocs)))
  (list "You fell into a pit and broke every bone in your body!\n"
        'death
        'house
        'debris))

(define-input-test 'death "no"
  '()
  "n\n"
  '((print-stars cont)
    (print-stars $death-count))
  (list "Oh dear, you seem to have gotten yourself killed.  I might be able to\nhelp you out, but I've never really done this before.  Do you want me\nto try to reincarnate you?"
        yesno-prompt
        "OK.\n"
        'quit
        1))

(define-input-test 'death "yes1"
  '()
  "y\n"
  '((print-stars cont)
    (print-stars $death-count))
  (list "Oh dear, you seem to have gotten yourself killed.  I might be able to\nhelp you out, but I've never really done this before.  Do you want me\nto try to reincarnate you?"
        yesno-prompt
        "All right.  But don't blame me if something goes wr......\n                 --- POOF!! ---\nYou are engulfed in a cloud of orange smoke.  Coughing and gasping,\nyou emerge from the smoke and find....\n"
        'commence
        1))

(define-input-test 'death "yes2"
  '(($set-death-count (K c1)))
  "y\n"
  '((print-stars cont)
    (print-stars $death-count))
  (list "You clumsy oaf, you've done it again!  I don't know how long I can\nkeep this up.  Do you want me to try reincarnating you again?"
        yesno-prompt
        "Okay, now where did I put my resurrection kit?....  >POOF!<\nEverything disappears in a dense cloud of orange smoke.\n"
        'commence
        2))

(define-input-test 'death "yes3"
  '(($set-death-count (K c2)))
  "y\n"
  '((print-stars cont)
    (print-stars $death-count))
  (list "Now you've really done it!  I'm out of orange smoke!  You don't expect\nme to do a decent reincarnation without any orange smoke, do you?"
        yesno-prompt
        "Okay, if you're so smart, do it yourself!  I'm leaving!\n"
        'quit
        3))

(define-input-test 'death "move-items"
  '(($carry LAMP)
    ($set-prop-of LAMP (K c1))
    ($carry KEYS)
    ($carry BOTTLE)
    ($carry WATER)
    ($set-oldlocs (K (icons cobbles debris))))
  "y\n"
  '((print-stars cont)
    (print-stars $location)
    (print-stars (car $oldlocs))
    (print-stars (cdr $oldlocs))
    (print-stars ($place-of LAMP))
    (print-stars ($prop-of LAMP))
    (print-stars ($place-of KEYS))
    (print-stars ($place-of BOTTLE))
    (print-stars ($place-of WATER)))
  (list "Oh dear, you seem to have gotten yourself killed.  I might be able to\nhelp you out, but I've never really done this before.  Do you want me\nto try to reincarnate you?"
        yesno-prompt
        "All right.  But don't blame me if something goes wr......\n                 --- POOF!! ---\nYou are engulfed in a cloud of orange smoke.  Coughing and gasping,\nyou emerge from the smoke and find....\n"
        'commence
        'house
        'house
        'debris
        'road
        0
        'debris
        'debris
        'limbo))

(define-test 'dwarves-upset ""
  '()
  '((print-stars cont))
  (list "The resulting ruckus has awakened the dwarves.  There are now several\nthreatening little dwarves in the room with you!  Most of them throw\nknives at you!  All of them get you!\n"
        'quit))

(define-test 'cycle "count-start"
  '(($set-location (K outside)))
  '((print-stars cont)
    (print-stars (car $hint-count))
    (print-stars (cons1-length (cdr $hint-count))))
  (list 'cycle2
        0
        1))

(define-test 'cycle "count-inc"
  '(($set-location (K outside))
    ($set-hint-count (K (cons c0 (cons1 V)))))
  '((print-stars cont)
    (print-stars (car $hint-count))
    (print-stars (cons1-length (cdr $hint-count))))
  (list 'cycle2
        0
        2))

(define-test 'cycle "count-reset"
  '(($set-location (K hill))
    ($set-hint-count (K (cons c0 (cons1 V)))))
  '((print-stars cont)
    (print-bool (pair? $hint-count)))
  (list 'cycle2
        #f))

(define-test 'cycle "count-change"
  '(($set-location (K bird))
    ($set-hint-count (K (cons c0 (cons1 (cons1 V))))))
  '((print-stars cont)
    (print-stars (car $hint-count))
    (print-stars (cons1-length (cdr $hint-count))))
  (list 'cycle2
        1
        1))

(define-test 'cycle "already-hinted"
  '(($set-location (K bird))
    (set-nth world set-hinted c1 (K c0))
    ($set-hint-count (K (cons c1 (cons1 V)))))
  '((print-stars cont)
    (print-bool (pair? $hint-count)))
  (list 'cycle2
        #f))

(define-test 'cycle "under-thresh"
  '(($set-location (K outside))
    ($set-hint-count (K (cons c0 (c2 cons1 V)))))
  '((print-stars cont)
    (print-stars (car $hint-count))
    (print-stars (cons1-length (cdr $hint-count))))
  (list 'cycle2
        0
        3))

(define-test 'cycle "grate-condition-not-met"
  '(($set-location (K outside))
    ($set-hint-count (K (cons c0 (c3 cons1 V))))
    ($carry KEYS))
  '((print-stars cont)
    (print-bool (pair? $hint-count)))
  (list 'cycle2
        #f))

(define-input-test 'cycle "reject1"
  '(($set-location (K outside))
    ($set-hint-count (K (cons c0 (c3 cons1 V)))))
  "n\n"
  '((print-stars cont)
    (print-bool (pair? $hint-count))
    (print-stars (car $hinted)))
  (list "Are you trying to get into the cave?"
        yesno-prompt
        "OK.\n"
        'cycle2
        #f
        2))

(define-input-test 'cycle "reject2"
  '(($set-location (K outside))
    ($set-hint-count (K (cons c0 (c3 cons1 V)))))
  "y\nn\n"
  '((print-stars cont)
    (print-bool (pair? $hint-count))
    (print-stars (car $hinted)))
  (list "Are you trying to get into the cave?"
        yesno-prompt
        " I am prepared to give you a hint,\n"
        " but it will cost you 2 points.  "
        "Do you want the hint?"
        yesno-prompt
        "OK.\n"
        'cycle2
        #f
        2))

(define-input-test 'cycle "grate-accept"
  '(($set-location (K outside))
    ($set-hint-count (K (cons c0 (c3 cons1 V))))
    ($set-limit (K (to-cons1 c31))))
  "y\ny\n"
  '((print-stars cont)
    (print-bool (pair? $hint-count))
    (print-stars (car $hinted))
    (print-stars (cons1-length $limit)))
  (list "Are you trying to get into the cave?"
        yesno-prompt
        " I am prepared to give you a hint,\n"
        " but it will cost you 2 points.  "
        "Do you want the hint?"
        yesno-prompt
        "The grate is very solid and has a hardened steel lock.  You cannot\nenter without a key, and there are no keys in sight.  I would recommend\nlooking elsewhere for the keys.\n"
        'cycle2
        #f
        0
        91))

(define-input-test 'cycle "do-not-enpower-lamp"
  '(($set-location (K outside))
    ($set-hint-count (K (cons c0 (c3 cons1 V))))
    ($set-limit (K (to-cons1 c30))))
  "y\ny\n"
  '((print-stars cont)
    (print-bool (pair? $hint-count))
    (print-stars (car $hinted))
    (print-stars (cons1-length $limit)))
  (list "Are you trying to get into the cave?"
        yesno-prompt
        " I am prepared to give you a hint,\n"
        " but it will cost you 2 points.  "
        "Do you want the hint?"
        yesno-prompt
        "The grate is very solid and has a hardened steel lock.  You cannot\nenter without a key, and there are no keys in sight.  I would recommend\nlooking elsewhere for the keys.\n"
        'cycle2
        #f
        0
        30))

(define-test 'cycle "bird-condition-not-met"
  '(($set-location (K bird))
    ($set-hint-count (K (cons c1 (c5 cons1 V))))
    ($set-oldobj (K BIRD)))
  '((print-stars cont)
    (print-stars (car $hint-count))
    (print-stars (cons1-length (cdr $hint-count))))
  (list 'cycle2
        1
        6))

(define-input-test 'cycle "bird-accept"
  '(($set-location (K bird))
    ($set-limit (K (to-cons1 c31)))
    ($set-hint-count (K (cons c1 (c5 cons1 V))))
    ($set-oldobj (K BIRD))
    ($carry ROD))
  "y\ny\n"
  '((print-stars cont)
    (print-bool (pair? $hint-count))
    (print-stars (nth c1 $hinted))
    (print-stars (cons1-length $limit)))
  (list "Are you trying to catch the bird?"
        yesno-prompt
        " I am prepared to give you a hint,\n"
        " but it will cost you 2 points.  "
        "Do you want the hint?"
        yesno-prompt
        "Something seems to be frightening the bird just now and you cannot\ncatch it no matter what you try.  Perhaps you might try later.\n"
        'cycle2
        #f
        0
        91))

(define-test 'cycle "snake-condition-not-met"
  '(($set-location (K hmk))
    ($set-hint-count (K (cons c2 (c8 cons1 V))))
    ($carry BIRD))
  '((print-stars cont)
    (print-bool (pair? $hint-count)))
  (list 'cycle2
        #f))

(define-input-test 'cycle "snake-accept"
  '(($set-location (K hmk))
    ($set-hint-count (K (cons c2 (c8 cons1 V))))
    ($set-limit (K (to-cons1 c31))))
  "y\ny\n"
  '((print-stars cont)
    (print-bool (pair? $hint-count))
    (print-stars (nth c2 $hinted))
    (print-stars (cons1-length $limit)))
  (list "Are you trying to deal somehow with the snake?"
        yesno-prompt
        " I am prepared to give you a hint,\n"
        " but it will cost you 2 points.  "
        "Do you want the hint?"
        yesno-prompt
        "You can't kill the snake, or drive it away, or avoid it, or anything\nlike that.  There is a way to get by, but you don't have the necessary\nresources right now.\n"
        'cycle2
        #f
        0
        91))

(define-test 'cycle "twist-condition-not-met"
  '(($set-location (K like3))
    ($set-oldlocs (K (icons like2 like1)))
    ($set-hint-count (K (cons c3 (c75 cons1 V))))
    ($carry BIRD))
  '((print-stars cont)
    (print-bool (pair? $hint-count)))
  (list 'cycle2
        #f))

(define-test 'cycle "twist-condition-not-met2"
  '(($set-location (K like3))
    ($set-oldlocs (K (icons like2 like1)))
    ($set-hint-count (K (cons c3 (c75 cons1 V))))
    ($drop BIRD like1)
    ($carry LAMP)
    ($carry KEYS))
  '((print-stars cont)
    (print-bool (pair? $hint-count)))
  (list 'cycle2
        #f))

(define-input-test 'cycle "twist-accept"
  '(($set-location (K like3))
    ($set-oldlocs (K (icons like2 like1)))
    ($set-hint-count (K (cons c3 (c75 cons1 V))))
    ($set-limit (K (to-cons1 c31)))
    ($carry LAMP)
    ($carry KEYS))
  "y\ny\n"
  '((print-stars cont)
    (print-bool (pair? $hint-count))
    (print-stars (nth c3 $hinted))
    (print-stars (cons1-length $limit)))
  (list "Do you need help getting out of the maze?"
        yesno-prompt
        " I am prepared to give you a hint,\n"
        " but it will cost you 4 points.  "
        "Do you want the hint?"
        yesno-prompt
        "You can make the passages look less alike by dropping things.\n"
        'cycle2
        #f
        0
        151))

(define-test 'cycle "dark-condition-not-met"
  '(($set-location (K droom))
    ($set-prop-of EMERALD (K c0))
    ($set-prop-of PYRAMID (K c0))
    ($set-hint-count (K (cons c4 (c25 cons1 V)))))
  '((print-stars cont)
    (print-bool (pair? $hint-count)))
  (list 'cycle2
        #f))

(define-input-test 'cycle "dark-accept"
  '(($set-location (K droom))
    ($set-hint-count (K (cons c4 (c25 cons1 V))))
    ($set-prop-of EMERALD (K c0))
    ($set-limit (K (to-cons1 c31))))
  "y\ny\n"
  '((print-stars cont)
    (print-bool (pair? $hint-count))
    (print-stars (nth c4 $hinted))
    (print-stars (cons1-length $limit)))
  (list "Are you trying to explore beyond the Plover Room?"
        yesno-prompt
        " I am prepared to give you a hint,\n"
        " but it will cost you 5 points.  "
        "Do you want the hint?"
        yesno-prompt
        "There is a way to explore that region without having to worry about\nfalling into a pit.  None of the objects available is immediately\nuseful for discovering the secret.\n"
        'cycle2
        #f
        0
        181))

(define-input-test 'cycle "witt-accept"
  '(($set-location (K witt))
    ($set-hint-count (K (cons c5 (c20 cons1 V))))
    ($set-limit (K (to-cons1 c31))))
  "y\ny\n"
  '((print-stars cont)
    (print-bool (pair? $hint-count))
    (print-stars (nth c5 $hinted))
    (print-stars (cons1-length $limit)))
  (list "Do you need help getting out of here?"
        yesno-prompt
        " I am prepared to give you a hint,\n"
        " but it will cost you 3 points.  "
        "Do you want the hint?"
        yesno-prompt
        "Don't go west.\n"
        'cycle2
        #f
        0
        121))

(define-test 'quit "1-turn"
  '((set-nth world set-hinted c8 (K c0))
    ($set-turns (K (to-cons1 c1))))
  '()
  (list "You scored 32 points out of a possible 350, using 1 turn.\n"
        "You are obviously a rank amateur.  Better luck next time.\n"
        "To achieve the next higher rating, you need 3 more points.\n"))

(define-test 'quit "1-point"
  '((set-nth world set-hinted c4 (K c0))
    ($set-death-count (K c3))
    ($set-turns (K (to-cons1 c2))))
  '()
  (list "You scored 1 point out of a possible 350, using 2 turns.\n"
        "You are obviously a rank amateur.  Better luck next time.\n"
        "To achieve the next higher rating, you need 34 more points.\n"))

(define-test 'quit "1-more-point"
  '((set-nth world set-hinted c8 (K c2))
    ($set-turns (K (to-cons1 c2))))
  '()
  (list "You scored 34 points out of a possible 350, using 2 turns.\n"
        "You are obviously a rank amateur.  Better luck next time.\n"
        "To achieve the next higher rating, you need 1 more point.\n"))

(define-test 'quit "rank2"
  '((set-nth world set-hinted c8 (K c3))
    ($set-turns (K (to-cons1 c2))))
  '()
  (list "You scored 35 points out of a possible 350, using 2 turns.\n"
        "Your score qualifies you as a novice class adventurer.\n"
        "To achieve the next higher rating, you need 65 more points.\n"))

(define-test 'quit "rank3"
  '((set-nth world set-hinted c8 (K c68))
    ($set-turns (K (to-cons1 c2))))
  '()
  (list "You scored 100 points out of a possible 350, using 2 turns.\n"
        "You have achieved the rating \"Experienced Adventurer\".\n"
        "To achieve the next higher rating, you need 30 more points.\n"))

(define-test 'quit "rank4"
  '((set-nth world set-hinted c8 (K c98))
    ($set-turns (K (to-cons1 c2))))
  '()
  (list "You scored 130 points out of a possible 350, using 2 turns.\n"
        "You may now consider yourself a \"Seasoned Adventurer\".\n"
        "To achieve the next higher rating, you need 70 more points.\n"))

(define-test 'quit "rank5"
  '((set-nth world set-hinted c8 (K c168))
    ($set-turns (K (to-cons1 c2))))
  '()
  (list "You scored 200 points out of a possible 350, using 2 turns.\n"
        "You have reached \"Junior Master\" status.\n"
        "To achieve the next higher rating, you need 50 more points.\n"))

(define-test 'quit "rank6"
  '((set-nth world set-hinted c8 (K c218))
    ($set-turns (K (to-cons1 c2))))
  '()
  (list "You scored 250 points out of a possible 350, using 2 turns.\n"
        "Your score puts you in Master Adventurer Class C.\n"
        "To achieve the next higher rating, you need 50 more points.\n"))

(define-test 'quit "rank7"
  '((set-nth world set-hinted c8 (K c268))
    ($set-turns (K (to-cons1 c2))))
  '()
  (list "You scored 300 points out of a possible 350, using 2 turns.\n"
        "Your score puts you in Master Adventurer Class B.\n"
        "To achieve the next higher rating, you need 30 more points.\n"))

(define-test 'quit "rank8"
  '((set-nth world set-hinted c8 (K c298))
    ($set-turns (K (to-cons1 c2))))
  '()
  (list "You scored 330 points out of a possible 350, using 2 turns.\n"
        "Your score puts you in Master Adventurer Class A.\n"
        "To achieve the next higher rating, you need 19 more points.\n"))

(define-test 'quit "rank9"
  '((set-nth world set-hinted c8 (K c317))
    ($set-turns (K (to-cons1 c2))))
  '()
  (list "You scored 349 points out of a possible 350, using 2 turns.\n"
        "All of Adventuredom gives tribute to you, Adventure Grandmaster!\n"
        "To achieve the next higher rating would be a neat trick!\n"
        "Congratulations!!\n"))

(define (main args)
  (let ((testname (if (null? (cdr args)) #f (string->symbol (cadr args)))))
    (for-each
     (lambda (testcase)
       (if (or (not testname) (eq? testname (car testcase)))
           (apply test-proc testcase)))
     (reverse tests)))
  0)
