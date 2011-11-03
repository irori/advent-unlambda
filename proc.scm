#!/usr/bin/env gosh
(require "enum.scm")

(defmacro goto cons)
(defmacro ($goto label)
  (cons label world))

(define procedures '())

(define (define-proc name body)
  (define-enum (list name) (length procedures))
  (set! procedures (cons body procedures)))

; 75 Simulate an adventure, going to quit when finished
(define-proc 'mainloop
  '(lambda (world)
     ; TODO: Check for interference with the proposed move to newloc 153
     (let-world (($set-location (K $newloc)))
       ; TODO: Possibly move dwarves and the pirate 161
       ($goto commence))))

; 141 Report the long description and continue
(defmacro handle-look
  (lambda (world)
    (let-world (($set-was-dark (K V))
		($set-nth set-visits $location (K V)))
      (if (cons1? (1-of-1 $verbose))
	  ((string "Sorry, but I am not allowed to give more detail.  I will repeat the\nlong description of your location.\n")
	   ($set-verbose 1-of-1))
	  world))))

; 75 try-move:
(define-proc 'try-move
  '(lambda (world)
     (let-world (($set-newloc (K $location)))
       (cond ((= $mot NOWHERE)
	      ($goto mainloop))
             ; TODO: handle BACK
	     ((= $mot LOOK)
	      (goto mainloop (handle-look world)))
             ((= $mot CAVE)
              ((if< $location min-in-cave
                    (string "I can't see where the cave is, but hereabouts no stream can run on\nthe surface for long.  I would try the stream.\n")
                    (string "I need more detailed instructions to do that.\n"))
               ($goto mainloop)))
	     (else
	      (goto go-for-it
		    ($set-oldlocs (lambda (ol-ool)
				    (cons $location (car ol-ool))))))))))

; 76 Get user input; goto try_move if motion is requested
(define-proc 'get-user-input
  '(lambda (world)
     (let-world (($set-verb (K ABSTAIN))
                 ($set-oldverb (K ABSTAIN))
                 ($set-oldobj (K $obj))
		 ($set-obj (K NOTHING)))
       ($goto cycle))))

; 76 cycle:
(define-proc 'cycle
  '(lambda (world)
     ; TODO: Check if a hint applies, and give it if requested 195
     (let ((words listen))
       (let-world (($set-was-dark (K $dark))
                   ($set-rand cdr)
		   ($set-word12 (K words)))
                   ; TODO: adjust knife_loc 169
                   ; TODO: adjust prop<0 objects after close
	 ($goto pre-parse)))))

; 76 pre_parse:
(define-proc 'pre-parse
  '(lambda (world)
     ((lambda (next)
        (let-world (($set-turns cons1))
          (if (= $verb SAY)
              (if (word? (cdr $word12))
                  (next ($set-verb (K ABSTAIN)))
                  ($goto transitive))
              (next world))))
      (lambda (world)
        ; TODO: adjust foobar 138
        ($goto clocks-and-lamp)))))

; 178 Check the clocks and the lamp
(define-proc 'clocks-and-lamp
  '(lambda (world)
     ; TODO: implement clock check
     ($goto check-the-lamp)))

; 184 Check the clocks and the lamp
(define-proc 'check-the-lamp
  '(lambda (world)
     (let ((old-limit $limit))
       (let-world (($set-limit (if (= ($prop-of LAMP) c1) 1-of-1 I)))
         (cond ((and (not (cons1? (c30 1-of-1 $limit)))
                     ($here? BATTERIES)
                     (zero? ($prop-of BATTERIES))
                     ($here? LAMP))
                ((string "Your lamp is getting dim.  I'm taking the liberty of replacing\nthe batteries.\n")
                 (let-world (($set-prop-of BATTERIES (K c1))
                             ($drop BATTERIES $location)
                             ($set-limit (K (to-cons1 (pow c50 c2)))))
                   ($goto handle-special-inputs))))
               ((and (cons1? old-limit) (not (cons1? $limit)))
                (begin
                  (($here? LAMP)
                   (string "Your lamp has run out of power.\n") I)
                  (goto handle-special-inputs ($set-prop-of LAMP (K c0)))))
               ((if< $location min-in-cave (not (cons1? $limit)) V)
                ((string "There's not much point in wandering around out here, and you can't\nexplore the cave without a lamp.  So let's just call it a day.\n")
                 ($goto give-up)))
               ((and (not (cons1? (c30 1-of-1 $limit)))
                     $not-warned
                     ($here? LAMP))
                ((string "Your lamp is getting dim")
                 (cond ((nonzero? ($prop-of BATTERIES))
                        (string ", and you're out of spare batteries.  You'd\nbest start wrapping this up.\n"))
                       ((zero? ($place-of BATTERIES))
                        (string ".  You'd best start wrapping this up, unless\nyou can find some fresh batteries.  I seem to recall that there's\na vending machine in the maze.  Bring some coins with you.\n"))
                       (else
                        (string ".  You'd best go back for those batteries.\n")))
                 (goto handle-special-inputs ($set-not-warned (K V)))))
               (else
                ($goto handle-special-inputs)))))))

(defmacro ($try-motion m)
  (goto try-move (set-mot world (K m))))
(defmacro ($report str)
  (str #\newline
   (goto get-user-input world)))
(defmacro ($default-to v)
  ($report (nth v $default-msg)))

; 83 Handle additional special cases of input
(define-proc 'handle-special-inputs
  '(lambda (world)
     ($word12
      (lambda (word1 word2)
        (cond ((and (motion? word1) (= (word-meaning word1) ENTER))
               (cond ((or (and (noun? word2) (= (word-meaning word2) WATER))
                          (and (motion? word2) (= (word-meaning word2) STREAM)))
                      (if (water-here world)
                          ($report (string "Your feet are now wet."))
                          ($default-to GO)))
                     ((word? word2)
                      ($goto shift))
                     (else
                      ($goto parse-label))))
              ((and (noun? word1)
                    (or (= (word-meaning word1) WATER)
                        (= (word-meaning word1) OIL))
                    (noun? word2)
                    (or (= (word-meaning word2) PLANT)
                        (= (word-meaning word2) DOOR))
                    (= $location ($place-of (word-meaning word2))))
               (goto parse-label
                     ($set-word12 (lambda (old)
                                    (cons (car old) (action-word POUR))))))
              (else
               ($goto parse-label)))))))

; 76 parse:
(define-proc 'parse-label
  '(lambda (world)
     ; TODO: give advice about going WEST 80
     ($goto look-at-word1)))

; 76 shift:
(define-proc 'shift
  '(lambda (world)
     (goto parse-label ($set-word12 (lambda (old) (cons (cdr old) V))))))

; Gee, I don't understand
(defmacro unknown-word
  (lambda (world)
    ($set-rand
     (lambda (bits)
       (bits
	(lambda (b)
	  (b (string "I don't know that word.\n")
	     (lambda (bits2)
	       (bits2
		(lambda (b2)
		  (b2 (string "What?\n")
		      (string "I don't understand that!\n"))))))))))))

; 78 Look at word1 and exit to the right place if it completes a command
(define-proc 'look-at-word1
  '(lambda (world)
     ($word12
      (lambda (word1 word2)
        (if (word? word1)
            ((word-type word1)
             (lambda (_)  ; motion
               (goto try-move ($set-mot (K (word-meaning word1)))))
             (lambda (_)  ; object
               (let-world (($set-obj (K (word-meaning word1))))
                 (if (or ($toting? $obj)
                         ($at-loc? $obj))
                     ($goto handle-object-word)
                     ($goto check-object-location))))
             (lambda (_)  ; verb
	       (let-world (($set-verb (K (word-meaning word1))))
                 ; TODO: handle SAY
		 (if (word? word2)
		     ($goto shift)
		     ($goto (ifnonzero $obj transitive intransitive)))))
             (lambda (_)  ; message
               ($report (nth (word-meaning word1) $message)))
             I)
            (goto cycle (unknown-word world)))))))

; 78 case object_type:
(define-proc 'handle-object-word
  '(lambda (world)
     (if (word? (cdr $word12))
         ($goto shift)
         (if (nonzero? $verb)
             ($goto transitive)
             ((string "What do you want to do with the ")
              (word-letters (car $word12))
              (string "?\n")
              ($goto cycle))))))

(define-proc 'intransitive
  '(lambda (world)
     ($goto (nth $verb
		 (list V
		       intransitive-take  ;TAKE
		       get-object  ;DROP
		       intransitive-open  ;OPEN
		       intransitive-open  ;CLOSE
		       transitive  ;ON
		       transitive  ;OFF
		       not-implemented  ;WAVE
		       not-implemented  ;CALM
		       report-default  ;GO
		       report-default  ;RELAX
		       transitive  ;POUR
		       intransitive-eat  ;EAT
		       transitive  ;DRINK
		       not-implemented  ;RUB
		       not-implemented  ;TOSS
		       not-implemented  ;WAKE
		       not-implemented  ;FEED
		       transitive  ;FILL
		       not-implemented  ;BREAK
		       transitive  ;BLAST
		       transitive  ;KILL
		       not-implemented  ;SAY
		       not-implemented  ;READ
		       not-implemented  ;FEEFIE
		       intransitive-brief  ;BRIEF
		       not-implemented  ;FIND
		       intransitive-inventory  ;INVENTORY
		       not-implemented  ;SCORE
		       not-implemented  ;QUIT
		       )))))
	    

(define-proc 'transitive
  '(lambda (world)
     ($goto (nth $verb
		 (list V
		       transitive-take  ;TAKE
		       transitive-drop  ;DROP
		       transitive-open  ;OPEN
		       transitive-open  ;CLOSE
		       transitive-on  ;ON
		       transitive-off  ;OFF
		       not-implemented  ;WAVE
		       not-implemented  ;CALM
		       not-implemented  ;GO
		       not-implemented  ;RELAX
		       not-implemented  ;POUR
		       transitive-eat  ;EAT
		       not-implemented  ;DRINK
		       not-implemented  ;RUB
		       not-implemented  ;TOSS
		       not-implemented  ;WAKE
		       not-implemented  ;FEED
		       not-implemented  ;FILL
		       not-implemented  ;BREAK
		       not-implemented  ;BLAST
		       not-implemented  ;KILL
		       not-implemented  ;SAY
		       not-implemented  ;READ
		       not-implemented  ;FEEFIE
		       report-default  ;BRIEF
		       not-implemented  ;FIND
		       not-implemented  ;INVENTORY
		       not-implemented  ;SCORE
		       not-implemented  ;QUIT
		       )))))
           

; 79 report_default:
(define-proc 'report-default
  '(lambda (world)
     (begin
       ((nth $verb $default-msg) #\newline I)
       ($goto get-user-input))))

; 79 get_object:
(define-proc 'get-object
  '(lambda (world)
     (begin
       ((word-letters (car $word12)) I)
       (print " what?\n")
       ($goto cycle))))

; 79 cant_see_it:
(define-proc 'cant-see-it
  '(lambda (world)
     (if (and (or (= $verb FIND) (= $verb INVENTORY))
              (not (word? (cdr $word12))))
         ($goto transitive)
         (begin
           (print "I see no ")
           ((word-letters (car $word12)) I)
           (print " here.\n")
           ($goto get-user-input)))))

(defmacro (dark world)
  (and (not (lighted? $location))
       (or (zero? ($prop-of LAMP))
           (not ($here? LAMP)))))
(defmacro $dark (dark world))

(defmacro pitch-dark-msg
  (string "It is now pitch dark.  If you proceed you will most likely fall into a pit."))

; 86 Report the current state
(define-proc 'commence
  `(lambda (world)
     (if (= $location limbo)
         ($goto death)
         ((lambda (next)
            (if (and $dark (not (forced-move? $location)))
                (let-rand r 35
                  (if (and $was-dark r)
                      ($goto pitch-dark)
                      (next pitch-dark-msg world)))
                (next ((nth $location room-desc)
                       (if (cons1? (nth $location $visits))
                           ->cdr ->car))
                      world)))
          (lambda (p world)
            (begin
              (($toting? BEAR)
               (string "You are being followed by a very large, tame bear.\n") I)
              (#\newline p #\newline I)
              (if (forced-move? $location)
                  ($goto try-move)
                  ; TODO: give optinal plugh hint 157
                  ($goto (if $dark
                             get-user-input
                             describe-objects)))))))))

(defmacro increment-visits
  (lambda (world)
    ($set-nth set-visits $location
	      (lambda (x)
		(if (cons1? x)
		    (if (cons1? $verbose)
			(1-of-1 x)
			x)
		    (cons1 (cons1 (cons1 (cons1 V)))))))))

(defmacro ($describe-single-object tt)
  ((if (= tt TREADS)
       (cond (($toting? GOLD) V)
             ((= $location emist) cdr)
             (else I))
       I)
   (($prop-of tt) cdr (nth tt $note))
   ->car
   #\newline I))

; 88 Describe the objects at this location
(define-proc 'describe-objects
  '(lambda (world)
     (let loop ((world world)
                (lst $objects-here))
       (if (null? lst)
           (goto get-user-input (increment-visits world))
           (let* ((bas ($base-of (car lst)))
                  (tt (if (zero? bas) (car lst) bas)))
             (let-world ((if (($prop-of tt) I I)  ; TODO: check !closed
                             world
                             ($set-prop-of tt (K (cond ((= tt RUG) c1)
                                                       ((= tt CHAIN) c1)
                                                       (else c0))))))
                         ; TODO: tally--, Zap the lamp 183
               (begin
                 ($describe-single-object tt)
                 (loop world (cdr lst)))))))))

; 90 Make sure obj is meaningful at the current location
(define-proc 'check-object-location
  '(lambda (world)
     (cond ((and (= $obj GRATE) (< $location min-lower-loc))
            (cond ((or (= $location road)
                       (= $location valley)
                       (= $location slit))
                   ($try-motion DEPRESSION))
                  ((or (= $location cobbles)
                       (= $location debris)
                       (= $location awk)
                       (= $location bird)
                       (= $location spit))
                   ($try-motion ENTRANCE))
                  (else
                   ($goto cant-see-it))))
           ; TODO: handle dwarf
           ((and (= $obj PLANT) ($at-loc? PLANT2) (nonzero? ($prop-of PLANT2)))
            (goto handle-object-word ($set-obj (K PLANT2))))
           ; TODO: handle knife
           ((and (= $obj ROD) ($here? ROD2))
            (goto handle-object-word ($set-obj (K ROD2))))
           ((and (= $obj WATER)
                 (or (and ($here? BOTTLE) (zero? ($prop-of BOTTLE)))
                     (water-here world)))
            ($goto handle-object-word))
           ((and (= $obj OIL)
                 (or (and ($here? BOTTLE) (= ($prop-of BOTTLE) c2))
                     (oil-here world)))
            ($goto handle-object-word))
           (else
            ($goto cant-see-it)))))

; 92 case TAKE:
(define-proc 'intransitive-take
  '(lambda (world)
     (let ((objs $objects-here))
       (if (and (pair? objs) (null? (cdr objs)))  ; TODO: check dwarf
           (goto transitive ($set-obj (K (car objs))))
           ($goto get-object)))))

; 92 case EAT:
(define-proc 'intransitive-eat
  '(lambda (world)
     (if ($here? FOOD)
         (goto transitive ($set-obj (K FOOD)))
         ($goto get-object))))


; 93 case OPEN: case CLOSE:
(define-proc 'intransitive-open
  '(lambda (world)
     (let ((object (cond ((or (= ($place-of GRATE) $location)
                              (= ($place-of GRATE_) $location))
                          GRATE)
                         ((= ($place-of DOOR) $location) DOOR)
                         (($here? CLAM) CLAM)
                         (($here? OYSTER) OYSTER)
                         (else V))))
       (if ($here? CHAIN)
           (if (object I I)
               ($goto get-object)
               (goto transitive ($set-obj (K CHAIN))))
           (if (object I I)
               (goto transitive ($set-obj (K object)))
               ($report (string "There is nothing here with a lock!")))))))

; 94 case INVENTORY:
(define-proc 'intransitive-inventory
  '(lambda (world)
     (begin
       (let ((lst $objects-toting))
         (cond ((null? lst)
                (print "You're not carrying anything.\n"))
               ((and (= (car lst) BEAR) (null? (cdr lst)))
                V)
               (else
                (for-each
                 (lambda (o)
                   ((not (= o BEAR)) #\space (nth o objname) #\newline I))
                 ((string "You are currently holding the following:\n") lst)))))
       (($toting? BEAR)
        (string "You are being followed by a very large, tame bear.\n") I)
       ($goto get-user-input))))

; 95 case BRIEF:
(define-proc 'intransitive-brief
  '(lambda (world)
     (let-world (($set-verbose (K V)))
       ($report (string "Okay, from now on I'll only describe a place in full the first time\nyou come to it.  To get the full description, say \"LOOK\".")))))

; 98 case EAT:
(defsyntax (eat-special? x)
  `(nth ,x ,(make-boolean-list
             '(BIRD SNAKE CLAM OYSTER DWARF DRAGON TROLL BEAR))))

(define-proc 'transitive-eat
  '(lambda (world)
     (cond ((= $obj FOOD)
            (let-world (($destroy FOOD))
              ($report (string "Thank you, it was delicious!"))))
           ((eat-special? $obj)
            ($report (string "I think I just lost my appetite.")))
           (else
            ($goto report-default)))))

; 102 case ON:
(define-proc 'transitive-on
  '(lambda (world)
     (if (not ($here? LAMP))
         ($goto report-default)
         (if (cons1? $limit)
             (let-world (($set-prop-of LAMP (K c1)))
               ((string "Your lamp is now on.\n")
                ($goto (if $was-dark commence get-user-input))))
             ($report (string "Your lamp has run out of power."))))))

; 102 case OFF:
(define-proc 'transitive-off
  '(lambda (world)
     (if (not ($here? LAMP))
         ($goto report-default)
         (let-world (($set-prop-of LAMP (K c0)))
           (begin
             (print "Your lamp is now off.\n")
             ($dark pitch-dark-msg #\newline I)
             ($goto get-user-input))))))

; 112 case TAKE:
(define-proc 'transitive-take
  '(lambda (world)
     (call/cc
      (lambda (ret)
        (begin
          (($toting? $obj)  ; already carrying it
           ret ($goto report-default))
          ((nonzero? ($base-of $obj))  ; it is immovable
           (immovable-msg world) #\newline
           ret ($goto get-user-input))
          (let-world ((take-liquid world ret))
            (begin
              ((carrying-too-many? world)
               (string "You can't carry anything more.  You'll have to drop something first.\n")
               ret ($goto get-user-input))
              (let-world ((take-bird world ret)
                          (take-cage-bird world)
                          ($carry $obj)
                          (take-liquid-in-bottle world))
                ($report (string "OK."))))))))))

(defmacro (carrying-too-many? world)
  (let loop ((lst (cdr (place world)))
             (n c1)
             (count (to-cons1 c6)))
    (lst
     (lambda (hd tl)
       (let ((next (loop tl (succ n))))
         (cond ((or (hd I I) (= n WATER) (= n OIL)) (next count))
               ((cons1? count) (next (1-of-1 count)))
               (else I)))))))

(defmacro (immovable-msg world)
  (cond ((ifnonzero ($prop-of BEAR) (= $obj CHAIN) V)
         (string "The chain is still locked."))
        ((and (= $obj BEAR) (= ($prop-of BEAR) c1))
         (string "The bear is still chained to the wall."))
        ((and (= $obj PLANT) (not (nonzero? ($prop-of PLANT)))) ; prop[PLANT]<=0
         (string "The plant has exceptionally deep roots and cannot be pulled free."))
        (else (string "You can't be serious!"))))

(defmacro take-cage-bird
  (lambda (world)
    (cond ((= $obj BIRD)
	   ($carry CAGE))
	  ((and (= $obj CAGE) (nonzero? ($prop-of BIRD)))
	   ($carry BIRD))
	  (else world))))

; 113 Check special cases for taking a liquid
(defmacro take-liquid
  (lambda (world ret)
    (if (or (= $obj WATER) (= $obj OIL))
        (if (and ($here? BOTTLE) $object-in-bottle)
            ($set-obj (K BOTTLE))
            (let-world (($set-obj (K BOTTLE)))
              (if ($toting? BOTTLE)
                  (let-world (($set-oldverb (K $verb))
                              ($set-verb (K FILL)))
                    (ret ($goto transitive)))
                  ((string "You have nothing in which to carry it.\n")
                   ret ($goto get-user-input)))))
        world)))

(defmacro take-liquid-in-bottle
  (lambda (world)
    (if (and (= $obj BOTTLE) (not $bottle-empty))
        ($carry (ifnonzero ($prop-of BOTTLE) OIL WATER))
        world)))

; 114 Check special cases for taking a bird
(defmacro take-bird
  (lambda (world ret)
    (if (and (= $obj BIRD) (zero? ($prop-of BIRD)))
	(begin
	  (($toting? ROD)
	   (string "The bird was unafraid when you entered, but as you approach it becomes\ndisturbed and you cannot catch it.\n")
	   ret ($goto get-user-input))
	  (if ($toting? CAGE)
	      ($set-prop-of BIRD (K c1))
	      ((string "You can catch the bird, but you cannot carry it.\n")
	       ret ($goto get-user-input))))
	world)))

; 115 Check special cases for dropping a liquid
(defmacro drop-liquid
  (lambda (world)
    (let-world ((if $object-in-bottle ($set-obj (K BOTTLE)) world))
      (if (and (= $obj BOTTLE) (not $bottle-empty))
          ($drop (ifnonzero ($prop-of BOTTLE) OIL WATER) limbo)
          world))))

; 117 case DROP:
(define-proc 'transitive-drop
  '(lambda (world)
     (let-world ((if (and (= $obj ROD) ($toting? ROD2) (not ($toting? ROD)))
                     ($set-obj (K ROD2))
                     world))
       (cond ((not ($toting? $obj))
              ($goto report-default))
             ((and (= $obj COINS) ($here? PONY))
              (drop-coins world))
             ((= $obj BIRD)
              (drop-bird world))
             ((and (= $obj VASE) (not (= $location soft)))
              (drop-vase world))
             ((and (= $obj BEAR) ($at-loc? TROLL))
              (drop-bear world))
             ((and (= $obj CAGE) (nonzero? ($prop-of BIRD)))
              (let-world (($drop BIRD $location)
                          ($drop CAGE $location))
                ($report (string "OK."))))
             (else
              (let-world ((drop-liquid world)
                          ($drop $obj $location))
                ($report (string "OK."))))))))

; 118 Put coins in the vending machine
(defmacro drop-coins
  (lambda (world)
    (let-world (($destroy COINS)
                ($drop BATTERIES $location)
                ($set-prop-of BATTERIES (K c0)))
      ($report (car (nth BATTERIES $note))))))

; 119 Chase the troll away
(defmacro drop-bear
  (lambda (world)
    (let-world (($destroy TROLL)
                ($destroy TROLL_)
                ($drop TROLL2 swside)
                ($drop TROLL2_ neside)
                ($set-prop-of TROLL (K c2))
                ($drop BEAR $location))
      ($report (string "The bear lumbers toward the troll, who lets out a startled shriek and\nscurries away.  The bear soon gives up the pursuit and wanders back.")))))

; 120 Check special cases for dropping the bird
(defmacro drop-bird
  (lambda (world)
    (if ($here? SNAKE)
	(let-world (($destroy SNAKE)
		    ($set-prop-of SNAKE (K c1))
                    ; TODO: if (closed) goto dwarves_upset;
		    ($set-prop-of BIRD (K c0))
                    ($drop BIRD $location))
	  ($report (string "The little bird attacks the green snake, and in an astounding flurry\ndrives the snake away.")))
        ; TODO: handle dragon case
        (let-world (($set-prop-of BIRD (K c0))
                    ($drop BIRD $location))
          ($report (string "OK."))))))

; 121 Check special cases for dropping the vase
(defmacro drop-vase
  (lambda (world)
    (let ((pillow-here (= ($place-of PILLOW) $location)))
      (let-world (($set-prop-of VASE (K (if pillow-here c0 c2)))
                  (if pillow-here world ($set-base-of VASE (K VASE)))
                  ($drop VASE $location))
        ($report (nth (if pillow-here c1 c3) (nth VASE $note)))))))

; 130 case OPEN: case CLOSE:
(define-proc 'transitive-open
  '(lambda (world)
     (cond ((or (= $obj OYSTER) (= $obj CLAM))
            (open-close-clam-oyster world))
           ((= $obj GRATE)
            (open-close-grate world))
           ((= $obj CHAIN)
            (open-close-chain world))
           ((= $obj KEYS)
            ($report (string "You can't lock or unlock the keys.")))
           ((= $obj CAGE)
            ($report (string "It has no lock.")))
           ((= $obj DOOR)
            ($report (ifnonzero ($prop-of DOOR) (string "OK.")
                                (string "The door is extremely rusty and refuses to open."))))
           (else
            ($goto report-default)))))

; 131 Open/close grate
(defmacro open-close-grate
  (lambda (world)
    (if ($here? KEYS)
        ; TODO: panic at closing time
        ((nth (+ ($prop-of GRATE) (if (= $verb OPEN) c2 c0))
              (list (string "It was already locked.")
                    (string "The grate is now locked.")
                    (string "The grate is now unlocked.")
                    (string "It was already unlocked.")))
         #\newline
         (goto get-user-input
               ($set-prop-of GRATE (K (if (= $verb OPEN) c1 c0)))))
        ($report (string "You have no keys!")))))

; 132 Close chain
(defmacro close-chain
  (lambda (world)
    (cond ((not (= $location barr))
           ($report (string "There is nothing here to which the chain can be locked.")))
          ((nonzero? ($prop-of CHAIN))
           ($report (string "It was already locked.")))
          (else
           (let-world (($set-prop-of CHAIN (K c2))
                       ($set-base-of CHAIN (K CHAIN))
                       (if ($toting? CHAIN) ($drop CHAIN $location) world))
             ($report (string "The chain is now locked.")))))))

; 133 Open chain
(defmacro open-chain
  (lambda (world)
    (cond ((zero? ($prop-of CHAIN))
           ($report (string "It was already unlocked.")))
          ((zero? ($prop-of BEAR))
           ($report (string "There is no way to get past the bear to unlock the chain, which is\nprobably just as well.")))
          (else
           (let-world (($set-prop-of CHAIN (K c0))
                       ($set-base-of CHAIN (K NOTHING))
                       (if (= ($prop-of BEAR) c3)
                           ($set-base-of BEAR (K BEAR))
                           (set-prop-of BEAR (K c2)
                                        ($set-base-of BEAR (K NOTHING)))))
             ($report (string "The chain is now unlocked.")))))))

; Open/close chain
(defmacro open-close-chain
  (lambda (world)
    (if ($here? KEYS)
        (if (= $verb OPEN)
            (open-chain world)
            (close-chain world))
        ($report (string "You have no keys!")))))

(defmacro $clam-oyster
  (if (= $obj CLAM) (string "clam") (string "oyster")))

; 134 Open/close clam/oyster
(defmacro open-close-clam-oyster
  (lambda (world)
    (cond ((= $verb CLOSE)
           ($report (string "What?")))
          ((not ($toting? TRIDENT))
           ((string "You don't have anything strong enough to open the ")
            $clam-oyster
            (string ".\n")
            ($goto get-user-input)))
          (($toting? $obj)
           ((string "I advise you to put down the ")
            $clam-oyster
            (string " before opening it.  >")
            (if (= $obj CLAM) (string "STRAIN") (string "WRENCH"))
            (string "!<\n")
            ($goto get-user-input)))
          ((= $obj CLAM)
           (let-world (($destroy CLAM)
                       ($drop OYSTER $location)
                       ($drop PEARL sac))
             ($report (string "A glistening pearl falls out of the clam and rolls away.  Goodness,\nthis must really be an oyster.  (I never was very good at identifying\nbivalves.)  Whatever it is, it has now snapped shut again."))))
          (else
           ($report (string "The oyster creaks open, revealing nothing but oyster inside.\nIt promptly snaps shut again."))))))

(defmacro ppass-msg
  (string "Something you're carrying won't fit through the tunnel with you.\nYou'd best take inventory and drop something.\n"))

; 146 Determine the next location, newloc
(define-proc 'go-for-it
  '(lambda (world)
     (let ((q (find-inst $mot (nth $location travels))))
       (if (not (pair? q))
           (report-inapplicable-motion world)
           (let-world ((apply-inst q world))
             (cond ((= $newloc ppass)
                    (let ((os $objects-toting))
                      (if (or (null? os)
                              (and (= (car os) EMERALD) (null? (cdr os))))
                          (goto mainloop
                                ($set-newloc (K (if (= $location alcove)
                                                    proom alcove))))
                          (ppass-msg
                           (goto mainloop ($set-newloc (K $location)))))))
                   ; TODO: implement troll
                   (else
                    ($goto mainloop))))))))

(defmacro (report-inapplicable-motion world)
  ((cond ((= $mot CRAWL)
          (string "Which way?"))
         ((or (= $mot XYZZY) (= $mot PLUGH))
          (nth WAVE $default-msg))
         ((or (= $verb FIND) (= $verb INVENTORY))
          (nth FIND $default-msg))
         ((or (= $mot IN) (= $mot OUT))
          (string "I don't know in from out here.  Use compass points or name something\nin the general direction you want to go."))
         ((or (= $mot FORWARD) (= $mot L) (= $mot R))
          (string "I am unsure how you are facing.  Use compass points or nearby objects."))
         ((<= $mot FORWARD)
          (string "There is no way to go in that direction."))
         (else
          (string "I don't know how to apply that word here.")))
   #\newline
   ($goto mainloop)))

(define-proc 'not-implemented
  '(lambda (world)
     ((string "\nnot implemented\n") exit I)))

(define-proc 'give-up
  `(lambda (world)
     (exit (print "give-up: not implemented\n"))))

(define-proc 'death
  `(lambda (world)
     (exit (print "death: not implemented\n"))))

(define-proc 'pitch-dark
  `(lambda (world)
     (exit (print "'pitch-dark: not implemented\n"))))

(define program-table
  (map (lambda (x) (compile-to-string (if (undefined? x) 'V x)))
       (reverse procedures)))

(add-unl-macro!
 'program-table '() `(list ,@program-table))

(defmacro initial-pc mainloop)
