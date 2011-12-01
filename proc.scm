#!/usr/bin/env gosh
(require "enum.scm")

(defmacro goto cons)
(defmacro ($goto label)
  (cons label world))

(define procedures '())

(define (define-proc name body)
  (define-enum (list name) (length procedures))
  (push! procedures (cons name body)))

(defmacro ok (string "OK."))

(defmacro start-msg
  (string "Somewhere nearby is Colossal Cave, where others have found fortunes in
treasure and gold, though it is rumored that some who enter are never
seen again.  Magic is said to work in the cave.  I will be your eyes
and hands.  Direct me with commands of one or two words.  I should
warn you that I look at only the first five letters of each word, so
you'll have to enter \"NORTHEAST\" as \"NE\" to distinguish it from
\"NORTH\".  Should you get stuck, type \"HELP\" for some general hints.
For information on how to end your adventure, etc., type \"INFO\".
                        -  -  -
The first adventure program was developed by Willie Crowther.
Most of the features of the current program were added by Don Woods.
This Unlambda version was done by Kunihiko Sakamoto (irorin@gmail.com).
It is based on the CWEB version written by Don Knuth."))

; The entry point of the program
(define-proc 'offer0
  '(lambda (world)
     (if (yes (string "Welcome to Adventure!!  Would you like instructions?")
              start-msg (K I))
         (let-world (($set-nth-hinted c6 (K c0))
                     ($set-limit (K (to-cons1 c1000))))
           ($goto mainloop))
         ($goto mainloop))))

; 75 Simulate an adventure, going to quit when finished
(define-proc 'mainloop
  '(lambda (world)
     ; 153 Check for interference with the proposed move to newloc
     (cond ((and (not $not-closing) (< $newloc min-in-cave) (nonzero? $newloc))
            (let-world ((panic-at-closing-time world)
                        ($set-newloc (K $location)))
              ($goto move-dwarves)))
           ((= $newloc $location)
            ($goto move-dwarves))
           (else
            (begin
              ((<= $newloc max-pirate-loc)
               c5
               (lambda (lst)
                 (lst (lambda (dwf rest)
                        (if (and (= (odloc dwf) $newloc) (dseen dwf))
                            (let-world (($set-newloc (K $location)))
                              ((print "A little dwarf with a big knife blocks your way.\n")
                               $return ($goto move-dwarves)))
                            rest))))
               (cdr $dwarf))
              (let-world (($set-location (K $newloc)))
                ($goto move-dwarves)))))))

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
             ((= $mot BACK)
              ($goto go-back))
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

; 76 listen()
(define-proc 'call-listen
  '(lambda (world)
     (let ((words listen)
           (cont $word12))
       (let-world (($set-word12 (K words)))
	 ($goto cont)))))

; 76 pre_parse:
(define-proc 'pre-parse
  '(lambda (world)
     (let-world (($set-word12 clear-special-word))
       ((lambda (next)
          (let-world (($set-turns cons1))
            (if (= $verb SAY)
                (if (word? (cdr $word12))
                    (next ($set-verb (K ABSTAIN)))
                    ($goto transitive))
                (next world))))
        (lambda (world)
          (let-world (($set-foobar (lambda (fb) (fb I c0))))
            ($goto clocks-and-lamp)))))))

(defmacro (clear-special-word word12)
  (word12
   (lambda (w1 w2)
     (let ((clear (lambda (w)
                    (if (special-word? w)
                        (make-non-word (car w))
                        w))))
       (cons (clear w1) (clear w2))))))

(defmacro ($try-motion m)
  (goto try-move (set-mot world (K m))))
(defmacro $stay-put ($try-motion NOWHERE))
(defmacro ($report str)
  (str #\newline
   (goto get-user-input world)))

; 83 Handle additional special cases of input
(define-proc 'handle-special-inputs
  '(lambda (world)
     ($word12
      (lambda (word1 word2)
        ; BUG: "h2o" should not be a synonym of "water" here
        (cond ((and (motion? word1) (= (word-meaning word1) ENTER))
               (cond ((or (and (noun? word2) (= (word-meaning word2) WATER))
                          (and (motion? word2) (= (word-meaning word2) STREAM)))
                      (if (water-here world)
                          ($report (string "Your feet are now wet."))
                          ($default-to GO)))
                     ((pair? word2)
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

; 80 Give advice about going WEST
(define-proc 'parse-label
  '(lambda (world)
     (let ((word1 (car $word12)))
       (if (and (motion? word1)
                (= (word-meaning word1) W)
                (word-aux word1)
                (cons1? $west-count))
           (let-world (($set-west-count 1-of-1))
             (begin
               ((not (cons1? $west-count))
                (string " If you prefer, simply type W rather than WEST.\n")
                I)
               ($goto look-at-word1)))
           ($goto look-at-word1)))))

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
            ((nth
              (word-type word1)
              (list 
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
                   (cond ((= $verb SAY)
                          ($goto (if (pair? word2) transitive intransitive)))
                         ((pair? word2)
                          ($goto shift))
                         (else
                          ($goto (ifnonzero $obj transitive intransitive))))))
               (lambda (_)  ; message
                 ($report (nth (word-meaning word1) message)))))
             I)
            (goto cycle (unknown-word world)))))))

; 78 case object_type:
(define-proc 'handle-object-word
  '(lambda (world)
     (if (pair? (cdr $word12))
         ($goto shift)
         (if (nonzero? $verb)
             ($goto transitive)
             (begin
               (print "What do you want to do with the ")
               ((word-letters (car $word12)) I)
               (print "?\n")
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
		       get-object  ;WAVE
		       get-object  ;CALM
		       report-default  ;GO
		       report-default  ;RELAX
		       transitive  ;POUR
		       intransitive-eat  ;EAT
		       transitive  ;DRINK
		       get-object  ;RUB
		       get-object  ;TOSS
		       get-object  ;WAKE
		       get-object  ;FEED
		       transitive  ;FILL
		       get-object  ;BREAK
		       transitive  ;BLAST
		       transitive  ;KILL
		       get-object  ;SAY
		       intransitive-read  ;READ
		       intransitive-feefie  ;FEEFIE
		       intransitive-brief  ;BRIEF
		       get-object  ;FIND
		       intransitive-inventory  ;INVENTORY
		       intransitive-score  ;SCORE
		       intransitive-quit  ;QUIT
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
		       transitive-wave  ;WAVE
		       report-default  ;CALM
		       report-default  ;GO
		       report-default  ;RELAX
		       transitive-pour  ;POUR
		       transitive-eat  ;EAT
		       transitive-drink  ;DRINK
		       transitive-rub  ;RUB
		       transitive-toss  ;TOSS
		       transitive-wake  ;WAKE
		       transitive-feed  ;FEED
		       transitive-fill  ;FILL
		       transitive-break  ;BREAK
		       transitive-blast  ;BLAST
		       transitive-kill  ;KILL
		       transitive-say  ;SAY
		       transitive-read  ;READ
		       report-default  ;FEEFIE
		       report-default  ;BRIEF
		       transitive-find  ;FIND
		       transitive-find  ;INVENTORY
		       report-default  ;SCORE
		       report-default  ;QUIT
		       )))))
           
(defmacro ($change-to v)
  (let-world (($set-oldverb (K $verb))
              ($set-verb (K v)))
    ($goto transitive)))

; 79 report_default:
(define-proc 'report-default
  '(lambda (world)
     (begin
       ((nth $verb default-msg) #\newline I)
       ($goto get-user-input))))
(defmacro ($default-to v)
  (goto report-default ($set-verb (K v))))

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
              (not (pair? (cdr $word12))))
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

; 85 Make special adjustments before looking at new input
(define-proc 'cycle2
  '(lambda (world)
     (let-world (($set-was-dark (K $dark))
                 ($set-rand cdr)
                 (adjust-knife-loc world)
                 (adjust-props-after-closed world)
                 ($set-word12 (K pre-parse)))
       ($goto call-listen))))

; 169 Make special adjustments before looking at new input
(defmacro adjust-knife-loc
  (lambda (world)
    (if (and (nonzero? $knife-loc)
             (not (= $knife-loc $location)))
        ($set-knife-loc (K limbo))
        world)))

(defsyntax (prop-after-close obj)
  `(if (nth ,obj ,(make-boolean-list '(BOTTLE SNAKE BIRD))) c1 c0))

; 182 Make special adjustments before looking at new input
(defmacro adjust-props-after-closed
  (lambda (world)
    (if $closed?
        (begin
          ((not (churchnum? ($prop-of OYSTER)))
           ($toting? OYSTER)
           (cadr (nth OYSTER $note)) #\newline I)
          (let loop ((lst $objects-toting)
                     (world world))
            (if (null? lst)
                world
                (if (churchnum? ($prop-of (car lst)))
                    (loop (cdr lst) world)
                    (let-world (($set-prop-of (car lst)
                                              (K (prop-after-close (car lst)))))
                      (loop (cdr lst) world))))))
        world)))

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
                  (let* ((bits $rand)
                         (r ((car bits) V ((cadr bits) V I)))
                         (world ($set-rand cddr)))
                    (begin
                      (r (= $location y2) $not-closing
                         (string "A hollow voice says \"PLUGH\".\n") I)
                      ($goto (if $dark
                                 get-user-input
                                 describe-objects)))))))))))

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
             (let-world ((if (or (churchnum? ($prop-of tt)) $closed?)
                             world
                             (spot-treasure tt world)))
               (begin
                 ($describe-single-object tt)
                 (loop world (cdr lst)))))))))

(defmacro (spot-treasure tt)
  (lambda (world)
    (let-world (($set-prop-of tt (K (cond ((= tt RUG) c1)
                                          ((= tt CHAIN) c1)
                                          (else c0))))
                ($set-tally pred))
      (if (and (= $tally $lost-treasures)
               (nonzero? $tally)
               (cons1? (c35 1-of-1 $limit)))
          ($set-limit (K (to-cons1 c35)))
          world))))

; 90 Make sure obj is meaningful at the current location
(define-proc 'check-object-location
  '(lambda (world)
     (cond ((and (= $obj GRATE))
            (let ((m (nth $location (list V          ; limbo
                                          DEPRESSION ; road
                                          V          ; hill
                                          V          ; house
                                          DEPRESSION ; valley
                                          V          ; forest
                                          V          ; woods
                                          DEPRESSION ; slit
                                          V          ; outside
                                          V          ; inside
                                          ENTRANCE   ; cobbles
                                          ENTRANCE   ; debris
                                          ENTRANCE   ; awk
                                          ENTRANCE   ; bird
                                          ENTRANCE)))) ; spit
              (if (churchnum? m)
                  ($try-motion m)
                  ($goto cant-see-it))))
           ((and (= $obj DWARF) $dwarf-here?)
            ($goto handle-object-word))
           ((and (= $obj PLANT) ($at-loc? PLANT2) (nonzero? ($prop-of PLANT2)))
            (goto handle-object-word ($set-obj (K PLANT2))))
           ((and (= $obj KNIFE) (= $location $knife-loc))
            (let-world (($set-knife-loc (K V)))
              ($report (string "The dwarves' knives vanish as they strike the walls of the cave."))))
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

; 192 Deal with death and resurrection
(define-proc 'dwarves-upset
  `(lambda (world)
     ((string "The resulting ruckus has awakened the dwarves.  There are now several\nthreatening little dwarves in the room with you!  Most of them throw\nknives at you!  All of them get you!\n")
      ($goto quit))))

; 92 case TAKE:
(define-proc 'intransitive-take
  '(lambda (world)
     (let ((objs $objects-here))
       (if (and (pair? objs) (null? (cdr objs)) (not $dwarf-here?))
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
           (if (churchnum? object)
               ($goto get-object)
               (goto transitive ($set-obj (K CHAIN))))
           (if (churchnum? object)
               (goto transitive ($set-obj (K object)))
               ($report (string "There is nothing here with a lock!")))))))

; 93 case READ:
(define-proc 'intransitive-read
  '(lambda (world)
     (if $dark
         ($goto get-object)
	 (let-world ((if ($here? MAG) ($set-obj (K MAG)) world)
		     (if ($here? TABLET)
			 (if (nonzero? $obj)
			     ($return ($goto get-object))
			     ($set-obj (K TABLET)))
			 world)
		     (if ($here? MESSAGE)
			 (if (nonzero? $obj)
			     ($return ($goto get-object))
			     ($set-obj (K MESSAGE)))
			 world)
		     (if (and $closed? ($toting? OYSTER))
			 ($set-obj (K OYSTER))
			 world))
	   (if (nonzero? $obj)
	       ($goto transitive)
	       ($goto get-object))))))

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

; 95 case SCORE:
(define-proc 'intransitive-score
  '(lambda (world)
     ((string "If you were to quit now, you would score ")
      (print-digit (sub (score world) c4)) I
      (string "\nout of a possible 350.\n") I
      (if (yes (string "Do you indeed wish to quit now?") ok ok)
          ($goto give-up)
          ($goto get-user-input)))))

; 95 case QUIT:
(define-proc 'intransitive-quit
  '(lambda (world)
     (if (yes (string "Do you really wish to quit now?") ok ok)
         ($goto give-up)
         ($goto get-user-input))))

; 95 give_up:
(define-proc 'give-up
  `(lambda (world)
     (let-world (($set-nth-hinted c8 (K c0)))
       ($goto quit))))

; 97 case SAY:
(define-proc 'transitive-say
  '(lambda (world)
     (let-world ((if (pair? (cdr $word12))
                     ($set-word12 (lambda (p) (cons (cdr p) (cdr p))))
                     world))
       (if (nth (word-meaning (car $word12)) magic-word-list)
           (let-world (($set-word12 (lambda (p) (cons (car p) V)))
                       ($set-obj (K NOTHING)))
             ($goto look-at-word1))
           (begin
             (print "Okay, \"")
             ((word-letters (car $word12)) I)
             (print "\".\n")
             ($goto get-user-input))))))

(add-unl-macro!
 'magic-word-list '()
 (make-boolean-list '(XYZZY PLUGH PLOVER FEEFIE)))

; 98 case EAT:
(define-proc 'transitive-eat
  '(lambda (world)
     (cond ((= $obj FOOD)
            (let-world (($destroy FOOD))
              ($report (string "Thank you, it was delicious!"))))
           ((eat-special? $obj)
            ($report (string "I think I just lost my appetite.")))
           (else
            ($goto report-default)))))

(defsyntax (eat-special? x)
  `(nth ,x ,(make-boolean-list
             '(BIRD SNAKE CLAM OYSTER DWARF DRAGON TROLL BEAR))))

; 99 case WAVE:
(define-proc 'transitive-wave
  '(lambda (world)
     (if (and (= $obj ROD)
              (or (= $location efiss)
                  (= $location wfiss))
              ($toting? ROD)
              $not-closing)
         (let-world (($set-prop-of CRYSTAL (lambda (x) (x (K c0) c1))))
           ($report (cadr ((ifnonzero ($prop-of CRYSTAL) I cdr)
                           (nth CRYSTAL $note)))))
         (if (or ($toting? $obj)
                 (and (= $obj ROD) ($toting? ROD2)))
             ($goto report-default)
             ($default-to DROP)))))

; 99 case BLAST:
(define-proc 'transitive-blast
  '(lambda (world)
     (if (and $closed? (churchnum? ($prop-of ROD2)))
         (let ((bonus (cond (($here? ROD2) (blast-msg1 c25))
                            ((= $location neend) (blast-msg2 c30))
                            (else (blast-msg3 c45)))))
           (let-world (($set-nth-hinted c9 (K bonus)))
             ($goto quit)))
         ($goto report-default))))

(defmacro blast-msg1
  (string "There is a loud explosion and you are suddenly splashed across the
walls of the room.\n"))
(defmacro blast-msg2
  (string "There is a loud explosion and a twenty-foot hole appears in the far
wall, burying the snakes in the rubble.  A river of molten lava pours
in through the hole, destroying everything in its path, including you!\n"))
(defmacro blast-msg3
  (string "There is a loud explosion, and a twenty-foot hole appears in the far
wall, burying the dwarves in the rubble.  You march through the hole
and find yourself in the main office, where a cheering band of
friendly elves carry the conquering adventurer off into the sunset.\n"))

; 99 case RUB:
(define-proc 'transitive-rub
  '(lambda (world)
     (if (= $obj LAMP)
         ($goto report-default)
         ($default-to TOSS))))

; 100 case FIND: case INVENTORY:
(define-proc 'transitive-find
  '(lambda (world)
     (cond (($toting? $obj)
            ($default-to TAKE))
           ($closed?
            ($report (string "I daresay whatever you want is around here somewhere.")))
           ((or ($at-loc? $obj)
                (and $object-in-bottle (= ($place-of BOTTLE) $location))
                (and (= $obj WATER) (water-here world))
                (and (= $obj OIL) (oil-here world))
                (and (= $obj DWARF) $dwarf-here?))
            ($report (string "I believe what you want is right here with you.")))
           (else
            ($goto report-default)))))

; 101 case BREAK:
(define-proc 'transitive-break
  '(lambda (world)
     (cond ((and (= $obj VASE) (zero? ($prop-of VASE)))
            ((string "You have taken the vase and hurled it delicately to the ground.\n")
             (goto smash (if ($toting? VASE) ($drop VASE $location) world))))
           ((not (= $obj MIRROR))
            ($goto report-default))
           ($not-closed
            ($report (string "It is too far up for you to reach.")))
           (else
            ((string "You strike the mirror a resounding blow, whereupon it shatters into a\nmyriad tiny fragments.\n")
             ($goto dwarves-upset))))))

; 101 smash:
(define-proc 'smash
  '(lambda (world)
     (let-world (($set-prop-of VASE (K c2))
                 ($set-base-of VASE (K VASE)))
       ($goto get-user-input))))

; 101 case WAKE:
(define-proc 'transitive-wake
  '(lambda (world)
     (if (and $closed? (= $obj DWARF))
         ((string "You prod the nearest dwarf, who wakes up grumpily, takes one look at\nyou, curses, and grabs for his axe.\n")
          ($goto dwarves-upset))
         ($goto report-default))))

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

; 106 case DRINK:
(define-proc 'transitive-drink
  '(lambda (world)
     ((lambda (next)
        (cond ((zero? $obj)
               (if (or (water-here world)
                       (and ($here? BOTTLE)
                            (zero? ($prop-of BOTTLE))))
                   (next world)
                   ($goto get-object)))
              ((= $obj WATER)
               (next world))
              (else
               ($default-to EAT))))
      (lambda (world)
        (if (and ($here? BOTTLE) (zero? ($prop-of BOTTLE)))
            (let-world (($set-prop-of BOTTLE (K c1))
                        ($drop WATER limbo))
              ($report (string "The bottle of water is now empty.")))
            ($goto report-default))))))

; 107 case POUR:
(define-proc 'transitive-pour
  '(lambda (world)
     ((lambda (next)
        (if (or (zero? $obj) (= $obj BOTTLE))
            (cond ((zero? ($prop-of BOTTLE))
                   (next ($set-obj (K WATER))))
                  ((= ($prop-of BOTTLE) c2)
                   (next ($set-obj (K OIL))))
                  (else
                   ($goto get-object)))
            (next world)))
      (lambda (world)
        (cond ((not ($toting? $obj))
               ($goto report-default))
              ((and (not (= $obj WATER)) (not (= $obj OIL)))
               ($report (string "You can't pour that.")))
              (else
               (let-world (($set-prop-of BOTTLE (K c1))
                           ($drop $obj limbo))
                 (cond ((= $location ($place-of PLANT))
                        (water-plant world))
                       ((= $location ($place-of DOOR))
                        (oil-door world))
                       (else
                        ($report (string "Your bottle is empty and the ground is wet.")))))))))))

; 108 Try to water the plant
(defmacro water-plant
  (lambda (world)
    (if (= $obj WATER)
        ((nth ($prop-of PLANT) (cdr (nth PLANT $note))) #\newline I
         (let-world (($set-prop-of PLANT (lambda (n) (if> n c2 c0 (+ n c2))))
                     ($set-prop-of PLANT2 (K (nth ($prop-of PLANT)
                                                  (list c0 c0 c1 c1 c2)))))
           $stay-put))
        ($report (string "The plant indignantly shakes the oil off its leaves and asks, \"Water?\"")))))

; 109 Pour water or oil on the door
(defmacro oil-door
  (lambda (world)
    (if (= $obj WATER)
        (let-world (($set-prop-of DOOR (K c0)))
          ($report (string "The hinges are quite thoroughly rusted now and won't budge.")))
        (let-world (($set-prop-of DOOR (K c1)))
          ($report (string "The oil has freed up the hinges so that the door will now open."))))))

; 110 case FILL:
(define-proc 'transitive-fill
  '(lambda (world)
     (cond ((= $obj VASE)
            (fill-vase world))
           ((not ($here? BOTTLE))
            ($goto (if (= $obj NOTHING) get-object report-default)))
           ((and (not (= $obj NOTHING)) (not (= $obj BOTTLE)))
            ($goto report-default))
           ((not $bottle-empty)
            ($report (string "Your bottle is already full.")))
           ((no-liquid-here world)
            ($report (string "There is nothing here with which to fill the bottle.")))
           (else
            (let ((oil (oil-here world)))
              (let-world (($set-prop-of BOTTLE (K (if oil c2 c0)))
                          (if ($toting? BOTTLE)
                              ($carry (if oil OIL WATER))
                              world))
                ((string "Your bottle is now full of ")
                 (if oil (string "oil") (string "water"))
                 (string ".\n") I
                 ($goto get-user-input))))))))

; 111 Try to fill the vase
(defmacro fill-vase
  (lambda (world)
    (cond ((no-liquid-here world)
           ($report (string "There is nothing here with which to fill the vase.\n")))
          (($toting? VASE)
           ((string "The sudden change in temperature has delicately shattered the vase.\n")
            ($goto smash)))
          (else
           ($default-to DROP)))))

; 112 case TAKE:
(define-proc 'transitive-take
  '(lambda (world)
     (begin
       (($toting? $obj)  ; already carrying it
	$return ($goto report-default))
       ((nonzero? ($base-of $obj))  ; it is immovable
	(immovable-msg world) #\newline
	$return ($goto get-user-input))
       (let-world ((take-liquid world))
	 (begin
	   ((carrying-too-many? world)
	    (string "You can't carry anything more.  You'll have to drop something first.\n")
	    $return ($goto get-user-input))
	   (let-world ((take-bird world)
		       (take-cage-bird world)
		       ($carry $obj)
		       (take-liquid-in-bottle world))
	     ($report ok)))))))

(defmacro (carrying-too-many? world)
  (let loop ((lst (cdr (place world)))
             (n c1)
             (count (to-cons1 c6)))
    (lst
     (lambda (hd tl)
       (let ((next (loop tl (succ n))))
         (cond ((or (churchnum? hd) (= n WATER) (= n OIL)) (next count))
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
  (lambda (world)
    (if (or (= $obj WATER) (= $obj OIL))
        (if (and ($here? BOTTLE) $object-in-bottle)
            ($set-obj (K BOTTLE))
            (let-world (($set-obj (K BOTTLE)))
              (if ($toting? BOTTLE)
                  ($return ($change-to FILL))
                  ((string "You have nothing in which to carry it.\n")
                   $return ($goto get-user-input)))))
        world)))

(defmacro take-liquid-in-bottle
  (lambda (world)
    (if (and (= $obj BOTTLE) (not $bottle-empty))
        ($carry (ifnonzero ($prop-of BOTTLE) OIL WATER))
        world)))

; 114 Check special cases for taking a bird
(defmacro take-bird
  (lambda (world)
    (if (and (= $obj BIRD) (zero? ($prop-of BIRD)))
	(begin
	  (($toting? ROD)
	   (string "The bird was unafraid when you entered, but as you approach it becomes\ndisturbed and you cannot catch it.\n")
	   $return ($goto get-user-input))
	  (if ($toting? CAGE)
	      ($set-prop-of BIRD (K c1))
	      ((string "You can catch the bird, but you cannot carry it.\n")
	       $return ($goto get-user-input))))
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
                ($report ok)))
             (else
              (let-world ((drop-liquid world)
                          ($drop $obj $location))
                ($report ok)))))))

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
    (cond (($here? SNAKE)
           (let-world (($destroy SNAKE)
                       ($set-prop-of SNAKE (K c1))
                       ($set-prop-of BIRD (K c0))
                       ($drop BIRD $location))
             ((string "The little bird attacks the green snake, and in an astounding flurry\ndrives the snake away.\n")
              ($goto (if $not-closed get-user-input dwarves-upset)))))
          ((and ($at-loc? DRAGON) (zero? ($prop-of DRAGON)))
           (let-world (($destroy BIRD)
                       ($set-prop-of BIRD (K c0))
                       (if (= ($place-of SNAKE) hmk)
                           ($set-lost-treasures succ)
                           world))
             ($report (string "The little bird attacks the green dragon, and in an astounding flurry\ngets burnt to a cinder.  The ashes blow away."))))
          (else
           (let-world (($set-prop-of BIRD (K c0))
                       ($drop BIRD $location))
             ($report ok))))))

; 121 Check special cases for dropping the vase
(defmacro drop-vase
  (lambda (world)
    (let ((pillow-here (= ($place-of PILLOW) $location)))
      (let-world (($set-prop-of VASE (K (if pillow-here c0 c2)))
                  (if pillow-here world ($set-base-of VASE (K VASE)))
                  ($drop VASE $location))
        ($report (nth (if pillow-here c1 c3) (nth VASE $note)))))))

; 122 case TOSS:
(define-proc 'transitive-toss
  '(lambda (world)
     (let-world ((if (and (= $obj ROD) ($toting? ROD2) (not ($toting? ROD)))
                     ($set-obj (K ROD2))
                     world))
       (cond ((not ($toting? $obj))
              ($goto report-default))
             ((and (treasure? $obj) ($at-loc? TROLL))
              (throw-troll world))
             ((and (= $obj FOOD) ($here? BEAR))
              (let-world (($set-obj (K BEAR)))
                ($change-to FEED)))
             ((not (= $obj AXE))
              ($change-to DROP))
             ($dwarf-here?
              (throw-dwarf world))
             ((and ($at-loc? DRAGON) (zero? ($prop-of DRAGON)))
              (let-world (($drop AXE $location))
                ((string "The axe bounces harmlessly off the dragon's thick scales.\n")
                 $stay-put)))
             (($at-loc? TROLL)
              (let-world (($drop AXE $location))
                ((string "The troll deftly catches the axe, examines it carefully, and tosses it\nback, declaring, \"Good workmanship, but it's not valuable enough.\"\n")
                 $stay-put)))
             ((and ($here? BEAR) (zero? ($prop-of BEAR)))
              (throw-bear world))
             (else
              (let-world (($set-obj (K NOTHING)))
                ($change-to KILL)))))))

; 123 Throw the axe at the bear
(defmacro throw-bear
  (lambda (world)
    (let-world (($drop AXE $location)
                ($set-prop-of AXE (K c1))
                ($set-base-of AXE (K AXE)))
      ; BUG: the axe should be described after the bear
      ($report (string "The axe misses and lands near the bear where you can't get at it.")))))

; 124 Snarf a treasure for the troll
(defmacro throw-troll
  (lambda (world)
    (let-world (($drop $obj limbo)
                ($destroy TROLL)
                ($destroy TROLL_)
                ($drop TROLL2 swside)
                ($drop TROLL2_ neside))
      ($report (string "The troll catches your treasure and scurries away out of sight.")))))

; 163 Throw the axe at a dwarf
(defmacro throw-dwarf
  (lambda (world)
    (let loop ((j c1))
      (if (= (dloc (nth j $dwarf)) $location)
          (let-world (($drop AXE $location))
            (let-rand r 67
              (if r
                  (begin
                    (if $dkill-panic
                        (print "You killed a little dwarf.\n")
                        (print "You killed a little dwarf.  The body vanishes in a cloud of greasy\nblack smoke.\n"))
                    (let-world (($set-nth-dwarf j
                                   (lambda (d) (make-dwarf limbo (odloc d) V)))
                                ($set-dkill-panic (K I)))
                      $stay-put))
                  ((string "You attack a little dwarf, but he dodges out of the way.\n")
                   $stay-put))))
          (loop (succ j))))))

; 125 case KILL:
(define-proc 'transitive-kill
  '(lambda (world)
     (let-world ((if (zero? $obj) (object-to-attack world) world))
       (cond ((= $obj BIRD)
              (attack-bird world))
             ((= $obj DRAGON)
              (if (zero? ($prop-of DRAGON))
                  (attack-dragon world)
                  ($report (string "For crying out loud, the poor thing is already dead!"))))
             ((or (= $obj CLAM) (= $obj OYSTER))
              ($report (string "The shell is very strong and impervious to attack.")))
             ((= $obj SNAKE)
              ($report (string "Attacking the snake both doesn't work and is very dangerous.")))
             ((= $obj DWARF)
              (if $not-closed
                  ($report (string "With what?  Your bare hands?"))
                  ($goto dwarves-upset)))
             ((= $obj TROLL)
              ($report (string "Trolls are close relatives with the rocks and have skin as tough as\na rhinoceros hide.  The troll fends off your blows effortlessly.")))
             ((= $obj BEAR)
              ($report
               (nth ($prop-of BEAR)
                    (cons (string "With what?  Your bare hands?  Against HIS bear hands?")
                          (c2 (cons (string "The bear is confused; he only wants to be your friend."))
                              (cons (string "For crying out loud, the poor thing is already dead!") V))))))
             (else
              ($goto report-default))))))

; 126 See if there's a unique object to attack
(defmacro object-to-attack
  (lambda (world)
    (let ((lst ((if $dwarf-here? (icons DWARF) I)
                ((if ($here? SNAKE) (icons SNAKE) I)
                 ((if (and ($at-loc? DRAGON) (zero? ($prop-of DRAGON)))
                      (icons DRAGON) I)
                  ((if ($at-loc? TROLL) (icons TROLL) I)
                   ((if (and ($here? BEAR) (zero? ($prop-of BEAR)))
                        (icons BEAR) I)
                    V)))))))
      (if (pair? lst)
          (if (pair? (cdr lst))
              ($return ($goto get-object))
              ($set-obj (K (car lst))))
          (let ((lst ((if (and ($here? BIRD) (not (= $oldverb TOSS)))
                          (icons BIRD) I)
                      ((if (or ($here? CLAM) ($here? OYSTER))
                           (icons CLAM) I)
                       V))))
            (if (pair? lst)
                (if (pair? (cdr lst))
                    ($return ($goto get-object))
                    ($set-obj (K (car lst))))
                ($return ($report (string "There is nothing here to attack.")))))))))

; 127 Dispatch the poor bird
(defmacro attack-bird
  (lambda (world)
    (if $not-closed
        (let-world (($destroy BIRD)
                    ($set-prop-of BIRD (K c0))
                    (if (= ($place-of SNAKE) hmk)
                        ($set-lost-treasures succ)
                        world))
          ($report (string "The little bird is now dead.  Its body disappears.")))
        ($report (string "Oh, leave the poor unhappy bird alone.")))))

; 128 Fun stuff for dragon
(define-proc 'attack-dragon-cont
  '(lambda (world)
     (if (special-word? (car $word12))
         (let-world (($set-prop-of DRAGON (K c2))
                     ($set-prop-of RUG (K c0))
                     ($set-base-of RUG (K NOTHING))
                     ($set-base-of DRAGON_ (K DRAGON_))
                     ($destroy DRAGON_)
                     ($set-base-of RUG_ (K RUG_))
                     ($destroy RUG_)
                     ($set-place move-to-scan2)
                     ($set-location (K scan2)))
           ((cadr (nth DRAGON $note)) #\newline
            $stay-put))
         ($goto pre-parse))))

(defrecmacro (move-to-scan2 places)
  (places
   (lambda (hd tl)
     (cons (if (or (= hd scan1) (= hd scan3))
               scan2
               hd)
           (move-to-scan2 tl)))))

(defmacro attack-dragon
  (lambda (world)
    (let-world (($set-verb (K ABSTAIN))
                ($set-obj (K NOTHING))
                ($set-word12 (K attack-dragon-cont)))
      ((string "With what?  Your bare hands?\n")
       ($goto call-listen)))))

; 129 case FEED:
(define-proc 'transitive-feed
  '(lambda (world)
     (cond ((= $obj BIRD)
            ($report (string "It's not hungry (it's merely pinin' for the fjords).  Besides, you\nhave no bird seed.")))
           ((= $obj TROLL)
            ($report (string "Gluttony is not one of the troll's vices.  Avarice, however, is.")))
           ((= $obj DRAGON)
            (if (zero? ($prop-of DRAGON))
                       ($report 
                        (string "There's nothing here it wants to eat (except perhaps you)."))
                       ($default-to EAT)))
           ((= $obj SNAKE)
            (if (and $not-closed ($here? BIRD))
                (let-world (($destroy BIRD)
                            ($set-prop-of BIRD (K c0))
                            ($set-lost-treasures succ))
                  ($report (string "The snake has now devoured your bird.")))
                ($report (string "There's nothing here it wants to eat (except perhaps you)."))))
           ((= $obj BEAR)
            (cond (($here? FOOD)
                   (let-world (($destroy FOOD)
                               ($set-prop-of BEAR (K c1))
                               ($set-prop-of AXE (K c0))
                               ($set-base-of AXE (K NOTHING)))
                     ($report (string "The bear eagerly wolfs down your food, after which he seems to calm\ndown considerably and even becomes rather friendly."))))
                  ((zero? ($prop-of BEAR))
                   ($report (string "There's nothing here it wants to eat (except perhaps you).")))
                  ((= ($prop-of BEAR) c3)
                   ($change-to EAT))
                  (else
                   ($goto report-default))))
           ((= $obj DWARF)
            (if ($here? FOOD)
                (let-world (($set-dflag succ))
                  ($report (string "You fool, dwarves eat only coal!  Now you've made him REALLY mad!")))
                ($goto report-default)))
           (else
            ($default-to CALM)))))

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
            ($report (ifnonzero ($prop-of DOOR) ok
                                (string "The door is extremely rusty and refuses to open."))))
           (else
            ($goto report-default)))))

; 180 Panic at closing time
(defmacro panic-at-closing-time
  (lambda (world)
    ((string "A mysterious recorded voice groans into life and announces:\n\"This exit is closed.  Please leave via main office.\"\n")
     (if $dkill-panic
         world
         (let-world (($set-clock2 (K (to-cons1 c15)))
                     ($set-dkill-panic (K I)))
           world)))))

; 131 Open/close grate
(defmacro open-close-grate
  (lambda (world)
    (if ($here? KEYS)
        (if $not-closing
            ((nth (+ ($prop-of GRATE) (if (= $verb OPEN) c2 c0))
                  (list (string "It was already locked.")
                        (string "The grate is now locked.")
                        (string "The grate is now unlocked.")
                        (string "It was already unlocked.")))
             #\newline
             (goto get-user-input
                   ($set-prop-of GRATE (K (if (= $verb OPEN) c1 c0)))))
            (goto get-user-input (panic-at-closing-time world)))
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

; 135 case READ:
(define-proc 'transitive-read
  '(lambda (world)
     (cond ($dark ($goto cant-see-it))
           ((= $obj MAG)
            ($report (string "I'm afraid the magazine is written in dwarvish.")))
           ((= $obj TABLET)
            ($report (string "\"CONGRATULATIONS ON BRINGING LIGHT INTO THE DARK-ROOM!\"")))
           ((= $obj MESSAGE)
            ($report (string "\"This is not the maze where the pirate hides his treasure chest.\"")))
           ((and (= $obj OYSTER) $closed? ($toting? OYSTER))
            (if (zero? (nth c7 $hinted))
                ($report (string "It says the same thing it did before."))
                (offer1 world)))
           (else
            ($goto report-default)))))

; offer oyster hint
(defmacro offer1
  (lambda (world)
    (if (yes (string "Hmmm, this looks like a clue, which means it'll cost you 10 points to\nread it.  Should I go ahead and read it anyway?")
             (string "It says, \"There is something strange about this place, such that one\nof the words I've always known now has a new effect.\"")
             ok)
        (let-world (($set-nth-hinted c7 (K c0))
                    (if (cons1? (c30 1-of-1 $limit))
                        ($set-limit (c300 cons1))
                        world))
          ($goto get-user-input))
        ($goto get-user-input))))

; 136 case FEEFIE:
(define-proc 'intransitive-feefie
  '(lambda (world)
     (if (= $foobar (word-aux (car $word12)))
         (if (< $foobar c3)
             (let-world (($set-foobar (lambda (x _ _) (succ x))))
               ($report ok))
             (let-world (($set-foobar (K c0)))
               (if (or (= ($place-of EGGS) giant)
                       (and ($toting? EGGS) (= $location giant)))
                   ($default-to WAVE)
                   (begin
                     ((nth (cond ((= $location giant) c0)
                                 (($here? EGGS) c1)
                                 (else c2))
                           (nth EGGS $note)) #\newline I)
                     (let-world ((if (and (zero? ($place-of EGGS))
                                          (zero? ($place-of TROLL))
                                          (zero? ($prop-of TROLL)))
                                     ($set-prop-of TROLL (K c1))
                                     world)
                                 ($drop EGGS giant))
                       ($goto get-user-input))))))
         (if (zero? $foobar)
             ($default-to WAVE)
             ($report (string "What's the matter, can't you read?  Now you'd best start over."))))))
         

; 143 Try to go back
(define-proc 'go-back
  '(lambda (world)
     ($oldlocs
      (lambda (oldloc oldoldloc)
        (let ((l (if (forced-move? oldloc) oldoldloc oldloc))
              (world ($set-oldlocs (lambda (p) (cons $location (car p))))))
          (if (= l $location)
              ((string "Sorry, but I no longer seem to remember how you got here.\n")
               ($goto mainloop))
              (let ((m ((nth $location back-table) l)))
                (if (churchnum? m)
                    (goto go-for-it ($set-mot (K m)))
                    ((string "You can't get there from here.\n")
                     ($goto mainloop))))))))))

; 146 Determine the next location, newloc
(define-proc 'go-for-it
  '(lambda (world)
     (let ((q (find-inst $mot (nth $location travels))))
       (if (not (pair? q))
           (report-inapplicable-motion world)
           (let-world ((apply-inst q world))
             (cond ((= $newloc ppass)
                    (go-ppass world))
                   ((= $newloc troll)
                    (go-troll world))
                   (else
                    ($goto mainloop))))))))

; 148 Report on inapplicable motion and continue
(defmacro (report-inapplicable-motion world)
  ((cond ((= $mot CRAWL)
          (string "Which way?"))
         ((or (= $mot XYZZY) (= $mot PLUGH))
          (string "Nothing happens."))
         ((or (= $verb FIND) (= $verb INVENTORY))
          (string "I can only tell you what you see as you move about and manipulate\nthings.  I cannot tell you where remote things are."))
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

; 149 Choose newloc via plover-alcove passage
(defmacro go-ppass
  (lambda (world)
    (let ((os $objects-toting))
      (if (or (null? os)
              (and (= (car os) EMERALD) (null? (cdr os))))
          (goto mainloop
                ($set-newloc (K (if (= $location alcove)
                                    proom alcove))))
          ((string "Something you're carrying won't fit through the tunnel with you.\nYou'd best take inventory and drop something.\n")
           (goto mainloop ($set-newloc (K $location))))))))

; 161 Possibly move dwarves and the pirate
(define-proc 'move-dwarves
  '(lambda (world)
     (if (and (<= $location max-pirate-loc) (nonzero? $location))
         (cond ((zero? $dflag)
                (goto commence (if>= $location min-lower-loc
                                     ($set-dflag (K c1))
                                     world)))
               ((= $dflag c1)
                (let-rand r 5
                  (if (if>= $location min-lower-loc r V)
                      ($goto see-first-dwarf)
                      ($goto commence))))
               (else
                ($goto move-dwarves-and-the-pirate)))
         ($goto commence))))

; 162 Advance dflag to 2
(define-proc 'see-first-dwarf
  '(lambda (world)
     (let* ((bits $rand)
            (try ((car bits succ I) (cadr bits c1 c0))))
       (let-world (($set-dflag (K c2))
                   ($set-rand cddr)
                   (try kill-random-dwarf world)
                   (init-dwarves world)
                   ($drop AXE $location))
         ((string "A little dwarf just walked around a corner, saw you, threw a little\naxe at you, cursed, and ran away.  (The axe missed.)\n")
          ($goto commence))))))

(defmacro kill-random-dwarf
  (lambda (world)
    (let ((n (random (to-cons1 c3) $rand))
          (world ($set-rand (c4 cdr))))
      (if (zero? n)
          world
          ($set-nth-dwarf (div (+ c2 n) c3) (set-dloc limbo))))))

(defmacro init-dwarves
  (lambda (world)
    ($set-dwarf
     (lambda (ds)
       (cons (car ds)
             (let loop ((lst (cdr ds)))
               (lst
                (lambda (dwarf rest)
                  (cons (let ((l (if (= (dloc dwarf) $location)
                                     nugget
                                     (dloc dwarf))))
                          (make-dwarf l l V))
                        (loop rest))))))))))

(defmacro print-dwarf-locations
  (lambda (world)
    (begin
      (print "\ndwarf locations:")
      (let loop ((lst $dwarf))
        (lst
         (lambda (hd tl)
           (#\space (if (dseen hd) #\* I) (nth (dloc hd) location-names)
            loop tl))))
      (#\newline I))))

(defsyntax (debug-print-dwarf world e)
  (if *debug-print-dwarf*
      `(begin
         (print-dwarf-locations ,world)
         ,e)
      e))

; 164 Move dwarves and the pirate
(define-proc 'move-dwarves-and-the-pirate
  '(lambda (world)
     (let-world ((cdr (c6 random-move-dwarf (cons c0 world))))
       (debug-print-dwarf world
         ($goto (if (dseen (car $dwarf))
                    pirate-follow
                    dwarves-follow))))))

(defmacro (random-move-dwarf pair)
  (pair
   (lambda (i world)
     (let* ((dwf (nth i $dwarf))
            (ploc (nth (dloc dwf) dwarf-moves)))
       (if (zero? (dloc dwf))
           (cons (succ i) world)
           (let* ((ploc2 (remove (lambda (dest)
                                   (or (and (zero? i) (> dest max-pirate-loc))
                                       (= dest (odloc dwf))))
                                 ploc))
                  (new-odloc (dloc dwf))
                  (new-dloc (if (null? ploc2) (odloc dwf)
                                (random-select ploc2 $rand)))
                  (new-dseen (or (= new-dloc $location)
                                 (= new-odloc $location)
                                 (and (dseen dwf)
                                      (>= $location min-lower-loc)))))
             (let-world (($set-nth-dwarf i (K (make-dwarf new-dloc
                                                          new-odloc
                                                          new-dseen)))
                         ($set-rand (c6 cdr)))
               (cons (succ i) world))))))))

; 167 Make dwarf j follow
(define-proc 'dwarves-follow
  '(lambda (world)
     (let ((env (c5 dwarf-follow (lambda (f) (f c1 c0 c0 c0 world)))))
       (env
        (lambda (_ dtotal attack stick world)
          (if (zero? dtotal)
              ($goto commence)
              (begin
                ((if (= dtotal c1)
                     (string "There is a threatening little dwarf")
                     ((string "There are ")
                      (nth dtotal (list I I #\2 #\3 #\4 #\5))
                      (string " threatening little dwarves")))
                 (string " in the room with you!\n") I)
                (if (zero? attack)
                    ($goto commence)
                    (let-world (($set-dflag (lambda (o) (if (= o c2) c3 o))))
                      (begin
                        ((if (= attack c1)
                             (string "One sharp nasty knife is thrown")
                             (#\space
                              (nth attack (list I I #\2 #\3 #\4 #\5))
                              (string " of them throw knives")))
                         (string " at you --- ") I)
                        (if (zero? stick)
                            ((if (= attack c1) (string "it misses!\n")
                                 (string "none of them hit you!\n"))
                             ($goto commence))
                            (begin
                              ((if (= stick c1)
                                   (if (= attack c1) (string "it gets you!\n")
                                       (string "one of them gets you!\n"))
                                   ((nth stick (list I I #\2 #\3 #\4 #\5))
                                    (string " of them get you!\n"))) I)
                              (goto death
                                    ($set-oldlocs
                                     (lambda (o) (cons (car o) $location))))))))))))))))


(defmacro (dwarf-follow env)
  (env
   (lambda (i dtotal attack stick world)
     (let ((dwf (nth i $dwarf)))
       (if (dseen dwf)
           (let-world (($set-nth-dwarf i (set-dloc $location)))
             (if (= (odloc dwf) $location)
                 (let* ((n (random (to-cons1 c4) $rand))
                        (next-stick
                         ((if< n (mul c3 (sub $dflag c2)) succ I) stick)))
                   (let-world (($set-knife-loc
                                (lambda (o) (and (churchnum? o) $location)))
                               ($set-rand (c5 cdr)))
                     (lambda (f)
                       (f (succ i) (succ dtotal) (succ attack) next-stick world))))
                 (lambda (f)
                   (f (succ i) (succ dtotal) attack stick world))))
           (lambda (f)
             (f (succ i) dtotal attack stick world)))))))

; 172 Make the pirate track you
(define-proc 'pirate-follow
  '(lambda (world)
     (let-world (($set-dwarf (lambda (ds) (cons (set-dloc $location (car ds))
                                                (cdr ds)))))
       (if (or (= $location max-pirate-loc) (churchnum? ($prop-of CHEST)))
           ($goto dwarves-follow)
           (let loop ((i min-treasure)
                      (k V))
             (cond ((<= i max-obj)
                    (if (and (not ($too-easy? i)) ($toting? i))
                        (snatch-all-treasures world)
                        (loop (succ i) (or k ($here? i)))))
                   ((and (= $tally (succ $lost-treasures))
                         (not k)
                         $pirate-not-spotted
                         (nonzero? ($prop-of LAMP))
                         ($here? LAMP))
                    (spot-pirate world))
                   (else
                    (let-rand r 20
                      (begin
                        ((and r (not (= (odloc (car $dwarf))
                                        (dloc (car $dwarf)))))
                         (string "There are faint rustling noises from the darkness behind you.\n") I)
                        ($goto dwarves-follow))))))))))

; 173 move_chest:
(define-proc 'move-chest
  '(lambda (world)
     (let-world (($set-dwarf (lambda (ds)
                               (cons (make-dwarf chest-loc chest-loc V)
                                     (cdr ds)))))
       (if $pirate-not-spotted
           (let-world (($drop CHEST chest-loc)
                       ($drop MESSAGE message-loc))
             ($goto dwarves-follow))
           ($goto dwarves-follow)))))

; 173 Take booty and hide it in the chest
(defmacro snatch-all-treasures
  (lambda (world)
    ((string "Out from the shadows behind you pounces a bearded pirate!  \"Har, har,\"\nhe chortles, \"I'll just take all this booty and hide it away with me\nchest deep in the maze!\"  He snatches your treasure and vanishes into\nthe gloom.\n")
     (let loop ((i min-treasure)
                (world world))
       (cond ((> i max-obj)
              ($goto move-chest))
             ((and (not ($too-easy? i))
                   (or (and (zero? ($base-of i)) (= ($place-of i) $location))
                       ($toting? i)))
              (loop (succ i) ($drop i chest-loc)))
             (else
              (loop (succ i) world)))))))

; 175 Let the pirate be spotted
(defmacro spot-pirate
  (lambda (world)
    ((string "There are faint rustling noises from the darkness behind you.  As you\nturn toward them, the beam of your lamp falls across a bearded pirate.\nHe is carrying a large chest.  \"Shiver me timbers!\" he cries, \"I've\nbeen spotted!  I'd best hie meself off to the maze to hide me chest!\"\nWith that, he vanishes into the gloom.\n")
     ($goto move-chest))))

; 178 Check the clocks and the lamp
(define-proc 'clocks-and-lamp
  '(lambda (world)
     (cond ((cons1? $clock1)
            (if (and (zero? $tally)
                     (>= $location min-lower-loc)
                     (not (= $location y2)))
                (let-world (($set-clock1 1-of-1))
                  (if (cons1? $clock1)
                      ($goto check-the-lamp)
                      ($goto warn-close)))
                ($goto check-the-lamp)))
           ((cons1? $clock2)
            (let-world (($set-clock2 1-of-1))
              (if (cons1? $clock2)
                  ($goto check-the-lamp)
                  ($goto close-the-cave))))
           (else
            ($goto check-the-lamp)))))

; 179 Warn that the cave is closing
(define-proc 'warn-close
  '(lambda (world)
     (let-world (($set-prop-of GRATE (K c0))
                 ($set-prop-of CRYSTAL (K c0))
                 (kill-all-dwarves world)
                 ($set-dkill-panic (K V))
                 ($destroy TROLL)
                 ($destroy TROLL_)
                 ($drop TROLL2 swside)
                 ($drop TROLL2_ neside)
                 (if (= ($prop-of BEAR) c3)
                     world
                     ($destroy BEAR))
                 ($set-prop-of CHAIN (K c0))
                 ($set-base-of CHAIN (K NOTHING))
                 ($set-prop-of AXE (K c0))
                 ($set-base-of AXE (K NOTHING)))
       ((string "A sepulchral voice, reverberating through the cave, says, \"Cave\nclosing soon.  All adventurers exit immediately through main office.\"\n")
        ($goto handle-special-inputs)))))

; 181 Close the cave
(define-proc 'close-the-cave
  '(lambda (world)
     (let-world (($drop BOTTLE neend) ($set-prop-of BOTTLE (K V))
                 ($drop PLANT neend)  ($set-prop-of PLANT (K V))
                 ($drop OYSTER neend) ($set-prop-of OYSTER (K V))
                 ($drop LAMP neend)   ($set-prop-of LAMP (K V))
                 ($drop ROD neend)    ($set-prop-of ROD (K V))
                 ($drop DWARF neend)  ($set-prop-of DWARF (K V))
                 ($drop MIRROR neend) ($set-prop-of MIRROR (K V))
                 ($set-location (K neend))
                 ($set-oldlocs (lambda (o) (cons neend (cdr o))))
                 ($drop GRATE swend)  ; prop[GRATE] still zero
                 ($drop SNAKE swend)  ($set-prop-of SNAKE (K V))
                 ($drop BIRD swend)   ($set-prop-of BIRD (K V))
                 ($drop CAGE swend)   ($set-prop-of CAGE (K V))
                 ($drop ROD2 swend)   ($set-prop-of ROD2 (K V))
                 ($drop PILLOW swend) ($set-prop-of PILLOW (K V))
                 ($drop MIRROR_ swend)
                 ($set-nth-hinted c9 (K c10)))
       (let loop ((lst $objects-toting)
                  (world world))
         (if (null? lst)
             ((string "The sepulchral voice intones, \"The cave is now closed.\"  As the echoes\nfade, there is a blinding flash of light (and a small puff of orange\nsmoke). . . .    Then your eyes refocus; you look around and find...\n")
              $stay-put)
             (loop (cdr lst) ($destroy (car lst))))))))

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

; 188 Deal with death and resurrection
(define-proc 'pitch-dark
  `(lambda (world)
     (let-world (($set-oldlocs (lambda (p) (cons (car p) $location))))
       ((string "You fell into a pit and broke every bone in your body!\n")
        ($goto death)))))

(defmacro max-deaths c3)

; 189 Deal with death and resurrection
(define-proc 'death
  `(lambda (world)
     (let-world (($set-death-count succ))
       (cond ((not $not-closing)
              ((string "It looks as though you're dead.  Well, seeing as how it's so close\nto closing time anyway, let's just call it a day.\n")
               ($goto quit)))
             ((not (and (yes (nth $death-count death-wishes-q)
                             (nth $death-count death-wishes-y)
                             ok)
                        (< $death-count max-deaths)))
              ($goto quit))
             (else
              (let-world ((if ($toting? LAMP)
                              (set-prop-of LAMP (K c0)
                                           ($drop LAMP road))
                              world)
                          ($drop WATER limbo)
                          ($drop OIL limbo)
                          (drop-items world $objects-toting)
                          ($set-location (K house))
                          ($set-oldlocs (lambda (p) (cons house (cdr p)))))
                ($goto commence)))))))

(defmacro death-wishes-q
  (list
   V
   (string "Oh dear, you seem to have gotten yourself killed.  I might be able to\nhelp you out, but I've never really done this before.  Do you want me\nto try to reincarnate you?")
   (string "You clumsy oaf, you've done it again!  I don't know how long I can\nkeep this up.  Do you want me to try reincarnating you again?")
   (string "Now you've really done it!  I'm out of orange smoke!  You don't expect\nme to do a decent reincarnation without any orange smoke, do you?")))

(defmacro death-wishes-y
  (list
   V
   (string "All right.  But don't blame me if something goes wr......\n                 --- POOF!! ---\nYou are engulfed in a cloud of orange smoke.  Coughing and gasping,\nyou emerge from the smoke and find....")
   (string "Okay, now where did I put my resurrection kit?....  >POOF!<\nEverything disappears in a dense cloud of orange smoke.")
   (string "Okay, if you're so smart, do it yourself!  I'm leaving!")))

(defrecmacro (drop-items world objs)
  (if (null? objs)
      world
      (objs
       (lambda (hd tl)
         (let-world (($drop hd (cdr $oldlocs)))
           (drop-items world tl))))))

; 151 Cross troll bridge if possible
(defmacro go-troll
  (lambda (world)
    (if (= ($prop-of TROLL) c1)
        (let-world (($drop TROLL swside)
                    ($drop TROLL_ neside)
                    ($set-prop-of TROLL (K c0))
                    ($destroy TROLL2)
                    ($destroy TROLL2_)
                    ($set-newloc (K $location)))
          ((string "The troll steps out from beneath the bridge and blocks your way.\n")
           ($goto mainloop)))
        (let-world (($set-newloc (K (if (= $location neside) swside neside)))
                    (if (zero? ($prop-of TROLL))
                        ($set-prop-of TROLL (K c1))
                        world))
          (if (not ($toting? BEAR))
              ($goto mainloop)
              (let-world (($set-prop-of BRIDGE (K c1))
                          ($set-prop-of TROLL (K c2))
                          ($drop BEAR $newloc)
                          ($set-base-of BEAR (K BEAR))
                          ($set-prop-of BEAR (K c3))
                          ($set-lost-treasures
                           (if (and (not (churchnum? ($prop-of SPICES)))
                                    (>= ($place-of SPICES) neside))
                               succ I))
                          ($set-lost-treasures
                           (if (and (not (churchnum? ($prop-of CHAIN)))
                                    (>= ($place-of CHAIN) neside))
                               succ I))
                          ($set-oldlocs (lambda (p) (cons (car p) $newloc))))
                ((string "Just as you reach the other side, the bridge buckles beneath the\nweight of the bear, who was still following you around.  You\nscrabble desperately for support, but as the bridge collapses you\nstumble back and fall into the chasm.\n")
                 ($goto death))))))))

; 195 Check if a hint applies, and give it if requested
(defmacro hint-thresh
  (list c4 c5 c8 c75 c25 c20))

(define-proc 'cycle
  '(lambda (world)
     (let ((hintid (nth $location room-hint)))
       (if (or (not (churchnum? hintid)) (zero? (nth hintid $hinted)))
           (let-world (($set-hint-count (K V)))
             ($goto cycle2))
           (let-world (($set-hint-count
                        (lambda (p)
                          (cons hintid
                                (cons1 (and (= (car p) hintid) (cdr p)))))))
             (if (>= (cons1-length (cdr $hint-count))
                     (nth (car $hint-count) hint-thresh))
                 (if ((nth (car $hint-count)
                           (list cave-hint
                                 bird-hint
                                 snake-hint
                                 twist-hint
                                 dark-hint
                                 witt-hint))
                      world)
                     (let-world ((offer world)
                                 ($set-hint-count (K V)))
                       ($goto cycle2))
                     (let-world ((if (= (car $hint-count) c1)
                                     world
                                     ($set-hint-count (K V))))
                       ($goto cycle2)))
                 ($goto cycle2)))))))

(defmacro cave-hint
  (lambda (world)
    (and (zero? ($prop-of GRATE)) (not ($here? KEYS)))))

(defmacro bird-hint
  (lambda (world)
    (and ($here? BIRD) (= $oldobj BIRD) ($toting? ROD))))

(defmacro snake-hint
  (lambda (world)
    (and ($here? SNAKE) (not ($here? BIRD)))))

(defmacro twist-hint
  (lambda (world)
    (and (not (let loop ((lst $place))
                (lst
                 (lambda (hd tl)
                   (or (= hd $location)
                       (= hd (car $oldlocs))
                       (= hd (cdr $oldlocs))
                       (loop tl))))))
         (pair? (cdr $objects-toting)))))

(defmacro dark-hint
  (lambda (world)
    (and (churchnum? ($prop-of EMERALD))
         (not (churchnum? ($prop-of PYRAMID))))))

(defmacro witt-hint
  (lambda (world) I))

(defmacro hint-cost-string
  (list #\2 #\2 #\2 #\4 #\5 #\3))

(defmacro hint-prompt
  (list (string "Are you trying to get into the cave?")
        (string "Are you trying to catch the bird?")
        (string "Are you trying to deal somehow with the snake?")
        (string "Do you need help getting out of the maze?")
        (string "Are you trying to explore beyond the Plover Room?")
        (string "Do you need help getting out of here?")))

(defmacro hints
  (list (string "The grate is very solid and has a hardened steel lock.  You cannot\nenter without a key, and there are no keys in sight.  I would recommend\nlooking elsewhere for the keys.")
        (string "Something seems to be frightening the bird just now and you cannot\ncatch it no matter what you try.  Perhaps you might try later.")
        (string "You can't kill the snake, or drive it away, or avoid it, or anything\nlike that.  There is a way to get by, but you don't have the necessary\nresources right now.")
        (string "You can make the passages look less alike by dropping things.")
        (string "There is a way to explore that region without having to worry about\nfalling into a pit.  None of the objects available is immediately\nuseful for discovering the secret.")
        (string "Don't go west.")))

(defmacro offer
  (lambda (world)
    (if (yes (nth (car $hint-count) hint-prompt)
             (string " I am prepared to give you a hint,")
             ok)
        ((string " but it will cost you ")
         (nth (car $hint-count) hint-cost-string)
         (string " points.  ") I
         (if (yes (string "Do you want the hint?")
                  (nth (car $hint-count) hints)
                  ok)
             (let-world ((if (cons1? (c30 1-of-1 $limit))
                             ($set-limit ((* c30 (nth (car $hint-count) $hinted)) cons1))
                             world)
                         ($set-nth-hinted (car $hint-count) (K c0)))
               world)
             world))
        world)))

(defmacro (treasure-score world)
  (let rec ((k min-treasure))
    (if (> k max-obj)
        c0
        (+ (cond ((not (churchnum? ($prop-of k)))
                  c0)
                 ((not (and (= ($place-of k) house) (zero? ($prop-of k))))
                  c2)
                 ((< k CHEST)
                  c12)
                 (else
                  (if< CHEST k c16 c14)))
           (rec (succ k))))))

(defrecmacro (hint-score lst)
  (if (null? lst)
      c0
      (lst
       (lambda (hd tl)
         (+ hd (hint-score tl))))))

; 197 score()
(defmacro score
  (lambda (world)
    (sub (+ (ifnonzero $dflag c25 c0)
            (treasure-score world)
            (* c10 (sub max-deaths $death-count))
            (if (= ($place-of MAG) witt) c1 c0)
            (if $not-closing c0 c25)
            (hint-score $hinted))
         c31)))

(defmacro class-score
  (list c35 c100 c130 c200 c250 c300 c330 c349 c1024))

(defmacro highest-class c8)

(defmacro class-message
  (list 
   (string "You are obviously a rank amateur.  Better luck next time.")
   (string "Your score qualifies you as a novice class adventurer.")
   (string "You have achieved the rating \"Experienced Adventurer\".")
   (string "You may now consider yourself a \"Seasoned Adventurer\".")
   (string "You have reached \"Junior Master\" status.")
   (string "Your score puts you in Master Adventurer Class C.")
   (string "Your score puts you in Master Adventurer Class B.")
   (string "Your score puts you in Master Adventurer Class A.")
   (string "All of Adventuredom gives tribute to you, Adventure Grandmaster!")))

(defmacro (rank-score score)
  (let loop ((n c0)
             (lst class-score))
    (if (< score (car lst))
        n
        (loop (succ n) (cdr lst)))))

(define-proc 'quit
  `(lambda (world)
     (let ((k (score world))
           (itoa (print-digit)))
       (begin
         (print "You scored ")
         ((itoa k) I)
         (print " point")
         ((if (= k c1) I #\s) I)
         (print " out of a possible 350, using ")
         ((itoa (cons1-length $turns)) I)
         (print " turn")
         ((if (cons1? (1-of-1 $turns)) #\s I) I)
         (print ".\n")
         (let ((rank (rank-score k)))
           (begin
             ((nth rank class-message) I)
             (print "\nTo achieve the next higher rating")
             (if (< rank highest-class)
                 (begin
                   (print ", you need ")
                   ((itoa (sub (nth rank class-score) k)) I)
                   (print " more point")
                   ((if (= (succ k) (nth rank class-score)) I #\s) I)
                   (print ".\n"))
                 (print " would be a neat trick!\nCongratulations!!\n"))
             V))))))

(define program-table
  (map (lambda (x) (cons (car x) (compile-to-string (cdr x))))
       (reverse procedures)))

(define (print-program-table-sizes)
  (let* ((lst (map (lambda (x) (cons (car x) (string-length (cdr x))))
		   program-table))
	 (lst (sort-by lst cdr)))
    (for-each (lambda (p)
		(print (cdr p) "\t" (car p)))
	      lst)))

(add-unl-macro!
 'program-table '() `(list ,@(map cdr program-table)))

(add-unl-macro!
 'label-names '()
 (cons 'list
       (map (lambda (x) `(string ,(symbol->string (car x))))
            program-table)))

(defmacro initial-label offer0)
