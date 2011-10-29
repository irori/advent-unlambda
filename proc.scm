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
                       ((zero? (nth BATTERIES $place))
                        (string ".  You'd best start wrapping this up, unless\nyou can find some fresh batteries.  I seem to recall that there's\na vending machine in the maze.  Bring some coins with you.\n"))
                       (else
                        (string ".  You'd best go back for those batteries.\n")))
                 (goto handle-special-inputs ($set-not-warned (K V)))))
               (else
                ($goto handle-special-inputs)))))))

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
                    (= $location (nth (word-meaning word2) $place)))
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
		       quit  ;WAVE
		       quit  ;CALM
		       report-default  ;GO
		       report-default  ;RELAX
		       transitive  ;POUR
		       intransitive-eat  ;EAT
		       transitive  ;DRINK
		       quit  ;RUB
		       quit  ;TOSS
		       quit  ;WAKE
		       quit  ;FEED
		       transitive  ;FILL
		       quit  ;BREAK
		       transitive  ;BLAST
		       transitive  ;KILL
		       quit  ;SAY
		       quit  ;READ
		       quit  ;FEEFIE
		       intransitive-brief  ;BRIEF
		       quit  ;FIND
		       intransitive-inventory  ;INVENTORY
		       quit  ;SCORE
		       quit  ;QUIT
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
		       quit  ;WAVE
		       quit  ;CALM
		       quit  ;GO
		       quit  ;RELAX
		       quit  ;POUR
		       transitive-eat  ;EAT
		       quit  ;DRINK
		       quit  ;RUB
		       quit  ;TOSS
		       quit  ;WAKE
		       quit  ;FEED
		       quit  ;FILL
		       quit  ;BREAK
		       quit  ;BLAST
		       quit  ;KILL
		       quit  ;SAY
		       quit  ;READ
		       quit  ;FEEFIE
		       report-default  ;BRIEF
		       quit  ;FIND
		       quit  ;INVENTORY
		       quit  ;SCORE
		       quit  ;QUIT
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

; 90 Make sure obj is meaningful at the current location
(define-proc 'check-object-location
  '(lambda (world)
     ; TODO: implement this
     ($goto cant-see-it)))

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
           (let* ((bas (nth (car lst) $base))
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
     (let ((object (cond ((or (= (nth GRATE $place) $location)
                              (= (nth GRATE_ $place) $location))
                          GRATE)
                         ((= (nth DOOR $place) $location) DOOR)
                         (($here? CLAM) CLAM)
                         (($here? OYSTER) OYSTER)
                         (else V))))
       (if ($here? CHAIN)
           (if (object I I)
               ($goto get-object)
               (goto transitive ($set-obj (K CHAIN))))
           (if (object I I)
               (goto transitive ($set-obj (K object)))
               ((string "There is nothing here with a lock!\n")
                ($goto get-user-input)))))))

; 94 case INVENTORY:
(define-proc 'intransitive-inventory
  '(lambda (world)
     (let ((lst $objects-toting))
       (begin
         (if (null? lst)
             (print "You're not carrying anything.\n")
             (begin
               (print "You are currently holding the following:\n")
               (for-each (lambda (o)
                           (#\space (nth o objname) #\newline I))
                         lst)))
         ($goto get-user-input)))))

; 95 case BRIEF:
(define-proc 'intransitive-brief
  '(lambda (world)
     ((string "Okay, from now on I'll only describe a place in full the first time\nyou come to it.  To get the full description, say \"LOOK\".\n")
      (goto get-user-input ($set-verbose (K V))))))

; 98 case EAT:
(define-proc 'transitive-eat
  '(lambda (world)
     (if (= $obj FOOD)
         ((string "Thank you, it was delicious!\n")
          (goto get-user-input ($destroy FOOD)))
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
             ((string "Your lamp has run out of power.\n")
              ($goto get-user-input))))))

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
          ((nonzero? (nth $obj $base))  ; it is immovable
           (string "You can't be serious!\n")
           ret ($goto get-user-input))
         (let-world ((take-bird world ret)
		     (take-cage-bird world)
		     ($carry $obj))
           ((string "OK.\n")
	    ($goto get-user-input))))))))

(defmacro take-cage-bird
  (lambda (world)
    (cond ((= $obj BIRD)
	   ($carry CAGE))
	  ((and (= $obj CAGE) (nonzero? ($prop-of BIRD)))
	   ($carry BIRD))
	  (else world))))

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

; 120 Check special cases for dropping the bird
(defmacro try-drop-bird
  (lambda (world ret)
    (if ($here? SNAKE)
	(let-world (($destroy SNAKE)
		    ($set-prop-of SNAKE (K c1))
		    ($set-prop-of BIRD (K c0)))
	  ((string "The little bird attacks the green snake, and in an astounding flurry\ndrives the snake away.\n")
	   ret (goto get-user-input ($drop BIRD $location))))
	I)))  ; TODO: handle dragon case

(defmacro drop-cage-bird
  (lambda (world)
    (cond ((= $obj BIRD)
	   ($set-prop-of BIRD (K c0)))
	  ((and (= $obj CAGE) (nonzero? ($prop-of BIRD)))
	   ($drop BIRD $location))
	  (else world))))

; 117 case DROP:
(define-proc 'transitive-drop
  '(lambda (world)
     (call/cc
      (lambda (ret)
        (begin
          ((not ($toting? $obj))
           ret ($goto report-default))
          ((= $obj BIRD)
           try-drop-bird world ret)
          (let-world ((drop-cage-bird world)
		      ($drop $obj $location))
            (begin
              (print "OK.\n")
              ($goto get-user-input))))))))

(defmacro open-close-grate
  (lambda (world)
    ((nth (+ ($prop-of GRATE) (if (= $verb OPEN) c2 c0))
	  (list (string "It was already locked.")
		(string "The grate is now locked.")
		(string "The grate is now unlocked.")
		(string "It was already unlocked.")))
     #\newline
     ($set-prop-of GRATE (K (if (= $verb OPEN) c1 c0))))))

; 130 case OPEN: calse CLOSE:
(define-proc 'transitive-open
  '(lambda (world)
     (if (= $obj GRATE)
         (if ($here? KEYS)
             (goto get-user-input (open-close-grate world))
             ((string "You have no keys!\n")
              ($goto get-user-input)))
         ($goto report-default))))

; 146 Determine the next location, newloc
(define-proc 'go-for-it
  '(lambda (world)
     (let ((q (find-inst $mot
                         (nth $location travels))))
       (if (not (pair? q))
           ((string "There is no way to ...\n")
            ($goto mainloop))
           (goto mainloop (apply-inst q world))))))

(define-proc 'quit
  '(lambda (world)
     ((string "\nquitting...\n") exit I)))

(define-proc 'give-up
  `(lambda (world)
     (exit (print "give-up: not implemented"))))

(define-proc 'death
  `(lambda (world)
     (exit (print "death: not implemented"))))

(define-proc 'pitch-dark
  `(lambda (world)
     (exit (print "'pitch-dark: not implemented"))))


(add-unl-macro!
 'program-table '()
 `(list ,@(map (lambda (x) (compile-to-string (if (undefined? x) 'V x)))
               (reverse procedures))))

(defmacro initial-pc mainloop)
