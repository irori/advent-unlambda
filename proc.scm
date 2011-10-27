#!/usr/bin/env gosh
(require "enum.scm")

(defmacro goto cons)
(defmacro (goto$ label)
  (cons label world))

(define procedures '())

(define (define-proc name body)
  (define-enum (list name) (length procedures))
  (set! procedures (cons body procedures)))

; 75 Simulate an adventure, going to quit when finished
(define-proc 'mainloop
  '(lambda (world)
     (let-world (($set-location (K $newloc)))
       (goto$ commence))))

; 141 Report the long description and continue
(defmacro handle-look
  (lambda (world)
    (let-world (($set-was-dark (K V))
		(set-nth$ set-visits $location (K V)))
      ((if (cons1? (1-of-1 $verbose))
	   ((string "Sorry, but I am not allowed to give more detail.  I will repeat the\nlong description of your location.\n")
	    ($set-verbose 1-of-1))
	   world)))))

; 75 try-move:
(define-proc 'try-move
  '(lambda (world)
     ((cond ((= $mot NOWHERE)
             (lambda (world) (goto$ mainloop)))
            ((= $mot LOOK)
             (lambda (world) (goto mainloop (handle-look world))))
            (else
             (lambda (world)
               (goto go-for-it
                     ($set-oldlocs (lambda (ol-ool)
				     (cons $location (car ol-ool))))))))
      ($set-newloc (K $location)))))

; 76 Get user input; goto try_move if motion is requested
(define-proc 'get-user-input
  '(lambda (world)
     (let-world (($set-verb (K ABSTAIN))
		 ($set-obj (K NOTHING)))
       (goto$ cycle))))

; 76 cycle:
(define-proc 'cycle
  '(lambda (world)
     (let ((words listen))
       (let-world (($set-was-dark (K (dark world)))
		   ($set-word12 (K words)))
	 (goto$ pre-parse)))))

; 76 pre_parse:
(define-proc 'pre-parse
  '(lambda (world)
     ; turns++
     (let ((old-limit $limit))
       (let-world (($set-limit (if (= (nth LAMP $prop) c1) 1-of-1 I))
		   (if (and (cons1? old-limit) (not (cons1? $limit)))
		       (begin
			 ((here? LAMP world)
			  (string "Your lamp has run out of power.\n") I)
			 (set-prop-of$ LAMP (K c0)))
		       world))
	 (goto$ look-at-word1)))))

; 76 shift:
(define-proc 'shift
  '(lambda (world)
     (goto look-at-word1 ($set-word12 (lambda (old) (icons (cdr old) V))))))

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
                 (if (or (toting? $obj world)
                         (at-loc? $obj world))
                     (goto$ handle-object-word)
                     (goto$ cant-see-it))))
             (lambda (_)  ; verb
	       (let-world (($set-verb (K (word-meaning word1))))
		 (if (word? word2)
		     (goto$ shift)
		     (goto$ (if $obj transitive intransitive)))))
             (lambda (_)  ; message
               (begin ((nth (word-meaning word1) $message) #\newline I)
                      (goto$ get-user-input)))
             I)
            (goto cycle (unknown-word world)))))))

; 78
(define-proc 'handle-object-word
  '(lambda (world)
     (if (word? (cdr $word12))
         (goto$ shift)
         (if (nonzero? $verb)
             (goto$ transitive)
             (begin
               ((string "What do you want to do with the ") I)
               ((word-letters (car $word12)) I)
               ((string "?\n") I)
               (goto$ cycle))))))

(define-proc 'intransitive
  '(lambda (world)
     (goto$ (nth $verb
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
     (goto$ (nth $verb
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
       (goto$ get-user-input))))

; 79 get_object:
(define-proc 'get-object
  '(lambda (world)
     (begin
       ((word-letters (car $word12)) I)
       ((string " what?\n") I)
       (goto$ cycle))))

; 79 cant_see_it:
(define-proc 'cant-see-it
  '(lambda (world)
     (if (and (or (= $verb FIND) (= $verb INVENTORY))
              (not (word? (cdr $word12))))
         (goto$ transitive)
         (begin
           ((string "I see no ") I)
           ((word-letters (car $word12)) I)
           ((string " here.\n") I)
           (goto$ get-user-input)))))

(defmacro (dark world)
  (and (not (lighted? $location))
       (or (zero? (nth LAMP $prop))
           (not (here? LAMP world)))))

(defmacro pitch-dark-msg
  (string "It is now pitch dark.  If you proceed you will most likely fall into a pit."))

; 86 Report the current state
(define-proc 'commence
  `(lambda (world)
     (let ((next (commence-sub world)))
       (if (and (dark world) (not (forced-move? $location)))
           (next pitch-dark-msg)
           (let ((selector (if (cons1? (nth $location $visits))
                               ->cdr ->car)))
             (next ((nth $location room-desc) selector)))))))
         

(defmacro increment-visits
  (lambda (world)
    (set-nth$ set-visits $location
	     (lambda (x)
	       (if (cons1? x)
		   (if (cons1? $verbose)
		       (1-of-1 x)
		       x)
		   (cons1 (cons1 (cons1 (cons1 V)))))))))

; 88 Describe the objects at this location
(define-proc 'describe-objects
  '(lambda (world)
     (begin
       (for-each (lambda (obj)
                   (let* ((bas (nth obj $base))
                          (tt (if (zero? bas) obj bas)))
                     ((nth (nth tt $prop) (nth tt $note))
                      #\newline I)))
                 (objects-here world))
       (goto get-user-input (increment-visits world)))))

(defmacro commence-sub
  (lambda (world p)
    (begin
      (#\newline I)
      (p #\newline I)
      (if (forced-move? $location)
	  (goto$ try-move)
	  (goto$ (if (dark world)
		     get-user-input
		     describe-objects))))))

; 92 case TAKE:
(define-proc 'intransitive-take
  '(lambda (world)
     (let ((objs (objects-here world)))
       (if (and (pair? objs) (null? (cdr objs)))  ; TODO: check dwarf
           (goto transitive ($set-obj (K (car objs))))
           (goto$ get-object)))))

; 92 case EAT:
(define-proc 'intransitive-eat
  '(lambda (world)
     (if (here? FOOD world)
         (goto transitive ($set-obj (K FOOD)))
         (goto$ get-object))))


; 93 case OPEN: case CLOSE:
(define-proc 'intransitive-open
  '(lambda (world)
     (let ((object (cond ((or (= (nth GRATE $place) $location)
                              (= (nth GRATE_ $place) $location))
                          GRATE)
                         ((= (nth DOOR $place) $location) DOOR)
                         ((here? CLAM world) CLAM)
                         ((here? OYSTER world) OYSTER)
                         (else V))))
       (if (here? CHAIN world)
           (if (object I I)
               (goto$ get-object)
               (goto transitive ($set-obj (K CHAIN))))
           (if (object I I)
               (goto transitive ($set-obj (K object)))
               ((string "There is nothing here with a lock!\n")
                (goto$ get-user-input)))))))

; 94 case INVENTORY:
(define-proc 'intransitive-inventory
  '(lambda (world)
     (let ((lst (objects-toting world)))
       (begin
         (if (null? lst)
             ((string "You're not carrying anything.\n") I)
             (begin
               ((string "You are currently holding the following:\n") I)
               (for-each (lambda (o)
                           (#\space (nth o objname) #\newline I))
                         lst)))
         (goto$ get-user-input)))))

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
          (goto get-user-input (destroy FOOD world)))
         (goto$ report-default))))

; 102 case ON:
(define-proc 'transitive-on
  '(lambda (world)
     (if (not (here? LAMP world))
         (goto$ report-default)
         (if (cons1? $limit)
             (let-world ((set-prop-of$ LAMP (K c1)))
               ((string "Your lamp is now on.\n")
                (goto$ (if (was-dark world)
			   commence
			   get-user-input))))
             ((string "Your lamp has run out of power.\n")
              (goto$ get-user-input))))))

; 102 case OFF:
(define-proc 'transitive-off
  '(lambda (world)
     (if (not (here? LAMP world))
         (goto$ report-default)
         (let-world ((set-prop-of$ LAMP (K c0)))
           (begin
             ((string "Your lamp is now off.\n") I)
             ((dark world) pitch-dark-msg #\newline I)
             (goto$ get-user-input))))))

; 112 case TAKE:
(define-proc 'transitive-take
  '(lambda (world)
     (call/cc
      (lambda (ret)
        (begin
          ((toting? $obj world)  ; already carrying it
           ret (goto$ report-default))
          ((nonzero? (nth $obj $base))  ; it is immovable
           (string "You can't be serious!\n")
           ret (goto$ get-user-input))
         (let-world ((take-bird world ret)
		     (take-cage-bird world)
		     (carry $obj world))
           (begin
             ((string "OK.\n") I)
             (goto$ get-user-input))))))))

(defmacro take-cage-bird
  (lambda (world)
    (cond ((= $obj BIRD)
	   (carry CAGE world))
	  ((and (= $obj CAGE) (nonzero? (nth BIRD $prop)))
	   (carry BIRD world))
	  (else world))))

; 114 Check special cases for taking a bird
(defmacro take-bird
  (lambda (world ret)
    (if (and (= $obj BIRD) (zero? (nth BIRD $prop)))
	(begin
	  ((toting? ROD world)
	   (string "The bird was unafraid when you entered, but as you approach it becomes\ndisturbed and you cannot catch it.\n")
	   ret (goto$ get-user-input))
	  (if (toting? CAGE world)
	      (set-prop-of$ BIRD (K c1))
	      ((string "You can catch the bird, but you cannot carry it.\n")
	       ret (goto$ get-user-input))))
	world)))

; 120 Check special cases for dropping the bird
(defmacro try-drop-bird
  (lambda (world ret)
    (if (here? SNAKE world)
	(let-world ((destroy SNAKE world)
		    (set-prop-of$ SNAKE (K c1))
		    (set-prop-of$ BIRD (K c0)))
	  ((string "The little bird attacks the green snake, and in an astounding flurry\ndrives the snake away.\n")
	   ret (goto get-user-input (drop BIRD $location world))))
	I)))  ; TODO: handle dragon case

(defmacro drop-cage-bird
  (lambda (world)
    (cond ((= $obj BIRD)
	   (set-prop-of$ BIRD (K c0)))
	  ((and (= $obj CAGE) (nonzero? (nth BIRD $prop)))
	   (drop BIRD $location world))
	  (else world))))

; 117 case DROP:
(define-proc 'transitive-drop
  '(lambda (world)
     (call/cc
      (lambda (ret)
        (begin
          ((not (toting? $obj world))
           ret (goto$ report-default))
          ((= $obj BIRD)
           try-drop-bird world ret)
          (let-world ((drop-cage-bird world)
		      (drop $obj $location world))
            (begin
              ((string "OK.\n") I)
              (goto$ get-user-input))))))))

(defmacro open-close-grate
  (lambda (world)
    ((nth (+ (nth GRATE $prop) (if (= $verb OPEN) c2 c0))
	  (list (string "It was already locked.")
		(string "The grate is now locked.")
		(string "The grate is now unlocked.")
		(string "It was already unlocked.")))
     #\newline
     (set-prop-of$ GRATE (K (if (= $verb OPEN) c1 c0))))))

; 130 case OPEN: calse CLOSE:
(define-proc 'transitive-open
  '(lambda (world)
     (if (= $obj GRATE)
         (if (here? KEYS world)
             (goto get-user-input (open-close-grate world))
             ((string "You have no keys!\n")
              (goto$ get-user-input)))
         (goto$ report-default))))

; 146 Determine the next location, newloc
(define-proc 'go-for-it
  '(lambda (world)
     (let ((q (find-inst $mot
                         (nth $location travels))))
       (if (not (pair? q))
           ((string "There is no way to ...\n")
            (goto$ mainloop))
           (goto mainloop (apply-inst q world))))))

(define-proc 'quit
  '(lambda (world)
     ((string "\nquitting...\n") exit I)))

(add-unl-macro!
 'program-table '()
 `(list ,@(map (lambda (x) (compile-to-string (if (undefined? x) 'V x)))
               (reverse procedures))))

(defmacro initial-pc mainloop)
